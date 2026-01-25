{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : EscrowContract
Description : Two-party escrow smart contract for Cardano
License     : MIT

This Plutus smart contract implements a secure escrow system where:
- A Buyer locks funds with a specified Seller address and deadline
- The Buyer can release funds to the Seller at any time
- The Buyer can refund after the deadline passes
- The Seller cannot withdraw without Buyer's approval
-}

module EscrowContract where

import           PlutusTx.Prelude
import qualified PlutusTx
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified Plutus.V1.Ledger.Interval as Interval
import           Prelude                   (Show)
import           GHC.Generics              (Generic)
import qualified PlutusTx.Builtins         as Builtins

-- | Escrow Datum: stored on-chain with the locked UTxO
data EscrowDatum = EscrowDatum
    { buyer    :: PubKeyHash  -- ^ The buyer who locked the funds
    , seller   :: PubKeyHash  -- ^ The seller who will receive funds on release
    , deadline :: POSIXTime   -- ^ Deadline after which buyer can refund
    } deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''EscrowDatum
PlutusTx.makeLift ''EscrowDatum

-- | Escrow Redeemer: actions that can be performed
data EscrowAction
    = Release  -- ^ Buyer releases funds to seller
    | Refund   -- ^ Buyer refunds after deadline
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''EscrowAction
PlutusTx.makeLift ''EscrowAction

-- | Main validator logic
{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkEscrowValidator datum action ctx = case action of
    Release -> traceIfFalse "Release: Buyer signature required" buyerSigned &&
               traceIfFalse "Release: Seller must receive funds" sellerPaid
    Refund  -> traceIfFalse "Refund: Buyer signature required" buyerSigned &&
               traceIfFalse "Refund: Deadline not yet passed" deadlinePassed &&
               traceIfFalse "Refund: Buyer must receive funds" buyerPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Check if buyer signed the transaction
    buyerSigned :: Bool
    buyerSigned = txSignedBy info (buyer datum)

    -- Check if deadline has passed (for refund)
    deadlinePassed :: Bool
    deadlinePassed = Interval.contains 
                        (Interval.from (deadline datum)) 
                        (txInfoValidRange info)

    -- Get the value locked in the script being spent
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "Own input not found"
        Just i  -> txInInfoResolved i

    lockedValue :: Value
    lockedValue = txOutValue ownInput

    -- Check seller receives the locked value (minus fees handled by transaction)
    sellerPaid :: Bool
    sellerPaid = valuePaidTo info (seller datum) `geq` lockedValue

    -- Check buyer receives refund
    buyerPaid :: Bool
    buyerPaid = valuePaidTo info (buyer datum) `geq` lockedValue

-- | Typed validator
data Escrow
instance ValidatorTypes Escrow where
    type instance DatumType Escrow = EscrowDatum
    type instance RedeemerType Escrow = EscrowAction

typedValidator :: TypedValidator Escrow
typedValidator = mkTypedValidator @Escrow
    $$(PlutusTx.compile [|| mkEscrowValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @EscrowDatum @EscrowAction

-- | The compiled validator script
validator :: Validator
validator = validatorScript typedValidator

-- | The validator hash (script address)
valHash :: ValidatorHash
valHash = validatorHash typedValidator

-- | The script address on the blockchain
scrAddress :: Address
scrAddress = scriptHashAddress valHash

-- | Helper to create datum for a new escrow
mkEscrowDatum :: PubKeyHash -> PubKeyHash -> POSIXTime -> EscrowDatum
mkEscrowDatum buyerPkh sellerPkh deadlineTime = EscrowDatum
    { buyer    = buyerPkh
    , seller   = sellerPkh
    , deadline = deadlineTime
    }

{-
================================================================================
                            COMPILATION INSTRUCTIONS
================================================================================

1. SET UP PLUTUS ENVIRONMENT:
   - Install Nix: https://nixos.org/download.html
   - Clone plutus-apps: git clone https://github.com/input-output-hk/plutus-apps
   - Enter nix-shell: cd plutus-apps && nix-shell

2. CREATE CABAL PROJECT:
   Create a file named `escrow.cabal`:
   
   ```
   cabal-version: 2.4
   name:          escrow-contract
   version:       1.0.0
   
   library
     exposed-modules:  EscrowContract
     build-depends:    base ^>=4.14
                     , plutus-tx
                     , plutus-tx-plugin
                     , plutus-ledger-api
                     , plutus-script-utils
                     , bytestring
     default-language: Haskell2010
     ghc-options:      -Wall -fobject-code -fno-ignore-interface-pragmas
                       -fno-omit-interface-pragmas -fno-strictness
                       -fno-spec-constr -fno-specialise
   ```

3. COMPILE THE CONTRACT:
   ```bash
   cabal build
   ```

4. SERIALIZE THE VALIDATOR:
   Create a serialization script to export the compiled Plutus script:
   
   ```haskell
   import Cardano.Api
   import qualified Data.ByteString.Short as SBS
   import qualified Plutus.V2.Ledger.Api as Plutus
   
   main :: IO ()
   main = do
     let script = Plutus.unValidatorScript EscrowContract.validator
         scriptSBS = SBS.toShort . Plutus.serialiseToRawBytes $ script
     writeFileTextEnvelope "escrow.plutus" Nothing (PlutusScript PlutusScriptV2 scriptSBS)
   ```

5. DEPLOY TO TESTNET:
   ```bash
   # Get script address
   cardano-cli address build \
     --payment-script-file escrow.plutus \
     --testnet-magic 1 \
     --out-file escrow.addr
   
   cat escrow.addr  # This is your script address for frontend integration
   ```

================================================================================
                              FRONTEND INTEGRATION
================================================================================

Once deployed, provide the script address to integrate with the React frontend.
The frontend will use Lucid to:
1. Build transactions that lock funds to the script address with EscrowDatum
2. Build transactions to spend from the script with Release/Refund redeemer

Example Lucid integration (TypeScript):

```typescript
import { Lucid, Data, fromText } from "lucid-cardano";

// Your deployed script address
const SCRIPT_ADDRESS = "addr_test1...";

// Datum type matching Plutus
const EscrowDatum = Data.Object({
  buyer: Data.Bytes(),
  seller: Data.Bytes(),
  deadline: Data.Integer(),
});

// Create escrow (lock funds)
async function createEscrow(lucid: Lucid, sellerAddr: string, amount: bigint, deadlineMs: number) {
  const buyerPkh = lucid.utils.getAddressDetails(await lucid.wallet.address()).paymentCredential!.hash;
  const sellerPkh = lucid.utils.getAddressDetails(sellerAddr).paymentCredential!.hash;
  
  const datum = Data.to({
    buyer: buyerPkh,
    seller: sellerPkh,
    deadline: BigInt(deadlineMs),
  }, EscrowDatum);
  
  const tx = await lucid
    .newTx()
    .payToContract(SCRIPT_ADDRESS, { inline: datum }, { lovelace: amount })
    .complete();
  
  return tx.sign().complete();
}

// Release funds to seller
async function releaseEscrow(lucid: Lucid, utxo: UTxO) {
  const tx = await lucid
    .newTx()
    .collectFrom([utxo], Data.to(0n)) // 0 = Release redeemer
    .attachSpendingValidator(escrowScript)
    .addSigner(await lucid.wallet.address())
    .complete();
  
  return tx.sign().complete();
}
```

================================================================================
-}
