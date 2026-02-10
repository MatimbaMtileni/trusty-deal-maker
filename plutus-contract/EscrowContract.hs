{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module EscrowContract where

import PlutusTx
import PlutusTx.Prelude
import Ledger
import Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V1.Ledger.Interval as Interval

data EscrowDatum = EscrowDatum
  { buyer    :: PubKeyHash
  , seller   :: PubKeyHash
  , deadline :: POSIXTime
  }

data EscrowAction = Release | Refund

PlutusTx.unstableMakeIsData ''EscrowDatum
PlutusTx.unstableMakeIsData ''EscrowAction
PlutusTx.makeLift ''EscrowDatum
PlutusTx.makeLift ''EscrowAction

{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkEscrowValidator dat action ctx =
  case action of
    Release ->
      traceIfFalse "Seller signature missing" sellerSigned &&
      traceIfFalse "Escrow value not paid to seller" paysSeller

    Refund  ->
      traceIfFalse "Buyer signature missing" buyerSigned &&
      traceIfFalse "Deadline not reached" deadlineReached &&
      traceIfFalse "Escrow value not refunded to buyer" paysBuyer
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sellerSigned :: Bool
    sellerSigned = txSignedBy info (seller dat)

    buyerSigned :: Bool
    buyerSigned = txSignedBy info (buyer dat)

    deadlineReached :: Bool
    deadlineReached =
      Interval.from (deadline dat) `Interval.contains`
        txInfoValidRange info

    ownInput :: TxOut
    ownInput =
      case findOwnInput ctx of
        Nothing -> traceError "Escrow input missing"
        Just i  -> txInInfoResolved i

    escrowValue :: Value
    escrowValue = txOutValue ownInput

    paysSeller :: Bool
    paysSeller = valuePaidTo info (seller dat) == escrowValue

    paysBuyer :: Bool
    paysBuyer = valuePaidTo info (buyer dat) == escrowValue
