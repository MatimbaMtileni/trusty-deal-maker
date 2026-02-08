{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SerializeEscrow
Description : Serialize the escrow validator to a Plutus script file
License     : MIT

This executable serializes the compiled Plutus validator to a file
that can be used with cardano-cli for deployment.
-}

module Main where

import           Cardano.Api
import           Cardano.Api.Shelley        (PlutusScript (..))
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Short      as SBS
import qualified Plutus.V2.Ledger.Api       as Plutus
import           Codec.Serialise            (serialise)

import           EscrowContract             (validator)

-- | Main entry point - serializes the validator
main :: IO ()
main = do
    putStrLn "Serializing Escrow Contract..."
    
    -- Get the validator script
    let script = Plutus.unValidatorScript validator
    
    -- Serialize to CBOR
    let scriptCBOR = serialise script
        scriptSBS = SBS.toShort $ LBS.toStrict scriptCBOR
    
    -- Write to file in Plutus script format
    result <- writeFileTextEnvelope "escrow.plutus" (Just "Escrow Validator Script") $
        PlutusScriptSerialised scriptSBS
    
    case result of
        Left err -> putStrLn $ "Error writing script: " ++ show err
        Right () -> do
            putStrLn "Successfully wrote escrow.plutus"
            putStrLn ""
            putStrLn "To get the script address, run:"
            putStrLn "  cardano-cli address build \\"
            putStrLn "    --payment-script-file escrow.plutus \\"
            putStrLn "    --testnet-magic 1 \\"
            putStrLn "    --out-file escrow.addr"
