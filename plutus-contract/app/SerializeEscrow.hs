{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cardano.Api
import           Cardano.Api.Shelley        (PlutusScript (..))
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Short      as SBS
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as BS8
import qualified Plutus.V2.Ledger.Api       as Plutus
import           Codec.Serialise            (serialise)

import           EscrowContract             (validator)

main :: IO ()
main = do
    putStrLn "Serializing Escrow Contract..."

    let script        = Plutus.unValidatorScript validator
        scriptCBOR    = serialise script
        scriptSBS     = SBS.toShort $ LBS.toStrict scriptCBOR
        plutusScript  = PlutusScriptSerialised scriptSBS

    -- 1️⃣ Write .plutus (for cardano-cli)
    result <- writeFileTextEnvelope
        "escrow.plutus"
        (Just "Escrow Validator Script")
        plutusScript

    case result of
        Left err -> putStrLn $ "Error writing script: " ++ show err
        Right () -> do
            putStrLn "✅ escrow.plutus written"

            -- 2️⃣ Write Base64 (for Lucid)
            let base64 = B64.encode $ SBS.fromShort scriptSBS
            BS8.writeFile "escrow.base64" base64

            putStrLn "✅ escrow.base64 written"
            putStrLn ""
            putStrLn "Next steps:"
            putStrLn "  cardano-cli address build \\"
            putStrLn "    --payment-script-file escrow.plutus \\"
            putStrLn "    --testnet-magic 1 \\"
            putStrLn "    --out-file escrow.addr"
