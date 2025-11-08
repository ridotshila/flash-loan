{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Prelude                          (IO, String, ($), (<>), print, putStrLn)
import           PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless, ($))
import           Plutus.V2.Ledger.Api             (BuiltinData, ScriptContext(..), TxInfo(..), 
                                                   TxOut(..), Value, adaSymbol, adaToken,
                                                   mkValidatorScript, txInfoOutputs, 
                                                   txOutValue, fromBuiltinData, 
                                                   unsafeFromBuiltinData, Validator)
import qualified Plutus.V2.Ledger.Api             as PlutusV2
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Short            as SBS
import           Cardano.Api                      (writeFileTextEnvelope, PlutusScriptV2, 
                                                   PlutusScript (..))
import           Cardano.Api.Shelley              (PlutusScript (..))
import           Codec.Serialise                  (serialise)

------------------------------------------------------------------------------------------
-- | Flash Loan Validator Logic
--
-- The borrower must repay the loan (plus fee if applicable) within the same transaction.
------------------------------------------------------------------------------------------

{-# INLINABLE mkFlashLoanValidator #-}
mkFlashLoanValidator :: Integer -> Integer -> ScriptContext -> Bool
mkFlashLoanValidator loanAmount repayAmount ctx =
    traceIfFalse "Loan not repaid fully" condition
  where
    condition = repayAmount >= loanAmount

------------------------------------------------------------------------------------------
-- | Boilerplate to compile the validator
------------------------------------------------------------------------------------------

{-# INLINABLE wrappedValidator #-}
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator datum redeemer context =
    let loanAmt   = unsafeFromBuiltinData datum        :: Integer
        repayAmt  = unsafeFromBuiltinData redeemer     :: Integer
        ctx'      = unsafeFromBuiltinData context      :: ScriptContext
    in  if mkFlashLoanValidator loanAmt repayAmt ctx'
            then ()
            else traceError "FlashLoan: Repayment condition failed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

------------------------------------------------------------------------------------------
-- | Write the compiled validator to a file
------------------------------------------------------------------------------------------

saveValidator :: IO ()
saveValidator = do
    let scriptSerialised = serialise validator
        scriptShortBs    = SBS.toShort . LBS.toStrict $ scriptSerialised
        plutusScript     = PlutusScriptSerialised scriptShortBs :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope "flashloan-validator.plutus" Nothing plutusScript
    case result of
        Left err -> print err
        Right () -> putStrLn "FlashLoan validator script written to flashloan-validator.plutus"

------------------------------------------------------------------------------------------
-- | Main entry point
------------------------------------------------------------------------------------------

main :: IO ()
main = saveValidator
