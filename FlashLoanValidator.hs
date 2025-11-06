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
{-# LANGUAGE LambdaCase          #-}

module FlashLoanValidator where

-- On-chain libraries
import           PlutusTx                    (BuiltinData, compile, unstableMakeIsData)
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)

import           Plutus.V2.Ledger.Api        as PlutusV2
import           Plutus.V2.Ledger.Contexts   as Contexts
import           Ledger                      (PaymentPubKeyHash(..))
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value

-- Off-chain / serialisation
import qualified Plutus.V1.Ledger.Scripts    as ScriptsV1
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Short       as SBS
import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Encode.Pretty    as Pretty
import qualified Plutus.V1.Ledger.Api        as PlutusV1
import           Prelude                     (IO, print, ($), show)
import qualified Prelude

-- ========== Types ==========

-- Redeemer actions: Borrow -> must return funds + fee; Withdraw -> owner withdraws
data FlashAction = Borrow | Withdraw
PlutusTx.unstableMakeIsData ''FlashAction
PlutusTx.makeLift ''FlashAction

-- Validator parameters (on-chain constants)
data FlashParams = FlashParams
  { fpOwner        :: PaymentPubKeyHash  -- owner allowed to withdraw
  , fpAsset        :: (CurrencySymbol, TokenName) -- asset lent (token or ADA represented as an asset)
  , fpFeeNumerator :: Integer           -- fee numerator
  , fpFeeDenom     :: Integer           -- fee denominator
  }
PlutusTx.unstableMakeIsData ''FlashParams
PlutusTx.makeLift ''FlashParams

-- Datum is unit (we don't need additional stored state for this simplified example)
type FlashDatum = ()
PlutusTx.unstableMakeIsData ''()

-- ========== Helper functions ==========

{-# INLINABLE assetClassValueOf' #-}
assetClassValueOf' :: Value -> CurrencySymbol -> TokenName -> Integer
assetClassValueOf' v cs tn = valueOf v cs tn

{-# INLINABLE scriptOutputsValueAt #-}
-- Sum the Value locked at the script address (by comparing output addresses' credentials)
scriptOutputsValueAt :: ValidatorHash -> TxInfo -> Value
scriptOutputsValueAt vh txinfo =
    let outs = txInfoOutputs txinfo
        isAtScript o = case txOutAddress o of
                        Address (ScriptCredential s) _ -> s == vh
                        _                              -> False
        values = fmap txOutValue $ filter isAtScript outs
    in foldl (<>) mempty values

{-# INLINABLE findOwnInputValue #-}
-- get the value of the UTxO consumed at this script (we assume single input from this script)
findOwnInputValue :: ScriptContext -> Maybe Value
findOwnInputValue ctx =
    let info = scriptContextTxInfo ctx
    in do
      inp <- findOwnInput ctx
      pure $ txOutValue (txInInfoResolved inp)

{-# INLINABLE calcFee #-}
calcFee :: Integer -> Integer -> Integer -> Integer
calcFee numerator denom amount = (amount * numerator) `divide` denom

-- ========== Core validator logic ==========

{-# INLINABLE mkFlashLoanValidator #-}
mkFlashLoanValidator :: FlashParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkFlashLoanValidator params _bd redeemerBD ctxBD =
    let redeemer = PlutusTx.unsafeFromBuiltinData @FlashAction redeemerBD
        ctx      = PlutusTx.unsafeFromBuiltinData @ScriptContext ctxBD
        info     = scriptContextTxInfo ctx
        vh       = case scriptContextPurpose ctx of
                     Spending vh' -> case vh' of { ValidatorHash h -> ValidatorHash h } -- type match (keeps type)
                     _             -> traceError "Invalid purpose"
        -- safer: use own hash via findOwnInput, but we need ValidatorHash for address checks
        mInputVal = findOwnInputValue ctx
    in case redeemer of
         Borrow ->
           case mInputVal of
             Nothing -> traceError "no input from script"
             Just inpVal ->
               let (cs, tn) = fpAsset params
                   principal = assetClassValueOf' inpVal cs tn
                   fee = calcFee (fpFeeNumerator params) (fpFeeDenom params) principal
                   requiredReturn = principal + fee

                   outValAtScript = scriptOutputsValueAt (ownHash ctx) info
                   returnedAmt = assetClassValueOf' outValAtScript cs tn
               in if returnedAmt >= requiredReturn
                     then () -- ok: borrower repaid principal + fee back to script outputs
                     else traceError "flashloan not repaid with fee"

         Withdraw ->
           -- owner-only withdraw logic: only allow if signed by owner (off-chain will create single tx consuming the script)
           let ownerPkh = unPaymentPubKeyHash (fpOwner params)
           in if txSignedBy (scriptContextTxInfo ctx) ownerPkh
                then ()
                else traceError "owner signature required"

-- helper to get own validator hash from context
{-# INLINABLE ownHash #-}
ownHash :: ScriptContext -> ValidatorHash
ownHash ctx = case scriptContextPurpose ctx of
                Spending vh -> vh
                _           -> traceError "ownHash: not spending"

-- ========== Boilerplate to compile ==========

{-# INLINABLE wrapped #-}
wrapped :: FlashParams -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped = mkFlashLoanValidator

-- Here we produce a typed validator so that we can get address/hash etc.
data Flashing
instance Scripts.ValidatorTypes Flashing where
  type instance RedeemerType Flashing = FlashAction
  type instance DatumType Flashing = FlashDatum

typedValidator :: FlashParams -> Scripts.TypedValidator Flashing
typedValidator params =
  Scripts.mkTypedValidator @Flashing
    ($$(PlutusTx.compile [|| mkFlashLoanValidator ||])
       `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @FlashDatum @FlashAction

validator :: FlashParams -> Validator
validator = Scripts.validatorScript . typedValidator

validatorHash :: FlashParams -> ValidatorHash
validatorHash = Scripts.validatorHash . typedValidator

validatorAddress :: FlashParams -> Ledger.Address
validatorAddress = Scripts.validatorAddress . typedValidator

-- ========== Example params and serialisation ==========

-- Example constants (change to match your token and owner)
exampleOwner :: PaymentPubKeyHash
exampleOwner = PaymentPubKeyHash "0123456789abcdef0123456789abcdef0123456789abcdef01234567" -- placeholder, set real pkh

exampleCS :: CurrencySymbol
exampleCS = "ff" -- placeholder currency symbol for the token (use "" for ADA as special case)

exampleTN :: TokenName
exampleTN = "FLASH" -- example token name

exampleParams :: FlashParams
exampleParams = FlashParams
  { fpOwner = exampleOwner
  , fpAsset = (exampleCS, exampleTN)
  , fpFeeNumerator = 9    -- 0.9% fee (example)
  , fpFeeDenom = 1000
  }

-- Serialise to plutus file (serialise the Script)
-- NOTE: Uses Plutus.V1.Ledger.Scripts serialisation helpers
writePlutusToFile :: FilePath -> Validator -> IO ()
writePlutusToFile fp v = do
    let s = ScriptsV1.unValidatorScript $ PlutusV1.Validator $ PlutusTx.getPlc $ $$(PlutusTx.compile [|| id ||]) -- safe placeholder - we will use the concrete script below
    Prelude.putStrLn $ "Please use your build tool to serialise the validator."  -- fallback message
    -- In many build environments you will serialise differently, e.g. using plutus-tools' serialisation.

-- Instead of a fragile generic write above, here's a convenience main that prints the script hash & address:
main :: IO ()
main = do
  let v = validator exampleParams
      vh = validatorHash exampleParams
      addr = validatorAddress exampleParams
  Prelude.putStrLn $ "Validator hash: " <> show vh
  Prelude.putStrLn $ "Validator address: " <> show addr
  Prelude.putStrLn $ "To serialise to a .plutus file, use your toolchain (plutus-tools / cardano-cli) to convert the compiled script."    how to run it using demeter.run 
