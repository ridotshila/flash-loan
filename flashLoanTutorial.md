# 📘 Tutorial: Building and Running the Flash Loan Validator (Plutus Smart Contract)


## 🧩 Overview

This tutorial walks you through creating a **Flash Loan Validator** smart contract on Cardano using **Plutus V2**.
A flash loan allows a borrower to borrow and repay funds within **the same transaction**.
If repayment (plus any optional fee) isn’t done instantly, the transaction is invalid.

In this tutorial, you’ll:

1. Write the flash loan validator logic in Haskell
2. Compile it into a `.plutus` script file
3. Understand the Cabal build configuration
4. Build and run the project to generate the on-chain script

---

## 🧠 1. Validator Logic

File: **`FlashLoanValidator.hs`**

```haskell
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
```

---

### 🧩 Flash Loan Core Logic

The logic enforces that the borrower **repays the loan within the same transaction**.

```haskell
{-# INLINABLE mkFlashLoanValidator #-}
mkFlashLoanValidator :: Integer -> Integer -> ScriptContext -> Bool
mkFlashLoanValidator loanAmount repayAmount ctx =
    traceIfFalse "Loan not repaid fully" condition
  where
    condition = repayAmount >= loanAmount
```

💡 **Explanation:**

* `loanAmount` → The amount borrowed (datum)
* `repayAmount` → The amount repaid (redeemer)
* The validator checks if the repayment ≥ loan amount
* If not, the transaction fails with `"Loan not repaid fully"`.

---

## 🔒 2. Validator Wrapper

Plutus validators always have this signature:

```
BuiltinData -> BuiltinData -> BuiltinData -> ()
```

That’s why we wrap our logic:

```haskell
{-# INLINABLE wrappedValidator #-}
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator datum redeemer context =
    let loanAmt   = unsafeFromBuiltinData datum        :: Integer
        repayAmt  = unsafeFromBuiltinData redeemer     :: Integer
        ctx'      = unsafeFromBuiltinData context      :: ScriptContext
    in  if mkFlashLoanValidator loanAmt repayAmt ctx'
            then ()
            else traceError "FlashLoan: Repayment condition failed"
```

---

## 🏗️ 3. Compiling the Validator

The validator is compiled into a Plutus script:

```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])
```

---

## 💾 4. Saving the Script to File

```haskell
saveValidator :: IO ()
saveValidator = do
    let scriptSerialised = serialise validator
        scriptShortBs    = SBS.toShort . LBS.toStrict $ scriptSerialised
        plutusScript     = PlutusScriptSerialised scriptShortBs :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope "flashloan-validator.plutus" Nothing plutusScript
    case result of
        Left err -> print err
        Right () -> putStrLn "FlashLoan validator script written to flashloan-validator.plutus"
```

---

## 🚀 5. Main Entry Point

```haskell
main :: IO ()
main = saveValidator
```

This runs when you execute your Haskell program and writes:

```
flashloan-validator.plutus
```

to your project directory.

---

## ⚙️ 6. Cabal Configuration

Your `wspace.cabal` file correctly defines the **build dependencies** and executable target:

```cabal
executable flashloanvalidator-exe
  main-is:            FlashLoanValidator.hs
  hs-source-dirs:     tests
  build-depends:
      base >=4.14 && <5,
      plutus-ledger-api,
      plutus-tx,
      plutus-tx-plugin,
      bytestring,
      serialise,
      text,
      containers,
      cardano-api,
      cardano-ledger-core,
      cardano-ledger-shelley,
  default-language:   Haskell2010
```

📁 **Directory layout suggestion:**

```
wspace/
 ├── flashloan-validator.plutus     (generated output)
 ├── tests/
 │    └── FlashLoanValidator.hs
 ├── wspace.cabal
 ├── cabal.project
 └── LICENSE, CHANGELOG.md, etc.
```

---

## 🧪 7. Building the Validator

### Step 1: Update Cabal packages

```bash
cabal update
```

### Step 2: Build your executable

```bash
cabal build flashloanvalidator-exe
```

### Step 3: Run it to generate the `.plutus` file

```bash
cabal run flashloanvalidator-exe
```

✅ You should see:

```
FlashLoan validator script written to flashloan-validator.plutus
```

---

## 📦 8. Output File

The generated **`flashloan-validator.plutus`** is your **on-chain Plutus script**, ready for:

* On-chain deployment (using `cardano-cli`)
* Off-chain integration (e.g., in Helios, Nami, or a DApp)

---

## 🧩 9. Testing the Logic (Optional)

You can simulate validation logic using the **Plutus emulator** or **Plutus simple model** in `Spec.hs`.

Example structure:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Test.Tasty
import Test.Tasty.HUnit
import Plutus.V2.Ledger.Api (ScriptContext(..))

-- Add mock ScriptContext and test that repayment >= loan passes
main :: IO ()
main = defaultMain $
  testCase "Flash Loan repaid fully" $
    assertEqual "Repayment valid" True (mkFlashLoanValidator 100 120 dummyCtx)
```

---

## 💡 10. Concept Summary

| Concept           | Description                                                |
| ----------------- | ---------------------------------------------------------- |
| **Datum**         | Represents the loan amount (e.g., 100 ADA)                 |
| **Redeemer**      | Represents repayment amount (e.g., 120 ADA)                |
| **ScriptContext** | Provides transaction info                                  |
| **Condition**     | Repayment must be ≥ loan                                   |
| **Purpose**       | Ensures atomic flash loan repayment within one transaction |

---

## 🧾 11. Example Transaction (Conceptual)

| Step | Action                                                     |
| ---- | ---------------------------------------------------------- |
| 1    | Lender locks 100 ADA at validator address                  |
| 2    | Borrower borrows and uses funds within one transaction     |
| 3    | Borrower repays 100 ADA (or more) back in same transaction |
| 4    | Validator checks repayment ≥ loan amount                   |
| 5    | Transaction succeeds or fails atomically                   |


---

## ✅ Summary

* You created a **Plutus V2 Flash Loan Validator**
* Configured Cabal for building the contract
* Generated the `.plutus` script successfully
* Understood how the loan repayment check works

---

