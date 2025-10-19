## 🧩 OVERVIEW

This Haskell program defines a **Cardano smart contract** (validator script) for a **Flash Loan**.
It uses the **Plutus V2 API**, and when built, it produces a `validator.plutus` file that can be deployed or tested on-chain.

The goal:
✅ Ensure that a borrower repays the borrowed amount *within the same transaction*, or else the transaction fails.

---

## ⚙️ SECTION 1 — Flash Loan Types

### Code:

```haskell
data FlashParams = FlashParams { owner :: V2.PubKeyHash }
```

* **FlashParams** → immutable contract parameters (set when deploying).

  * `owner` = admin or liquidity pool owner who must receive repayments.

```haskell
data FlashDatum = FlashDatum { borrower :: V2.PubKeyHash, amount :: Integer }
```

* **FlashDatum** → per-loan data stored on-chain.

  * `borrower`: address of the borrower.
  * `amount`: how much ADA (or lovelace) was borrowed.

```haskell
data FlashRedeemer = Borrow | Repay
```

* **FlashRedeemer** → defines user action.

  * `Borrow` = when borrower requests funds.
  * `Repay` = when borrower repays in the same transaction.

✅ `PlutusTx.makeLift` and `unstableMakeIsData` allow these custom types to be serialized for on-chain use.

---

## ⚙️ SECTION 2 — Validator Logic (`mkValidator`)

### Code:

```haskell
mkValidator :: FlashParams -> FlashDatum -> FlashRedeemer -> V2.ScriptContext -> Bool
```

This is the **core validation function**.

It takes:

1. `FlashParams` → contract setup values.
2. `FlashDatum` → loan data.
3. `FlashRedeemer` → action.
4. `ScriptContext` → full transaction context.

And returns:
✅ `True` if the transaction is valid, ❌ `False` if it should fail.

---

### Logic Explanation

```haskell
case r of
  Borrow -> traceIfFalse "Borrower not signed" borrowerSigned
  Repay  -> traceIfFalse "Repayment not verified" repaymentOk
```

* If **Borrow**, we only check that the **borrower signed the transaction**.
* If **Repay**, we check that repayment was made correctly.

---

#### Borrower Signature Check

```haskell
borrowerSigned = V2.txSignedBy info (borrower d)
```

Ensures the borrower’s public key is part of the transaction’s signatures — prevents others from taking loans.

---

#### Repayment Check

```haskell
repaymentOk =
  let inputsValue  = Contexts.valueSpent info
      outputsValue = Contexts.valuePaidTo info (owner p)
  in Value.geq outputsValue (Value.scale (amount d) (Value.lovelaceValueOf 1))
```

* Gets the **value spent and paid** in the transaction.
* Ensures that **the owner receives** at least the borrowed amount back.
* Uses `Value.geq` (greater or equal) to allow extra repayment (e.g., with fees).

---

## ⚙️ SECTION 3 — Validator Wrapper

### Code:

```haskell
validatorWrapper :: FlashParams -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
```

This function acts as a **bridge between on-chain and off-chain code**.
Plutus validators always receive `BuiltinData` values — raw serialized data.

The wrapper:

1. Converts (`unsafeFromBuiltinData`) each argument back to Haskell types.
2. Calls `mkValidator`.
3. Fails with `traceError` if validation fails.

---

## ⚙️ SECTION 4 — Compiling the Validator

### Code:

```haskell
validator :: FlashParams -> Validator
validator p =
  PlutusTx.validatorToScript $
    $$(PlutusTx.compile [|| \p' -> validatorWrapper p' ||])
    `PlutusTx.applyCode` PlutusTx.liftCode p
```

* `$$(PlutusTx.compile ...)` → compiles the Haskell code into on-chain Plutus Core.
* `liftCode` injects the `FlashParams` constant (owner PubKeyHash).
* Produces a compiled `Validator` object.

---

## ⚙️ SECTION 5 — Writing `validator.plutus`

### Code:

```haskell
writeValidator :: FilePath -> Validator -> IO ()
writeValidator fp v = do
    let scriptSBS = SBS.toShort . LBS.toStrict $ serialise $ V2.unValidatorScript v
    SBS.writeFile fp scriptSBS
```

This writes the compiled validator binary to disk as a `.plutus` file,
which can later be deployed or used in the Cardano CLI.

---

## ⚙️ SECTION 6 — `main` Function (Execution)

### Code:

```haskell
main :: IO ()
main = do
    let exampleOwner = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        params = FlashParams { owner = exampleOwner }
        val    = validator params
```

* Creates example `FlashParams` with a dummy public key hash (`ffffffff…`).
* Builds the validator.
* Writes the script to `validator.plutus`.

Then:

```haskell
let scriptHash = Utils.validatorHash val
let addr = Utils.validatorAddress scriptHash Nothing
```

* Calculates the **script hash** (unique fingerprint).
* Generates the **Cardano validator address** (where funds can be sent).

Finally:

```haskell
putStrLn "✅ Done! Flash Loan validator compiled successfully."
```

Outputs a summary and success message.

---

## 🧠 SUMMARY — What This Validator Does

✅ Ensures:

* The borrower’s signature is verified.
* Repayment occurs in the same transaction.
* Owner receives at least the borrowed amount.

❌ Transaction fails otherwise.

---

