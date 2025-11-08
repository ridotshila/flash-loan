# FlashLoan Validator on Cardano

This repository contains a **Flash Loan Validator** implemented in **Haskell** using the **Plutus V2 smart contract framework** for Cardano. The validator ensures that borrowers repay loans (plus an optional fee) within the same transaction, following the principles of flash loans in decentralized finance (DeFi).

---

## Overview

Flash loans are **unsecured, atomic loans** that must be borrowed and repaid within the same transaction. This repository provides a **Plutus validator** for Cardano that enforces repayment conditions.

The validator checks that:

- The loaned amount is repaid in full within the same transaction.  
- Optionally, a fee (e.g., 1%) can be added for real-world scenarios.  

---

## Features

- ✅ Plutus V2 Validator for Flash Loans  
- ✅ Loan repayment validation  
- ✅ Easy-to-compile Haskell code  
- ✅ Output script ready for deployment on Cardano testnets or mainnet  

---


