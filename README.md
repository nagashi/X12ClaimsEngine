# Healthcare Claims X12 Processing DSL

A Domain-Specific Language (DSL) for healthcare claims validation and X12 processing, implemented in Haskell.

## Overview

This project provides a type-safe, composable DSL for defining business rules for healthcare claims validation. It features both an internal DSL (embedded in Haskell) and an external DSL (text-based syntax for business analysts).

## Features

- **Type-Safe Internal DSL**: Uses GADTs to ensure rule correctness at compile time
- **Business-Friendly External DSL**: SQL-like syntax that non-programmers can read and write
- **Comprehensive Validation**: Support for amount checks, diagnosis codes, procedure codes, place of service, and claim types
- **Composable Rules**: Combine simple rules into complex validation logic using `AND`, `OR`, and `NOT`
- **Full Test Coverage**: Comprehensive test suite using Hspec

## Project Structure

```
claims-x12-dsl/
├── src/
│   ├── Claims/
│   │   ├── Types.hs           # Core domain types and Rule GADT
│   │   ├── Interpreter.hs     # Rule evaluation engine
│   │   ├── Rules.hs           # Example business rules
│   │   ├── Parser.hs          # External DSL parser (Parsec)
│   │   └── Parser/
│   │       └── AST.hs         # Abstract syntax tree for external DSL
│   └── Lib.hs                 # Main library exports
├── app/
│   └── Main.hs                # Example application
├── test/
│   └── Spec.hs                # Test suite
├── examples/
│   └── sample-rules.dsl       # Example external DSL rules
└── package.yaml               # Project dependencies
```

## Getting Started

### Prerequisites

You'll need Stack installed. If you don't have it:

**macOS:**
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

**Linux:**
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

**Windows:**
Download the installer from [haskellstack.org](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### Installation

1. **Clone or navigate to the project:**
   ```bash
   cd /path/to/claims-x12-dsl
   ```

2. **Build the project:**
   ```bash
   stack build
   ```
   
   This will:
   - Download and install the correct GHC version (9.6.6) if needed
   - Install all dependencies
   - Compile the library and executable
   - First build takes ~60 seconds; subsequent builds are much faster

3. **Verify the build:**
   ```bash
   stack test
   ```
   
   You should see:
   ```
   22 examples, 0 failures
   Finished in 0.0036 seconds
   ```

### Running the Application

**Run the example application:**
```bash
stack run
```

This validates a sample $75,000 inpatient claim and displays results:
```
Healthcare Claims X12 Processing DSL
====================================

Validating example claim:
Claim ID: "CLM001"
Amount: $75000.00
Type: Inpatient

Validation Results:
-------------------
Rule 1: FAIL - "Review Required: High value claim exceeds $50,000"
Rule 2: PASS
Rule 3: PASS
Rule 4: PASS
Rule 5: PASS
```

### Running Tests

**Run all tests:**
```bash
stack test
```

**Run tests with detailed output:**
```bash
stack test --test-arguments="--format=progress"
```

**Run tests with coverage:**
```bash
stack test --coverage
```

### Interactive Development

**Start a REPL (GHCi):**
```bash
stack ghci
```

Then you can interactively test the DSL:
```haskell
ghci> :load src/Claims/Types.hs
ghci> :load src/Claims/Interpreter.hs
ghci> import Data.Decimal
ghci> import Data.Time
-- Create a claim and test rules interactively
```

## Usage

### Internal DSL (Haskell)

```haskell
import Claims.Types
import Claims.Interpreter
import Data.Decimal (realFracToDecimal)
import Data.Time (fromGregorian)

-- Define a claim
claim = Claim
  { claimId = "CLM001"
  , patientId = "PAT12345"
  , providerId = "PRV98765"
  , serviceDate = fromGregorian 2025 1 5
  , totalAmount = realFracToDecimal 2 75000
  , diagnosisCodes = ["I21.0", "I25.10"]
  , procedureCodes = ["99223", "93000"]
  , placeOfService = "21"
  , claimType = Inpatient
  }

-- Define a rule
highValueRule :: Rule ValidationResult
highValueRule = 
  If (greaterThan 50000)
    (needsReview "High value claim exceeds $50,000")
    approve

-- Evaluate the rule
result = eval claim highValueRule
```

### External DSL (Business Analyst Syntax)

Create a rules file (e.g., `rules.dsl`):

```
RULE HighValueReview
DESCRIPTION "Flag high-value claims for manual review"
WHEN
  claim.amount > 50000
THEN
  REQUIRE_REVIEW "Claim exceeds high-value threshold"
END

RULE EmergencyRoomValidation
DESCRIPTION "Validate ER claims have appropriate diagnoses"
WHEN
  claim.place_of_service = "23" AND
  NOT (claim.has_diagnosis "S06" OR claim.has_diagnosis "I21")
THEN
  REJECT "Emergency room claim missing emergency diagnosis"
END
```

Parse and use the rules:

```haskell
import Claims.Parser

rulesText <- readFile "rules.dsl"
case parseRules rulesText of
  Left err -> print err
  Right parsedRules -> do
    let internalRules = map convertToInternalRule parsedRules
    let results = validateClaim claim internalRules
    print results
```

## Built-in Business Rules

The library includes several example business rules:

1. **High Value Claims Review**: Claims exceeding $50,000 require manual review
2. **Emergency Room Validation**: ER claims must have emergency diagnosis codes
3. **Inpatient Admission**: Inpatient claims must include admission procedure codes
4. **Complex Professional Claims**: Multi-condition validation for professional claims
5. **Outpatient Surgery Minimum**: Outpatient surgeries must meet minimum amounts

## External DSL Syntax

### Conditions

- Amount comparisons: `claim.amount > 50000`, `claim.amount BETWEEN 0 AND 100`
- Code checks: `claim.has_diagnosis "I21"`, `claim.has_procedure "99223"`
- Claim properties: `claim.type = "Inpatient"`, `claim.place_of_service = "23"`
- Logical operators: `AND`, `OR`, `NOT`

### Actions

- `APPROVE`: Automatically approve the claim
- `REJECT "message"`: Reject the claim with a reason
- `REQUIRE_REVIEW "message"`: Flag for manual review

## Dependencies

- **base**: Core Haskell library
- **text**: Efficient text handling
- **time**: Date/time support
- **Decimal**: Precise decimal arithmetic for currency
- **parsec**: Parser combinator library for external DSL
- **containers**: Data structures
- **hspec**: Testing framework
- **QuickCheck**: Property-based testing

## License

BSD-3-Clause

## Author

Based on the Healthcare Claims X12 Processing DSL specification.
