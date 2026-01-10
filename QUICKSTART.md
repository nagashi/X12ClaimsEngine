# Quick Start Guide

## Getting Started

### Build the Project

```bash
cd to where claims-x12-dsl resides.
stack build
```

### Run the Example Application

```bash
stack run
```

This will validate an example claim against all built-in rules and display the results.

### Run Tests

```bash
stack test
```

## Example Usage

### 1. Using the Internal DSL

Create a new Haskell file and import the library:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Claims.Types
import Claims.Interpreter
import Claims.Rules
import Data.Time (fromGregorian)
import Data.Decimal (realFracToDecimal)

-- Create a claim
myClaim = Claim
  { claimId = "CLM002"
  , patientId = "PAT999"
  , providerId = "PRV123"
  , serviceDate = fromGregorian 2025 1 10
  , totalAmount = realFracToDecimal 2 25000
  , diagnosisCodes = ["J18.9"]
  , procedureCodes = ["99223"]
  , placeOfService = "21"
  , claimType = Inpatient
  }

-- Validate against all rules
main = do
  let results = validateClaim myClaim allRules
  mapM_ print results
```

### 2. Creating Custom Rules

```haskell
import Claims.Types

-- Custom rule: Reject claims with specific diagnosis
diabetesScreeningRule :: Rule ValidationResult
diabetesScreeningRule =
  If (hasDx "E11.9" `And` greaterThan 5000)
    (needsReview "High-cost diabetes claim")
    approve

-- Complex rule with multiple conditions
surgeryRule :: Rule ValidationResult
surgeryRule =
  If (isType Outpatient `And` pos "24" `And` between 500 10000)
    approve
    (If (greaterThan 10000)
      (needsReview "High-value outpatient surgery")
      (reject "Surgical amount below minimum"))
```

### 3. Using the External DSL Parser

Load rules from a file:

```haskell
import Claims.Parser
import System.IO

main = do
  rulesText <- readFile "examples/sample-rules.dsl"
  case parseRules rulesText of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right parsedRules -> do
      let internalRules = map convertToInternalRule parsedRules
      -- Now use internalRules for validation
      let results = validateClaim someClaim internalRules
      print results
```

### 4. External DSL Rule Format

See `examples/sample-rules.dsl` for complete examples. Basic format:

```
RULE RuleName
DESCRIPTION "Description of what this rule does"
WHEN
  condition1 AND condition2
THEN
  ACTION "message"
END
```

Available conditions:
- `claim.amount > 5000`
- `claim.amount BETWEEN 100 AND 1000`
- `claim.has_diagnosis "I21"`
- `claim.has_procedure "99223"`
- `claim.place_of_service = "23"`
- `claim.type = "Inpatient"`

Available actions:
- `APPROVE`
- `REJECT "reason"`
- `REQUIRE_REVIEW "reason"`

## Project Structure

```
src/
├── Claims/
│   ├── Types.hs          - Core domain types and GADT rules
│   ├── Interpreter.hs    - Rule evaluation engine
│   ├── Rules.hs          - Example business rules
│   ├── Parser.hs         - External DSL parser
│   └── Parser/AST.hs     - AST for external DSL
└── Lib.hs                - Main library exports

app/Main.hs               - Example application
test/Spec.hs              - Test suite (22 passing tests)
examples/sample-rules.dsl - Example external DSL rules
```

## What's Included

### Built-in Rules

1. **High Value Claims Review** - Claims over $50,000 need review
2. **Emergency Room Validation** - ER claims need emergency diagnoses
3. **Inpatient Admission Check** - Inpatient claims need admission codes
4. **Complex Professional Claims** - Multi-condition professional claim validation
5. **Outpatient Surgery Minimum** - Outpatient surgery amount validation

### Test Coverage

- ✓ All rule operators (AND, OR, NOT)
- ✓ Amount comparisons (greater than, less than, between)
- ✓ Diagnosis code validation
- ✓ Procedure code validation
- ✓ Claim type checking
- ✓ Place of service validation
- ✓ All business rules
- ✓ External DSL parser

Total: 22 passing tests

## Next Steps

1. **Add More Rules**: Create custom rules in `src/Claims/Rules.hs`
2. **Extend Parser**: Add more condition types to the external DSL
3. **Add X12 Support**: Implement X12 transaction parsing (837, 835, etc.)
4. **Create Web Interface**: Build a web UI for business analysts to edit rules
5. **Add Persistence**: Store rules in a database with versioning
