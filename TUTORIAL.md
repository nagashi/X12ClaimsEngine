# Claims X12 DSL Tutorial

This tutorial will guide you through using the `claims-x12-dsl` library to validate healthcare claims.

## Installation

### As a library dependency

Add to your `package.yaml`:

```yaml
dependencies:
  - claims-x12-dsl
```

Or to your `.cabal` file:

```cabal
build-depends: claims-x12-dsl
```

### Using Stack

```bash
stack install claims-x12-dsl
```

## Quick Start

### 1. Import the library

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Claims.Types
import Claims.Interpreter
import Data.Decimal (realFracToDecimal)
import Data.Time (fromGregorian)
```

### 2. Create a claim

```haskell
claim = Claim
  { claimId = "CLM001"
  , patientId = "PAT12345"
  , providerId = "PRV98765"
  , serviceDate = fromGregorian 2025 1 5
  , totalAmount = realFracToDecimal 2 15000  -- $15,000.00
  , diagnosisCodes = ["I21.0"]
  , procedureCodes = ["99223"]
  , placeOfService = "21"
  , claimType = Inpatient
  }
```

### 3. Define validation rules

```haskell
-- Simple rule: approve if under $10k, review if over
simpleRule = 
  If (greaterThan 10000)
    (needsReview "High value claim")
    approve

-- Complex rule: check multiple conditions
complexRule = 
  If (And (greaterThan 5000) (isType Inpatient))
    (If (hasDx "I21")
      approve
      (needsReview "High value inpatient without emergency diagnosis"))
    approve
```

### 4. Evaluate rules

```haskell
result = eval claim simpleRule
-- Result: Invalid "Review Required: High value claim"
```

## Rule Building Blocks

### Predicates (return Bool)

| Function | Description | Example |
|----------|-------------|---------|
| `greaterThan n` | Amount > n | `greaterThan 5000` |
| `lessThan n` | Amount < n | `lessThan 1000` |
| `between low high` | low ≤ Amount ≤ high | `between 100 500` |
| `hasDx "code"` | Has diagnosis code | `hasDx "I21.0"` |
| `hasPx "code"` | Has procedure code | `hasPx "99223"` |
| `isType type` | Claim is of type | `isType Inpatient` |
| `pos "code"` | Place of service | `pos "23"` |

### Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `And r1 r2` | Both rules true | `And (greaterThan 1000) (isType Inpatient)` |
| `Or r1 r2` | Either rule true | `Or (hasDx "I21") (hasDx "I22")` |
| `Not r` | Negation | `Not (greaterThan 5000)` |

### Actions (return ValidationResult)

| Action | Description | Example |
|--------|-------------|---------|
| `approve` | Approve claim | `approve` |
| `reject "msg"` | Reject with reason | `reject "Missing diagnosis"` |
| `needsReview "msg"` | Flag for review | `needsReview "Unusual pattern"` |

### Control Flow

```haskell
If condition thenAction elseAction
```

## Common Patterns

### Pattern 1: Threshold-based approval

```haskell
autoApproveUnder5k = 
  If (lessThan 5000)
    approve
    (needsReview "Manual review required")
```

### Pattern 2: Multiple conditions

```haskell
erValidation = 
  If (And (pos "23") (Or (hasDx "S06") (hasDx "I21")))
    approve
    (reject "ER claim missing emergency diagnosis")
```

### Pattern 3: Tiered validation

```haskell
tieredApproval = 
  If (lessThan 1000)
    approve
    (If (lessThan 5000)
      (needsReview "Medium value")
      (needsReview "High value"))
```

### Pattern 4: Type-specific rules

```haskell
inpatientRule = 
  If (isType Inpatient)
    (If (And (greaterThan 10000) (hasPx "99223"))
      approve
      (needsReview "Inpatient without admission code"))
    approve
```

## Batch Validation

Validate a claim against multiple rules:

```haskell
rules = [rule1, rule2, rule3, rule4]
results = validateClaim claim rules
-- Returns: [ValidationResult]

-- Check if all passed
allValid = all (== Valid) results
```

## External DSL (Text-based Rules)

For non-programmers, use the text-based DSL:

```
RULE HighValueReview
DESCRIPTION "Flag high-value claims"
WHEN
  claim.amount > 50000
THEN
  REQUIRE_REVIEW "High value claim"
END
```

Parse and use:

```haskell
import Claims.Parser

rulesText <- readFile "rules.dsl"
case parseRules rulesText of
  Left err -> putStrLn $ "Parse error: " ++ show err
  Right rules -> do
    let internalRules = map convertToInternalRule rules
    let results = validateClaim claim internalRules
    print results
```

## Tips

1. **Start simple**: Build rules incrementally, testing each piece
2. **Use type system**: Let the compiler catch mistakes with GADTs
3. **Compose rules**: Combine small rules into larger validations
4. **Test thoroughly**: Use the test suite as examples
5. **Document rules**: Add comments explaining business logic

## Next Steps

- Read the [API documentation](https://yourdomain.com/docs)
- Check `examples/` directory for more examples
- Review `test/Spec.hs` for comprehensive examples
- See `ARCHITECTURE.md` for design details

## Getting Help

- Open an issue on GitHub
- Check the README for FAQs
- Review the Haddock documentation
