# Healthcare Claims X12 Processing DSL in Haskell

## Overview

This document describes a Domain-Specific Language (DSL) designed for healthcare claims X12 processing, implemented in Haskell. The DSL allows business analysts to define and manage business rules for claims validation without requiring deep programming knowledge.

## Architecture

The system consists of two main components:

- **Internal DSL**: Embedded in Haskell, providing type-safe rule construction
- **External DSL**: Text-based syntax for business analysts

## Internal DSL Implementation

### Core Domain Types

```haskell
data Claim = Claim
  { claimId :: Text
  , patientId :: Text
  , providerId :: Text
  , serviceDate :: Day
  , totalAmount :: Decimal
  , diagnosisCodes :: [Text]
  , procedureCodes :: [Text]
  , placeOfService :: Text
  , claimType :: ClaimType
  }

data ClaimType = Inpatient | Outpatient | Professional

data ValidationResult = Valid | Invalid Text
```

### Rule Expression Types

The DSL uses GADTs (Generalized Algebraic Data Types) to ensure type safety:

```haskell
data Rule a where
  -- Predicates
  AmountGreaterThan :: Decimal -> Rule Bool
  AmountLessThan :: Decimal -> Rule Bool
  AmountBetween :: Decimal -> Decimal -> Rule Bool
  
  HasDiagnosisCode :: Text -> Rule Bool
  HasProcedureCode :: Text -> Rule Bool
  
  IsClaimType :: ClaimType -> Rule Bool
  PlaceOfServiceIs :: Text -> Rule Bool
  
  -- Logical operators
  And :: Rule Bool -> Rule Bool -> Rule Bool
  Or :: Rule Bool -> Rule Bool -> Rule Bool
  Not :: Rule Bool -> Rule Bool
  
  -- Actions
  Reject :: Text -> Rule ValidationResult
  Approve :: Rule ValidationResult
  RequireReview :: Text -> Rule ValidationResult
  
  -- Control flow
  If :: Rule Bool -> Rule ValidationResult -> Rule ValidationResult -> Rule ValidationResult
```

### Smart Constructors

For cleaner, more readable syntax:

```haskell
greaterThan :: Decimal -> Rule Bool
lessThan :: Decimal -> Rule Bool
between :: Decimal -> Decimal -> Rule Bool
hasDx :: Text -> Rule Bool
hasPx :: Text -> Rule Bool
isType :: ClaimType -> Rule Bool
pos :: Text -> Rule Bool
reject :: Text -> Rule ValidationResult
approve :: Rule ValidationResult
needsReview :: Text -> Rule ValidationResult
```

### Example Business Rules

#### Rule 1: High-Value Claims Review

```haskell
highValueClaimRule :: Rule ValidationResult
highValueClaimRule = 
  If (greaterThan 50000)
    (needsReview "High value claim exceeds $50,000")
    approve
```

**Business Logic**: Any claim exceeding $50,000 requires manual review.

#### Rule 2: Emergency Room Validation

```haskell
erClaimRule :: Rule ValidationResult
erClaimRule =
  If (pos "23" `And` Not (hasDx "S06" `Or` hasDx "I21"))
    (reject "ER claim without emergency diagnosis code")
    approve
```

**Business Logic**: Emergency room claims (POS 23) must have appropriate emergency diagnosis codes (head injury S06 or heart attack I21).

#### Rule 3: Inpatient Admission Codes

```haskell
inpatientRule :: Rule ValidationResult
inpatientRule =
  If (isType Inpatient `And` Not (hasPx "99221" `Or` hasPx "99222" `Or` hasPx "99223"))
    (reject "Inpatient claim missing admission procedure code")
    approve
```

**Business Logic**: Inpatient claims must include one of the standard admission procedure codes.

#### Rule 4: Complex Combination Rule

```haskell
complexRule :: Rule ValidationResult
complexRule =
  If (isType Professional `And` greaterThan 10000 `And` pos "11")
    (If (hasDx "Z00.00" `Or` hasDx "Z00.01")
      (reject "Preventive care should not exceed $10,000 in office setting")
      (needsReview "High-value professional claim in office"))
    approve
```

**Business Logic**: Professional claims over $10,000 in office settings are handled differently based on whether they're preventive care (which should be rejected) or other services (which need review).

#### Rule 5: Outpatient Surgery Validation

```haskell
outpatientSurgeryRule :: Rule ValidationResult
outpatientSurgeryRule =
  If (isType Outpatient `And` pos "24" `And` between 0 100)
    (reject "Outpatient surgical claim amount too low")
    approve
```

**Business Logic**: Outpatient surgical procedures (POS 24) should not have amounts below $100, which likely indicates a billing error.

### Interpreter

The interpreter evaluates rules against actual claims:

```haskell
eval :: Claim -> Rule a -> a
eval claim rule = case rule of
  AmountGreaterThan amt -> totalAmount claim > amt
  AmountLessThan amt -> totalAmount claim < amt
  AmountBetween low high -> totalAmount claim >= low && totalAmount claim <= high
  HasDiagnosisCode code -> code `elem` diagnosisCodes claim
  HasProcedureCode code -> code `elem` procedureCodes claim
  IsClaimType ct -> claimType claim == ct
  PlaceOfServiceIs p -> placeOfService claim == p
  And r1 r2 -> eval claim r1 && eval claim r2
  Or r1 r2 -> eval claim r1 || eval claim r2
  Not r -> not (eval claim r)
  Reject msg -> Invalid msg
  Approve -> Valid
  RequireReview msg -> Invalid ("Review Required: " <> msg)
  If cond thenRule elseRule ->
    if eval claim cond
      then eval claim thenRule
      else eval claim elseRule
```

### Running Validations

```haskell
validateClaim :: Claim -> [Rule ValidationResult] -> [ValidationResult]
validateClaim claim rules = map (eval claim) rules
```

## External DSL - Business Analyst Syntax

### Syntax Structure

Each rule follows this format:

```
RULE RuleName
DESCRIPTION "Human-readable description"
WHEN
  [conditions]
THEN
  [action]
[ELSE WHEN
  [conditions]
THEN
  [action]]
END
```

### Example Rules in External Syntax

#### Example 1: High-Value Review

```
RULE HighValueReview
DESCRIPTION "Flag high-value claims for manual review"
WHEN
  claim.amount > 50000
THEN
  REQUIRE_REVIEW "Claim exceeds high-value threshold"
END
```

#### Example 2: Emergency Room Validation

```
RULE EmergencyRoomValidation
DESCRIPTION "Validate ER claims have appropriate diagnoses"
WHEN
  claim.place_of_service = "23" AND
  NOT (claim.has_diagnosis "S06" OR claim.has_diagnosis "I21")
THEN
  REJECT "Emergency room claim missing emergency diagnosis"
END
```

#### Example 3: Inpatient Admission Check

```
RULE InpatientAdmissionCheck
DESCRIPTION "Ensure inpatient claims include admission codes"
WHEN
  claim.type = "Inpatient" AND
  NOT (claim.has_procedure "99221" OR 
       claim.has_procedure "99222" OR 
       claim.has_procedure "99223")
THEN
  REJECT "Missing required inpatient admission procedure code"
END
```

#### Example 4: Preventive Care Limit with Multiple Conditions

```
RULE PreventiveCareLimit
DESCRIPTION "Cap preventive care costs in office setting"
WHEN
  claim.type = "Professional" AND
  claim.amount > 10000 AND
  claim.place_of_service = "11" AND
  (claim.has_diagnosis "Z00.00" OR claim.has_diagnosis "Z00.01")
THEN
  REJECT "Preventive care exceeds maximum allowed amount"
ELSE WHEN
  claim.type = "Professional" AND
  claim.amount > 10000 AND
  claim.place_of_service = "11"
THEN
  REQUIRE_REVIEW "High-value professional claim needs verification"
END
```

#### Example 5: Outpatient Surgery Minimum

```
RULE OutpatientSurgeryMinimum
DESCRIPTION "Validate outpatient surgery claims meet minimum"
WHEN
  claim.type = "Outpatient" AND
  claim.place_of_service = "24" AND
  claim.amount < 100
THEN
  REJECT "Outpatient surgical procedure amount below minimum threshold"
END
```

#### Example 6: Duplicate Claim Detection

```
RULE DuplicateClaimCheck
DESCRIPTION "Check for potential duplicate submissions"
WHEN
  claim.amount BETWEEN 0 AND 999999 AND
  EXISTS_RECENT_CLAIM_FOR patient.id WITHIN 7 DAYS WITH
    same_procedure_codes AND
    same_provider_id
THEN
  REQUIRE_REVIEW "Potential duplicate claim detected"
END
```

#### Example 7: Modifier Validation

```
RULE ModifierValidation
DESCRIPTION "Validate procedure code modifiers"
WHEN
  claim.has_procedure_with_modifier "99213" "25" AND
  NOT claim.has_procedure_distinct_from "99213"
THEN
  REJECT "Modifier 25 requires a separate significant procedure"
END
```

### Available Conditions

**Amount Comparisons:**

- `claim.amount > value`
- `claim.amount < value`
- `claim.amount = value`
- `claim.amount >= value`
- `claim.amount <= value`
- `claim.amount BETWEEN low AND high`

**Code Checks:**

- `claim.has_diagnosis "code"`
- `claim.has_procedure "code"`
- `claim.has_procedure_with_modifier "code" "modifier"`

**Claim Properties:**

- `claim.type = "Inpatient" | "Outpatient" | "Professional"`
- `claim.place_of_service = "code"`

**Logical Operators:**

- `AND`
- `OR`
- `NOT`

### Available Actions

- `APPROVE` - Automatically approve the claim
- `REJECT "message"` - Reject the claim with a reason
- `REQUIRE_REVIEW "message"` - Flag for manual review with a note

## Parser Implementation

The external DSL is parsed using Parsec, a parser combinator library in Haskell:

### Abstract Syntax Tree (AST)

```haskell
data ParsedRule = ParsedRule
  { ruleName :: String
  , ruleDescription :: String
  , conditions :: Condition
  , action :: Action
  , elseAction :: Maybe (Condition, Action)
  }

data Condition
  = CompareAmount CompOp Double
  | AmountBetween Double Double
  | CheckDiagnosis String
  | CheckProcedure String
  | CheckPlaceOfService String
  | CheckClaimType String
  | AndCond Condition Condition
  | OrCond Condition Condition
  | NotCond Condition

data CompOp = Gt | Lt | Eq | Gte | Lte

data Action
  = RejectClaim String
  | ApproveClaim
  | RequireReview String
```

## Key Benefits

### For the Organization

- **Separation of Concerns**: Business rules are separate from processing logic
- **Version Control**: Rules stored as text files can be tracked in source control
- **Hot Reloading**: Rules can be updated without recompiling the application
- **Auditability**: Clear trail of what rules were applied and when
- **Compliance**: Easy to demonstrate regulatory compliance

### For Business Analysts

- **Readable Syntax**: SQL-like syntax familiar to business users
- **Self-Documenting**: Rules include descriptions and clear logic
- **No Programming Required**: Can create and modify rules without coding
- **Immediate Testing**: Rules can be tested against sample claims

### For Developers

- **Type Safety**: Haskell's type system prevents invalid rules at compile time
- **Composability**: Rules can be combined and reused
- **Testability**: Each rule can be unit tested independently
- **Maintainability**: Clear structure makes updates straightforward

## X12 Transaction Support

This DSL can be extended to support specific X12 transaction sets:

### 837 (Claims)

- Professional (837P)
- Institutional (837I)
- Dental (837D)

### 835 (Remittance Advice)

- Payment validation
- Adjustment reason codes

### 270/271 (Eligibility)

- Coverage verification
- Benefit checks

### Common Validations

**Diagnosis Codes:**

- ICD-10-CM code validation
- Principal vs. secondary diagnosis rules
- Admitting diagnosis requirements

**Procedure Codes:**

- CPT/HCPCS code validation
- Modifier requirements (25, 59, etc.)
- Units of service validation

**Place of Service Codes:**

- POS 11 (Office)
- POS 21 (Inpatient Hospital)
- POS 22 (Outpatient Hospital)
- POS 23 (Emergency Room)
- POS 24 (Ambulatory Surgical Center)

**Amount Validations:**

- Charge amount reasonableness
- Payment amount calculations
- Allowed amount vs. billed amount

## Implementation Roadmap

### Phase 1: Core DSL

- Implement basic internal DSL with rule types
- Create interpreter for rule evaluation
- Build simple external syntax parser

### Phase 2: X12 Integration

- Add support for 837 claim parsing
- Implement segment-level validations
- Add loop and segment relationship rules

### Phase 3: Advanced Features

- Date-based rules and effective dates
- Rule versioning and history
- Performance optimization for large claim volumes

### Phase 4: Tooling

- Web-based rule editor for business analysts
- Rule testing framework with sample claims
- Reporting and analytics dashboard

## Usage Example

### Define a Claim

```haskell
exampleClaim :: Claim
exampleClaim = Claim
  { claimId = "CLM001"
  , patientId = "PAT12345"
  , providerId = "PRV98765"
  , serviceDate = read "2025-01-05"
  , totalAmount = 75000
  , diagnosisCodes = ["I21.0", "I25.10"]
  , procedureCodes = ["99223", "93000"]
  , placeOfService = "21"
  , claimType = Inpatient
  }
```

### Run Validation

```haskell
let rules = [ highValueClaimRule
            , erClaimRule
            , inpatientRule
            , complexRule
            , outpatientSurgeryRule
            ]

let results = validateClaim exampleClaim rules
```

### Expected Results

For the example claim above:

- **High Value Review**: TRIGGERED - Amount $75,000 exceeds threshold
- **ER Validation**: PASS - Not an ER claim
- **Inpatient Rule**: PASS - Has admission code 99223
- **Complex Rule**: PASS - Not applicable
- **Outpatient Surgery**: PASS - Not an outpatient claim

## Conclusion

This DSL provides a robust, type-safe foundation for healthcare claims processing while remaining accessible to business analysts. The combination of internal and external DSLs leverages Haskell's strengths while providing a user-friendly interface for rule management.

The system can be extended to support additional X12 transaction types, more complex validation logic, and integration with external systems such as claim clearinghouses and payer adjudication platforms.
