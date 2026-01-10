{-# LANGUAGE OverloadedStrings #-}

module Claims.Rules
  ( -- * Business Rules
    highValueClaimRule
  , erClaimRule
  , inpatientRule
  , complexRule
  , outpatientSurgeryRule
  , allRules
  ) where

import Claims.Types

-- | Rule 1: High-Value Claims Review
-- Business Logic: Any claim exceeding $50,000 requires manual review
highValueClaimRule :: Rule ValidationResult
highValueClaimRule = 
  If (greaterThan 50000)
    (needsReview "High value claim exceeds $50,000")
    approve

-- | Rule 2: Emergency Room Validation
-- Business Logic: Emergency room claims (POS 23) must have appropriate 
-- emergency diagnosis codes (head injury S06 or heart attack I21)
erClaimRule :: Rule ValidationResult
erClaimRule =
  If (pos "23" `And` Not (hasDx "S06" `Or` hasDx "I21"))
    (reject "ER claim without emergency diagnosis code")
    approve

-- | Rule 3: Inpatient Admission Codes
-- Business Logic: Inpatient claims must include one of the standard 
-- admission procedure codes
inpatientRule :: Rule ValidationResult
inpatientRule =
  If (isType Inpatient `And` Not (hasPx "99221" `Or` hasPx "99222" `Or` hasPx "99223"))
    (reject "Inpatient claim missing admission procedure code")
    approve

-- | Rule 4: Complex Combination Rule
-- Business Logic: Professional claims over $10,000 in office settings 
-- are handled differently based on whether they're preventive care 
-- (which should be rejected) or other services (which need review)
complexRule :: Rule ValidationResult
complexRule =
  If (isType Professional `And` greaterThan 10000 `And` pos "11")
    (If (hasDx "Z00.00" `Or` hasDx "Z00.01")
      (reject "Preventive care should not exceed $10,000 in office setting")
      (needsReview "High-value professional claim in office"))
    approve

-- | Rule 5: Outpatient Surgery Validation
-- Business Logic: Outpatient surgical procedures (POS 24) should not 
-- have amounts below $100, which likely indicates a billing error
outpatientSurgeryRule :: Rule ValidationResult
outpatientSurgeryRule =
  If (isType Outpatient `And` pos "24" `And` between 0 100)
    (reject "Outpatient surgical claim amount too low")
    approve

-- | All business rules combined
allRules :: [Rule ValidationResult]
allRules = 
  [ highValueClaimRule
  , erClaimRule
  , inpatientRule
  , complexRule
  , outpatientSurgeryRule
  ]
