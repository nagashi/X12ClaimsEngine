{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Claims.Types
  ( -- * Core Domain Types
    Claim(..)
  , ClaimType(..)
  , ValidationResult(..)
    -- * Rule Types
  , Rule(..)
    -- * Smart Constructors
  , greaterThan
  , lessThan
  , between
  , hasDx
  , hasPx
  , isType
  , pos
  , reject
  , approve
  , needsReview
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Data.Decimal (Decimal)

-- | Represents a healthcare claim
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
  } deriving (Show, Eq)

-- | Types of healthcare claims
data ClaimType 
  = Inpatient 
  | Outpatient 
  | Professional
  deriving (Show, Eq)

-- | Result of claim validation
data ValidationResult 
  = Valid 
  | Invalid Text
  deriving (Show, Eq)

-- | Rule expression GADT for type-safe DSL
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

-- | Smart constructor for amount greater than
greaterThan :: Decimal -> Rule Bool
greaterThan = AmountGreaterThan

-- | Smart constructor for amount less than
lessThan :: Decimal -> Rule Bool
lessThan = AmountLessThan

-- | Smart constructor for amount between range
between :: Decimal -> Decimal -> Rule Bool
between = AmountBetween

-- | Smart constructor for has diagnosis code
hasDx :: Text -> Rule Bool
hasDx = HasDiagnosisCode

-- | Smart constructor for has procedure code
hasPx :: Text -> Rule Bool
hasPx = HasProcedureCode

-- | Smart constructor for claim type check
isType :: ClaimType -> Rule Bool
isType = IsClaimType

-- | Smart constructor for place of service check
pos :: Text -> Rule Bool
pos = PlaceOfServiceIs

-- | Smart constructor for reject action
reject :: Text -> Rule ValidationResult
reject = Reject

-- | Smart constructor for approve action
approve :: Rule ValidationResult
approve = Approve

-- | Smart constructor for review action
needsReview :: Text -> Rule ValidationResult
needsReview = RequireReview
