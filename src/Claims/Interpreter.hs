{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Claims.Interpreter
  ( eval
  , validateClaim
  ) where

import Claims.Types
import Data.Text (Text)
import qualified Data.Text as T

-- | Evaluate a rule against a claim
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

-- | Validate a claim against a list of rules
validateClaim :: Claim -> [Rule ValidationResult] -> [ValidationResult]
validateClaim claim rules = map (eval claim) rules
