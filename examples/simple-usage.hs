#!/usr/bin/env stack
-- stack script --resolver lts-22.28 --package claims-x12-dsl --package text --package Decimal --package time

{-# LANGUAGE OverloadedStrings #-}

-- | Simple example showing how to use claims-x12-dsl library
module Main where

import Claims.Types
import Claims.Interpreter
import Data.Decimal (realFracToDecimal)
import Data.Time (fromGregorian)

main :: IO ()
main = do
  putStrLn "Healthcare Claims DSL - Simple Usage Example"
  putStrLn "============================================\n"
  
  -- Create a sample claim
  let claim = Claim
        { claimId = "CLM001"
        , patientId = "PAT12345"
        , providerId = "PRV98765"
        , serviceDate = fromGregorian 2025 1 5
        , totalAmount = realFracToDecimal 2 15000
        , diagnosisCodes = ["I21.0", "I25.10"]
        , procedureCodes = ["99223", "93000"]
        , placeOfService = "21"
        , claimType = Inpatient
        }
  
  -- Define a simple rule
  let highValueRule = 
        If (greaterThan 10000)
          (needsReview "High value claim")
          approve
  
  -- Evaluate the rule
  let result = eval claim highValueRule
  
  putStrLn $ "Claim ID: " ++ show (claimId claim)
  putStrLn $ "Amount: $" ++ show (totalAmount claim)
  putStrLn $ "Result: " ++ show result
