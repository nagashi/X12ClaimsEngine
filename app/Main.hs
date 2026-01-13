{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Claims.Types
import Claims.Interpreter
import Claims.Rules
import Data.Time (fromGregorian)
import Data.Decimal (realFracToDecimal)

-- | Example claim from the requirements document
exampleClaim :: Claim
exampleClaim = Claim
  { claimId = "CLM001"
  , patientId = "PAT12345"
  , providerId = "PRV98765"
  , serviceDate = fromGregorian 2025 1 5
  , totalAmount = realFracToDecimal 2 (75000 :: Double)
  , diagnosisCodes = ["I21.0", "I25.10"]
  , procedureCodes = ["99223", "93000"]
  , placeOfService = "21"
  , claimType = Inpatient
  }

main :: IO ()
main = do
  putStrLn "Healthcare Claims X12 Processing DSL"
  putStrLn "===================================="
  putStrLn ""
  
  putStrLn "Validating example claim:"
  putStrLn $ "Claim ID: " ++ show (claimId exampleClaim)
  putStrLn $ "Amount: $" ++ show (totalAmount exampleClaim)
  putStrLn $ "Type: " ++ show (claimType exampleClaim)
  putStrLn ""
  
  let results = validateClaim exampleClaim allRules
  
  putStrLn "Validation Results:"
  putStrLn "-------------------"
  mapM_ printResult $ zip [1..] results
  
  where
    printResult (n, Valid) = 
      putStrLn $ "Rule " ++ show (n :: Int) ++ ": PASS"
    printResult (n, Invalid msg) = 
      putStrLn $ "Rule " ++ show (n :: Int) ++ ": FAIL - " ++ show msg
