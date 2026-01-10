{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Claims.Types
import Claims.Interpreter
import Claims.Rules
import Claims.Parser
import Claims.Parser.AST
import Data.Time (fromGregorian)
import Data.Decimal (realFracToDecimal)
import Data.Text (Text)

main :: IO ()
main = hspec $ do
  describe "Claims.Interpreter" $ do
    it "evaluates amount greater than correctly" $ do
      let claim = testClaim { totalAmount = realFracToDecimal 2 60000 }
      eval claim (greaterThan 50000) `shouldBe` True
      eval claim (greaterThan 70000) `shouldBe` False
    
    it "evaluates amount less than correctly" $ do
      let claim = testClaim { totalAmount = realFracToDecimal 2 40000 }
      eval claim (lessThan 50000) `shouldBe` True
      eval claim (lessThan 30000) `shouldBe` False
    
    it "evaluates amount between correctly" $ do
      let claim = testClaim { totalAmount = realFracToDecimal 2 50 }
      eval claim (between 0 100) `shouldBe` True
      eval claim (between 60 100) `shouldBe` False
    
    it "evaluates diagnosis code check correctly" $ do
      let claim = testClaim { diagnosisCodes = ["I21.0", "I25.10"] }
      eval claim (hasDx "I21.0") `shouldBe` True
      eval claim (hasDx "S06") `shouldBe` False
    
    it "evaluates procedure code check correctly" $ do
      let claim = testClaim { procedureCodes = ["99223", "93000"] }
      eval claim (hasPx "99223") `shouldBe` True
      eval claim (hasPx "99221") `shouldBe` False
    
    it "evaluates claim type correctly" $ do
      let claim = testClaim { claimType = Inpatient }
      eval claim (isType Inpatient) `shouldBe` True
      eval claim (isType Outpatient) `shouldBe` False
    
    it "evaluates place of service correctly" $ do
      let claim = testClaim { placeOfService = "21" }
      eval claim (pos "21") `shouldBe` True
      eval claim (pos "23") `shouldBe` False
    
    it "evaluates AND correctly" $ do
      let claim = testClaim { totalAmount = realFracToDecimal 2 60000, claimType = Inpatient }
      eval claim (greaterThan 50000 `And` isType Inpatient) `shouldBe` True
      eval claim (greaterThan 50000 `And` isType Outpatient) `shouldBe` False
    
    it "evaluates OR correctly" $ do
      let claim = testClaim { claimType = Inpatient }
      eval claim (isType Inpatient `Or` isType Outpatient) `shouldBe` True
      eval claim (isType Professional `Or` isType Outpatient) `shouldBe` False
    
    it "evaluates NOT correctly" $ do
      let claim = testClaim { claimType = Inpatient }
      eval claim (Not (isType Outpatient)) `shouldBe` True
      eval claim (Not (isType Inpatient)) `shouldBe` False
  
  describe "Claims.Rules" $ do
    it "highValueClaimRule triggers for claims over $50,000" $ do
      let claim = testClaim { totalAmount = realFracToDecimal 2 75000 }
      eval claim highValueClaimRule `shouldBe` Invalid "Review Required: High value claim exceeds $50,000"
    
    it "highValueClaimRule approves claims under $50,000" $ do
      let claim = testClaim { totalAmount = realFracToDecimal 2 40000 }
      eval claim highValueClaimRule `shouldBe` Valid
    
    it "erClaimRule rejects ER claims without emergency diagnosis" $ do
      let claim = testClaim { placeOfService = "23", diagnosisCodes = ["Z00.00"] }
      eval claim erClaimRule `shouldBe` Invalid "ER claim without emergency diagnosis code"
    
    it "erClaimRule approves ER claims with emergency diagnosis" $ do
      let claim = testClaim { placeOfService = "23", diagnosisCodes = ["I21"] }
      eval claim erClaimRule `shouldBe` Valid
    
    it "inpatientRule rejects inpatient claims without admission code" $ do
      let claim = testClaim { claimType = Inpatient, procedureCodes = ["93000"] }
      eval claim inpatientRule `shouldBe` Invalid "Inpatient claim missing admission procedure code"
    
    it "inpatientRule approves inpatient claims with admission code" $ do
      let claim = testClaim { claimType = Inpatient, procedureCodes = ["99223", "93000"] }
      eval claim inpatientRule `shouldBe` Valid
    
    it "outpatientSurgeryRule rejects low-value outpatient surgery claims" $ do
      let claim = testClaim { claimType = Outpatient, placeOfService = "24", totalAmount = realFracToDecimal 2 50 }
      eval claim outpatientSurgeryRule `shouldBe` Invalid "Outpatient surgical claim amount too low"
    
    it "outpatientSurgeryRule approves normal outpatient surgery claims" $ do
      let claim = testClaim { claimType = Outpatient, placeOfService = "24", totalAmount = realFracToDecimal 2 500 }
      eval claim outpatientSurgeryRule `shouldBe` Valid
  
  describe "Claims.Parser" $ do
    it "parses a simple APPROVE rule" $ do
      let input = unlines
            [ "RULE SimpleApprove"
            , "DESCRIPTION \"Always approve\""
            , "WHEN"
            , "  claim.amount < 1000"
            , "THEN"
            , "  APPROVE"
            , "END"
            ]
      case parseRules input of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right [rule] -> do
          Claims.Parser.AST.ruleName rule `shouldBe` "SimpleApprove"
          ruleDescription rule `shouldBe` "Always approve"
          Claims.Parser.AST.action rule `shouldBe` ApproveClaim
        Right _ -> expectationFailure "Expected exactly one rule"
    
    it "parses a REJECT rule with amount comparison" $ do
      let input = unlines
            [ "RULE HighValueReject"
            , "DESCRIPTION \"Reject high value claims\""
            , "WHEN"
            , "  claim.amount > 100000"
            , "THEN"
            , "  REJECT \"Amount too high\""
            , "END"
            ]
      case parseRules input of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right [rule] -> do
          Claims.Parser.AST.action rule `shouldBe` RejectClaim "Amount too high"
        Right _ -> expectationFailure "Expected exactly one rule"
    
    it "parses a rule with AND condition" $ do
      let input = unlines
            [ "RULE ComplexRule"
            , "DESCRIPTION \"Complex condition\""
            , "WHEN"
            , "  claim.amount > 5000 AND claim.place_of_service = \"23\""
            , "THEN"
            , "  REQUIRE_REVIEW \"Needs review\""
            , "END"
            ]
      case parseRules input of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right [rule] -> do
          case Claims.Parser.AST.conditions rule of
            AndCond _ _ -> return ()
            _ -> expectationFailure "Expected AND condition"
        Right _ -> expectationFailure "Expected exactly one rule"
    
    it "parses a rule with diagnosis code check" $ do
      let input = unlines
            [ "RULE DiagnosisCheck"
            , "DESCRIPTION \"Check diagnosis\""
            , "WHEN"
            , "  claim.has_diagnosis \"I21\""
            , "THEN"
            , "  APPROVE"
            , "END"
            ]
      case parseRules input of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right [rule] -> do
          Claims.Parser.AST.conditions rule `shouldBe` CheckDiagnosis "I21"
        Right _ -> expectationFailure "Expected exactly one rule"

-- | Test claim helper
testClaim :: Claim
testClaim = Claim
  { claimId = "TEST001"
  , patientId = "PAT001"
  , providerId = "PRV001"
  , serviceDate = fromGregorian 2025 1 1
  , totalAmount = realFracToDecimal 2 1000
  , diagnosisCodes = []
  , procedureCodes = []
  , placeOfService = "11"
  , claimType = Professional
  }
