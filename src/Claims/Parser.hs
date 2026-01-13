{-# LANGUAGE OverloadedStrings #-}

module Claims.Parser
  ( parseRule
  , parseRules
  , convertToInternalRule
  ) where

import Claims.Parser.AST
import qualified Claims.Types as T
import Claims.Types (Rule, ClaimType(..), ValidationResult)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Text (pack)
import Data.Decimal (realFracToDecimal)

-- Lexer definition
lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef
  { P.reservedNames = 
      [ "RULE", "DESCRIPTION", "WHEN", "THEN", "ELSE", "END"
      , "AND", "OR", "NOT", "BETWEEN"
      , "APPROVE", "REJECT", "REQUIRE_REVIEW"
      , "claim", "amount", "type", "place_of_service"
      , "has_diagnosis", "has_procedure"
      , "Inpatient", "Outpatient", "Professional"
      ]
  , P.reservedOpNames = [">", "<", "=", ">=", "<="]
  }

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

identifier :: Parser String
identifier = P.identifier lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

float :: Parser Double
float = P.float lexer

natural :: Parser Integer
natural = P.natural lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

dot :: Parser String
dot = P.dot lexer

-- | Parse a complete rule
parseRule :: Parser ParsedRule
parseRule = do
  whiteSpace
  reserved "RULE"
  name <- identifier
  reserved "DESCRIPTION"
  desc <- stringLiteral
  reserved "WHEN"
  cond <- parseCondition
  reserved "THEN"
  act <- parseAction
  elseAct <- optionMaybe parseElseWhen
  reserved "END"
  return $ ParsedRule name desc cond act elseAct

-- | Parse ELSE WHEN clause
parseElseWhen :: Parser (Condition, Action)
parseElseWhen = do
  reserved "ELSE"
  reserved "WHEN"
  cond <- parseCondition
  reserved "THEN"
  act <- parseAction
  return (cond, act)

-- | Parse a condition expression
parseCondition :: Parser Condition
parseCondition = buildExpressionParser table term
  where
    table = 
      [ [Prefix (reserved "NOT" >> return NotCond)]
      , [Infix (reserved "AND" >> return AndCond) AssocLeft]
      , [Infix (reserved "OR" >> return OrCond) AssocLeft]
      ]
    
    term = parens parseCondition
       <|> try parseAmountBetween
       <|> try parseAmountComparison
       <|> try parseHasDiagnosis
       <|> try parseHasProcedure
       <|> try parsePlaceOfService
       <|> try parseClaimType

parens :: Parser a -> Parser a
parens = P.parens lexer

-- | Parse amount comparison (e.g., claim.amount > 50000)
parseAmountComparison :: Parser Condition
parseAmountComparison = do
  reserved "claim"
  _ <- dot
  reserved "amount"
  op <- parseCompOp
  value <- try float <|> fmap fromInteger natural
  return $ CompareAmount op value

-- | Parse amount between (e.g., claim.amount BETWEEN 0 AND 100)
parseAmountBetween :: Parser Condition
parseAmountBetween = do
  reserved "claim"
  _ <- dot
  reserved "amount"
  reserved "BETWEEN"
  low <- try float <|> fmap fromInteger natural
  reserved "AND"
  high <- try float <|> fmap fromInteger natural
  return $ AmountBetween low high

-- | Parse has diagnosis code (e.g., claim.has_diagnosis "S06")
parseHasDiagnosis :: Parser Condition
parseHasDiagnosis = do
  reserved "claim"
  _ <- dot
  reserved "has_diagnosis"
  code <- stringLiteral
  return $ CheckDiagnosis code

-- | Parse has procedure code (e.g., claim.has_procedure "99221")
parseHasProcedure :: Parser Condition
parseHasProcedure = do
  reserved "claim"
  _ <- dot
  reserved "has_procedure"
  code <- stringLiteral
  return $ CheckProcedure code

-- | Parse place of service (e.g., claim.place_of_service = "23")
parsePlaceOfService :: Parser Condition
parsePlaceOfService = do
  reserved "claim"
  _ <- dot
  reserved "place_of_service"
  reservedOp "="
  pos <- stringLiteral
  return $ CheckPlaceOfService pos

-- | Parse claim type (e.g., claim.type = "Inpatient")
parseClaimType :: Parser Condition
parseClaimType = do
  reserved "claim"
  _ <- dot
  reserved "type"
  reservedOp "="
  ct <- stringLiteral
  return $ CheckClaimType ct

-- | Parse comparison operator
parseCompOp :: Parser CompOp
parseCompOp = 
  (reservedOp ">=" >> return Gte)
  <|> (reservedOp "<=" >> return Lte)
  <|> (reservedOp ">" >> return Gt)
  <|> (reservedOp "<" >> return Lt)
  <|> (reservedOp "=" >> return Eq)

-- | Parse an action
parseAction :: Parser Action
parseAction = 
  (reserved "APPROVE" >> return ApproveClaim)
  <|> (reserved "REJECT" >> RejectClaim <$> stringLiteral)
  <|> (reserved "REQUIRE_REVIEW" >> RequireReview <$> stringLiteral)

-- | Parse multiple rules from a file
parseRules :: String -> Either ParseError [ParsedRule]
parseRules input = parse (many parseRule <* eof) "" input

-- | Convert a parsed external DSL rule to an internal DSL rule
convertToInternalRule :: ParsedRule -> Rule ValidationResult
convertToInternalRule pr = 
  case elseAction pr of
    Nothing -> T.If (convertCondition (conditions pr)) (convertAction (action pr)) T.approve
    Just (elseCond, elseAct) -> 
      T.If (convertCondition (conditions pr)) 
           (convertAction (action pr))
           (T.If (convertCondition elseCond) (convertAction elseAct) T.approve)

-- | Convert a parsed condition to an internal rule condition
convertCondition :: Condition -> Rule Bool
convertCondition cond = case cond of
  CompareAmount op value -> 
    let amt = realFracToDecimal 2 value
        epsilon = realFracToDecimal 2 (0.01 :: Double)
    in case op of
      Gt -> T.greaterThan amt
      Lt -> T.lessThan amt
      Eq -> T.AmountGreaterThan amt `T.And` T.AmountLessThan (amt + epsilon)
      Gte -> T.greaterThan amt `T.Or` (T.AmountGreaterThan amt `T.And` T.AmountLessThan (amt + epsilon))
      Lte -> T.lessThan amt `T.Or` (T.AmountGreaterThan amt `T.And` T.AmountLessThan (amt + epsilon))
  AmountBetween low high -> T.between (realFracToDecimal 2 low) (realFracToDecimal 2 high)
  CheckDiagnosis code -> T.hasDx (pack code)
  CheckProcedure code -> T.hasPx (pack code)
  CheckPlaceOfService p -> T.pos (pack p)
  CheckClaimType ct -> T.isType (convertClaimType ct)
  AndCond c1 c2 -> convertCondition c1 `T.And` convertCondition c2
  OrCond c1 c2 -> convertCondition c1 `T.Or` convertCondition c2
  NotCond c -> T.Not (convertCondition c)

-- | Convert a parsed action to an internal rule action
convertAction :: Action -> Rule ValidationResult
convertAction act = case act of
  ApproveClaim -> T.approve
  RejectClaim msg -> T.reject (pack msg)
  RequireReview msg -> T.needsReview (pack msg)

-- | Convert a claim type string to ClaimType
convertClaimType :: String -> ClaimType
convertClaimType "Inpatient" = Inpatient
convertClaimType "Outpatient" = Outpatient
convertClaimType "Professional" = Professional
convertClaimType _ = Professional  -- default
