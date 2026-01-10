module Claims.Parser.AST
  ( ParsedRule(..)
  , Condition(..)
  , Action(..)
  , CompOp(..)
  ) where

-- | A parsed rule from the external DSL
data ParsedRule = ParsedRule
  { ruleName :: String
  , ruleDescription :: String
  , conditions :: Condition
  , action :: Action
  , elseAction :: Maybe (Condition, Action)
  } deriving (Show, Eq)

-- | Condition expressions in the external DSL
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
  deriving (Show, Eq)

-- | Comparison operators
data CompOp = Gt | Lt | Eq | Gte | Lte
  deriving (Show, Eq)

-- | Actions that can be taken on a claim
data Action
  = RejectClaim String
  | ApproveClaim
  | RequireReview String
  deriving (Show, Eq)
