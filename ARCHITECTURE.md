# Architecture Documentation

This document provides C4 model diagrams for the Healthcare Claims X12 Processing DSL application using Mermaid.

## C4 Model Overview

The C4 model provides different levels of abstraction:
- **Level 1: System Context** - How the system fits in the world
- **Level 2: Container** - High-level technology choices
- **Level 3: Component** - Components within containers
- **Level 4: Code** - Implementation details (classes, functions)

---

## Level 1: System Context Diagram

Shows how the Claims DSL system fits into the healthcare claims processing ecosystem.

```mermaid
graph TB
    subgraph "Healthcare Organization"
        BA[Business Analyst<br/>Defines validation rules]
        Dev[Developer<br/>Implements custom logic]
        System[Claims Processing System<br/>Validates claims using DSL]
    end
    
    subgraph "External Systems"
        Payer[Insurance Payers<br/>Receive validated claims]
        Provider[Healthcare Providers<br/>Submit claims]
    end
    
    BA -->|Writes rules in<br/>external DSL| System
    Dev -->|Writes rules in<br/>internal DSL| System
    Provider -->|Submits claims| System
    System -->|Validated claims| Payer
    System -->|Rejection/Review<br/>notifications| Provider
    
    style System fill:#1168bd,stroke:#0b4884,color:#ffffff
    style BA fill:#08427b,stroke:#052e56,color:#ffffff
    style Dev fill:#08427b,stroke:#052e56,color:#ffffff
```

---

## Level 2: Container Diagram

Shows the high-level containers (applications/services) that make up the system.

```mermaid
graph TB
    subgraph "Claims DSL Application [Haskell/Stack]"
        CLI[CLI Application<br/>Main.hs<br/>Demonstrates validation]
        Lib[Core Library<br/>Lib.hs<br/>Exposes DSL API]
        Parser[External DSL Parser<br/>Claims.Parser<br/>Parsec-based parser]
        Interpreter[Rule Engine<br/>Claims.Interpreter<br/>Evaluates rules]
        Types[Domain Model<br/>Claims.Types<br/>Core types and GADT]
        Rules[Business Rules<br/>Claims.Rules<br/>Predefined rules]
    end
    
    BA[Business Analyst] -->|.dsl files| Parser
    Dev[Developer] -->|Haskell code| Types
    CLI -->|Uses| Lib
    Lib -->|Exports| Parser
    Lib -->|Exports| Interpreter
    Lib -->|Exports| Types
    Lib -->|Exports| Rules
    Parser -->|Converts to| Types
    Interpreter -->|Evaluates| Types
    Rules -->|Uses| Types
    
    style Lib fill:#1168bd,stroke:#0b4884,color:#ffffff
    style Parser fill:#438dd5,stroke:#2e6295,color:#ffffff
    style Interpreter fill:#438dd5,stroke:#2e6295,color:#ffffff
    style Types fill:#438dd5,stroke:#2e6295,color:#ffffff
```

---

## Level 3: Component Diagram

Shows the internal components and their relationships.

```mermaid
graph TB
    subgraph "Claims.Types Module"
        Claim[Claim<br/>Data Type]
        ClaimType[ClaimType<br/>Enum]
        ValidationResult[ValidationResult<br/>Result Type]
        RuleGADT[Rule GADT<br/>Type-safe rules]
        SmartCtor[Smart Constructors<br/>greaterThan, hasDx, etc.]
    end
    
    subgraph "Claims.Interpreter Module"
        Eval[eval<br/>Rule → Claim → Result]
        ValidateClaim[validateClaim<br/>Runs multiple rules]
    end
    
    subgraph "Claims.Rules Module"
        HighValue[highValueClaimRule]
        ERRule[erClaimRule]
        Inpatient[inpatientRule]
        Complex[complexRule]
        Outpatient[outpatientSurgeryRule]
    end
    
    subgraph "Claims.Parser Module"
        ParseRule[parseRule<br/>Parsec parser]
        ParseRules[parseRules<br/>Multiple rules]
        Convert[convertToInternalRule<br/>AST → Rule]
    end
    
    subgraph "Claims.Parser.AST Module"
        ParsedRule[ParsedRule<br/>External rule representation]
        Condition[Condition<br/>External conditions]
        Action[Action<br/>External actions]
    end
    
    SmartCtor -->|Creates| RuleGADT
    HighValue -->|Uses| SmartCtor
    ERRule -->|Uses| SmartCtor
    Inpatient -->|Uses| SmartCtor
    Complex -->|Uses| SmartCtor
    Outpatient -->|Uses| SmartCtor
    
    Eval -->|Pattern matches| RuleGADT
    Eval -->|Reads| Claim
    Eval -->|Returns| ValidationResult
    ValidateClaim -->|Calls| Eval
    
    ParseRule -->|Produces| ParsedRule
    ParseRules -->|Uses| ParseRule
    Convert -->|Reads| ParsedRule
    Convert -->|Produces| RuleGADT
    
    style RuleGADT fill:#1168bd,stroke:#0b4884,color:#ffffff
    style Eval fill:#1168bd,stroke:#0b4884,color:#ffffff
```

---

## Level 4: Algorithm Flow Diagrams

### 4.1 Rule Evaluation Algorithm

```mermaid
flowchart TD
    Start([Start: eval claim rule]) --> CheckType{What is<br/>rule type?}
    
    CheckType -->|AmountGreaterThan| CompareAmount[Compare claim.totalAmount > threshold]
    CheckType -->|HasDiagnosisCode| CheckDx[Check if code in claim.diagnosisCodes]
    CheckType -->|And| EvalBoth[Evaluate both sub-rules]
    CheckType -->|Or| EvalEither[Evaluate both sub-rules]
    CheckType -->|Not| EvalNegate[Evaluate sub-rule and negate]
    CheckType -->|If| EvalCondition[Evaluate condition]
    CheckType -->|Approve| ReturnValid[Return Valid]
    CheckType -->|Reject| ReturnInvalid[Return Invalid with message]
    
    CompareAmount --> ReturnBool[Return Bool result]
    CheckDx --> ReturnBool
    EvalBoth --> AndResult{Both true?}
    AndResult -->|Yes| ReturnTrue[Return True]
    AndResult -->|No| ReturnFalse[Return False]
    EvalEither --> OrResult{Either true?}
    OrResult -->|Yes| ReturnTrue
    OrResult -->|No| ReturnFalse
    EvalNegate --> NegateResult[Return !result]
    
    EvalCondition --> CondResult{Condition<br/>true?}
    CondResult -->|True| EvalThen[Evaluate then-branch]
    CondResult -->|False| EvalElse[Evaluate else-branch]
    EvalThen --> ReturnResult[Return ValidationResult]
    EvalElse --> ReturnResult
    
    ReturnBool --> End([End])
    ReturnTrue --> End
    ReturnFalse --> End
    NegateResult --> End
    ReturnValid --> End
    ReturnInvalid --> End
    ReturnResult --> End
    
    style Start fill:#00bc7e,stroke:#009063,color:#ffffff
    style End fill:#00bc7e,stroke:#009063,color:#ffffff
    style CheckType fill:#1168bd,stroke:#0b4884,color:#ffffff
    style CondResult fill:#1168bd,stroke:#0b4884,color:#ffffff
```

### 4.2 External DSL Parsing Algorithm

```mermaid
flowchart TD
    Start([Start: parseRule input]) --> SkipWS[Skip whitespace]
    SkipWS --> ParseRULE[Parse RULE keyword]
    ParseRULE --> ParseName[Parse rule name]
    ParseName --> ParseDESC[Parse DESCRIPTION keyword]
    ParseDESC --> ParseDescStr[Parse description string]
    ParseDescStr --> ParseWHEN[Parse WHEN keyword]
    ParseWHEN --> ParseCond[Parse condition expression]
    
    ParseCond --> BuildExpr{Build expression<br/>with precedence}
    BuildExpr --> CheckOp{Operator?}
    CheckOp -->|AND| ParseAND[Parse AND operator]
    CheckOp -->|OR| ParseOR[Parse OR operator]
    CheckOp -->|NOT| ParseNOT[Parse NOT prefix]
    CheckOp -->|None| ParseTerm[Parse terminal condition]
    
    ParseTerm --> TermType{Term type?}
    TermType -->|amount| ParseAmount[Parse amount comparison]
    TermType -->|has_diagnosis| ParseDx[Parse diagnosis check]
    TermType -->|has_procedure| ParsePx[Parse procedure check]
    TermType -->|place_of_service| ParsePOS[Parse POS check]
    TermType -->|type| ParseType[Parse claim type check]
    
    ParseAmount --> BuildCond[Build Condition AST node]
    ParseDx --> BuildCond
    ParsePx --> BuildCond
    ParsePOS --> BuildCond
    ParseType --> BuildCond
    
    ParseAND --> BuildCond
    ParseOR --> BuildCond
    ParseNOT --> BuildCond
    
    BuildCond --> ParseTHEN[Parse THEN keyword]
    ParseTHEN --> ParseAction[Parse action]
    ParseAction --> ActionType{Action type?}
    ActionType -->|APPROVE| BuildApprove[Build ApproveClaim]
    ActionType -->|REJECT| BuildReject[Build RejectClaim with message]
    ActionType -->|REQUIRE_REVIEW| BuildReview[Build RequireReview with message]
    
    BuildApprove --> CheckElse{ELSE WHEN<br/>clause?}
    BuildReject --> CheckElse
    BuildReview --> CheckElse
    
    CheckElse -->|Yes| ParseElse[Parse else condition and action]
    CheckElse -->|No| ParseEND[Parse END keyword]
    ParseElse --> ParseEND
    
    ParseEND --> BuildParsedRule[Build ParsedRule AST]
    BuildParsedRule --> End([End: Return ParsedRule])
    
    style Start fill:#00bc7e,stroke:#009063,color:#ffffff
    style End fill:#00bc7e,stroke:#009063,color:#ffffff
    style BuildExpr fill:#1168bd,stroke:#0b4884,color:#ffffff
    style CheckOp fill:#1168bd,stroke:#0b4884,color:#ffffff
    style TermType fill:#1168bd,stroke:#0b4884,color:#ffffff
```

### 4.3 AST to Internal Rule Conversion Algorithm

```mermaid
flowchart TD
    Start([Start: convertToInternalRule parsedRule]) --> CheckElse{Has ELSE<br/>clause?}
    
    CheckElse -->|No| SimpleConvert[Convert to simple If-Then-Approve]
    CheckElse -->|Yes| ComplexConvert[Convert to nested If-Then-Else]
    
    SimpleConvert --> ConvertCond1[Convert main condition]
    ConvertCond1 --> ConvertAct1[Convert main action]
    ConvertAct1 --> BuildIf1[Build: If cond action approve]
    
    ComplexConvert --> ConvertMainCond[Convert main condition]
    ConvertMainCond --> ConvertMainAct[Convert main action]
    ConvertMainAct --> ConvertElseCond[Convert else condition]
    ConvertElseCond --> ConvertElseAct[Convert else action]
    ConvertElseAct --> BuildNested[Build: If cond1 act1<br/>If cond2 act2 approve]
    
    BuildIf1 --> ReturnRule[Return internal Rule]
    BuildNested --> ReturnRule
    
    subgraph "Condition Conversion"
        ConvertCond1 --> CondType{Condition<br/>type?}
        CondType -->|CompareAmount| BuildAmountRule[Build AmountGreaterThan/LessThan]
        CondType -->|AmountBetween| BuildBetweenRule[Build AmountBetween]
        CondType -->|CheckDiagnosis| BuildDxRule[Build HasDiagnosisCode]
        CondType -->|CheckProcedure| BuildPxRule[Build HasProcedureCode]
        CondType -->|CheckPlaceOfService| BuildPOSRule[Build PlaceOfServiceIs]
        CondType -->|CheckClaimType| BuildTypeRule[Build IsClaimType]
        CondType -->|AndCond| BuildAndRule[Recursively convert both<br/>and combine with And]
        CondType -->|OrCond| BuildOrRule[Recursively convert both<br/>and combine with Or]
        CondType -->|NotCond| BuildNotRule[Recursively convert<br/>and wrap with Not]
    end
    
    subgraph "Action Conversion"
        ConvertAct1 --> ActType{Action<br/>type?}
        ActType -->|ApproveClaim| BuildApprove[Build Approve]
        ActType -->|RejectClaim| BuildReject[Build Reject with message]
        ActType -->|RequireReview| BuildReview[Build RequireReview with message]
    end
    
    BuildAmountRule --> ReturnCondRule[Return Rule Bool]
    BuildBetweenRule --> ReturnCondRule
    BuildDxRule --> ReturnCondRule
    BuildPxRule --> ReturnCondRule
    BuildPOSRule --> ReturnCondRule
    BuildTypeRule --> ReturnCondRule
    BuildAndRule --> ReturnCondRule
    BuildOrRule --> ReturnCondRule
    BuildNotRule --> ReturnCondRule
    
    BuildApprove --> ReturnActRule[Return Rule ValidationResult]
    BuildReject --> ReturnActRule
    BuildReview --> ReturnActRule
    
    ReturnRule --> End([End: Return Rule ValidationResult])
    
    style Start fill:#00bc7e,stroke:#009063,color:#ffffff
    style End fill:#00bc7e,stroke:#009063,color:#ffffff
    style CheckElse fill:#1168bd,stroke:#0b4884,color:#ffffff
    style CondType fill:#1168bd,stroke:#0b4884,color:#ffffff
    style ActType fill:#1168bd,stroke:#0b4884,color:#ffffff
```

### 4.4 Complete Validation Flow (End-to-End)

```mermaid
flowchart TD
    Start([Business Analyst writes rule]) --> WriteRule[Create rule.dsl file]
    WriteRule --> LoadFile[Load file into application]
    LoadFile --> CallParser[Call parseRules]
    
    CallParser --> ParseSuccess{Parse<br/>successful?}
    ParseSuccess -->|No| ParseError[Return ParseError]
    ParseSuccess -->|Yes| GetParsedRules[Get list of ParsedRule]
    
    GetParsedRules --> ConvertLoop[For each ParsedRule]
    ConvertLoop --> CallConvert[Call convertToInternalRule]
    CallConvert --> GetInternalRule[Get internal Rule]
    GetInternalRule --> CollectRules[Collect all internal rules]
    
    CollectRules --> CreateClaim[Create Claim data]
    CreateClaim --> CallValidate[Call validateClaim claim rules]
    
    CallValidate --> RuleLoop[For each rule]
    RuleLoop --> CallEval[Call eval claim rule]
    CallEval --> EvalRule[Evaluate rule against claim]
    
    EvalRule --> RuleResult{Rule result?}
    RuleResult -->|Valid| CollectValid[Collect Valid result]
    RuleResult -->|Invalid msg| CollectInvalid[Collect Invalid with message]
    
    CollectValid --> MoreRules{More<br/>rules?}
    CollectInvalid --> MoreRules
    MoreRules -->|Yes| RuleLoop
    MoreRules -->|No| AllResults[Collect all ValidationResults]
    
    AllResults --> AnalyzeResults{Any<br/>Invalid?}
    AnalyzeResults -->|Yes| ClaimFailed[Claim requires action]
    AnalyzeResults -->|No| ClaimPassed[Claim approved]
    
    ClaimFailed --> TakeAction{Action type?}
    TakeAction -->|Review| FlagReview[Flag for manual review]
    TakeAction -->|Reject| RejectClaim[Reject claim with reason]
    
    FlagReview --> End([End: Return results])
    RejectClaim --> End
    ClaimPassed --> End
    ParseError --> End
    
    style Start fill:#00bc7e,stroke:#009063,color:#ffffff
    style End fill:#00bc7e,stroke:#009063,color:#ffffff
    style ParseSuccess fill:#1168bd,stroke:#0b4884,color:#ffffff
    style RuleResult fill:#1168bd,stroke:#0b4884,color:#ffffff
    style AnalyzeResults fill:#1168bd,stroke:#0b4884,color:#ffffff
    style TakeAction fill:#1168bd,stroke:#0b4884,color:#ffffff
    style ClaimPassed fill:#00bc7e,stroke:#009063,color:#ffffff
    style ClaimFailed fill:#d73a49,stroke:#a72630,color:#ffffff
    style ParseError fill:#d73a49,stroke:#a72630,color:#ffffff
```

---

## Data Flow Diagram

Shows how data flows through the system:

```mermaid
graph LR
    subgraph "Input"
        DSLFile[External DSL File<br/>.dsl]
        ClaimData[Claim Data<br/>JSON/Record]
    end
    
    subgraph "Processing Pipeline"
        Parse[Parser<br/>Text → AST]
        Convert[Converter<br/>AST → Rule]
        Eval[Evaluator<br/>Rule × Claim → Result]
    end
    
    subgraph "Output"
        Results[Validation Results<br/>List of Valid/Invalid]
    end
    
    DSLFile -->|Text| Parse
    Parse -->|ParsedRule| Convert
    Convert -->|Rule ValidationResult| Eval
    ClaimData -->|Claim| Eval
    Eval -->|ValidationResult| Results
    
    style Parse fill:#438dd5,stroke:#2e6295,color:#ffffff
    style Convert fill:#438dd5,stroke:#2e6295,color:#ffffff
    style Eval fill:#438dd5,stroke:#2e6295,color:#ffffff
    style Results fill:#00bc7e,stroke:#009063,color:#ffffff
```

---

## Type System Architecture

Shows the GADT-based type system:

```mermaid
graph TB
    subgraph "Rule GADT [Phantom Type]"
        RuleBool[Rule Bool<br/>Predicates]
        RuleResult[Rule ValidationResult<br/>Actions]
    end
    
    subgraph "Rule Bool Constructors"
        AmtGT[AmountGreaterThan :: Decimal → Rule Bool]
        AmtLT[AmountLessThan :: Decimal → Rule Bool]
        AmtBet[AmountBetween :: Decimal → Decimal → Rule Bool]
        HasDx[HasDiagnosisCode :: Text → Rule Bool]
        HasPx[HasProcedureCode :: Text → Rule Bool]
        IsType[IsClaimType :: ClaimType → Rule Bool]
        POS[PlaceOfServiceIs :: Text → Rule Bool]
        And[And :: Rule Bool → Rule Bool → Rule Bool]
        Or[Or :: Rule Bool → Rule Bool → Rule Bool]
        Not[Not :: Rule Bool → Rule Bool]
    end
    
    subgraph "Rule ValidationResult Constructors"
        App[Approve :: Rule ValidationResult]
        Rej[Reject :: Text → Rule ValidationResult]
        Rev[RequireReview :: Text → Rule ValidationResult]
        If[If :: Rule Bool →<br/>Rule ValidationResult →<br/>Rule ValidationResult →<br/>Rule ValidationResult]
    end
    
    RuleBool --> AmtGT
    RuleBool --> AmtLT
    RuleBool --> AmtBet
    RuleBool --> HasDx
    RuleBool --> HasPx
    RuleBool --> IsType
    RuleBool --> POS
    RuleBool --> And
    RuleBool --> Or
    RuleBool --> Not
    
    RuleResult --> App
    RuleResult --> Rej
    RuleResult --> Rev
    RuleResult --> If
    
    If -.->|Uses as condition| RuleBool
    And -.->|Combines| RuleBool
    Or -.->|Combines| RuleBool
    Not -.->|Negates| RuleBool
    
    style RuleBool fill:#1168bd,stroke:#0b4884,color:#ffffff
    style RuleResult fill:#1168bd,stroke:#0b4884,color:#ffffff
```

---

## Module Dependency Graph

```mermaid
graph TD
    Main[app/Main.hs] --> Lib[src/Lib.hs]
    Test[test/Spec.hs] --> Lib
    
    Lib --> Types[src/Claims/Types.hs]
    Lib --> Interpreter[src/Claims/Interpreter.hs]
    Lib --> Rules[src/Claims/Rules.hs]
    Lib --> Parser[src/Claims/Parser.hs]
    
    Interpreter --> Types
    Rules --> Types
    Parser --> Types
    Parser --> AST[src/Claims/Parser/AST.hs]
    
    style Lib fill:#1168bd,stroke:#0b4884,color:#ffffff
    style Types fill:#438dd5,stroke:#2e6295,color:#ffffff
```

---

## Summary

This architecture demonstrates:

1. **Clear Separation of Concerns**: Each module has a distinct responsibility
2. **Type Safety**: GADT ensures compile-time correctness
3. **Extensibility**: Easy to add new rule types or conditions
4. **Composability**: Rules can be combined using logical operators
5. **Two-DSL Approach**: Internal (type-safe) and external (business-friendly)
6. **Clean Data Flow**: Text → AST → Internal Rule → Evaluation → Result
