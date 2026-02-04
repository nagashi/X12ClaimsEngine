# Claims Processing System - Main Flow and Components

## Overview

This flowchart illustrates the complete flow of the Healthcare Claims X12 Processing System, showing how claims move through the system from initial input to final storage, including validation, business rules application, and data management.

---

## Main System Flow

```mermaid
flowchart TD
    Start([Healthcare Provider<br/>Submits Claim]) --> InputType{Input<br/>Format?}
    
    InputType -->|X12 Format| X12Parser[X12 Parser<br/>Parse EDI 837]
    InputType -->|JSON Format| JSONParser[JSON Parser<br/>Parse JSON]
    InputType -->|Direct Entry| WebForm[Web Form<br/>Manual Entry]
    
    X12Parser --> X12toJSON[X12 to JSON<br/>Converter]
    X12toJSON --> ClaimObject[Claim Object<br/>Internal Representation]
    JSONParser --> ClaimObject
    WebForm --> ClaimObject
    
    ClaimObject --> Validator{Validation<br/>Layer}
    
    subgraph "Validation & Rules Engine"
        Validator --> LoadRules[Load Business Rules]
        LoadRules --> RuleSource{Rule<br/>Source?}
        
        RuleSource -->|External DSL| ExtDSL[External DSL<br/>Parser<br/>.dsl files]
        RuleSource -->|Internal DSL| IntDSL[Internal DSL<br/>Haskell Code]
        RuleSource -->|Database| DBRules[Database<br/>Stored Rules]
        
        ExtDSL --> ParseDSL[Parse DSL Syntax<br/>Parsec Parser]
        ParseDSL --> AST[Abstract Syntax Tree<br/>ParsedRule]
        AST --> Convert[Convert to<br/>Internal Rules]
        IntDSL --> InternalRules[Internal Rules<br/>GADT Types]
        DBRules --> LoadFromDB[Load & Deserialize]
        LoadFromDB --> InternalRules
        Convert --> InternalRules
        
        InternalRules --> Interpreter[Rule Interpreter<br/>eval function]
        Interpreter --> ApplyRules[Apply All Rules<br/>to Claim]
        
        ApplyRules --> RuleResult{Validation<br/>Results}
    end
    
    RuleResult -->|All Valid| Approved[Approved Status]
    RuleResult -->|Any Reject| Rejected[Rejected Status<br/>with Reasons]
    RuleResult -->|Needs Review| Review[Review Required<br/>with Notes]
    
    Approved --> StoreApproved[Store to Database<br/>Approved Claims Table]
    Rejected --> StoreRejected[Store to Database<br/>Rejected Claims Table]
    Review --> StoreReview[Store to Database<br/>Review Queue Table]
    
    StoreApproved --> NotifyProvider[Notify Provider<br/>Approval Confirmation]
    StoreRejected --> NotifyProvider[Notify Provider<br/>Rejection Reasons]
    StoreReview --> NotifyAnalyst[Notify Analyst<br/>Review Required]
    
    StoreApproved --> UpdatePayer[Update Payer System<br/>Submit to Insurance]
    
    subgraph "Database Storage"
        DB[(Claims Database)]
        ClaimsTable[Claims Table<br/>All Claims]
        RulesTable[Rules Table<br/>Business Rules]
        AuditTable[Audit Log Table<br/>All Actions]
        ValidationTable[Validation Results<br/>Rule Execution History]
        
        StoreApproved --> DB
        StoreRejected --> DB
        StoreReview --> DB
        DB --> ClaimsTable
        DB --> AuditTable
        DB --> ValidationTable
    end
    
    subgraph "User Interface Components"
        ClaimViewer[Claim Viewer<br/>Display Claims]
        RulesManager[Rules Management UI<br/>Create/Edit Rules]
        Dashboard[Analytics Dashboard<br/>Reports & Metrics]
        ReviewQueue[Review Queue UI<br/>Manual Review]
        
        DB --> ClaimViewer
        DB --> Dashboard
        StoreReview --> ReviewQueue
        RulesManager --> RulesTable
    end
    
    NotifyProvider --> End([End])
    NotifyAnalyst --> End
    UpdatePayer --> End
    
    style Start fill:#00bc7e,stroke:#009063,color:#ffffff
    style End fill:#00bc7e,stroke:#009063,color:#ffffff
    style Validator fill:#1168bd,stroke:#0b4884,color:#ffffff
    style RuleResult fill:#1168bd,stroke:#0b4884,color:#ffffff
    style Approved fill:#00bc7e,stroke:#009063,color:#ffffff
    style Rejected fill:#d73a49,stroke:#a72630,color:#ffffff
    style Review fill:#f9826c,stroke:#d4411e,color:#ffffff
    style DB fill:#6f42c1,stroke:#4c2e85,color:#ffffff
```

---

## Component Architecture

```mermaid
flowchart LR
    subgraph "External Systems"
        Provider[Healthcare Provider<br/>X12 EDI System]
        Payer[Insurance Payer<br/>Adjudication System]
        Clearinghouse[Claims Clearinghouse<br/>837/835 Processing]
    end
    
    subgraph "Input Layer"
        X12Input[X12 Input Handler<br/>EDI 837 Parser]
        JSONInput[JSON Input Handler<br/>REST API]
        WebInput[Web Interface<br/>Manual Entry]
    end
    
    subgraph "Conversion Layer"
        X12Converter[X12 to JSON Converter<br/>Transaction Set Mapping]
        Normalizer[Data Normalizer<br/>Standardize Format]
    end
    
    subgraph "Core Processing"
        ClaimModel[Claim Domain Model<br/>Claims.Types]
        RuleEngine[Rule Engine<br/>Claims.Interpreter]
        RuleParser[DSL Parser<br/>Claims.Parser]
        ValidationEngine[Validation Engine<br/>validateClaim]
    end
    
    subgraph "Business Rules Management"
        ExternalDSL[External DSL Files<br/>.dsl format]
        InternalDSL[Internal DSL<br/>Haskell GADT]
        RuleDB[Rules Database<br/>Versioned Rules]
        RuleEditor[Rule Editor UI<br/>Business Analyst Tool]
    end
    
    subgraph "Data Storage"
        ClaimsDB[(Claims Database<br/>PostgreSQL/SQLite)]
        AuditDB[(Audit Database<br/>Compliance Log)]
        CacheLayer[Cache Layer<br/>Redis/In-Memory]
    end
    
    subgraph "Output & Reporting"
        ClaimViewer[Claim Viewer UI<br/>Search & Display]
        ReviewQueue[Review Queue<br/>Analyst Workbench]
        Reports[Analytics & Reports<br/>Dashboard]
        Notifications[Notification Service<br/>Email/SMS/API]
    end
    
    Provider -->|Submit X12| X12Input
    Provider -->|Submit JSON| JSONInput
    X12Input --> X12Converter
    X12Converter --> Normalizer
    JSONInput --> Normalizer
    WebInput --> Normalizer
    
    Normalizer --> ClaimModel
    ClaimModel --> ValidationEngine
    
    ExternalDSL --> RuleParser
    RuleParser --> RuleEngine
    InternalDSL --> RuleEngine
    RuleDB --> RuleEngine
    RuleEditor --> RuleDB
    
    RuleEngine --> ValidationEngine
    
    ValidationEngine --> ClaimsDB
    ValidationEngine --> AuditDB
    ClaimsDB --> CacheLayer
    
    ClaimsDB --> ClaimViewer
    ClaimsDB --> ReviewQueue
    ClaimsDB --> Reports
    ValidationEngine --> Notifications
    
    ReviewQueue -->|Approved| Payer
    ClaimsDB -->|Batch Submit| Clearinghouse
    Clearinghouse -->|Remittance| Payer
    
    style ClaimModel fill:#1168bd,stroke:#0b4884,color:#ffffff
    style RuleEngine fill:#1168bd,stroke:#0b4884,color:#ffffff
    style ValidationEngine fill:#1168bd,stroke:#0b4884,color:#ffffff
    style ClaimsDB fill:#6f42c1,stroke:#4c2e85,color:#ffffff
```

---

## Business Rules Processing Flow

```mermaid
flowchart TD
    Start([Business Analyst<br/>Creates Rule]) --> RuleType{Rule<br/>Format?}
    
    RuleType -->|Text-based| WriteDSL[Write External DSL<br/>sample-rules.dsl]
    RuleType -->|Code-based| WriteHaskell[Write Haskell Code<br/>Internal DSL]
    RuleType -->|UI-based| UseEditor[Use Rule Editor<br/>Web Interface]
    
    WriteDSL --> SaveFile[Save .dsl File]
    WriteHaskell --> CompileCode[Compile Haskell]
    UseEditor --> SaveDB[Save to Database]
    
    SaveFile --> LoadTime[Load at Runtime]
    CompileCode --> LoadTime
    SaveDB --> LoadTime
    
    LoadTime --> ParsePhase{Parse<br/>Required?}
    
    ParsePhase -->|Yes - External DSL| ParseText[Parse DSL Text<br/>Parsec Parser]
    ParsePhase -->|No - Internal DSL| SkipParse[Use Directly]
    
    ParseText --> BuildAST[Build AST<br/>ParsedRule Type]
    BuildAST --> ConvertAST[Convert to Internal<br/>convertToInternalRule]
    ConvertAST --> InternalRule[Internal Rule<br/>Rule ValidationResult]
    SkipParse --> InternalRule
    
    InternalRule --> RuleLibrary[Rule Library<br/>Collection of Rules]
    
    RuleLibrary --> ApplyClaim[Apply to Claim<br/>eval claim rule]
    
    ApplyClaim --> EvalCondition{Evaluate<br/>Condition?}
    
    EvalCondition -->|Boolean Check| CheckPredicate[Check Predicate<br/>Amount, Dx, Px, etc.]
    EvalCondition -->|Logical Op| CombineRules[Combine Rules<br/>AND, OR, NOT]
    EvalCondition -->|Conditional| EvalIf[Evaluate If-Then-Else]
    
    CheckPredicate --> ReturnBool[Return Bool Result]
    CombineRules --> ReturnBool
    
    ReturnBool --> EvalIf
    EvalIf --> ThenBranch{Condition<br/>True?}
    
    ThenBranch -->|Yes| ExecuteThen[Execute Then Action]
    ThenBranch -->|No| ExecuteElse[Execute Else Action]
    
    ExecuteThen --> ActionType{Action<br/>Type?}
    ExecuteElse --> ActionType
    
    ActionType -->|Approve| ReturnValid[Return Valid]
    ActionType -->|Reject| ReturnInvalid[Return Invalid<br/>with Message]
    ActionType -->|Review| ReturnReview[Return RequireReview<br/>with Note]
    
    ReturnValid --> CollectResults[Collect All Results]
    ReturnInvalid --> CollectResults
    ReturnReview --> CollectResults
    
    CollectResults --> FinalDecision{Final<br/>Decision}
    
    FinalDecision -->|All Valid| ApproveClaim[Approve Claim]
    FinalDecision -->|Any Reject| RejectClaim[Reject Claim]
    FinalDecision -->|Any Review| QueueReview[Queue for Review]
    
    ApproveClaim --> End([Store Result])
    RejectClaim --> End
    QueueReview --> End
    
    style Start fill:#00bc7e,stroke:#009063,color:#ffffff
    style End fill:#00bc7e,stroke:#009063,color:#ffffff
    style EvalCondition fill:#1168bd,stroke:#0b4884,color:#ffffff
    style ThenBranch fill:#1168bd,stroke:#0b4884,color:#ffffff
    style FinalDecision fill:#1168bd,stroke:#0b4884,color:#ffffff
    style ApproveClaim fill:#00bc7e,stroke:#009063,color:#ffffff
    style RejectClaim fill:#d73a49,stroke:#a72630,color:#ffffff
    style QueueReview fill:#f9826c,stroke:#d4411e,color:#ffffff
```

---

## X12 to JSON Conversion Flow

```mermaid
flowchart TD
    Start([X12 EDI File<br/>837 Transaction]) --> ParseHeader[Parse ISA/GS Headers<br/>Interchange Control]
    
    ParseHeader --> IdentifyType{Transaction<br/>Type?}
    
    IdentifyType -->|837P| ProfessionalClaim[Professional Claim<br/>Physician Services]
    IdentifyType -->|837I| InstitutionalClaim[Institutional Claim<br/>Hospital/Facility]
    IdentifyType -->|837D| DentalClaim[Dental Claim<br/>Dental Services]
    
    ProfessionalClaim --> ParseLoops[Parse Loop Structures<br/>2000A/2000B/2300/2400]
    InstitutionalClaim --> ParseLoops
    DentalClaim --> ParseLoops
    
    ParseLoops --> ExtractSegments[Extract Segments]
    
    subgraph "Segment Processing"
        ExtractSegments --> NM1[NM1 - Name Segments<br/>Patient/Provider Info]
        ExtractSegments --> REF[REF - Reference<br/>IDs and Numbers]
        ExtractSegments --> DTP[DTP - Date/Time<br/>Service Dates]
        ExtractSegments --> CLM[CLM - Claim Info<br/>Amount/Type]
        ExtractSegments --> HI[HI - Health Info<br/>Diagnosis Codes]
        ExtractSegments --> SV1[SV1/SV2 - Services<br/>Procedure Codes]
    end
    
    NM1 --> MapJSON[Map to JSON Fields]
    REF --> MapJSON
    DTP --> MapJSON
    CLM --> MapJSON
    HI --> MapJSON
    SV1 --> MapJSON
    
    MapJSON --> BuildClaimObj{Build Claim<br/>Object}
    
    BuildClaimObj --> ClaimID[claimId: REF segment]
    BuildClaimObj --> PatientID[patientId: NM1~IL]
    BuildClaimObj --> ProviderID[providerId: NM1~82]
    BuildClaimObj --> ServiceDate[serviceDate: DTP~472]
    BuildClaimObj --> Amount[totalAmount: CLM02]
    BuildClaimObj --> DiagCodes[diagnosisCodes: HI~ABK]
    BuildClaimObj --> ProcCodes[procedureCodes: SV101]
    BuildClaimObj --> POS[placeOfService: CLM05-1]
    BuildClaimObj --> Type[claimType: CLM05-3]
    
    ClaimID --> JSONObject[JSON Claim Object]
    PatientID --> JSONObject
    ProviderID --> JSONObject
    ServiceDate --> JSONObject
    Amount --> JSONObject
    DiagCodes --> JSONObject
    ProcCodes --> JSONObject
    POS --> JSONObject
    Type --> JSONObject
    
    JSONObject --> Validate[Validate JSON Schema]
    
    Validate --> ValidCheck{Schema<br/>Valid?}
    
    ValidCheck -->|Yes| CreateClaim[Create Claim Type<br/>Haskell Record]
    ValidCheck -->|No| ErrorLog[Log Conversion Error<br/>Return Error]
    
    CreateClaim --> End([Claim Object<br/>Ready for Processing])
    ErrorLog --> EndError([Conversion Failed])
    
    style Start fill:#00bc7e,stroke:#009063,color:#ffffff
    style End fill:#00bc7e,stroke:#009063,color:#ffffff
    style EndError fill:#d73a49,stroke:#a72630,color:#ffffff
    style ValidCheck fill:#1168bd,stroke:#0b4884,color:#ffffff
    style JSONObject fill:#6f42c1,stroke:#4c2e85,color:#ffffff
```

---

## Database Schema & Storage

```mermaid
flowchart TD
    subgraph "Claims Table"
        ClaimsT[Claims<br/>----<br/>claim_id: PK<br/>patient_id<br/>provider_id<br/>service_date<br/>total_amount<br/>claim_type<br/>status<br/>created_at<br/>updated_at]
    end
    
    subgraph "Diagnosis Codes Table"
        DiagT[Diagnosis_Codes<br/>----<br/>id: PK<br/>claim_id: FK<br/>code<br/>sequence<br/>description]
    end
    
    subgraph "Procedure Codes Table"
        ProcT[Procedure_Codes<br/>----<br/>id: PK<br/>claim_id: FK<br/>code<br/>modifier<br/>description<br/>charge_amount]
    end
    
    subgraph "Rules Table"
        RulesT[Business_Rules<br/>----<br/>rule_id: PK<br/>rule_name<br/>description<br/>rule_dsl_text<br/>rule_type<br/>is_active<br/>version<br/>created_by<br/>created_at]
    end
    
    subgraph "Validation Results Table"
        ValidationT[Validation_Results<br/>----<br/>id: PK<br/>claim_id: FK<br/>rule_id: FK<br/>result<br/>message<br/>executed_at]
    end
    
    subgraph "Audit Log Table"
        AuditT[Audit_Log<br/>----<br/>id: PK<br/>claim_id: FK<br/>action<br/>user_id<br/>details<br/>timestamp]
    end
    
    subgraph "Review Queue Table"
        ReviewT[Review_Queue<br/>----<br/>id: PK<br/>claim_id: FK<br/>reason<br/>priority<br/>assigned_to<br/>status<br/>reviewed_at]
    end
    
    ClaimsT -->|1:N| DiagT
    ClaimsT -->|1:N| ProcT
    ClaimsT -->|1:N| ValidationT
    ClaimsT -->|1:N| AuditT
    ClaimsT -->|0:1| ReviewT
    RulesT -->|1:N| ValidationT
    
    style ClaimsT fill:#1168bd,stroke:#0b4884,color:#ffffff
    style RulesT fill:#1168bd,stroke:#0b4884,color:#ffffff
    style ValidationT fill:#6f42c1,stroke:#4c2e85,color:#ffffff
```

---

## User Interface Components

```mermaid
flowchart LR
    subgraph "Business Analyst Tools"
        RuleEditor[Rule Editor<br/>Create/Edit DSL Rules]
        RuleValidator[Rule Validator<br/>Test Rules]
        RuleVersioning[Version Control<br/>Rule History]
    end
    
    subgraph "Claims Viewer"
        Search[Search Claims<br/>Multi-criteria Filter]
        Details[Claim Details<br/>Full Information]
        History[Claim History<br/>Audit Trail]
    end
    
    subgraph "Review Queue UI"
        Queue[Review Queue<br/>Pending Claims List]
        ReviewForm[Review Form<br/>Approve/Reject/Modify]
        Notes[Add Notes<br/>Documentation]
    end
    
    subgraph "Analytics Dashboard"
        Metrics[Key Metrics<br/>Approval/Rejection Rates]
        Trends[Trend Analysis<br/>Time-based Charts]
        RulePerf[Rule Performance<br/>Execution Statistics]
    end
    
    subgraph "Admin Console"
        UserMgmt[User Management<br/>Roles & Permissions]
        SystemConfig[System Config<br/>Settings]
        DataExport[Data Export<br/>Reports & Files]
    end
    
    Database[(Database)]
    
    RuleEditor --> Database
    Search --> Database
    Queue --> Database
    Metrics --> Database
    UserMgmt --> Database
    
    Database --> Details
    Database --> History
    Database --> Trends
    Database --> RulePerf
    
    style Database fill:#6f42c1,stroke:#4c2e85,color:#ffffff
    style RuleEditor fill:#1168bd,stroke:#0b4884,color:#ffffff
    style Queue fill:#f9826c,stroke:#d4411e,color:#ffffff
```

---

## Summary

This flowchart system illustrates:

1. **Main System Flow**: Complete end-to-end processing from claim submission to storage and notification
2. **Component Architecture**: How different system components interact
3. **Business Rules Processing**: Detailed flow of rule creation, parsing, and application
4. **X12 to JSON Conversion**: How EDI transactions are converted to internal format
5. **Database Schema**: Data storage structure and relationships
6. **User Interface Components**: Tools available to different user roles

The system supports:
- Multiple input formats (X12, JSON, Web forms)
- Flexible business rules (External DSL, Internal DSL, Database)
- Comprehensive validation and review processes
- Full audit trail and compliance logging
- Rich user interfaces for all stakeholders
