RULE HighValueReview
DESCRIPTION "Flag high-value claims for manual review"
WHEN
  claim.amount > 50000
THEN
  REQUIRE_REVIEW "Claim exceeds high-value threshold"
END

RULE EmergencyRoomValidation
DESCRIPTION "Validate ER claims have appropriate diagnoses"
WHEN
  claim.place_of_service = "23" AND
  NOT (claim.has_diagnosis "S06" OR claim.has_diagnosis "I21")
THEN
  REJECT "Emergency room claim missing emergency diagnosis"
END

RULE InpatientAdmissionCheck
DESCRIPTION "Ensure inpatient claims include admission codes"
WHEN
  claim.type = "Inpatient" AND
  NOT (claim.has_procedure "99221" OR 
       claim.has_procedure "99222" OR 
       claim.has_procedure "99223")
THEN
  REJECT "Missing required inpatient admission procedure code"
END

RULE PreventiveCareLimit
DESCRIPTION "Cap preventive care costs in office setting"
WHEN
  claim.type = "Professional" AND
  claim.amount > 10000 AND
  claim.place_of_service = "11" AND
  (claim.has_diagnosis "Z00.00" OR claim.has_diagnosis "Z00.01")
THEN
  REJECT "Preventive care exceeds maximum allowed amount"
ELSE WHEN
  claim.type = "Professional" AND
  claim.amount > 10000 AND
  claim.place_of_service = "11"
THEN
  REQUIRE_REVIEW "High-value professional claim needs verification"
END

RULE OutpatientSurgeryMinimum
DESCRIPTION "Validate outpatient surgery claims meet minimum"
WHEN
  claim.type = "Outpatient" AND
  claim.place_of_service = "24" AND
  claim.amount < 100
THEN
  REJECT "Outpatient surgical procedure amount below minimum threshold"
END
