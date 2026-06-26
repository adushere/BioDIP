# =====================================================
# BioDIP GBIF RECORD VERIFICATION ASSESSMENT
# =====================================================
# This script classifies GBIF occurrence records according
# to their verification evidence and record type.
#
# Core functions:
# - Categorizes records using basisOfRecord and media fields
# - Separates preserved specimens from observations
# - Identifies photographic observations
# - Identifies observations lacking supporting media
# - Summarizes verification categories across the dataset
#
# Output:
# Verification statistics used to assess confidence levels,
# evidentiary support, and data quality within GBIF occurrence
# records.
# =====================================================
library(tidyverse)

india_sphingidae_verification <- india_sphingidae_clean %>%
  mutate(
    
    verification_category = case_when(
      
      basisOfRecord == "PRESERVED_SPECIMEN" ~
        "Preserved Specimen",
      
      basisOfRecord == "MATERIAL_SAMPLE" ~
        "Material Sample",
      
      basisOfRecord == "HUMAN_OBSERVATION" &
        mediaType != "" &
        !is.na(mediaType) ~
        "Photographic Observation",
      
      basisOfRecord == "HUMAN_OBSERVATION" ~
        "Observation Without Media",
      
      TRUE ~
        "Other"
    )
  )

table(india_sphingidae_verification$verification_category)

nrow(india_sphingidae_verification)

cat(
  "Total observations:",
  nrow(india_sphingidae_verification)
)

table(india_sphingidae_verification$verification_category)
