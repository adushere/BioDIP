# =====================================================
# BioDIP MOLECULAR DATA INTEGRATION PIPELINE
# =====================================================
# This script integrates molecular biodiversity records
# from BOLD and GenBank to construct a unified barcode
# reference dataset with explicit source provenance.
#
# Core functions:
# - Retrieves public Sphingidae records from BOLD
# - Extracts GenBank accessions linked to BOLD records
# - Mines GenBank for additional COI records
# - Detects BOLD-origin records absent from public BOLD
# - Identifies GenBank-exclusive molecular records
# - Assigns provenance and overlap classifications
# - Builds an integrated molecular reference dataset
#
# Output:
# A source-tracked molecular dataset containing
# public BOLD records, BOLD–GenBank overlaps,
# private/unavailable BOLD-linked records recovered
# through GenBank, and GenBank-exclusive records.
# =====================================================

# =========================================================
# INTEGRATED SPHINGIDAE BARCODE DATASET PIPELINE
# =========================================================
# DATA SOURCES INCLUDED:
#
# 1. PUBLIC BOLD RECORDS
# 2. PRIVATE/UNAVAILABLE BOLD-ORIGIN RECORDS
#    DETECTABLE THROUGH GENBANK METADATA
# 3. PURE GENBANK-ONLY RECORDS
#
# =========================================================

# =========================================================
# LOAD LIBRARIES
# =========================================================

library(bold)
library(dplyr)
library(rentrez)
library(stringr)
library(purrr)
library(tidyr)

# =========================================================
# STEP 1: DOWNLOAD PUBLIC BOLD IDS
# =========================================================

ids_sphingidae <- bold.public.search(
  taxonomy = list("Sphingidae")
)

View(ids_sphingidae)

dim(ids_sphingidae)

# =========================================================
# STEP 2: FETCH PUBLIC BOLD DATA
# =========================================================

bold_data <- bold.fetch(
  get_by = "processid",
  identifiers = ids_sphingidae$processid
)

View(bold_data)

dim(bold_data)

# =========================================================
# STEP 3: CLEAN PUBLIC BOLD DATA
# =========================================================

bold_data_clean <- bold_data %>%
  
  select(
    processid,
    sampleid,
    bin_uri,
    kingdom,
    phylum,
    class,
    order,
    family,
    genus,
    species,
    marker_code,
    nuc,
    coord,
    country_iso,
    province.state,
    identified_by,
    insdc_acs
  ) %>%
  
  mutate(
    source_origin = "BOLD_public",
    bold_status = "Present_in_BOLD",
    primary_source = "BOLD"
  )

View(bold_data_clean)

# =========================================================
# STEP 4: EXTRACT GENBANK ACCESSIONS
# ALREADY PRESENT IN PUBLIC BOLD
# =========================================================

bold_gb_accessions <- bold_data_clean %>%
  
  filter(
    !is.na(insdc_acs),
    insdc_acs != ""
  ) %>%
  
  dplyr::pull(insdc_acs) %>%
  
  unique()

length(bold_gb_accessions)

head(bold_gb_accessions)

# =========================================================
# STEP 5: SEARCH GENBANK
# =========================================================

search_term <- "Sphingidae[Organism] AND COI[All Fields]"

genbank_search <- rentrez::entrez_search(
  db = "nuccore",
  term = search_term,
  retmax = 50000
)

length(genbank_search$ids)

# =========================================================
# STEP 6: SPLIT GENBANK IDS INTO BATCHES
# =========================================================

batch_size <- 50

id_batches <- split(
  genbank_search$ids,
  ceiling(seq_along(genbank_search$ids) / batch_size)
)

length(id_batches)

# =========================================================
# STEP 7: FETCH GENBANK SUMMARIES
# =========================================================

genbank_summary_list <- lapply(
  
  id_batches,
  
  function(x) {
    
    rentrez::entrez_summary(
      db = "nuccore",
      id = x
    )
  }
)

length(genbank_summary_list)

# =========================================================
# STEP 8: CONVERT GENBANK SUMMARIES
# TO DATAFRAME
# =========================================================

genbank_df <- bind_rows(
  
  lapply(
    
    genbank_summary_list,
    
    function(batch) {
      
      tibble(
        
        accession = unlist(
          lapply(
            batch,
            function(x) {
              
              if(is.list(x) && "caption" %in% names(x)) {
                as.character(x$caption)
              } else {
                NA_character_
              }
            }
          )
        ),
        
        title = unlist(
          lapply(
            batch,
            function(x) {
              
              if(is.list(x) && "title" %in% names(x)) {
                as.character(x$title)
              } else {
                NA_character_
              }
            }
          )
        )
      )
    }
  )
)

View(genbank_df)

dim(genbank_df)

head(genbank_df)

# =========================================================
# STEP 9: DETECT LIKELY BOLD-ORIGIN RECORDS
# INSIDE GENBANK
# =========================================================
#
# THESE INCLUDE:
# - PRIVATE BOLD PROJECTS
# - EMBARGOED DATASETS
# - BOLD-LINKED SUBMISSIONS
#
# DETECTION IS BASED ON:
# - BOLD
# - BIN
# - BARCODE OF LIFE
# - PROJECT CODES (e.g., MOTH)
#
# =========================================================

genbank_df <- genbank_df %>%
  
  mutate(
    
    bold_linked = case_when(
      
      str_detect(
        title,
        regex("BOLD", ignore_case = TRUE)
      ) ~ TRUE,
      
      str_detect(
        title,
        regex("BIN", ignore_case = TRUE)
      ) ~ TRUE,
      
      str_detect(
        title,
        regex("Barcode of Life", ignore_case = TRUE)
      ) ~ TRUE,
      
      str_detect(
        title,
        regex("MOTH", ignore_case = TRUE)
      ) ~ TRUE,
      
      TRUE ~ FALSE
    )
  )

table(genbank_df$bold_linked)

# =========================================================
# STEP 10: IDENTIFY PRIVATE/UNAVAILABLE
# BOLD-ORIGIN RECORDS
# =========================================================

private_bold_genbank <- genbank_df %>%
  
  filter(
    bold_linked == TRUE,
    !(accession %in% bold_gb_accessions)
  ) %>%
  
  mutate(
    source_origin = "BOLD_private_with_GenBank",
    bold_status = "Private_or_Unavailable_in_BOLD",
    genbank_status = "Present_in_GenBank",
    overlap_class = "Private_BOLD_GenBank",
    primary_source = "GenBank"
  )

View(private_bold_genbank)

nrow(private_bold_genbank)

# =========================================================
# STEP 11: IDENTIFY PURE GENBANK-ONLY RECORDS
# =========================================================

genbank_only <- genbank_df %>%
  
  filter(
    !(accession %in% bold_gb_accessions),
    bold_linked == FALSE
  ) %>%
  
  mutate(
    source_origin = "GenBank_only",
    bold_status = "Absent_from_BOLD",
    genbank_status = "Present_in_GenBank",
    overlap_class = "GenBank_unique",
    primary_source = "GenBank"
  )

View(genbank_only)

nrow(genbank_only)

# =========================================================
# STEP 12: FINALIZE PUBLIC BOLD LABELS
# =========================================================

bold_data_clean <- bold_data_clean %>%
  
  mutate(
    
    genbank_status = case_when(
      
      !is.na(insdc_acs) &
        insdc_acs != "" ~
        "Present_in_GenBank",
      
      TRUE ~
        "Absent_in_GenBank"
    ),
    
    overlap_class = case_when(
      
      genbank_status == "Present_in_GenBank" ~
        "BOLD_GenBank_overlap",
      
      TRUE ~
        "BOLD_unique"
    )
  )

table(bold_data_clean$overlap_class)

# =========================================================
# STEP 13: COMBINE ALL DATA SOURCES
# =========================================================

master_dataset <- bind_rows(
  
  bold_data_clean,
  
  private_bold_genbank,
  
  genbank_only
)

View(master_dataset)

dim(master_dataset)

table(master_dataset$source_origin)

# =========================================================
# STEP 14: SAVE MASTER DATASET
# =========================================================

write.csv(
  master_dataset,
  "Sphingidae_master_dataset.csv",
  row.names = FALSE
)

# =========================================================
# END OF PIPELINE
# =========================================================
