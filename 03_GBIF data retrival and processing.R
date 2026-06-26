# =====================================================
# BioDIP GBIF DATA RECOVERY & CLEANING PIPELINE
# =====================================================
# This script retrieves a previously generated GBIF occurrence
# download (via GBIF download key) and performs data cleaning
# and quality control for Indian Sphingidae occurrence records.
#
# Core functions:
# - Downloads GBIF occurrence archive using download key
# - Imports GBIF Darwin Core archive into R
# - Filters records based on accepted taxonomic status
# - Performs spatial data quality filtering using CoordinateCleaner
# - Removes records failing coordinate validity checks
# - Exports cleaned occurrence dataset in CSV and RDS formats
#
# Output:
# A high-quality, coordinate-validated GBIF occurrence dataset
# used for species distribution analysis and integration with
# barcode-based molecular datasets in BioDIP.
# =====================================================
# =====================================================
# INDIAN SPHINGIDAE GBIF PROJECT
# RECOVER CLEANED GBIF DATASET
# =====================================================

# -----------------------------------------------------
# Load Packages
# -----------------------------------------------------

library(rgbif)
library(tidyverse)
library(CoordinateCleaner)

# -----------------------------------------------------
# Set Download Key
# -----------------------------------------------------

download_key <- "0011003-260519110011954"

# -----------------------------------------------------
# Recover GBIF Download Object
# -----------------------------------------------------

downloaded_data <- occ_download_get(
  download_key,
  path = "data_raw/",
  overwrite = FALSE
)

# -----------------------------------------------------
# Import GBIF Archive
# -----------------------------------------------------

india_sphingidae_full <- occ_download_import(
  downloaded_data
)

# -----------------------------------------------------
# Filter Accepted Taxa
# -----------------------------------------------------

india_sphingidae_qc <- india_sphingidae_full %>%
  filter(
    taxonomicStatus %in% c(
      "ACCEPTED",
      "PROVISIONALLY_ACCEPTED"
    )
  )

# -----------------------------------------------------
# Coordinate Cleaning
# -----------------------------------------------------

india_sphingidae_qc <- clean_coordinates(
  x = india_sphingidae_qc,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "species",
  tests = c(
    "capitals",
    "centroids",
    "equal",
    "gbif",
    "institutions",
    "zeros",
    "seas"
  )
)

# -----------------------------------------------------
# Retain Clean Coordinates
# -----------------------------------------------------

india_sphingidae_clean <- india_sphingidae_qc %>%
  filter(.summary == TRUE)

# -----------------------------------------------------
# Save Clean Dataset as CSV
# -----------------------------------------------------

write.csv(
  india_sphingidae_clean,
  file = "data_clean/india_sphingidae_clean.csv",
  row.names = FALSE
)

# -----------------------------------------------------
# Save Clean Dataset as RDS
# -----------------------------------------------------

saveRDS(
  india_sphingidae_clean,
  file = "data_clean/india_sphingidae_clean.rds"
)

# -----------------------------------------------------
# Verify Files
# -----------------------------------------------------

file.exists("data_clean/india_sphingidae_clean.csv")

file.exists("data_clean/india_sphingidae_clean.rds")

# -----------------------------------------------------
# Dataset Summary
# -----------------------------------------------------

dim(india_sphingidae_clean)

length(unique(india_sphingidae_clean$species))

length(unique(india_sphingidae_clean$genus))