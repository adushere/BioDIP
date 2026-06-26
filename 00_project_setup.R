# =====================================================
# BioDIP PROJECT INITIALIZATION & ENVIRONMENT SETUP
# =====================================================
# This script establishes the foundational structure for the
# BioDIP (Biodiversity Data Integration Protocol) project.
#
# It creates a standardized folder architecture for organizing
# raw data, cleaned data, metadata, scripts, outputs, figures,
# logs, and spatial/environmental layers.
#
# It also initializes the R analytical environment by:
# - Installing and loading required biodiversity and spatial packages
# - Setting global R options for reproducibility and stability
# - Defining project directory paths for modular workflow design
# - Configuring API timeout settings for GBIF data retrieval
#
# This script acts as the root setup layer for all downstream
# data mining, cleaning, integration, and analysis workflows
# in the BioDIP framework.
# =====================================================
dir.create("data_raw")
dir.create("data_clean")
dir.create("metadata")
dir.create("scripts")
dir.create("outputs")
dir.create("figures")
dir.create("logs")
dir.create("shapefiles")
dir.create("environmental_layers")
dir.create("manuscripts")
getwd()

# =====================================================
# INDIAN SPHINGIDAE GBIF PROJECT
# Project Setup
# =====================================================

# =====================================================
# Install Required Packages
# =====================================================

required_packages <- c(
  "tidyverse",
  "rgbif",
  "CoordinateCleaner",
  "sf",
  "terra",
  "data.table",
  "janitor",
  "lubridate",
  "countrycode"
)

required_packages %in% installed.packages()[, "Package"]


# =====================================================
# Load Required Packages
# =====================================================

library(tidyverse)
library(rgbif)
library(CoordinateCleaner)
library(sf)
library(terra)
library(data.table)
library(janitor)
library(lubridate)
library(countrycode)

sessionInfo()


# =====================================================
# Global Project Options
# =====================================================

options(stringsAsFactors = FALSE)

options(
  scipen = 999,
  digits = 4,
  timeout = 600
)


# =====================================================
# Project Paths
# =====================================================

raw_data_dir <- "data_raw"
clean_data_dir <- "data_clean"
metadata_dir <- "metadata"
scripts_dir <- "scripts"
outputs_dir <- "outputs"
figures_dir <- "figures"
logs_dir <- "logs"
shapefiles_dir <- "shapefiles"
env_layers_dir <- "environmental_layers"
manuscripts_dir <- "manuscripts"


# =====================================================
# Session Information
# =====================================================

project_start_time <- Sys.time()

project_start_time


list.files()

list.files(raw_data_dir)

list.files(clean_data_dir)


# =====================================================
# GBIF Connection Settings
# =====================================================

options(timeout = 600)

library(httr)

set_config(
  config(
    connecttimeout = 600,
    timeout = 600
  )
)