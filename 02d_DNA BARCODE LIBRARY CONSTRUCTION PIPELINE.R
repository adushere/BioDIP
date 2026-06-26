# =========================================================
# BioDIP DNA BARCODE LIBRARY CONSTRUCTION PIPELINE
# =========================================================
# This script constructs a curated DNA barcode reference
# library for Indian Sphingidae using BOLD-derived COI
# barcode records.
#
# Core functions:
# - Filters and validates species-level barcode records
# - Performs sequence quality filtering
# - Generates BIN-based species summaries
# - Identifies multi-BIN and shared-BIN taxa
# - Produces a curated non-redundant sequence dataset
# - Conducts sequence alignment and K2P analysis
# - Evaluates barcode gap performance
# - Identifies problematic and high-divergence taxa
# - Generates Neighbor-Joining phylogenetic trees
#
# Output:
# A quality-controlled DNA barcode reference library,
# barcode gap statistics, BIN diagnostics, divergence
# assessments, and phylogenetic products suitable for
# biodiversity assessment and taxonomic evaluation.
#
# BioDIP Role:
# Molecular Analysis Layer — DNA Barcode Library
# Construction, Validation, and Diagnostic Assessment.
# =========================================================
R Script for Indian Sphingid Barcode Library
# =========================================================
# BIN-BASED DNA BARCODE REFERENCE LIBRARY
# Indian Sphingidae from BOLD
# COMPLETE CURATED WORKFLOW SCRIPT
# (Error-Corrected Version)
# =========================================================

# =========================================================
# STEP 0 — INSTALL REQUIRED PACKAGES
# =========================================================

install.packages(c(
  "tidyverse",
  "readr",
  "seqinr",
  "msa",
  "Biostrings",
  "ape",
  "pegas",
  "spider"
))

# =========================================================
# STEP 1 — LOAD LIBRARIES
# =========================================================

library(tidyverse)
library(readr)
library(seqinr)
library(msa)
library(Biostrings)
library(ape)
library(pegas)
library(spider)

# =========================================================
# STEP 2 — IMPORT BOLD CSV
# =========================================================

bold_data <- read_csv(""D:/Adu/Publication/Moths/iBol conference/hawkmoth_bold_world.csv"")

# Inspect columns
colnames(bold_data)

# =========================================================
# STEP 3 — FILTER INDIAN RECORDS
# =========================================================

india_data <- bold_data %>%
  filter(country.ocean == "India")

# Check number of records
nrow(india_data)

# =========================================================
# STEP 4 — KEEP ONLY COI-5P BARCODES
# =========================================================

# Check marker types
unique(india_data$marker_code)

# Keep standard barcode region
india_data <- india_data %>%
  filter(marker_code == "COI-5P")

# =========================================================
# STEP 5 — REMOVE RECORDS WITHOUT SPECIES IDs
# =========================================================

india_data <- india_data %>%
  filter(!is.na(species),
         species != "")

# Remove ambiguous IDs
india_data <- india_data %>%
  filter(!grepl("sp\\.|cf\\.|aff\\.",
                species,
                ignore.case = TRUE))

# =========================================================
# STEP 6 — REMOVE RECORDS WITHOUT SEQUENCES
# =========================================================

india_data <- india_data %>%
  filter(!is.na(nuc),
         nuc != "")

# =========================================================
# STEP 7 — CALCULATE SEQUENCE LENGTH
# =========================================================

india_data$seq_length <- nchar(india_data$nuc)

summary(india_data$seq_length)

# =========================================================
# STEP 8 — REMOVE SHORT SEQUENCES
# =========================================================

india_data <- india_data %>%
  filter(seq_length >= 500)

# Dataset summary
dim(india_data)

length(unique(india_data$species))

# =========================================================
# STEP 9 — SAVE CLEAN DATASET
# =========================================================

write_csv(india_data,
          "Indian_Sphingidae_CLEAN.csv")

# =========================================================
# STEP 10 — VERIFIED BIN SUMMARY
# =========================================================

bin_summary <- india_data %>%
  filter(!is.na(bin_uri),
         bin_uri != "") %>%
  group_by(species, bin_uri) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  ) %>%
  arrange(species, desc(n_records))

View(bin_summary)

write_csv(bin_summary,
          "Verified_BIN_Summary.csv")

# =========================================================
# STEP 11 — SPECIES WITH MULTIPLE BINs
# =========================================================

species_multiple_bins <- india_data %>%
  filter(!is.na(bin_uri),
         bin_uri != "") %>%
  group_by(species) %>%
  summarise(
    n_bins = n_distinct(bin_uri),
    bins = paste(unique(bin_uri),
                 collapse = "; "),
    n_records = n(),
    .groups = "drop"
  ) %>%
  filter(n_bins > 1) %>%
  arrange(desc(n_bins))

View(species_multiple_bins)

write_csv(species_multiple_bins,
          "Species_With_Multiple_BINs.csv")

# =========================================================
# STEP 12 — SHARED BIN ANALYSIS
# =========================================================

shared_bins <- india_data %>%
  filter(!is.na(bin_uri),
         bin_uri != "") %>%
  group_by(bin_uri) %>%
  summarise(
    n_species = n_distinct(species),
    species_list = paste(unique(species),
                         collapse = "; "),
    n_records = n(),
    .groups = "drop"
  ) %>%
  filter(n_species > 1) %>%
  arrange(desc(n_species))

View(shared_bins)

write_csv(shared_bins,
          "Shared_BINs.csv")

# =========================================================
# STEP 13 — DEDUPLICATE SEQUENCES
# =========================================================

sum(duplicated(india_data$nuc))

india_unique <- india_data %>%
  distinct(nuc, .keep_all = TRUE)

# Compare sizes
nrow(india_data)
nrow(india_unique)

# =========================================================
# STEP 14 — CREATE FASTA HEADERS
# =========================================================

india_unique$fasta_header <- paste(
  india_unique$species,
  india_unique$bin_uri,
  india_unique$processid,
  sep = "|"
)

head(india_unique$fasta_header)

# =========================================================
# STEP 15 — PREPARE FASTA EXPORT
# =========================================================

fasta_sequences <- strsplit(india_unique$nuc, "")

# =========================================================
# STEP 16 — EXPORT FASTA FILE
# =========================================================

write.fasta(
  sequences = fasta_sequences,
  names = india_unique$fasta_header,
  file.out = "Indian_Sphingidae_Unique.fasta"
)

# =========================================================
# STEP 17 — READ FASTA FOR ALIGNMENT
# =========================================================

seqs <- readDNAStringSet(
  "Indian_Sphingidae_Unique.fasta"
)

length(seqs)

# =========================================================
# STEP 18 — MULTIPLE SEQUENCE ALIGNMENT
# =========================================================

alignment <- msa(
  seqs,
  method = "Muscle"
)

alignment

# =========================================================
# STEP 19 — EXPORT ALIGNED FASTA
# =========================================================

aligned <- as(alignment, "DNAStringSet")

writeXStringSet(
  aligned,
  filepath = "Indian_Sphingidae_Aligned.fasta"
)

# =========================================================
# STEP 20 — READ ALIGNED FASTA
# =========================================================

aligned_seqs <- read.dna(
  "Indian_Sphingidae_Aligned.fasta",
  format = "fasta"
)

# =========================================================
# STEP 21 — K2P DISTANCE MATRIX
# =========================================================

k2p_dist <- dist.dna(
  aligned_seqs,
  model = "K80",
  pairwise.deletion = TRUE
)

# Inspect
k2p_dist

# Small preview
as.matrix(k2p_dist)[1:5, 1:5]

# Save matrix
write.csv(
  as.matrix(k2p_dist),
  "K2P_Distance_Matrix.csv"
)

# =========================================================
# STEP 22 — EXTRACT SPECIES NAMES
# =========================================================

seq_names <- rownames(as.matrix(k2p_dist))

head(seq_names)

species_vector <- sapply(
  strsplit(seq_names, "\\|"),
  `[`,
  1
)

head(species_vector)

# =========================================================
# STEP 23 — BARCODE GAP ANALYSIS
# =========================================================

max_intra <- maxInDist(
  k2p_dist,
  species_vector
)

min_inter <- nonConDist(
  k2p_dist,
  species_vector
)

# =========================================================
# STEP 24 — CREATE BARCODE SUMMARY
# (CORRECTED VERSION)
# =========================================================

barcode_summary <- data.frame(
  Species = species_vector,
  Max_Intra = max_intra,
  Nearest_Neighbor = min_inter
)

# =========================================================
# STEP 25 — SPECIES-LEVEL SUMMARY
# =========================================================

barcode_summary_species <- barcode_summary %>%
  group_by(Species) %>%
  summarise(
    Max_Intra = max(Max_Intra,
                    na.rm = TRUE),
    Nearest_Neighbor = min(Nearest_Neighbor,
                           na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# STEP 26 — BARCODE GAP STATUS
# =========================================================

barcode_summary_species$Barcode_Gap <-
  barcode_summary_species$Nearest_Neighbor >
  barcode_summary_species$Max_Intra

head(barcode_summary_species)

# Save
write.csv(
  barcode_summary_species,
  "Barcode_Gap_Summary.csv",
  row.names = FALSE
)

# =========================================================
# STEP 27 — SPECIES WITHOUT BARCODE GAP
# =========================================================

problem_species <- barcode_summary_species %>%
  filter(Barcode_Gap == FALSE)

View(problem_species)

write.csv(
  problem_species,
  "Species_Without_Barcode_Gap.csv",
  row.names = FALSE
)

# =========================================================
# STEP 28 — HIGH INTRASPECIFIC DIVERGENCE
# =========================================================

high_divergence <- barcode_summary_species %>%
  filter(Max_Intra > 0.02)

View(high_divergence)

write.csv(
  high_divergence,
  "High_Intraspecific_Divergence.csv",
  row.names = FALSE
)

# =========================================================
# STEP 29 — NJ TREE CONSTRUCTION
# (CORRECTED VERSION USING njs())
# =========================================================

nj_tree <- njs(k2p_dist)

# Plot
plot(
  nj_tree,
  cex = 0.2,
  no.margin = TRUE
)

# =========================================================
# STEP 30 — SAVE NJ TREE
# =========================================================

write.tree(
  nj_tree,
  file = "Indian_Sphingidae_NJ_Tree.nwk"
)

# Optional PDF export
pdf("Indian_Sphingidae_NJ_Tree.pdf",
    width = 20,
    height = 25)

plot(
  nj_tree,
  cex = 0.2
)

dev.off()

# =========================================================
# END OF COMPLETE WORKFLOW
# =========================================================
