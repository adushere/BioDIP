# =====================================================
# BioDIP BOLD BIN-BASED DNA BARCODE PIPELINE
# =====================================================
# This script performs BIN-based DNA barcode processing and
# sequence analysis for Indian Sphingidae using BOLD-derived data.
#
# Core functions:
# - Imports and filters BOLD COI-5P barcode dataset
# - Applies strict sequence QC (species validity, marker filtering,
#   minimum sequence length thresholds)
# - Removes ambiguous taxonomic labels (sp., cf., aff.)
# - Computes sequence length statistics
# - Generates BIN-level summaries per species
# - Identifies species with multiple BIN assignments
# - Detects shared BINs across species (potential taxonomic conflict)
# - Removes duplicate sequences and prepares FASTA dataset
# - Performs multiple sequence alignment (MUSCLE via msa)
# - Computes genetic distances using K2P (Kimura 2-parameter)
# - Estimates barcode gap (intra vs interspecific divergence)
# - Builds NJ phylogenetic tree
# - Identifies problematic taxa and high divergence species
#
# Output:
# A curated and aligned barcode dataset with BIN structure analysis,
# barcode gap metrics, phylogenetic tree, and diagnostic outputs
# for downstream biodiversity and taxonomic inference in BioDIP.
# =====================================================
# =========================================================
# BIN-BASED DNA BARCODE REFERENCE LIBRARY
# Indian Sphingidae from BOLD
# Workflow Script (Up to Step 5.2)
# =========================================================

# =========================================================
# STEP 0 — INSTALL REQUIRED PACKAGES
# =========================================================

install.packages(c(
  "tidyverse",
  "readr",
  "seqinr",
  "msa",
  "Biostrings"
))

# =========================================================
# STEP 1 — LOAD LIBRARIES
# =========================================================

library(tidyverse)
library(readr)
library(seqinr)
library(msa)
library(Biostrings)

# =========================================================
# STEP 2 — IMPORT BOLD CSV
# =========================================================

bold_data <- read_csv("D:/Adu/Publication/Moths/iBol conference/hawkmoth_bold_world.csv")

# Check column names
colnames(bold_data)

# =========================================================
# STEP 3 — FILTER INDIAN RECORDS
# =========================================================

india_data <- bold_data %>%
  filter(country.ocean == "India")

# Check number of Indian records
nrow(india_data)

# =========================================================
# STEP 4 — KEEP ONLY COI-5P BARCODES
# =========================================================

# Check available markers
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

# Remove ambiguous identifications
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

# Check summary statistics
summary(india_data$seq_length)

# =========================================================
# STEP 8 — REMOVE SHORT SEQUENCES
# =========================================================

india_data <- india_data %>%
  filter(seq_length >= 500)

# Check dataset dimensions
dim(india_data)

# Species count
length(unique(india_data$species))

# =========================================================
# STEP 9 — SAVE CLEAN DATASET
# =========================================================

write_csv(india_data,
          "Indian_Sphingidae_CLEAN.csv")

# =========================================================
# STEP 10 — CREATE VERIFIED BIN SUMMARY
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

# View table
View(bin_summary)

# Save BIN summary
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

# View
View(species_multiple_bins)

# Save
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

# View
View(shared_bins)

# Save
write_csv(shared_bins,
          "Shared_BINs.csv")

# =========================================================
# STEP 13 — DEDUPLICATE SEQUENCES
# =========================================================

# Number of duplicate sequences
sum(duplicated(india_data$nuc))

# Keep only unique sequences
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

# Check headers
head(india_unique$fasta_header)

# =========================================================
# STEP 15 — CONVERT SEQUENCES FOR FASTA EXPORT
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

# =========================================================
# END OF SCRIPT (UP TO STEP 5.2)
# ============================================
=============
  alignment <- msa(
seqs,
method = "Muscle"
)
alignment
aligned <- as(alignment, "DNAStringSet")
writeXStringSet(
aligned,
filepath = "Indian_Sphingidae_Aligned.fasta"
)
aligned_seqs <- read.dna(
"Indian_Sphingidae_Aligned.fasta",
format = "fasta"
)
k2p_dist <- dist.dna(
aligned_seqs,
model = "K80",
pairwise.deletion = TRUE
)
k2p_dist
as.matrix(k2p_dist)[1:5, 1:5]
write.csv(
as.matrix(k2p_dist),
"K2P_Distance_Matrix.csv"
)
seq_names <- rownames(as.matrix(k2p_dist))
head(seq_names)
species_vector <- sapply(
strsplit(seq_names, "\\|"),
`[`,
1
)
head(species_vector)
barcode_gap <- maxInDist(
k2p_dist,
species_vector
)
head(barcode_gap)
write.csv(
barcode_gap,
"Barcode_Gap_Analysis.csv"
)
nj_tree <- nj(k2p_dist)
sum(is.na(k2p_dist))
nj_tree <- njs(k2p_dist)
plot(
nj_tree,
cex = 0.2,
no.margin = TRUE
)
write.tree(
nj_tree,
file = "Indian_Sphingidae_NJ_Tree.nwk"
)
which(is.na(as.matrix(k2p_dist)), arr.ind = TRUE)
seq_names <- rownames(as.matrix(k2p_dist))
species_vector <- sapply(
strsplit(seq_names, "\\|"),
`[`,
1
)
max_intra <- maxInDist(
k2p_dist,
species_vector
)
min_inter <- nonConDist(
k2p_dist,
species_vector
)
barcode_summary <- data.frame(
Species = names(max_intra),
Max_Intra = max_intra,
Nearest_Neighbor = min_inter
)
names(max_intra)
min_inter
str(max_intra)
head(max_intra)
species_names <- unique(species_vector)
barcode_summary <- data.frame(
Species = species_names,
Max_Intra = max_intra,
Nearest_Neighbor = min_inter
)
length(species_names)
length(max_intra)
length(min_inter)
barcode_summary <- data.frame(
Species = species_vector,
Max_Intra = max_intra,
Nearest_Neighbor = min_inter
)
barcode_summary_species <- barcode_summary %>%
group_by(Species) %>%
summarise(
Max_Intra = max(Max_Intra, na.rm = TRUE),
Nearest_Neighbor = min(Nearest_Neighbor, na.rm = TRUE),
.groups = "drop"
)
barcode_summary_species$Barcode_Gap <-
barcode_summary_species$Nearest_Neighbor >
barcode_summary_species$Max_Intra
head(barcode_summary_species)
write.csv(
barcode_summary_species,
"Barcode_Gap_Summary.csv",
row.names = FALSE
)
problem_species <- barcode_summary_species %>%
filter(Barcode_Gap == FALSE)
View(problem_species)
write.csv(
problem_species,
"Species_Without_Barcode_Gap.csv",
row.names = FALSE
)
high_divergence <- barcode_summary_species %>%
filter(Max_Intra > 0.02)
View(high_divergence)
write.csv(
high_divergence,
"High_Intraspecific_Divergence.csv",
row.names = FALSE
)
