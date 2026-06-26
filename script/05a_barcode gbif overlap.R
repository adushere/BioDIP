# =====================================================
# BioDIP SPECIES-LEVEL DATASET OVERLAP ANALYSIS
# =====================================================
# This script compares species representation between
# GBIF occurrence records and barcode reference datasets.
#
# Core functions:
# - Extracts unique standardized species names
# - Identifies species shared between datasets
# - Identifies GBIF-only species
# - Identifies barcode-only species
# - Quantifies barcode coverage relative to occurrence data
#
# Output:
# Species overlap tables used to assess barcode coverage,
# sampling gaps, and priority taxa for future barcoding efforts.
# =====================================================
gbif_species <- gbif_occ %>%
  distinct(species_std)

barcode_species <- barcode_raw %>%
  distinct(species_std)

shared_species <- inner_join(
  gbif_species,
  barcode_species,
  by = "species_std"
)

gbif_only_species <- anti_join(
  gbif_species,
  barcode_species,
  by = "species_std"
)

barcode_only_species <- anti_join(
  barcode_species,
  gbif_species,
  by = "species_std"
)

nrow(shared_species)

nrow(gbif_only_species)

nrow(barcode_only_species)

print(shared_species, n = Inf)

print(gbif_only_species, n = Inf)

print(barcode_only_species, n = Inf)
