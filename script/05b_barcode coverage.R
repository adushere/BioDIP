# =====================================================
# BioDIP BARCODE COVERAGE ASSESSMENT
# =====================================================
# This script evaluates species-level barcode coverage
# by comparing standardized GBIF occurrence species
# against the geographically validated barcode dataset.
#
# Core functions:
# - Extracts unique species from GBIF and barcode datasets
# - Identifies shared species between datasets
# - Identifies GBIF-only species lacking barcode coverage
# - Identifies barcode-only species absent from GBIF records
# - Calculates overall barcode coverage percentage
#
# Output:
# Species overlap statistics and barcode coverage metrics
# used for biodiversity gap analysis and prioritization
# of future barcoding efforts.
# =====================================================
gbif_species <- gbif_occ %>%
  distinct(species_std) %>%
  arrange(species_std)

barcode_species <- barcode_meta_final %>%
  distinct(species_std) %>%
  arrange(species_std)

nrow(gbif_species)

nrow(barcode_species)

head(gbif_species)

head(barcode_species)

gbif_only_species <- anti_join(
  gbif_species,
  barcode_species,
  by = "species_std"
)

nrow(gbif_only_species)

head(gbif_only_species, 20)


barcode_only_species <- anti_join(
  barcode_species,
  gbif_species,
  by = "species_std"
)

nrow(barcode_only_species)

barcode_only_species


shared_species <- semi_join(
  gbif_species,
  barcode_species,
  by = "species_std"
)

nrow(shared_species)

head(shared_species, 20)


total_gbif_species <- nrow(gbif_species)

total_barcode_species <- nrow(shared_species)

barcode_coverage_percent <- (
  total_barcode_species / total_gbif_species
) * 100

barcode_coverage_percent
