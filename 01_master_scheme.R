# =====================================================
# BioDIP MASTER DATA SCHEMA DEFINITION
# =====================================================
# This script defines the standardized data structure for
# the BioDIP (Biodiversity Data Integration Protocol) framework.
#
# It establishes a unified schema for integrating barcode
# (BOLD/GenBank) and occurrence (GBIF) datasets into a single
# harmonized biodiversity data model.
#
# The schema includes:
# - Taxonomic hierarchy and validation fields
# - Molecular barcode metadata (COI sequences, BINs)
# - Provenance tracking (source origin, overlap classification)
# - Geographic coordinates and spatial attributes
# - Quality control flags (sequence and coordinate validation)
# - Dataset management and versioning fields
#
# This structure serves as the backbone for all downstream
# data ingestion, cleaning, integration, and analysis steps
# in the BioDIP pipeline.
# =====================================================

master_columns <- c(
  
  # =====================================================
  # RECORD IDENTIFIERS
  # =====================================================
  "record_id",
  "processid",
  "sampleid",
  "fieldid",
  "specimenid",
  
  # =====================================================
  # TAXONOMY
  # =====================================================
  "order",
  "family",
  "subfamily",
  "tribe",
  "genus",
  "species",
  "subspecies",
  "identification_rank",
  "identification_method",
  
  # =====================================================
  # TAXONOMIC QC
  # =====================================================
  "original_species_name",
  "accepted_species_name",
  "taxonomic_status",
  "synonym_flag",
  
  # =====================================================
  # BOLD / BIN INFORMATION
  # =====================================================
  "bin_uri",
  "bin_status",
  "bold_record_status",
  
  # =====================================================
  # GENBANK INFORMATION
  # =====================================================
  "genbank_accession",
  "genbank_present",
  
  # =====================================================
  # SOURCE TRACKING
  # =====================================================
  "source_origin",
  "primary_source",
  "overlap_class",
  
  # =====================================================
  # SEQUENCE INFORMATION
  # =====================================================
  "marker",
  "sequence",
  "sequence_length",
  "ambiguous_bases",
  "stop_codon_flag",
  
  # =====================================================
  # SPECIMEN INFORMATION
  # =====================================================
  "sex",
  "life_stage",
  "voucher_status",
  "voucher_institution",
  
  # =====================================================
  # COLLECTION INFORMATION
  # =====================================================
  "collector",
  "collection_date",
  "country",
  "state",
  "district",
  "locality",
  
  # =====================================================
  # GEOGRAPHIC INFORMATION
  # =====================================================
  "latitude",
  "longitude",
  "elevation",
  
  # =====================================================
  # COORDINATE QC
  # =====================================================
  "coordinate_uncertainty",
  "coordinate_issue_flag",
  
  # =====================================================
  # IMAGING / METADATA
  # =====================================================
  "image_available",
  "institution_storing",
  
  # =====================================================
  # DATASET MANAGEMENT
  # =====================================================
  "dataset_name",
  "download_date",
  "remarks"
)

master_template <- data.frame(matrix(
  ncol = length(master_columns),
  nrow = 0
))

colnames(master_template) <- master_columns

str(master_template)