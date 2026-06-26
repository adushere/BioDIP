# =====================================================
# BioDIP MOLECULAR QC & SEQUENCE ANALYSIS PIPELINE
# =====================================================
# This script performs molecular sequence quality control,
# alignment, translation-based validation, and exploratory
# analysis for Indian Sphingidae barcode datasets.
#
# Core functions:
# - Computes sequence length distributions and filters sequences
# - Performs nucleotide cleaning (removal of gaps and invalid bases)
# - Translates COI sequences across all reading frames
# - Identifies optimal reading frame based on stop codon minimization
# - Performs forward and reverse complement translation checks
# - Applies Biostrings-based ORF evaluation for QC
# - Conducts multiple sequence alignment using DECIPHER
# - Filters sequences based on ambiguity and quality thresholds
# - Generates India-specific high-confidence barcode subset
# - Classifies sequences by ambiguity score (Clean → Severe)
# - Produces aligned sequence dataset for downstream phylogenetic use
#
# Output:
# QC-filtered, aligned COI barcode dataset with translation validation,
# used for barcode gap analysis, phylogenetic inference, and
# integration with GBIF occurrence data in BioDIP.
# =====================================================
sum(is.na(insdc_acs) & !is.na(accession)),
neither_present =
sum(is.na(insdc_acs) & is.na(accession))
)
missing_geo_data %>%
filter(!is.na(insdc_acs)) %>%
distinct(insdc_acs) %>%
head(30)
missing_geo_data %>%
filter(!is.na(accession)) %>%
distinct(accession) %>%
head(30)
test_accessions <- missing_geo_data %>%
filter(!is.na(insdc_acs)) %>%
distinct(insdc_acs) %>%
slice(1:20)
test_accessions
write.csv(
test_accessions,
"test_accessions.csv",
row.names = FALSE
)
master_data <- master_data %>%
mutate(
accession_status =
case_when(
str_detect(insdc_acs, "SUPPRESSED") ~ "Suppressed",
!is.na(insdc_acs) ~ "Active_INSDC",
!is.na(accession) ~ "Active_accession",
TRUE ~ "No_accession"
)
)
master_data %>%
count(accession_status)
master_data %>%
group_by(accession_status) %>%
summarise(
total_records = n(),
with_coordinates =
sum(!is.na(coord)),
percent_with_coordinates =
round(
(sum(!is.na(coord)) / n()) * 100,
2
),
with_country =
sum(!is.na(country_iso)),
percent_with_country =
round(
(sum(!is.na(country_iso)) / n()) * 100,
2
),
with_province =
sum(!is.na(province.state)),
percent_with_province =
round(
(sum(!is.na(province.state)) / n()) * 100,
2
)
)
save.image("D:/Adu/Publication/Moths/iBol conference/Barcode_library/global environment co-ordinate cleaning.RData")
master_data <- master_data %>%
mutate(sequence_length = nchar(nuc))
library(dplyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
master_data <- master_data %>%
mutate(sequence_length = nchar(nuc))
summary(master_data$sequence_length)
hist(
master_data$sequence_length,
breaks = 50,
main = "Sequence Length Distribution",
xlab = "Sequence Length"
)
master_data %>%
summarise(
below_500 = sum(sequence_length < 500, na.rm = TRUE),
below_600 = sum(sequence_length < 600, na.rm = TRUE),
full_length_650_plus = sum(sequence_length >= 650, na.rm = TRUE)
)
library(seqinr)
library(ape)
qc_data <- master_data %>%
filter(sequence_length >= 500)
dim(qc_data)
test_seq <- qc_data$nuc[1]
translated_test <- translate(
s2c(test_seq),
numcode = 5
)
c2s(translated_test)
library(Biostrings)
dna <- DNAString(test_seq)
frames <- lapply(0:2, function(i) {
translate(subseq(dna, start = i + 1))
})
frames
best_frame_translation <- function(seq) {
dna <- DNAString(seq)
frame_results <- lapply(0:2, function(i) {
aa <- translate(subseq(dna, start = i + 1))
stop_count <- sum(strsplit(as.character(aa), "")[[1]] == "*")
list(
frame = i + 1,
aa = as.character(aa),
stops = stop_count
)
})
best <- frame_results[[which.min(sapply(frame_results, function(x) x$stops))]]
return(best)
}
best_frame_translation(test_seq)
best_frame_translation(test_seq)
best_translation <- function(seq) {
dna <- DNAString(seq)
orientations <- list(
forward = dna,
reverse = reverseComplement(dna)
)
all_results <- list()
for (orientation_name in names(orientations)) {
current_seq <- orientations[[orientation_name]]
for (i in 0:2) {
aa <- translate(subseq(current_seq, start = i + 1))
aa_char <- as.character(aa)
stop_count <- sum(strsplit(aa_char, "")[[1]] == "*")
all_results[[length(all_results) + 1]] <- list(
orientation = orientation_name,
frame = i + 1,
aa = aa_char,
stops = stop_count
)
}
}
best <- all_results[[which.min(sapply(all_results, function(x) x$stops))]]
return(best)
}
best_translation(test_seq)
qc_subset <- qc_data %>%
slice(1:1000)
qc_subset <- qc_data[1:1000, ]
qc_results <- lapply(qc_subset$nuc, best_translation)
qc_subset$nuc_clean <- qc_subset$nuc %>%
toupper() %>%
str_replace_all("-", "") %>%
str_replace_all("\\s+", "")
invalid_seq <- qc_subset %>%
filter(str_detect(nuc_clean, "[^ACGTN]"))
nrow(invalid_seq)
nrow(invalid_seq)
invalid_seq %>%
select(processid, nuc_clean) %>%
head(10)
translation_subset <- qc_subset %>%
filter(!str_detect(nuc_clean, "[^ACGT]"))
dim(translation_subset)
qc_results <- lapply(
translation_subset$nuc_clean,
best_translation
)
stop_counts <- sapply(qc_results, function(x) x$stops)
table(stop_counts)
library(DECIPHER)
library(Biostrings)
align_subset <- translation_subset[1:200, ]
dna_set <- DNAStringSet(align_subset$nuc_clean)
aligned <- AlignSeqs(dna_set)
aligned
aligned_strings <- as.character(aligned)
aligned_results <- lapply(aligned_strings, best_translation)
best_translation <- function(seq) {
seq <- gsub("-", "", seq)
dna <- DNAString(seq)
orientations <- list(
forward = dna,
reverse = reverseComplement(dna)
)
all_results <- list()
for (orientation_name in names(orientations)) {
current_seq <- orientations[[orientation_name]]
for (i in 0:2) {
aa <- translate(subseq(current_seq, start = i + 1))
aa_char <- as.character(aa)
stop_count <- sum(strsplit(aa_char, "")[[1]] == "*")
all_results[[length(all_results) + 1]] <- list(
orientation = orientation_name,
frame = i + 1,
aa = aa_char,
stops = stop_count
)
}
}
best <- all_results[[which.min(sapply(all_results, function(x) x$stops))]]
return(best)
}
aligned_results <- lapply(aligned_strings, best_translation)
aligned_stop_counts <- sapply(aligned_results, function(x) x$stops)
table(aligned_stop_counts)
country == India
india_data <- master_data %>%
filter(
country_iso == "IN" |
final_country == "India"
)
india_data %>%
summarise(
total_records = n(),
unique_species = n_distinct(species),
unique_bins = n_distinct(bin_uri),
with_coordinates = sum(!is.na(coord)),
with_accessions = sum(!is.na(insdc_acs) | !is.na(accession))
)
india_data %>%
count(source_origin, sort = TRUE)
india_data %>%
group_by(source_origin) %>%
summarise(n = n()) %>%
arrange(desc(n))
india_qc <- india_data %>%
filter(
sequence_length >= 500
)
india_qc <- india_qc %>%
mutate(
nuc_clean = toupper(nuc)
)
india_qc %>%
summarise(
ambiguity_sequences =
sum(str_detect(nuc_clean, "[^ACGT]"))
)
india_qc <- india_qc %>%
mutate(
ambiguity_count =
str_count(nuc_clean, "[^ACGT]")
)
summary(india_qc$ambiguity_count)
india_qc %>%
arrange(desc(ambiguity_count)) %>%
select(processid, species, ambiguity_count) %>%
head(20)
india_qc <- india_qc %>%
mutate(
ambiguity_class =
case_when(
ambiguity_count == 0 ~ "Clean",
ambiguity_count <= 5 ~ "Low",
ambiguity_count <= 20 ~ "Moderate",
ambiguity_count <= 100 ~ "High",
TRUE ~ "Severe"
)
)
india_qc %>%
count(ambiguity_class)
india_qc <- india_qc %>%
mutate(
ambiguity_class =
case_when(
ambiguity_count == 0 ~ "Clean",
ambiguity_count <= 5 ~ "Low",
ambiguity_count <= 20 ~ "Moderate",
ambiguity_count <= 100 ~ "High",
TRUE ~ "Severe"
)
)
india_qc %>%
count(ambiguity_class)
colnames(india_qc)
india_qc %>%
group_by(ambiguity_class) %>%
summarise(n = n())
india_highconf <- india_qc %>%
filter(
ambiguity_class %in% c("Clean", "Low", "Moderate")
)
india_highconf %>%
summarise(
total_records = n(),
unique_species = n_distinct(species),
unique_bins = n_distinct(bin_uri)
)
india_dna <- DNAStringSet(india_highconf$nuc_clean)
india_aligned <- AlignSeqs(india_dna)
india_highconf <- india_highconf %>%
mutate(
nuc_clean =
str_replace_all(nuc_clean, "-", "")
)
india_dna <- DNAStringSet(india_highconf$nuc_clean)
india_aligned <- AlignSeqs(india_dna)
india_aligned
width(india_aligned)[1:20]
alphabetFrequency(india_aligned, baseOnly = FALSE)
save.image("Sphingidae_workflow_session.RData")
