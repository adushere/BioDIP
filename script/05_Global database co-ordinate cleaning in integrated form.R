
View(genbank_df)
dim(genbank_df)
head(genbank_df)
# =========================================================
# STEP 9: DETECT BOLD-ORIGIN RECORDS IN GENBANK
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
TRUE ~ FALSE
)
)
table(genbank_df$bold_linked)
# =========================================================
# STEP 10: IDENTIFY PRIVATE BOLD-LINKED RECORDS
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
# STEP 11: IDENTIFY GENBANK-ONLY RECORDS
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
# CHECK FOR "MOTH" PATTERN
# =========================================================
private_bold_genbank %>%
filter(
str_detect(title, "MOTH")
) %>%
View()
sum(
str_detect(
private_bold_genbank$title,
"MOTH"
)
)
bold_data_clean %>%
filter(
insdc_acs == "MG783953"
)
genbank_df %>%
filter(
accession == "MG783953"
)
# =========================================================
# IMPROVED DETECTION OF BOLD-ORIGIN RECORDS
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
nrow(private_bold_genbank)
private_bold_genbank %>%
filter(
str_detect(title, "MOTH")
)
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
View(master_dataset)
View(master_dataset)
master_data <- read_csv("Sphingidae_master_dataset.csv")
master_data <- read_csv("D:/Adu/Publication/Moths/iBol conference/Barcode_library/Sphingidae_master_dataset.csv")
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
master_data <- read_csv("D:/Adu/Publication/Moths/iBol conference/Barcode_library/Sphingidae_master_dataset.csv")
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
master_data <- read_csv("Sphingidae_master_dataset.csv")
dim(master_data)
names(master_data)
head(master_data)
str(master_data)
length(unique(master_data$country_iso))
sort(unique(master_data$country_iso))
master_data %>%
count(country_iso, sort = TRUE)
print(n = ...)
master_data %>%
filter(country_iso == "IN") %>%
nrow()
master_data %>%
filter(country_iso == "IN") %>%
head()
master_data %>%
filter(country_iso == "IN") %>%
summarise(
unique_species = n_distinct(species),
unique_genera = n_distinct(genus),
unique_bins = n_distinct(bin_uri)
)
master_data %>%
filter(is.na(country_iso)) %>%
summarise(
total_NA_records = n(),
records_with_coordinates = sum(!is.na(coord)),
records_without_coordinates = sum(is.na(coord))
)
master_data %>%
filter(is.na(country_iso), !is.na(coord)) %>%
select(processid, species, coord, province.state, accession, source_origin) %>%
head(20)
master_data %>%
filter(is.na(country_iso), !is.na(coord)) %>%
pull(coord) %>%
head(20)
master_data <- master_data %>%
separate(coord,
into = c("latitude", "longitude"),
sep = ",",
remove = FALSE)
master_data$latitude <- as.numeric(master_data$latitude)
master_data$longitude <- as.numeric(master_data$longitude)
str(master_data[, c("coord", "latitude", "longitude")])
summary(master_data[, c("latitude", "longitude")])
str(master_data[, c("coord", "latitude", "longitude")])
summary(master_data[, c("latitude", "longitude")])
master_data %>%
summarise(
missing_latitude = sum(is.na(latitude)),
missing_longitude = sum(is.na(longitude))
)
master_data %>%
filter(latitude < -90 | latitude > 90)
master_data %>%
filter(longitude < -180 | longitude > 180)
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
na_country_data <- master_data %>%
filter(is.na(country_iso), !is.na(latitude), !is.na(longitude))
dim(na_country_data)
na_points <- st_as_sf(
na_country_data,
coords = c("longitude", "latitude"),
crs = 4326
)
na_points
world <- ne_countries(scale = "medium", returnclass = "sf")
world
recovered_countries <- st_join(
na_points,
world[, c("iso_a2", "name")],
join = st_within
)
recovered_countries %>%
st_drop_geometry() %>%
select(processid, species, iso_a2, name) %>%
head(20)
recovered_countries %>%
st_drop_geometry() %>%
summarise(
total_records = n(),
recovered_country_names = sum(!is.na(name)),
unrecovered_records = sum(is.na(name))
)
recovered_countries %>%
st_drop_geometry() %>%
filter(is.na(name)) %>%
select(processid, species, coord) %>%
head(20)
country_summary <- master_data %>%
count(country_iso, sort = TRUE)
country_summary
print(country_summary, n = 200)
recovered_countries %>%
st_drop_geometry() %>%
summarise(
total_records_checked = n(),
successfully_recovered = sum(!is.na(name)),
failed_recovery = sum(is.na(name))
)
recovered_countries %>%
st_drop_geometry() %>%
summarise(
recovery_percent =
round(
(sum(!is.na(name)) / n()) * 100,
2
)
)
country_summary %>%
slice_max(n, n = 20)
country_summary %>%
slice_max(n, n = 10) %>%
summarise(total_top10 = sum(n))
top10_percent <- country_summary %>%
slice_max(n, n = 10) %>%
summarise(percent = round(sum(n) / nrow(master_data) * 100, 2))
top10_percent
recovery_table <- recovered_countries %>%
st_drop_geometry() %>%
select(processid, recovered_country = name)
master_data <- master_data %>%
left_join(recovery_table, by = "processid")
master_data <- master_data %>%
mutate(
final_country =
ifelse(
is.na(country_iso),
recovered_country,
country_iso
)
)
master_data <- master_data %>%
mutate(
country_recovery_status =
case_when(
!is.na(country_iso) ~ "Original_country_present",
is.na(country_iso) & !is.na(recovered_country) ~ "Recovered_from_coordinates",
TRUE ~ "Country_unresolved"
)
)
master_data %>%
count(country_recovery_status)
join = st_within
recovered_countries %>%
st_drop_geometry() %>%
filter(is.na(name)) %>%
select(processid, coord, latitude, longitude) %>%
head(50)
recovered_countries %>%
st_drop_geometry() %>%
filter(is.na(name)) %>%
select(processid, coord) %>%
head(50)
nearest_country_index <- st_nearest_feature(
na_points,
world
)
nearest_country_names <- world$name[nearest_country_index]
na_country_data$nearest_country <- nearest_country_names
na_country_data %>%
select(processid, coord, nearest_country) %>%
head(30)
na_country_data %>%
summarise(
total_records = n(),
recovered_nearest_country = sum(!is.na(nearest_country)),
unrecovered = sum(is.na(nearest_country))
)
na_country_data %>%
count(nearest_country, sort = TRUE)
na_country_data %>%
select(processid, coord, nearest_country) %>%
sample_n(20)
na_country_data %>%
summarise(
total_NA_country_records = n(),
recovered_from_coordinates = sum(!is.na(nearest_country)),
still_unresolved = sum(is.na(nearest_country))
)
na_country_data %>%
summarise(
recovery_percent =
round(
(sum(!is.na(nearest_country)) / n()) * 100,
2
)
)
na_country_data %>%
filter(is.na(nearest_country)) %>%
select(processid, coord) %>%
head(50)
unresolved_country_records <- master_data %>%
filter(is.na(country_iso))
unresolved_country_records %>%
summarise(
total_records = n(),
with_coordinates = sum(!is.na(coord)),
with_province = sum(!is.na(province.state)),
with_accession = sum(!is.na(accession)),
with_title = sum(!is.na(title))
)
unresolved_country_records %>%
select(
processid,
species,
province.state,
accession,
title,
source_origin
) %>%
head(50)
missing_geo_data <- master_data %>%
filter(is.na(coord))
missing_geo_data %>%
summarise(
total_missing_coordinates = n(),
with_country = sum(!is.na(country_iso)),
without_country = sum(is.na(country_iso)),
with_accession = sum(!is.na(insdc_acs)),
with_species = sum(!is.na(species)),
with_title = sum(!is.na(title)),
with_province = sum(!is.na(province.state))
)
missing_geo_data %>%
select(
processid,
species,
country_iso,
province.state,
insdc_acs,
accession,
title,
source_origin
) %>%
head(50)
missing_geo_data %>%
summarise(
insdc_accessions = sum(!is.na(insdc_acs)),
accession_field = sum(!is.na(accession))
)
missing_geo_data %>%
select(
processid,
insdc_acs,
accession,
source_origin,
primary_source
) %>%
head(50)
missing_geo_data %>%
filter(!is.na(insdc_acs) | !is.na(accession)) %>%
summarise(
recoverable_records = n(),
unique_species = n_distinct(species)
)
missing_geo_data %>%
summarise(
both_present =
sum(!is.na(insdc_acs) & !is.na(accession)),
only_insdc =
sum(!is.na(insdc_acs) & is.na(accession)),
only_accession =
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
