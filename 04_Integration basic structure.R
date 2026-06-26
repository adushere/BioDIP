setwd("D:/Adu/Publication/Moths/iBol conference/integration_project")
getwd()
dir.create("data")
dir.create("outputs")
dir.create("scripts")
list.dirs()


dir.create("data/gbif")
dir.create("data/barcode")

list.dirs("data")


dir.create("outputs/tables")
dir.create("outputs/maps")
dir.create("outputs/figures")

list.dirs("outputs")


packages_needed <- c(
  "tidyverse",
  "data.table",
  "janitor",
  "sf",
  "terra",
  "rnaturalearth",
  "rnaturalearthdata",
  "viridis",
  "patchwork"
)

packages_needed %in% installed.packages()[, "Package"]


library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(patchwork)

gbif_occ <- read_csv(
  "data/gbif/species_state_occurrence_matrix.csv"
)
glimpse(gbif_occ)


barcode_meta <- read_csv(
  "data/barcode/india_barcode_metadata_clean.csv"
)

glimpse(barcode_meta)

gbif_occ <- janitor::clean_names(gbif_occ)

barcode_meta <- janitor::clean_names(barcode_meta)



gbif_occ$species_std <- str_to_lower(
  str_trim(gbif_occ$species)
)

barcode_meta$species_std <- str_to_lower(
  str_trim(barcode_meta$species)
)



head(gbif_occ$species_std)

head(barcode_meta$species_std)


gbif_occ$state_std <- str_to_title(
  str_trim(gbif_occ$state_province)
)

barcode_meta$state_std <- str_to_title(
  str_trim(barcode_meta$province_state)
)


head(gbif_occ$state_std)

head(barcode_meta$state_std)

table(is.na(barcode_meta$province_state))
sum(!is.na(barcode_meta$latitude))

sort(unique(gbif_occ$state_std))
sort(unique(barcode_meta$state_std))


write_csv(
  gbif_occ,
  "outputs/tables/gbif_occ_standardized.csv"
)

write_csv(
  barcode_meta,
  "outputs/tables/barcode_meta_standardized.csv"
)

list.files("outputs/tables")
