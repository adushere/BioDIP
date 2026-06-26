# =====================================================
# BioDIP MOLECULAR DATA MINING — GENBANK + BOLD OVERLAP PIPELINE (STABLE VERSION)
# =====================================================
# This script retrieves COI sequence metadata for Sphingidae
# from GenBank (NCBI nuccore database) and integrates it with
# BOLD-derived accession information to build a molecular
# provenance classification system.
#
# Core functions:
# - Performs GenBank search for Sphingidae COI records
# - Implements batch processing to avoid API overload (HTTP 414)
# - Extracts accession IDs and sequence titles from summaries
# - Detects BOLD-linked records using metadata heuristics
# - Classifies records into GenBank-only and BOLD-linked groups
# - Merges classified datasets into a unified master molecular table
#
# Output:
# A provenance-annotated dataset used for downstream barcode
# QC, taxonomic harmonization, and integration with GBIF data.
# =====================================================
barcode_meta_final %>%
  count(recovered_state, sort = TRUE)

barcode_raw <- read_csv(
  "data/barcode/india_barcode_metadata_clean.csv"
)
dim(barcode_raw)

barcode_raw <- barcode_raw %>%
  janitor::clean_names()

names(barcode_raw)

barcode_raw$species_std <- str_to_lower(
  str_trim(barcode_raw$species)
)
head(barcode_raw$species_std)

barcode_raw$state_std <- str_to_title(
  str_trim(barcode_raw$province_state)
)
sort(unique(barcode_raw$state_std))

barcode_raw %>%
  mutate(
    has_coords = !is.na(latitude) & !is.na(longitude)
  ) %>%
  count(state_std, has_coords)

barcode_raw <- barcode_raw %>%
  mutate(
    
    spatial_quality_class = case_when(
      
      !is.na(latitude) &
        !is.na(longitude) ~
        "Coordinate_confirmed",
      
      (is.na(latitude) | is.na(longitude)) &
        !is.na(state_std) ~
        "Metadata_only",
      
      TRUE ~
        "Spatially_unresolved"
    )
    
  )

barcode_raw %>%
  count(spatial_quality_class)

barcode_raw <- barcode_raw %>%
  mutate(
    
    provenance_class = case_when(
      
      overlap_class == "BOLD_GenBank_overlap" ~
        "BOLD_GenBank_overlap",
      
      overlap_class == "BOLD_unique" ~
        "BOLD_only",
      
      overlap_class == "GenBank_only" ~
        "GenBank_only",
      
      TRUE ~
        "Other_or_unresolved"
    )
    
  )
barcode_raw %>%
  count(provenance_class)


barcode_raw %>%
  count(
    provenance_class,
    spatial_quality_class
  )





barcode_raw <- barcode_raw %>%
  mutate(
    
    mapping_class = paste(
      provenance_class,
      spatial_quality_class,
      sep = " | "
    )
    
  )


barcode_raw %>%
  count(mapping_class, sort = TRUE)

barcode_coord_confirmed <- barcode_raw %>%
  filter(
    spatial_quality_class == "Coordinate_confirmed"
  )
dim(barcode_coord_confirmed)


barcode_sf_final <- st_as_sf(
  barcode_coord_confirmed,
  coords = c("longitude", "latitude"),
  crs = 4326
)
barcode_sf_final


map_gbif_barcode <- ggplot() +
  
  geom_sf(
    data = india_states,
    fill = "gray97",
    color = "gray40",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = gbif_sf,
    color = "darkgreen",
    alpha = 0.25,
    size = 0.35
  ) +
  
  geom_sf(
    data = barcode_sf_final,
    aes(color = provenance_class),
    alpha = 0.9,
    size = 1.3
  ) +
  
  coord_sf() +
  
  scale_color_manual(
    values = c(
      "BOLD_GenBank_overlap" = "red",
      "BOLD_only" = "orange"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid.major = element_line(
      color = "gray90",
      linewidth = 0.2
    ),
    
    legend.position = "right",
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 11
    )
  ) +
  
  labs(
    title = "Indian Sphingidae Occurrence and Barcode Coverage",
    
    subtitle =
      "Green = GBIF occurrences | Colored points = Coordinate-confirmed barcode records",
    
    color = "Barcode provenance",
    
    caption =
      "GBIF occurrence framework integrated with Indian Sphingidae barcode metadata"
  )


map_gbif_barcode


map_gbif <- ggplot() +
  
  geom_sf(
    data = india_states,
    fill = "gray97",
    color = "gray40",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = gbif_sf,
    color = "darkgreen",
    alpha = 0.25,
    size = 0.35
  ) +
  
  coord_sf() +
  
  theme_minimal() +
  
  theme(
    
    panel.grid.major = element_line(
      color = "gray90",
      linewidth = 0.2
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 11
    )
    
  ) +
  
  labs(
    
    title = "GBIF Occurrence Records of Indian Sphingidae",
    
    subtitle =
      "Coordinate-confirmed occurrence records from GBIF",
    
    caption =
      "Cleaned GBIF occurrence framework"
    
  )
map_gbif


map_barcode_confirmed <- ggplot() +
  
  geom_sf(
    data = india_states,
    fill = "gray97",
    color = "gray40",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = barcode_sf_final,
    aes(color = provenance_class),
    alpha = 0.9,
    size = 1.5
  ) +
  
  coord_sf() +
  
  scale_color_manual(
    values = c(
      "BOLD_GenBank_overlap" = "red",
      "BOLD_only" = "orange"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    
    panel.grid.major = element_line(
      color = "gray90",
      linewidth = 0.2
    ),
    
    legend.position = "right",
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 11
    )
    
  ) +
  
  labs(
    
    title =
      "Coordinate-confirmed Barcode Records of Indian Sphingidae",
    
    subtitle =
      "Only barcode records with verified geographic coordinates",
    
    color =
      "Barcode provenance",
    
    caption =
      "Integrated Indian Sphingidae barcode framework"
    
  )
map_barcode_confirmed


barcode_metadata_only <- barcode_raw %>%
  filter(
    spatial_quality_class == "Metadata_only"
  )
barcode_metadata_only %>%
  count(state_std, sort = TRUE)

india_state_centroids <- st_centroid(india_states)
india_state_centroids

barcode_metadata_centroids <- barcode_metadata_only %>%
  
  left_join(
    st_drop_geometry(
      india_state_centroids %>%
        select(name, geometry)
    ),
    
    by = c("state_std" = "name")
  )
head(barcode_metadata_centroids)

names(barcode_metadata_centroids)

barcode_metadata_centroids %>%
  select(
    state_std,
    geometry
  ) %>%
  head()

barcode_metadata_centroids <- india_state_centroids %>%
  
  select(name, geometry) %>%
  
  left_join(
    barcode_metadata_only,
    by = c("name" = "state_std")
  )
barcode_metadata_centroids


barcode_metadata_centroids <- barcode_metadata_centroids %>%
  filter(
    !is.na(processid)
  )
nrow(barcode_metadata_centroids)

barcode_raw <- barcode_raw %>%
  mutate(
    
    state_std = case_when(
      
      state_std == "Andaman and Nicobar" ~
        "Andaman And Nicobar Islands",
      
      TRUE ~ state_std
    )
    
  )
sort(unique(barcode_raw$state_std))

barcode_metadata_only <- barcode_raw %>%
  filter(
    spatial_quality_class == "Metadata_only"
  )
barcode_metadata_centroids <- india_state_centroids %>%
  
  select(name, geometry) %>%
  
  left_join(
    barcode_metadata_only,
    by = c("name" = "state_std")
  ) %>%
  
  filter(
    !is.na(processid)
  )

nrow(barcode_metadata_centroids)

anti_join(
  
  barcode_metadata_only,
  
  st_drop_geometry(
    india_state_centroids %>%
      select(name)
  ),
  
  by = c("state_std" = "name")
)

anti_join(
  
  barcode_metadata_only,
  
  st_drop_geometry(
    india_state_centroids %>%
      select(name)
  ),
  
  by = c("state_std" = "name")
  
) %>%
  
  select(
    processid,
    species,
    state_std,
    province_state
  )

india_state_centroids <- india_state_centroids %>%
  mutate(
    name = str_squish(
      str_to_title(name)
    )
  )
barcode_raw <- barcode_raw %>%
  mutate(
    state_std = str_squish(
      str_to_title(state_std)
    )
  )
barcode_metadata_only <- barcode_raw %>%
  filter(
    spatial_quality_class == "Metadata_only"
  )
barcode_metadata_centroids <- india_state_centroids %>%
  
  select(name, geometry) %>%
  
  left_join(
    barcode_metadata_only,
    by = c("name" = "state_std")
  ) %>%
  
  filter(
    !is.na(processid)
  )
nrow(barcode_metadata_centroids)

unique(
  barcode_metadata_only$state_std[
    barcode_metadata_only$species == "Cypa decolor"
  ]
)
unique(
  india_state_centroids$name[
    grepl(
      "Andaman",
      india_state_centroids$name
    )
  ]
)
barcode_raw <- barcode_raw %>%
  mutate(
    
    state_std = case_when(
      
      state_std == "Andaman And Nicobar Islands" ~
        "Andaman And Nicobar",
      
      TRUE ~ state_std
    )
    
  )
barcode_metadata_only <- barcode_raw %>%
  filter(
    spatial_quality_class == "Metadata_only"
  )
barcode_metadata_centroids <- india_state_centroids %>%
  
  select(name, geometry) %>%
  
  left_join(
    barcode_metadata_only,
    by = c("name" = "state_std")
  ) %>%
  
  filter(
    !is.na(processid)
  )
nrow(barcode_metadata_centroids)



map_barcode_metadata_only <- ggplot() +
  
  geom_sf(
    data = india_states,
    fill = "gray97",
    color = "gray40",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = barcode_metadata_centroids,
    aes(color = provenance_class),
    alpha = 0.9,
    size = 2,
      ) +
  
  coord_sf() +
  
  scale_color_manual(
    values = c(
      "BOLD_GenBank_overlap" = "darkblue",
      "BOLD_only" = "deepskyblue3"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    
    panel.grid.major = element_line(
      color = "gray90",
      linewidth = 0.2
    ),
    
    legend.position = "right",
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 11
    )
    
  ) +
  
  labs(
    
    title =
      "Metadata-only Barcode Representation of Indian Sphingidae",
    
    subtitle =
      "State-centroid representation of barcode records lacking coordinates",
    
    color =
      "Barcode provenance",
    
    caption =
      "Triangles represent metadata-derived approximate geography"
    
  )

map_barcode_metadata_only


#Overlayed maps with co-ordinates and with state centroids

map_barcode_combined <- ggplot() +
  
  geom_sf(
    data = india_states,
    fill = "gray97",
    color = "gray40",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = barcode_sf_final,
    aes(color = provenance_class),
    alpha = 0.9,
    size = 1.5
  ) +
  
  geom_sf(
    data = barcode_metadata_centroids,
    aes(fill = provenance_class),
    shape = 21,
    color = "black",
    alpha = 0.7,
    size = 2
  ) +
  
  coord_sf() +
  
  scale_color_manual(
    values = c(
      "BOLD_GenBank_overlap" = "darkblue",
      "BOLD_only" = "deepskyblue3"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "BOLD_GenBank_overlap" = "red",
      "BOLD_only" = "orange"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    
    panel.grid.major = element_line(
      color = "gray90",
      linewidth = 0.2
    ),
    
    legend.position = "right",
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 11
    )
    
  ) +
  
  labs(
    
    title =
      "Integrated Barcode Infrastructure of Indian Sphingidae",
    
    subtitle =
      "Coordinate-confirmed records and metadata-derived barcode representation",
    
    color =
      "Coordinate-confirmed",
    
    fill =
      "Metadata-only",
    
    caption =
      "Large bordered circles represent metadata-derived approximate geography"
    
  )

map_barcode_combined


#GBIF - BARCODE combined map

map_gbif_barcode_full <- ggplot() +
  
  geom_sf(
    data = india_states,
    fill = "gray97",
    color = "gray40",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = gbif_sf,
    color = "darkgreen",
    alpha = 0.25,
    size = 0.35
  ) +
  
  geom_sf(
    data = barcode_sf_final,
    aes(color = provenance_class),
    alpha = 0.9,
    size = 1.5
  ) +
  
  geom_sf(
    data = barcode_metadata_centroids,
    aes(fill = provenance_class),
    shape = 21,
    color = "black",
    alpha = 0.7,
    size = 2
  ) +
  
  coord_sf() +
  
  scale_color_manual(
    values = c(
      "BOLD_GenBank_overlap" = "darkblue",
      "BOLD_only" = "deepskyblue3"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "BOLD_GenBank_overlap" = "red",
      "BOLD_only" = "orange"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    
    panel.grid.major = element_line(
      color = "gray90",
      linewidth = 0.2
    ),
    
    legend.position = "right",
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 11
    )
    
  ) +
  
  labs(
    
    title =
      "Integrated GBIF and Barcode Infrastructure of Indian Sphingidae",
    
    subtitle =
      "GBIF occurrences, coordinate-confirmed barcodes, and metadata-derived barcode representation",
    
    color =
      "Coordinate-confirmed",
    
    fill =
      "Metadata-only",
    
    caption =
      "Green = GBIF occurrences | Large bordered circles = metadata-derived barcode geography"
    
  )

map_gbif_barcode_full

dir.create(
  "outputs/maps",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  "outputs/maps/map_gbif.png",
  map_gbif,
  width = 10,
  height = 8,
  dpi = 600
)

ggsave(
  "outputs/maps/map_barcode_confirmed.png",
  map_barcode_confirmed,
  width = 10,
  height = 8,
  dpi = 600
)

ggsave(
  "outputs/maps/map_barcode_metadata_only.png",
  map_barcode_metadata_only,
  width = 10,
  height = 8,
  dpi = 600
)

ggsave(
  "outputs/maps/map_barcode_combined.png",
  map_barcode_combined,
  width = 10,
  height = 8,
  dpi = 600
)

ggsave(
  "outputs/maps/map_gbif_barcode_full.png",
  map_gbif_barcode_full,
  width = 10,
  height = 8,
  dpi = 600
)