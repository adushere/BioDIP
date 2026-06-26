# =====================================================
# BioDIP INFRASTRUCTURE VISUALIZATION PIPELINE
# =====================================================
# This script generates publication-ready spatial
# visualizations illustrating biodiversity data
# infrastructure across Indian ecozones.
#
# Core functions:
# - Creates a standardized Indian ecozone basemap
# - Visualizes GBIF occurrence infrastructure
# - Visualizes DNA barcode infrastructure
# - Produces integrated BioDIP infrastructure maps
# - Applies consistent ecozone color schemes
# - Exports high-resolution publication figures
#
# Output:
# A suite of publication-quality maps showing
# occurrence coverage, barcode coverage, and
# integrated biodiversity data infrastructure
# across Indian biogeographic regions.
# =====================================================
#########################################################
# MASTER ECOZONE BASEMAP
#########################################################

library(ggplot2)

library(grid)

ecozone_basemap <- ggplot(
  
  india_ecozones
  
)+
  
  geom_sf(
    
    aes(fill = biogeozone),
    
    color = NA
    
  )+
  
  scale_fill_manual(
    
    values = c(
      
      "Coasts" = "#2D9CDB",
      
      "Deccan Peninsula" = "#E69F00",
      
      "Desert" = "#A3B700",
      
      "Gangetic Plain" = "#120963",
      
      "Himalaya" = "#8C564B",
      
      "Islands" = "#FF69B4",
      
      "North-East" = "#8E44AD",
      
      "Semi-Arid" = "#D9B99B",
      
      "Trans-Himalaya" = "#D3D3D3",
      
      "Western Ghats" = "#216C00"
      
    ),
    
    name = NULL
    
  )+
  
  theme_void() +
  
  theme(
    
    legend.position = "right",
    
    legend.direction = "vertical",
    
    legend.text = element_text(
      
      size = 7
      
    ),
    
    legend.key.height = unit(
      
      0.28,
      
      "cm"
      
    ),
    
    legend.key.width = unit(
      
      0.28,
      
      "cm"
      
    ),
    
    legend.spacing.y = unit(
      
      0.03,
      
      "cm"
      
    ),
    
    legend.margin = margin(
      
      0,0,0,0
      
    ),
    
    legend.background = element_rect(
      
      fill = "white",
      
      colour = NA
      
    ),
    
    plot.background = element_rect(
      
      fill = "white",
      
      colour = NA
      
    )
    
  )



ecozone_basemap



###########################################################
# FIGURE 1 : GBIF INFRASTRUCTURE
###########################################################

fig1_gbif <- ecozone_basemap +
  
  geom_sf(
    
    data = gbif_sf,
    
    color = "#292EF3",
    
    alpha = 0.45,
    
    size = 0.35
    
  )+
  
  labs(
    
    title = "GBIF Occurrence Infrastructure",
    
    subtitle = "Indian Hawkmoth Occurrence Records"
    
  )



fig1_gbif

ggsave(
  
  filename = "Figure_1_GBIF.png",
  
  plot = fig1_gbif,
  
  width = 8,
  
  height = 8,
  
  dpi = 600,
  
  bg = "white"
  
)

###########################################################
# FIGURE 2 : BARCODE INFRASTRUCTURE
###########################################################

fig2_barcode <- ecozone_basemap +
  
  geom_sf(
    
    data = barcode_sf,
    
    color = "#FF0000",
    
    alpha = 0.95,
    
    size = 0.5
    
  ) +
  
  labs(
    
    title = "Barcode Infrastructure",
    
    subtitle = "Indian Hawkmoth DNA Barcode Records"
    
  )



fig2_barcode

ggsave(
  
  filename = "Figure_2_Barcode.png",
  
  plot = fig2_barcode,
  
  width = 8,
  
  height = 8,
  
  dpi = 600,
  
  bg = "white"
  
)
###########################################################
# FIGURE 3 : BIODIP INTEGRATED INFRASTRUCTURE
###########################################################

fig3_biodip <- ecozone_basemap +
  
  geom_sf(
    
    data = gbif_sf,
    
    color = "#292EF3",
    
    alpha = 0.45,
    
    size = 0.35
    
  ) +
  
  geom_sf(
    
    data = barcode_sf,
    
    color = "#FF0000",
    
    alpha = 0.95,
    
    size = 0.5
    
  ) +
  
  labs(
    
    title = "BioDIP Integrated Infrastructure",
    
    subtitle = "GBIF Occurrences and DNA Barcode Records"
    
  )



fig3_biodip

ggsave(
  
  filename = "Figure_3_BioDIP_Integrated.png",
  
  plot = fig3_biodip,
  
  width = 8,
  
  height = 8,
  
  dpi = 600,
  
  bg = "white"
  
)


###########################################################
# EXPORT ALL MAPS AS JPEG
###########################################################

# Figure 0 : Ecozone Basemap

ggsave(
  
  "Figure_0_Ecozone_Basemap.jpeg",
  
  ecozone_basemap,
  
  width = 10,
  
  height = 10,
  
  dpi = 600
  
)



# Figure 1 : GBIF Infrastructure

ggsave(
  
  "Figure_1_GBIF.jpeg",
  
  fig1_gbif,
  
  width = 10,
  
  height = 10,
  
  dpi = 600
  
)



# Figure 2 : Barcode Infrastructure

ggsave(
  
  "Figure_2_Barcode.jpeg",
  
  fig2_barcode,
  
  width = 10,
  
  height = 10,
  
  dpi = 600
  
)



# Figure 3 : BioDIP Integrated Infrastructure

ggsave(
  
  "Figure_3_BioDIP_Integrated.jpeg",
  
  fig3_biodip,
  
  width = 10,
  
  height = 10,
  
  dpi = 600
  
)
