###########################################################
# COMMON ECOZONE LEGEND (COMPACT VERSION)
###########################################################

library(ggplot2)

library(grid)

ecozone_legend <- ggplot(
  
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



ecozone_legend



###########################################################
# EXPORT LEGEND
###########################################################

ggsave(
  
  "Common_Ecozone_Legend.jpeg",
  
  ecozone_legend,
  
  width = 2.2,
  
  height = 4,
  
  dpi = 600
  
)
