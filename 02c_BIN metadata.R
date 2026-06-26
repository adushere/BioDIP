# =====================================================
# BioDIP TARGETED BIN METADATA RETRIEVAL PIPELINE
# =====================================================
# This script retrieves minimal occurrence metadata for a
# predefined set of BOLD BINs using the BOLD API.
#
# Core functions:
# - Queries selected BINs individually through BOLDconnectR
# - Retrieves process ID and geographic information
# - Implements error handling for failed BIN requests
# - Generates a lightweight BIN occurrence dataset
# - Supports geographic validation and metadata recovery
#
# Output:
# A BIN-level metadata table containing process IDs,
# country records, and coordinates for selected BINs.
# =====================================================
library(BOLDconnectR)
library(dplyr)
library(purrr)

my_bins <- c(
  "BOLD:ACE5460",
  "BOLD:AAD2845",
  "BOLD:AAA2393",
  "BOLD:AAB0645",
  "BOLD:AAB4653",
  "BOLD:AAA4631",
  "BOLD:ABY8586",
  "BOLD:AAF0756",
  "BOLD:ADL0757",
  "BOLD:AAE5824",
  "BOLD:AAL5673",
  "BOLD:AAA7779",
  "BOLD:ADL2519",
  "BOLD:AAC6760",
  "BOLD:AAW6578",
  "BOLD:AAB2442",
  "BOLD:AAE7656",
  "BOLD:AAB3024",
  "BOLD:AAA4630",
  "BOLD:ADG0374"
)

fetch_bin_minimal <- function(bin_id) {
  
  message(
    paste(
      "Fetching:",
      bin_id
    )
  )
  
  tryCatch(
    
    {
      
      out <- bold.public.search(
        bins = list(bin_id)
      )
      
      if (nrow(out) == 0) {
        return(NULL)
      }
      
      out %>%
        
        select(
          processid,
          country,
          lat,
          lon
        ) %>%
        
        mutate(
          queried_bin = bin_id
        )
    },
    
    error = function(e) {
      
      message(
        paste(
          "FAILED:",
          bin_id
        )
      )
      
      return(NULL)
    }
  )
}

all_bin_minimal <- map_dfr(
  my_bins,
  fetch_bin_minimal
)

View(all_bin_minimal)