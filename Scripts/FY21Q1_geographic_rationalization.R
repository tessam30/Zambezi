# PURPOSE: Is Zambia Geographically rationalized for care and treatment
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-03-08
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  
     
    msd_file <- return_latest(merdata, pattern = "PSNU_IM_FY19-21")
    
  # Functions  
    #source(here("Scripts/Z01_fetch_spdfs.R"))

# LOAD DATA ============================================================================  

  msd <- read_msd(msd_file) %>% 
      filter(indicator %in% c("HTS_TST", "TX_CURR"),
             standardizeddisaggregate == "Total Numerator",
             fundingagency != "Dedup",
             fiscal_year == 2021, 
             psnu != "_Military Zambia")
      


# MUNGE ============================================================================
  
  # Extract testing and care and treatment indicators by funding agency / mechanism and funding agency
  mech_indic <- 
    msd %>% 
      group_by(fundingagency, psnu, psnuuid, mech_name, indicator, mech_code) %>% 
      summarise(across(c("targets", "cumulative"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(agency_color = if_else(fundingagency == "USAID", paste0(denim, "cc"), paste0(old_rose, "cc"))) 
    
    mech_indic_geo <- 
      spdf_comm_zmb %>% 
      left_join(., mech_indic, by = c("uid" = "psnuuid"))

    
# VIZ ============================================================================

  #  map indicators across targets and results
    terr_map +
      geom_sf(data = mech_indic_geo %>% filter(!is.na(fundingagency)) %>% distinct(fundingagency, indicator, agency_color, psnu.x), 
              aes(fill = agency_color), color = "white") +
      facet_wrap(fundingagency ~ indicator, drop = T) +
      scale_fill_identity() +
      si_style_map() +
      labs(x = NULL, y = NULL, title = "Zambia is geographically rationalized for HTS_TST and TX_CURR",
           caption = "Source: FY21Q1 PSNU X IM MSD.")
      
    si_save(here(images, "ZMB_FY21_hts_rationalization.png"), scale = 1.25)

# Export data
    write_csv(mech_indic, here(dataout, "ZMB_FY21_geographic_rationalization_agency_mech.csv"))
