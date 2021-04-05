# PURPOSE: TX_CURR verification post-clean MER dta
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-04-01
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
     
    
  # Functions  
  

# LOAD DATA ============================================================================  
  
  msd_file <- return_latest(merdata, pattern = "Zambia")
    
  msd <- read_msd(msd_file) %>% 
    filter(indicator == "TX_CURR", 
           standardizeddisaggregate == "Total Numerator")
    
  
# MUNGE ============================================================================
  

  msd %>% 
    filter(snu1 == "Central Province", fundingagency == "HHS/CDC") %>% 
    group_by(fundingagency, mech_name, mech_code, fiscal_year, psnu) %>% 
    summarise(tx_curr = sum(qtr1, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(tx_curr_overall = sum(tx_curr))
  
  msd %>% group_by(fiscal_year, fundingagency, snu1) %>% 
    summarise(tx_curr = sum(qtr1, na.rm = T)) %>% prinf()
  
  
  # Pull out TX_CURR results 
  msd_long <- 
    msd %>% 
    reshape_msd(clean = T) %>% 
    filter(period_type == "results") %>% 
    group_by(snu1, psnu, mech_code, mech_name, period, period_type, fundingagency) %>% 
    summarise(tx_curr = sum(value, na.rm = T)) %>% 
    ungroup() 
  
  msd_long %>% filter(period == "FY21Q1") %>% 
    count(snu1, fundingagency) %>% spread(fundingagency, n) %>% 
    arrange(USAID)
  
  msd_long %>% 
    group_by(period) %>% 
    summarise(tx_curr = sum(tx_curr))
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

