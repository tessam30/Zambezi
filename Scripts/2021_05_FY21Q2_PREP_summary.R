# PURPOSE: Munge and Analysis of PrEP_NEW and PrEP_CURR for FY21 Q2
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-05-27
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

  msd <- return_latest(data, "FY19-21_20210514") %>% read_msd()
  

# MUNGE ============================================================================
  
  #  pull PrEP_NEW and PrEP_CURR for just USAID
   df_prep <- 
      msd %>% filter(fundingagency == "USAID",
                   indicator %in% c("PrEP_NEW", "PrEP_CURR"),
                   standardizeddisaggregate == "Total Numerator") %>% 
      group_by(fiscal_year, fundingagency, indicator) %>% 
      summarise(across(matches("qtr|targ"), sum, na.rm = T)) %>% 
    reshape_msd(qtrs_keep_cumulative = T) %>% 
      spread(period_type, value) %>% 
      arrange(indicator, period) %>% 
      mutate(fy = substr(period, 3,4)) %>%
      group_by(indicator, fy) %>% 
      fill(targets, .direction = "down") %>% 
      filter(!is.na(results)) %>% 
      mutate(cumulative = case_when(
        indicator == "PrEP_NEW" ~ cumsum(results), 
        TRUE ~ results
      ),
      indic_color = if_else(indicator == "PrEP_CURR", 
                            "#2d8073", "#5cac9e"),
      achv = ifelse(!is.na(targets), (cumulative / targets), NA_real_),
      targ_tip = if_else(achv >= 1, "white", grey60k))
    
  
# VIZ ============================================================================

  #  
    df_prep %>% 
      ungroup() %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = targets), fill = grey10k) +
      geom_col(aes(y = cumulative, fill = indic_color)) +
      geom_errorbar(aes(ymin = targets, ymax = targets, color = targ_tip)) +
      geom_text(aes(y = cumulative, label = comma(cumulative)), size = 12/.pt, color = grey90k,
                family = "Source Sans Pro", vjust = -0.5) +
      geom_text(aes(y = cumulative, label = percent(achv, 1)), size = 12/.pt, color = "white",
                family = "Source Sans Pro", vjust = 1.5) +
      geom_text(data = . %>% filter(period == "FY21Q2"), 
                aes(y = targets, label = comma(targets)), size = 12/.pt, color = grey90k,
                family = "Source Sans Pro", vjust = -0.5) +
      facet_wrap(~indicator) +
      si_style_xline() +
      scale_fill_identity() +
      scale_color_identity() + 
      coord_cartesian(expand = F, clip = "off") +
      theme(axis.text.y = element_blank(),
            strip.text = element_text(size = 14)) +
      labs(x = NULL, y = NULL, title = "USAID ZAMBIA IS ON PACE TO MEET PrEP_CUR and PrEP_NEW FY21 TARGETS\n",
           caption = "Source: FY21Q2i MSDfiltered to USAID only\n Author: Tim Essam | SIEI SI\n Date: 2021-05-27")
    

# SPINDOWN ============================================================================

  si_save("Images/ZMB_fy21q2_prep_summary.png", scale = 1.15)
    