# Purpose: 2020 Q4 map
# Author: Tim Essam
# Date: 2020-11-19
# Notes: Update from Q2 map but using gisr for complete map


# GLOBALS -----------------------------------------------------------------

  library(tidyverse)
  library(gisr)
  library(glitr)
  library(glamr)
  library(vroom)
  library(ICPIutilities)


  datain <- "Data"
  graphics <- "Graphics"
  source("Scripts/00_setup_maps.R")
  GIS <- "GIS"
  

  
# LOAD AND MUNGE ----------------------------------------------------------

  # MAP of PrEP factilities
  # Load sites from Genie extract
  geo <- read_csv(file.path(GIS, "Zambia - Facilities_locations_2020-08-05.csv"))
  
  # Pulled Genie PrEP_NEW & PrEP_CURR on 2020-11-19
  df_long <- vroom("Data/Genie_SITE_IM_Zambia_Daily_PrEP_2020_11_19.txt") %>% 
    filter(standardizeddisaggregate == "Total Numerator") %>% 
    reshape_msd() %>% 
    mutate(fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency)) %>% 
    mutate(fy = substr(period, 3, 6)) %>% 
    filter(fundingagency == "USAID") %>% 
    filter(str_detect(indicator, "PrEP"), str_detect(period, "cumulative")) %>% 
    left_join(geo, by = c("orgunituid" = "id"))

  zmb_base +
    geom_sf_text(data = get_admin1(list("Zambia")), aes(label = name), family = "Source Sans Pro", size = 3)+
    geom_point(data = df_long %>% filter(indicator == "PrEP_NEW"), 
               aes(y = latitude, x = longitude), shape = 21, fill = "#d7301f", stroke = 0.25, 
               colour = "white", alpha = 0.80) +
    labs(x = NULL, y = NULL,
         title = "PrEP_NEW facilities reporting in DATIM for FY20 \n",
         caption = "Source: DATIM Genie pull as of 2020-11-19") +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank())
  
  

  si_save(here(images, "PrEP_NEW_FY20.png"))  

  
  
