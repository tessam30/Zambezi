# Purpose: Munge and Analysis of OVC, DREAMS, GBV maps for Zambia
# Author: Tim Essam | SI, 
# Date: 2021-05-05
# Notes: URGENT Map request from Mission

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
  
    # Data pulled on 2021-05-05 using indicator filter for OVC_SERV, AGYW_PREV and GEND_GBV
    genie <- return_latest(file.path(merdata, "ZMB"), pattern = "Genie_PSNU_IM_OVC")

# LOAD DATA ============================================================================  
  
  df <- read_msd(genie)
    
  source("Scripts/Z00_Config.R")  
  source("Scripts/Z01_fetch_spdfs.R") 
    
# MUNGE ============================================================================
  
  #  Check that we have the right PSNUs
  indic_list <- df %>% distinct(indicator) %>% pull()  
    
  # ECAP Coverage
    ovc <- df %>% 
      filter(standardizeddisaggregate == "Total Numerator",
             indicator == "OVC_SERV",
             str_detect(mech_name, "Empowered")) %>% 
      group_by(psnuuid, psnu, indicator, mech_name, mech_code, fiscal_year, fundingagency, primepartner)%>% 
      summarise(across(.cols = matches("qtr|targ|cum"), sum, na.rm = TRUE)) %>% 
      arrange(mech_name) %>% ungroup() %>% 
      filter(psnu != "Mongu District")
      # write_csv(file.path(dataout, "OVC_SERV_Empowered_psnus.csv"))
    
    
  # DREAMS Coverage
   dreams <-  df %>% 
      filter(standardizeddisaggregate %in% c("Total Numerator"),
             indicator == "AGYW_PREV") %>% 
      group_by(psnuuid, psnu, indicator, mech_name, mech_code, fiscal_year, fundingagency, primepartner, standardizeddisaggregate) %>% 
      summarise(across(.cols = matches("qtr|targ|cum"), sum, na.rm = TRUE))%>% 
      arrange(mech_name, standardizeddisaggregate) %>% ungroup() %>% 
     mutate(agency_color = case_when(
       psnu %in% c("Mazabuka District", "Monze District", "Mongu District") ~ "#209ba7", 
       TRUE ~ denim
     ))
      # write_csv(file.path(dataout, "AGYW_PREV_psnus.csv"))

  # GEND_GBV
   gbv <-  df %>% 
      filter(standardizeddisaggregate == "Total Numerator",
             indicator == "GEND_GBV",
             fundingagency == "USAID") %>% 
      group_by(psnuuid, psnu, indicator, fiscal_year) %>% 
      summarise(across(.cols = matches("qtr|targ|cum"), sum, na.rm = TRUE))%>% 
      arrange(psnu) %>% 
     ungroup() 
      # write_csv(file.path(dataout, "GEND_GBV_psnus.csv"))
  

  
  
# VIZ ============================================================================

  #  OVC_first
   ovc_geo <- 
     spdf_comm_zmb %>% 
     right_join(., ovc, by = c("uid" = "psnuuid")) %>% 
     mutate(fill_color = case_when(
       str_detect(primepartner, "CATHOLIC") ~burnt_sienna,
       str_detect(primepartner, "CONCERN") ~ moody_blue,
       str_detect(primepartner, "CENTRE") ~ golden_sand
     ))
     
  terr_map +
    geom_sf(data = ovc_geo, aes(fill = fill_color), color = "white", alpha = 0.75) +
    geom_sf(data = spdf_reg_zmb, linetype = "dotted", fill = NA) +
    #geom_sf_label(data = spdf_reg_zmb, aes(label = province), size = 12/.pt, fill = "white", alpha = 0.75, label.size = NA) +
    geom_sf(data = spdf_ou_zmb, colour = "black", fill = "NA") +
    scale_fill_identity() +
    si_style_map() +
    labs(x = NULL, y = NULL, title = "ECAP GEOGRAPHIC COVERAGE", 
         caption = "Source: DATIM GENIE pull 2021-05-05 with Mongu District removed")
    
  si_save(file.path(images, "ZMB_OVC_ecap_geographic_coverage_nolabs.png"))
  
  # DREAMS
  dreams_geo <- 
    spdf_comm_zmb %>% 
    right_join(., dreams, by = c("uid" = "psnuuid"))
  
  terr_map +
    geom_sf(data = dreams_geo, aes(fill = agency_color), color = "white", alpha = 0.75) +
    geom_sf(data = spdf_reg_zmb, linetype = "dotted", fill = NA) +
    geom_sf_label(data = spdf_reg_zmb, aes(label = province), size = 12/.pt, fill = "white", alpha = 0.75, label.size = NA) +
    geom_sf(data = spdf_ou_zmb, colour = "black", fill = "NA") +
    scale_fill_identity() +
    si_style_map() +
    labs(x = NULL, y = NULL,
         title = "<span style = 'font-size:14pt;'>DREAMS COVERAGE BY AGENCY</span>", 
         subtitle = "Districts covered by <span style = 'color:#2057a7;'>**USAID**</span> and <span style = 'color:#209ba7;'>**CDC**</span>",
         caption = "Source: DATIM GENIE pull 2021-05-05.") +
    theme(plot.title = element_markdown(family = "Source Sans Pro Regular"),
          plot.subtitle = element_markdown(size = 11, family = "Source Sans Pro"),
          axis.text = element_blank())

  si_save(file.path(images, "ZMB_DREAMS_geographic_coverage.png"))
  
  # GBV
  gbv_geo <- 
  spdf_comm_zmb %>% 
    full_join(., gbv, by = c("uid" = "psnuuid")) %>% 
    mutate(ta_fill = case_when(
               str_detect(`psnu.x`, ("Kalomo|Choma|Monze|Mazabuka|Kafue|Mumbwa|Chibombo|Mpika|Nakonde|Nyimba|Katete")) ~ "#7396ee",
               str_detect(`psnu.x`, ("Chingola|Chipata|Chongwe|Kabwe|Kalulushi|Kapiri-Mposhi|Kitwe|Livingstone|Lusaka|Luanshya|Masaiti|Mongu|Mufulira|Ndola|Sesheke|Solwezi")) ~ "#074895",
               TRUE ~ NA_character_
             )
           ) 
  
  # Check that regex worked as intended
  gbv_geo %>% 
    st_drop_geometry() %>% 
    filter(!is.na(ta_fill)) %>% 
    arrange(ta_fill) %>% 
    count(ta_fill, psnu.x) %>% 
    spread(ta_fill, n) %>% 
    rename(dsd = `#074895`, ta = `#7396ee`) %>% 
    arrange(dsd) %>% 
    prinf()
  

  
  # Basic map of GBV coverage
  terr_map +
    geom_sf(data = gbv_geo, aes(fill = ta_fill), color = "white", alpha = 0.75) +
    geom_sf(data = spdf_reg_zmb, linetype = "dotted", fill = NA) +
    #geom_sf_label(data = spdf_reg_zmb, aes(label = province), size = 12/.pt, fill = "white", alpha = 0.75, label.size = NA) +
    geom_sf(data = spdf_ou_zmb, colour = "black", fill = "NA") +
    scale_fill_identity() +
    si_style_map() +
    labs(x = NULL, y = NULL,
         title = "<span style = 'font-size:14pt;'>USAID GBV COVERAGE BY SERVICE TYPE </span>", 
         subtitle = "Districts covered by <span style = 'color:#074895;'>**direct service delivery (DSD)**</span> and <span style = 'color:#7396ee;'>**technical assistance (TA)**</span>",
         caption = "Source: DATIM GENIE pull 2021-05-05.") +
    theme(plot.title = element_markdown(family = "Source Sans Pro Regular"),
          plot.subtitle = element_markdown(size = 11, family = "Source Sans Pro"),
          axis.text = element_blank())
  
  si_save(file.path(images, "ZMB_OVC_geographic_coverage_nolabel.png"))
  
  
  
# SPINDOWN ============================================================================

