# Purpose: Map PrEP_NEW sites across FY19 and FY20 and OVC geospatial coverage
# Author: Tim Essam | SI Asc
# Date: 2020_06_116


# Data Notes:
# Site By IM Extract
# DATIM data as of: 06/08/2020 11:03:22 UTC
# Genie report updated: 06/10/2020 10:52:19 UTC
# Current period(s): 2019 Target,  2019 Q1,  2019 Q2,  2019 Q3,  2019 Q4,  2020 Target,  2020 Q1,  2020 Q2
# Operating Unit: Global >Africa >Zambia >
# Indicator: HTS_TST ,HTS_TST_POS ,PrEP_CURR ,PrEP_NEW ,TX_CURR ,TX_NET_NEW ,TX_NEW ,TX_PVLS



# LOAD GENIE --------------------------------------------------------------

  library(glamr)
  library(tidyverse)
  library(scales)
  library(tidytext)
  library(here)
  library(ICPIutilities)
  library(ggrepel)
  library(patchwork)
  library(readxl)

# GLOBALS -----------------------------------------------------------------

  data <- "Data"
  data_out <- "Dataout"
  images <- "Images"
  gis <- "GIS"



# MUNGE AND PREP ----------------------------------------------------------

  unzip(here(data, "Genie-SiteByIMs-Zambia-Daily-2020-06-11.zip"), exdir = data)
  
  df <- vroom::vroom(here(data, "Genie_SITE_IM_Zambia_Daily_20ea2046-39b0-416c-96b7-1430ad0fcdeb.txt")) %>% 
    filter(disaggregate == "Total Numerator")

  df_long <- df %>%  reshape_msd() %>% 
    mutate(fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency)) %>% 
    mutate(fy = substr(period, 3, 6))
  
  # Previously, these came from the DATIM API pull. Pushed to drive until mypwd works again.
  df_orgs <- read_csv(here(data_out, "ZMB_site_lat_lon.csv"))
  
  # Unzip and assess shapefiles
  # unzip(here(gis, "zmb_adm_2020_shp.zip"), exdir = gis)
  # admin2 <- st_read(here(gis, "zmb_admbnda_adm2_2020.shp"))

  # Try simplified data as OCHA one takes a long time to render
  unzip(here(gis, "zmb_admbnda_adm2_2020_simplified.zip"), exdir = gis)
  admin2_simp <- st_read(here(gis, "zmb_admbnda_adm2_2020_simplified", 
    "zmb_admbnda_adm2_2020.shp"))
  
  
  # OVC Data
  ovc <- read_excel(here(data, "2020 06 Map Spreadsheet for Tim_OVC.xls"))
  
  

# MAP PREP ----------------------------------------------------------------
  
  # Source or run 00_setup_maps.R to get your basemap in place

  df_prep <- 
    df_long %>% 
    filter(fundingagency == "USAID") %>% 
    mutate(fy = substr(period, 3, 6)) %>% 
    filter(str_detect(indicator, "PrEP"), str_detect(period, "cumulative")) %>% 
    left_join(., df_orgs)
  
  remove(df_long)
  
  # Count how many orgunitUIDs per geography
  df_prep %>% filter(!is.na(val), !is.na(latitude)) %>% count(snu1, indicator, fy) %>% 
    group_by(indicator) %>% 
    mutate(total = sum(n)) %>% 
    spread(fy, n) %>% 
    mutate(diff = `2020` - `2019`)
  
  # Map PrEP_CURR
  zmb_base +
    geom_sf_text(data = get_admin1(list("Zambia")), aes(label = name), family = "Source Sans Pro", size = 3)+
    geom_point(data = df_prep %>% filter(indicator == "PrEP_NEW", fy != "2021"), 
      aes(y = latitude, x = longitude), shape = 21, fill = "#d7301f", stroke = 0.25, 
      colour = "white", alpha = 0.80) +
    facet_wrap(~fy) +
    labs(x = NULL, y = NULL,
      title = "PrEP_NEW facilities reporting in DATIM by fiscal year \n",
      caption = "Source: DATIM Genie pull as of 6/11/2020") +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
      axis.text.x = element_blank())
  
  si_save(here(images, "PrEP_NEW_FY19_FY20.png"))
  
  # Write the data to a .csv for checking
  write_csv(here(data_out, "ZMB_PrEP_NEW_cumulative_fy19_fy20.csv"), x = df_prep)
  #write_csv(here(data_out, "ZMB_site_lat_lon.csv"), x = df_orgs)
  

# OVC MAPS ----------------------------------------------------------------

  # 1) OVC Map by district- FY20 (Please list agency and partners for each district.   
  # Country and Province are included in spreadsheet, but not needed on map.)
  # 2) Same as above for FY21
  
  # Join the excel file to the shapefile using the district names
  ovc <- 
    ovc %>% 
    mutate(ADM2_EN = str_replace_all(District, pattern = " District", ""),
      Partner = case_when(
        Partner == "Zambia Centre for Communication Programme (ZCCP)" ~ "ZCCP",
        TRUE ~ Partner
      ),
      partner_fct = as_factor(Partner),
      ADM2_EN = case_when(
        ADM2_EN == "Kapiri-Mposhi" ~ "Kapiri Mposhi",
        ADM2_EN == "Chikankata"  ~ "Chikankanta",
        ADM2_EN == "Shiwang'andu" ~ "Shiwamg'andu",
        TRUE ~ ADM2_EN
      )
    )
  
    
  ovc_partners <- unique(ovc$Partner) %>% as_tibble() %>% 
    mutate(color = si_palettes$category20b[1:length(unique(ovc$Partner))]) %>% 
    rename(Partner = value)
  
  setdiff(unique(admin2_simp$ADM2_EN), unique(ovc$ADM2_EN))
  
 ovc_tmp <- left_join(ovc, ovc_partners)
 
 # Spatialize the data 
  ovc_geo <- admin2_simp %>% right_join(., ovc_tmp)
  
  
# CHECK NUMBERS AND DISTRIBUTION 
  ovc_geo %>% 
    st_drop_geometry(.) %>% 
    filter(`COP19 Presence` == "X") %>% 
    count(District, Partner, Agency, color)
  
  ovc_geo %>% 
    st_drop_geometry(.) %>% 
    filter(`COP20 Presence` == "X") %>% 
    count(District, Partner, Agency, color)
  
  # Grab bounding box coordinates
  mapRange_admin <- c(range(st_coordinates(admin2_simp)[, 1]), range(st_coordinates(admin2_simp)[, 2]))
  
  ovc_map <- function(cop_year, title = NULL, nrow = NULL) {
  # Notes - Zambia base ouline set to size = 2  
  # Stripped out zmb_base due to heaviness of grays; added admin0 polygon to frame maps  
    
  zmb_base +
    geom_sf(data = admin2_simp, colour = grey30k, fill = NA, size = 0.15) +
    geom_sf(data =  ovc_geo %>% 
        filter({{cop_year}} == "X"), aes(fill = color), colour = "white", size = 0.1) + 
      geom_sf(data = get_admin0(list("Zambia")), size = 0.5, fill = NA) +
    facet_wrap(Agency ~ Partner,
      nrow = {{nrow}}) +
      scale_fill_identity() +
    coord_sf(xlim = mapRange_admin[c(1:2)], ylim = mapRange_admin[c(3:4)]) +
    labs(x = NULL, y = NULL, 
      title = paste0({{title}}),
      caption = "Created on 2020-06-17 by USAID SIEI | SI") +
    si_style_nolines() +
    theme(legend.position = "none",
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.text = element_text(size = 10),
      panel.spacing = unit(1, "lines"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()) 
  }
  
  ovc19_map <- ovc_map(cop_year = `COP19 Presence`,
    title = "COP19 OVC PARTNERS GEOGRAPHIC COVERAGE BY FUNDING AGENCY") 
  
  si_save(here(images, "COP19_OVC_partnrs_map_by_agency_terrain.png"), plot = ovc19_map,
    scale = 1.25)
  
  
  ovc20_map <- ovc_map(cop_year = `COP20 Presence`,
    title = "COP20 OVC PARTNERS GEOGRAPHIC COVERAGE BY FUNDING AGENCY")
  si_save(here(images, "COP20_OVC_partnrs_map_by_agency_terrain.png"), plot = ovc20_map, 
    scale = 1.25)
 
  

# # Additional requests ---------------------------------------------------

  # 1) USG coverage of OVC districts nationwide for FY21 (please do not include Peace Corps or State.  It's so sad, but we don't know when (if!) Peace Corps will be back).  So just a map with the districts colored in where we have USAID or CDC presence- no need to include agency.
  # 
  # 2) USAID coverage of OVC districts nationwide for FY21.  
  
  
  # Collapse Agency
  ovc_geo_agency <- 
    ovc_geo %>% filter(Agency %in% c("USAID", "CDC"),
      `COP20 Presence` == "X") %>% 
    count(ADM2_EN, Agency) 
  
  
  #zmb_base +
  ggplot() +
    geom_sf(data = admin2_simp, colour = grey30k, fill = NA, size = 0.15) +
    geom_sf(data = ovc_geo_agency, aes(fill = Agency), 
      colour = "white", size = 0.25) +
    geom_sf(data = get_admin0(list("Zambia")), size = 0.5, fill = NA) +
    coord_sf(xlim = mapRange_admin[c(1:2)], ylim = mapRange_admin[c(3:4)]) +
    labs(x = NULL, y = NULL, 
      title = "USAID OR CDC PRESENCE IN FY21",
      caption = "Created on 2020-06-17 by USAID SIEI | SI") +
    facet_wrap(~Agency) +
    scale_fill_manual(values = c("CDC" = usaid_lightblue, "USAID" = usaid_blue)) +
    si_style_nolines() +
    theme(legend.position = "none",
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.text = element_text(size = 10),
      panel.spacing = unit(1, "lines"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()) 
  
  si_save(here(images, "USAID_OR_CDC_PRESENCE_FY21_nobasemap.png"), plot = last_plot(), 
    scale = 1)
  


# USAID ONLY --------------------------------------------------------------

  zmb_base +
  # ggplot() +
    geom_sf(data = admin2_simp, colour = grey30k, fill = NA, size = 0.15) +
    geom_sf(data = ovc_geo_agency %>% filter(Agency == "USAID"), aes(fill = Agency), 
      colour = "white", size = 0.25) +
    geom_sf(data = get_admin0(list("Zambia")), size = 0.5, fill = NA) +
    coord_sf(xlim = mapRange_admin[c(1:2)], ylim = mapRange_admin[c(3:4)]) +
    labs(x = NULL, y = NULL, 
      title = "USAID PRESENCE IN FY21",
      caption = "Created on 2020-06-17 by USAID SIEI | SI") +
    scale_fill_manual(values = c("USAID" = usaid_blue)) +
    si_style_nolines() +
    theme(legend.position = "none",
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.text = element_text(size = 10),
      panel.spacing = unit(1, "lines"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()) 
  
  si_save(here(images, "USAID_PRESENCE_FY21_terrain.png"), plot = last_plot(), 
    scale = 1)

  
  