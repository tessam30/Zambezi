# Purpose: Munge and Plot DREAMS analysis
# Author: Tim Essam
# Date: 2021-01-15
# Notes: For COP21


# PRELIMS -----------------------------------------------------------------

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gisr)
  library(here)
  library(readxl)
  library(sf)
  
  images <- "Images"
  data <- "Data"
  geo <- "GIS"
  
  file_name <- list.files("Data", pattern = "TE_tidy")
  

# READ and MUNGE ----------------------------------------------------------

  #   
  df_list <- excel_sheets(here(data, file_name)) %>% 
    set_names() %>% 
    purrr::map(.f = ~read_excel(path = here(data, file_name), sheet = .x, col_names = TRUE, ))
  
  df_list %>% purrr::map(.f = ~glimpse(.x))
  summary(df_list)
  admin_cw <- unique(df_list$`Number reached tidy`$District)
  
  # Shapefile for plotting
  zmb_geo <- st_read(here(geo, "zmb_admbnda_adm2_2020_simplified"))
  districts <- zmb_geo %>% st_drop_geometry() %>% count(ADM2_EN) %>% pull(ADM2_EN)

  # overlap between shapefile and admin_cw
  intersect(admin_cw, districts)

  
  