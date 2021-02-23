# Purpose: Munge and Analysis of SI Branch LOE Trackah
# Author: Tim Essam | SI, 
# Date: 2020-02-19
# Notes:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(googlesheets4)  
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
    
  # Functions  
    
  # URL
    url <- "https://docs.google.com/spreadsheets/d/1mea71Z3QcYHLC6bwuWnw0bc93WSRNZX6wUvcjFlEIN4/edit?ts=602fbdd9#gid=0"  
    
  

# LOAD DATA ============================================================================  
  load_secrets()
  
  id <- 
    gs4_get(url) 

  
 df <-  read_sheet(id$spreadsheet_id)

# MUNGE ============================================================================
  
  #  Reshape, tally totals for each person
 df_long <- 
   df %>% 
   pivot_longer(-Name,
                names_to = "workstream",
                values_to = "loe",
                values_drop_na = T) %>% 
   group_by(Name) %>% 
   mutate(loe_person = sum(loe, na.rm = T)) %>% 
   group_by(workstream) %>% 
   mutate(loe_workstream = sum(loe, na.rm = T)) %>%
   ungroup()
 
 df_long %>% 
   count(workstream, loe_workstream) %>% 
   arrange(desc(loe_workstream))
 
 df_long %>% 
   count(Name, loe_person) %>% 
   arrange(desc(loe_person))
 
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

