# Checking HFR for PD8

library(tidyverse)
library(readxl)
library(googlesheets4)


# PULL HFR PROBLEMS -------------------------------------------------------


hfr_prob <- read_sheet("1KOoq9D4aQWV9YWB9gdLMLi51idRsL5Mlw-AW28wVqZI") %>% 
  filter(Countryname == "Zambia") %>% 
  mutate(mech_code = as.character(`Mech Code`),
         id = row_number()) 

#global_hierarchy <- read_sheet("1CdH86n2PlcFnCCgCX0UGwnnaH1uLk0ud")

mech_list <- unique(hfr_prob$orgunituid)

# LOAD DATIM INFO

path <- "C:/Users/Tessam/Documents/github/pump_up_the_jam/Data"
datim_hierarchy <- vroom::vroom(file.path(path, "HFR_FY20_GLOBAL_orghierarchy_20200611.csv")) %>% 
  filter(operatingunit == "Zambia") 


datim_mech <- vroom::vroom(file.path(path, "HFR_FY20_GLOBAL_mechanisms_20200611.csv")) %>% 
  filter(operatingunit == "Zambia", mech_code %in% mech_list)  

  
mech_checklist <- df_long %>% 
  filter(mech_code %in% c("17413", "18304", "18487")) %>% count(mech_code, orgunituid)