# Purpose: Check VMMC numbers for BOBS compared to HFR
# Author: Tim Essam, SI CAC
# Date: 2020-08-04
# Notes:



# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(readxl)
library(here)

# GLOBALS -----------------------------------------------------------------
  
  fetch_in <- "BOBs"
  data_out <-  "Dataout"
  
  read_bob <- function(filename, sheet = "monthly_bob", skip = 4) {
    
    mech_name_tmp <- filename %>% str_extract(., pattern = regex(".+?(?=_BOB)"))
    
    df <- read_excel(
      here(fetch_in, filename),
      sheet = sheet,
      skip = skip
    )  %>% 
      mutate(mech_name = mech_name_tmp,
             mech_code = as.character(mech_code))
    
    print(df %>% names())
    
    return(df)    
    
  }
  
  

# MUNGE -------------------------------------------------------------------

  # Had to clean up file names and rename June to JUNE.
  # Need to have consistency in naming files for batching things
  bobs_june <-  list.files(fetch_in, "JUNE")  

  # Review the excel sheets for hidden tabs
  map(bobs_june, ~excel_sheets(path = here(data, .)))
  
  discover <- read_bob("DISCOVER_BOB_JUNE_2020.xlsx")
  
  equip <- read_bob("EQUIP_BOB_JUNE_2020_headers_fixed.xlsx")

  safe  <- read_bob("SAFE_BOB_JUNE_2020.xlsx")   
