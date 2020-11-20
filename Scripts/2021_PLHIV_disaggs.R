# Review Nat/Sub National Data
# Author: Tim Essam | SI
# Date: 2020-11-18
# Notes:


# GLOBALS -----------------------------------------------------------------

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gisr)
  library(here)

  datain <- "Data"


# LOAD AND MUNGE ----------------------------------------------------------

  df <- vroom::vroom(here(datain, "MER_Structured_Datasets_NAT_SUBNAT_FY15-20_20200814_v1_1.txt")) %>% 
    filter(operatingunit == "Zambia", indicator == "PLHIV")
  
  names(df)
  df %>% count(fiscal_year, indicator, categoryoptioncomboname) %>% prinf
  