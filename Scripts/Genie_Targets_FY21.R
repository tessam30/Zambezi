# Pull and process FY2021 Targets for key indicators
# Author: Tim Essam | SI
# Date: 2020_07_06



# GLOBALS -----------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(glitr)
  library(glamr)
  library(here)

  data_in <- "Data"
  data_out <- "Dataout"
  images <- "Images"
  
 indic_list <- c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_MMD")
 

# LOAD and MUNGE ----------------------------------------------------------

  tgt <- unzip(here(data_in, "Genie-SiteByIMs_FY2021-Zambia-Daily-2020-07-08.zip"), exdir = data_in) 

 targets21 <- read_msd(tgt)
 
 targets21_flt <- targets21 %>% 
   filter(disaggregate == "Total Numerator", indicator %in% indic_list) %>% 
   select(-contains("qtr"))
 
 targets21_flt %>% 
   filter(fundingagency != "DOD") %>% 
   group_by(fundingagency, indicator, primepartner, mech_code) %>% 
   summarise(targets = sum(targets, na.rm = T)) %>% 
   prinf()
 

 