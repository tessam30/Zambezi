# Zambia HFR Reporting
# Author: Tim Essam, SI OHA CAC
# Date: 2020-07-17
# Notes: In response to a question from Michelle re HFR reporting credit



# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(ICPIutilities)
library(here)
library(ggrepel)
library(patchwork)


# GLOBALS -----------------------------------------------------------------

data_in <- "Data"
data_out <- "Dataout"
images <- "Images"


# PREP Data ---------------------------------------------------------------


  unzip(here(data, "MER_Structured_Datasets_Site_IM_FY18-20_20200626_v2_1_Zambia.zip"), exdir = data)
  
  mer <- 
    read_msd(here(data, "MER_Structured_Datasets_Site_IM_FY18-20_20200626_v2_1_Zambia.txt"))



# MUNGE and SUMMARIZE -----------------------------------------------------

# GRAB only TX_CURR, flag sites with targets, with only results and with only targets
  
mer_tgts <- 
  mer %>% filter(indicator == "TX_CURR") %>% 
  filter(disaggregate == "Total Numerator", 
         fundingagency == "USAID", 
         fiscal_year == "2020") %>% 
  mutate(flag = if_else(!is.na(targets) & !is.na(qtr1), 1, 0),
         results_no_target = if_else(!is.na(qtr1) & is.na(targets), 1, 0),
         target_no_result = if_else(!is.na(targets) & is.na(qtr1), 1, 0)) 


# What does TX_CURR reporting look like by mech_code?
  mer_tgts %>% 
    reshape_msd() %>% 
    group_by(period, mech_code, mech_name) %>% 
    summarise(val = sum(val, na.rm = TRUE)) %>% 
    relocate(period, .before = last_col()) %>% 
    spread(period, val) %>% 
    relocate(contains("q"), .before = fy2020_targets) %>% 
    prinf()
  
  
  
# What is the summary of sites by the above categories?  
mer_tgts %>% 
  group_by(mech_code, mech_name) %>%
  summarise(both = sum(flag, na.rm = TRUE),
            results_only = sum(results_no_target, na.rm = TRUE),
            targets_only = sum(target_no_result, na.rm = TRUE)) %>% 
  rowwise() %>% 
  mutate(total_sites_DATIM = sum(both, results_only, targets_only))



