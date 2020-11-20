# Purpose: Print all targets for FY2021 and write to separate CSVs
# Author: Tim Essam | SI Asc
# Date: 2020_07_08


# Site By IM Extract
# DATIM data as of: 07/08/2020 03:44:48 UTC
# Genie report updated: 07/08/2020 10:43:00 UTC
# Current period(s): 2019 Target,  2019 Q1,  2019 Q2,  2019 Q3,  2019 Q4,  2020 Target,  2020 Q1,  2020 Q2,  2020 Q3

# PRELIMS -----------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(scales)
library(tidytext)
library(here)
library(ICPIutilities)
library(ggrepel)
library(patchwork)

# GLOBALS -----------------------------------------------------------------

data <- "Data"
data_out <- "Dataout"
images <- "Images"

# function for writing targets to CSV
output_csv <- function(data, names){ 
  write_csv(data, file.path(data_out, paste0(names,"_COP20_targets",".csv")))
}


# LOAD and MUNGE ----------------------------------------------------------

# Unzip Genie and MER so we can load w/ read_msd and vroom
unzip(here(data, "Genie-SiteByIMs_FY2021-Zambia-Daily-2020-07-08.zip"), exdir = data)
unzip(here(data, "MER_Structured_Datasets_Site_IM_FY18-20_20200626_v2_1_Zambia.zip"), exdir = data)


mer <- read_msd(here(data, "MER_Structured_Datasets_Site_IM_FY18-20_20200626_v2_1_Zambia.txt")) %>% 
  filter(fundingagency == "USAID", str_detect(indicator, "OVC"))

mer_ovc <- mer %>% filter(fundingagency == "USAID", str_detect(indicator, "OVC")) %>% 
  reshape_msd() %>% 
  filter(str_detect(period, "targets"))

mer_ovc %>% group_by(indicator, primepartner, period, mech_code, mech_name) %>% 
  summarise(targets = sum(val, na.rm = TRUE)) %>% 
  filter(indicator == "OVC_SERV_UNDER_18", period == "fy2020_targets") %>% 
  arrange(mech_code, period) %>% prinf()


df <- 
  vroom::vroom(here(data, "Genie_SITE_IM_Zambia_Daily_cf194d81-9d4b-4c61-8e61-33b7eca4ed56.txt")) %>% 
  filter(fundingagency == "USAID")

df_long <- 
  df %>%  
  reshape_msd() %>% 
  mutate(fy = substr(period, 3, 6))


# Set filters for COP20 targets to retrieve for target verification exercise

COP20_targets <- 
  df_long %>% 
  filter(str_detect(period, "target"), disaggregate == "Total Numerator",
         indicator == "OVC_SERV_UNDER_18") %>% 
  group_by(mech_code, mech_name, primepartner, indicator, fy) %>% 
  summarise(targets = sum(val, na.rm = TRUE)) %>% 
  ungroup() 

# Reproduce target tables for COP20
  mech_list <- 
    COP20_targets %>% 
    select(-c(mech_name)) %>%  
    group_split(mech_code) 

# Store mech_codes as character for using in a file name
  mech_code_list <- 
    mech_list %>% map(~unique(.$mech_code) %>% as.character)

# Save everything using custom function defined above that writes to data_out folder  
  list(mech_list, mech_code_list) %>% 
    pmap(output_csv)


