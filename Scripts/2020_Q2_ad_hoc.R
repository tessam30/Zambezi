# Purpose: Q2 Zambia Ad hoc questions
# Author: Tim Essam, SI CASC
# Date 2020-06-16
# Notes: Ad hoc requests from PTC


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)



# GLOBALS -----------------------------------------------------------------

  mer_in <- "C:/Users/Tessam/Documents/Data"
  data <- "Data"
  data_out <- "Dataout"
  images <- "Images"



# MUNGE -------------------------------------------------------------------

# Load most recent MSD
# df <- read_rds(file.path(mer_in, "MER_Structured_Datasets_PSNU_IM_FY18-20_20200605_v1_1.rds")) %>% 
#   filter(operatingunits == "Zambia") 

df_zmb <- read_msd(file.path(mer_in, "MER_Structured_Datasets_OU_IM_FY18-20_20200605_v1_1.txt")) %>% 
  filter(operatingunit == "Zambia") %>% 
  reshape_msd()

# QC Treatment variables
zmb_targ <- df_zmb %>% 
  filter(disaggregate  == "Total Numerator", 
              fundingagency == "USAID", 
              str_detect(period, "target"))


# Local Treatment Parnter (mech_code = 82075) appears to be the FY20 targets for EQUIP (mech_code = 18304)
# OPU states that no target shifts are needed, but this apears to be incorrect as the targets are not
# carried over to EQUIP
zmb_targ %>%
  filter(mech_code == 82075) %>% 
  group_by(mech_code, mech_name, indicator, period) %>% 
  summarise(targ = sum(val, na.rm = TRUE))




# 6MMD
df_zmb %>% filter(fundingagency == "USAID",
                  indicator == "TX_CURR",
                  disaggregate == "Age/Sex/ARVDispense/HIVStatus",
                  otherdisaggregate %in% c("ARV Dispensing Quantity - 6 or more months")) %>% 
  group_by(period) %>% 
  summarise(tx_mmd6 = sum(val, na.rm = TRUE))
  


df_pepfar_mmd <- 
  df %>%  
  filter(indicator == "TX_CURR", 
         disaggregate == "Age/Sex/ARVDispense/HIVStatus",
         otherdisaggregate %in% c("ARV Dispensing Quantity - 3 to 5 months", "ARV Dispensing Quantity - 6 or more months")) %>% 
  sum_indic() %>% 
  mutate(indicator = "TX_MMD3") 

