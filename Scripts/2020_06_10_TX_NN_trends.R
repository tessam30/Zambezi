# Purpose: Munge Genie Extract to look at TX_CURR and TX_NET_NEW trends
# Author: Tim Essam | SI Asc
# Date: 2020_06_10


# PRELIMS -----------------------------------------------------------------

library(glitr)
library(glamr)
library(tidyverse)
library(scales)
library(tidytext)
library(here)


# GLOBALS -----------------------------------------------------------------

  data <- "Data"
  data_out <- "Dataout"
  


# LOAD and MUNGE ----------------------------------------------------------

  unzip(file.path(here(data, "Genie-SiteByIMs-Zambia-Daily-2020-06-10.zip")), exdir = data)

  df <- vroom::vroom("Genie_SITE_IM_Zambia_Daily_4609e32a-5113-475b-8322-41c782c819d4.txt") %>% 
    filter(disaggregate == "Total Numerator")
  
  
  