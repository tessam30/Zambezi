# Purpose: Verify OPU and target shifts across provinces from GENIE
# Author: Tim Essam | SI, 
# Date: 2020-02-08
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
    library(tameDP)  
    library(ICPIutilities)
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"  
  
    source("./Scripts/Z00_Config.R")
    source("./Scripts/Z01_fetch_spdfs.R")

      
  # Functions  

# LOAD DATA ============================================================================  
  
  # Load latest genie pull for OVC_SERVE
  genie_file <- return_latest(folderpath = "Data/COP21/", pattern = "2021-02-08_OVC_SERVE.zip")
  dp_file <- return_latest(folderpath = "Data/COP21/", pattern = "Data Pack|$.xlsx")  
    
  opu <- read_msd(genie_file)  
    opu %>% count(mech_code)

  df_dp <- import_dp(dp_file)
  
    
# MUNGE ============================================================================
  
  #filter out to OVC serve
  df_dp_ovc <- 
    df_dp %>% 
    filter(str_detect(indicator_code, "OVC_SERV|OVC_HIVSTAT")) %>% 
    separate(psnu, into = c("psnu","district", "type", "psnuuid"), sep = " ") %>% 
    mutate(type = str_remove_all(type, "\\[|\\]"),
           psnuuid = str_remove_all(psnuuid, "\\[|\\]")) %>% 
    left_join(., spdf_comm_zmb %>% st_drop_geometry(), by = c("psnuuid" = "uid", "psnu" = "psnu"))
  
  df_dp_ovc_totals <- df_dp_ovc %>% 
    select(snu1, psnu, indicator_code, age, sex, keypop,
           tot_dedup_rollup_orig = `total deduplicated rollup...6`,
           tot_dedup_rollup =`total deduplicated rollup...92`, 
           mech_85114 = `85114_dsd...121`,
           mech_85120 = `85120_dsd`,
           mech_85121 = `85121_dsd`,
           mech_85115 = `85115_dsd...122`,
           mech_18487 = `18487_dsd...124`
           )

  df_dp_ovc %>% distinct(snu1, indicator_code) %>% arrange(snu1) %>% relocate(snu1, indicator_code)
  
  
  df_dp_ovc_totals %>% 
    filter(indicator_code != "OVC_HIVSTAT.N.total.T") %>% 
  write_csv(file.path("Dataout/COP21/ZMB_datapack_OVC_targets.csv"))
  
  
      
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

