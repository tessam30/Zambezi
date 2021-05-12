# Purpose: Validate FY21 Q2 data 
# Author: Tim Essam | SI, 
# Date: 2021-05-05
# Notes: URGENT Map request from Mission

# LOCALS & SETUP ============================================================================

  # Libraries
  library(glitr)
  library(glamr)
  library(gisr)
  library(Wavelength)
  library(ICPIutilities)
  library(tidyverse)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  
  
  # Set paths  
  data   <- "Data"
  dataout <- "Dataout"
  images  <- "Images"
  graphs  <- "Graphics"
  
  merdata <- glamr::si_path("path_msd")
  rasdata <- glamr::si_path("path_raster")
  shpdata <- glamr::si_path("path_vector")
  datim   <- glamr::si_path("path_datim")  
  
  # Functions  
  
  # Data pulled on 2021-05-05 using indicator filter for OVC_SERV, AGYW_PREV and GEND_GBV
  genie <- return_latest(file.path(merdata, "ZMB"), pattern = "Genie")
  

  # Generic function to calculate aggregates
  sum_indics <- function(df) {
    df %>% 
      mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
      group_by(fiscal_year, indicator) %>% 
      summarise(across(matches("qtr|cum|tar"), sum, na.rm = T)) %>% 
      select(-matches("(3|4)")) %>% 
      relocate(targets, .after = cumulative) %>% 
      ungroup() %>% 
      mutate(ach = cumulative / targets)
  }
  
# LOAD AND MUNGE ----------------------------------------------------------

  df <- read_msd(genie)  

  
  
  

  

  

  
  
  vl <- c("TX_PVLS")
  

# MUNGE PEPFAR CATEGORIES -------------------------------------------------

# TESTING -----------------------------------------------------------------  
  
  tst <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "HTS_INDEX_KNOWNPOS", "HTS_SELF", "HTS_RECENT",
           "PMTCT_STAT", "PMTCT_STAT_POS",
           "PMTCT_EID", "PMTCT_HEI_POS", "TB_STAT", "CXCA_SCRN", "OVC_HIVSTAT")
  
  tst_peds <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "HTS_INDEX_KNOWNPOS")
  
  
  tst_order <- c("HTS_TST", "HTS_TST_POS", "HTS_TST_PEDS", "HTS_TST_POS_PEDS", "HTS_INDEX", "HTS_INDEX_KNOWNPOS", 
                 "HTS_INDEX_PEDS", "HTS_INDEX_KNOWNPOS_PEDS", "HTS_SELF", "HTS_RECENT",
                 "PMTCT_STAT", "PMTCT_STAT_D", "PMTCT_STAT_POS",
                 "PMTCT_EID", "PMTCT_EID_D", "PMTCT_HEI_POS", "TB_STAT", "TB_STAT_D", "CXCA_SCRN", "OVC_HIVSTAT", "OVC_HIVSTAT_D")
  
  # First grab peds indicators
  hts_peds <- 
    df %>% 
    filter(indicator %in% tst_peds, 
           trendsfine %in% c("<01", "01-09", "10-14")) %>% 
    sum_indics() %>% 
    mutate(indicator = fct_relevel(paste0(indicator, "_PEDS"), paste0(tst_peds, "_PEDS"))) %>% 
    arrange(indicator)


  # Grab the rest, adding in rows to create space for derived indicators
  hts <-  
    df %>% 
    filter(indicator %in% tst, 
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    sum_indics() %>% 
    bind_rows(hts_peds) %>% 
    mutate(indicator = fct_relevel(indicator, tst_order)) %>% 
    arrange(indicator) %>% 
    filter(!indicator %in% c("PMTCT_EID_D", "OVC_HIVSTAT_D")) %>% 
    add_row(fiscal_year = 2021, 
            indicator = "HTS_POSITIVITY",
            .before = 3) %>% 
    add_row(fiscal_year = 2021, 
            indicator = "HTS_POSITIVITY_PEDS",
            .before = 6) %>% 
    add_row(fiscal_year = 2021, 
          indicator = "HTS_INDEX_POSITIVITY",
          .before = 9)  %>% 
    add_row(fiscal_year = 2021, 
            indicator = "HTS_INDEX_POSITIVITY_PEDS",
            .before = 12) %>% 
    add_row(fiscal_year = 2021, 
            indicator = "PCT_POS_FROM_INDEX",
            .before = 13) %>% 
    add_row(fiscal_year = 2021, 
            indicator = "ANC_KNOWN_STATUS",
            .before = 18) %>%
    add_row(fiscal_year = 2021, 
            indicator = "PMTCT_POSITIVITY",
            .before = 20) %>% 
    add_row(fiscal_year = 2021, 
            indicator = "PMTCT_LINKAGE",
            .before = 21) %>% 
    add_row(fiscal_year = 2021, 
            indicator = "PMTCT_EID_POSITIVITY",
            .before = 24) %>% 
    add_row(fiscal_year = 2021, 
            indicator = "TB_STAT_PCT",
            .before = 27) %>% 
    mutate(category = "TESTING")

  rm(hts_peds)
    

# TREATMENT ---------------------------------------------------------------

  # Treatment indicators
  trmt_peds <- c("TX_NEW", "TX_CURR", "TX_ML", "TX_RTT")
  
  trmt <- c("TX_NEW", "TX_CURR", "TB_ART", "PMTCT_ART", "TX_ML", "TX_RTT", "CXCA_TX", "CXCA_SCRN_POS", "TX_TB")
  
  trmt_order <- c("TX_NEW", "TX_NEW_PEDS", "TX_CURR", "TX_CURR_PEDS", "TB_ART", "TB_ART_D", 
                  "PMTCT_ART", "PMTCT_ART_D", "TX_ML", "TX_ML_PEDS", "TX_RTT", "TX_RTT_PEDS", 
                  "CXCA_TX", "CXCA_SCRN_POS", "TX_TB", "TX_TB_D")
  

  # TREATMENT
    tx_peds <- 
      df %>% 
      filter(indicator %in% trmt_peds, 
             trendsfine %in% c("<01", "01-09", "10-14")) %>% 
      sum_indics() %>% 
      mutate(indicator = fct_relevel(indicator, trmt_peds),
             indicator = fct_relevel(paste0(indicator, "_PEDS"), paste0(trmt_peds, "_PEDS"))) %>% 
      arrange(indicator)
  
  tx <- 
    df %>% 
    filter(indicator %in% trmt, 
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    sum_indics() %>% 
    bind_rows(tx_peds) %>% 
    mutate(indicator = fct_relevel(indicator, trmt_order)) %>% 
    arrange(indicator) %>% 
    add_row(fiscal_year = 2021,
             indicator = "LINKAGE TO TREATMENT",
             .before = 2) %>% 
    add_row(fiscal_year = 2021,
            indicator = "LINKAGE PEDS",
            .before = 4) %>% 
    add_row(fiscal_year = 2021,
          indicator = "NET NEW",
          .before = 6) %>% 
    add_row(fiscal_year = 2021,
          indicator = "NET NEW PEDS",
          .before = 8) %>% 
    add_row(fiscal_year = 2021,
            indicator = "CXCA_TX_PERCENT",
            .before = 19) %>% 
    add_row(fiscal_year = 2021,
            indicator = "TX_TB_PERCENT",
            .after = 21) %>% 
    filter(!indicator %in% c("TB_ART_D", "PMTCT_ART_D")) %>% 
    mutate(category = "TREATMENT")
  

# PREVENTION --------------------------------------------------------------
  
  # Prevention Indicators
  
  prev <- c("VMMC_CIRC", "KP_PREV", "OVC_SERV", "PP_PREV", "PrEP_CURR", "PrEP_NEW",
            "TB_PREV", "AGYW_PREV", "GEND_GBV")
  
  prev_order <- c("VMMC_CIRC", "VMMC_CIRC_15_29", "KP_PREV", "OVC_SERV", "PP_PREV", "PrEP_CURR", "PrEP_NEW",
                  "TB_PREV", "TB_PREV_D", "AGYW_PREV", "AGYW_PREV_D", "GEND_GBV")
  
  
 #PREVENTION
  vmmc_age <- 
    df %>% 
    filter(indicator == "VMMC_CIRC", 
           trendsfine  %in% c("15-19", "20-24", "25-29"),
           disaggregate == "Age/Sex") %>%
    sum_indics() %>% 
    mutate(indicator = paste0(indicator, "_15_29"))
  
  prev <- 
    df %>% 
    filter(indicator %in% prev,  
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    sum_indics() %>% 
    ungroup() %>% 
    bind_rows(., vmmc_age) %>% 
    mutate(indicator = fct_relevel(indicator, prev_order)) %>% 
    arrange(indicator) %>% 
    add_row(fiscal_year = 2021,
            indicator = "VMMC_PERCENT_TARGET_POP",
            .after = 3) %>%  
    add_row(fiscal_year = 2021,
            indicator = "TB_PREV_PERCENT",
            .after = 10) %>% 
    mutate(category = "PREVENTION")


# HEALTH SYSTEMS ----------------------------------------------------------
  
  hlth_sys <- c("SC_ARVDISP", "SC_CURR")
  hsys <-   
    df %>% 
    filter(indicator %in% hlth_sys,  
           standardizeddisaggregate %in% c("DispensedARVBottles", "CurrentARVBottles")) %>% 
    sum_indics() %>% 
    ungroup() %>% 
    mutate(category = "HEALTH SYSTEMS")


# VIRAL SUPRESSION ----------------------------------------------------------
 
  vl_order <- c("TX_PVLS", "TX_PVLS_PEDS", "TX_PVLS_D", "TX_PVLS_D_PEDS")
  
  vl_peds <- 
    df %>% 
    filter(indicator %in% "TX_PVLS",
           trendsfine %in% c("<01", "01-09", "10-14")) %>% 
    sum_indics() %>% 
    ungroup() %>% 
    mutate(indicator = fct_relevel(paste0(indicator, "_PEDS"), paste0(indicator, "_PEDS")))
  
 vl <- 
   df %>% 
    filter(indicator %in% "TX_PVLS",
           standardizeddisaggregate %in% c("Total Denominator","Total Numerator")) %>% 
    sum_indics() %>% 
    ungroup() %>% 
    bind_rows(vl_peds) %>% 
   mutate(indicator = fct_relevel(indicator, vl_order)) %>% 
   arrange(indicator) %>% 
   add_row(fiscal_year = 2021,
           indicator = "VLS", 
           .after = 4) %>% 
   add_row(fiscal_year = 2021,
           indicator = "VLS_PEDS",
           .after = 5) %>% 
   mutate(category = "VIRAL SUPPRESSION")
  


# BUILD TABLE and DERIVED INDICATORS --------------------------------------


  fyq2 <- 
   bind_rows(hts, tx, prev, hsys, vl) %>% 
   mutate(order = row_number(),
          )
  
