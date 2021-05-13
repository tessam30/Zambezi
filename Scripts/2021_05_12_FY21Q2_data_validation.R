# Purpose: Validate FY21 Q2 data 
# Author: Tim Essam | SI, 
# Date: 2021-05-12
# Notes: URGENT Validation request from the office
# time: took about 6 hours

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
  library(gt)
  
  
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

# MUNGE PEPFAR CATEGORIES -------------------------------------------------

# TESTING -----------------------------------------------------------------  
  
  # Process is the same for each category, order the variables, determine what disaggs to pull, then aggregate up
  
  tst <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "HTS_INDEX_KNOWNPOS", "HTS_SELF", "HTS_RECENT",
           "PMTCT_STAT", "PMTCT_STAT_POS",
           "PMTCT_EID", "PMTCT_HEI_POS", "TB_STAT", "CXCA_SCRN", "OVC_HIVSTAT")
  
  tst_peds <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "HTS_INDEX_KNOWNPOS")
  
  
  tst_order <- c("HTS_TST", "HTS_TST_POS", "HTS_TST_PEDS", "HTS_TST_POS_PEDS", "HTS_INDEX", "HTS_INDEX_KNOWNPOS", 
                 "HTS_INDEX_PEDS", "HTS_INDEX_KNOWNPOS_PEDS", "HTS_SELF", "HTS_RECENT",
                 "PMTCT_STAT_D", "PMTCT_STAT", "PMTCT_STAT_POS",
                 "PMTCT_EID", "PMTCT_HEI_POS", "PMTCT_EID_D","TB_STAT_D", "TB_STAT", "CXCA_SCRN", "OVC_HIVSTAT", "OVC_HIVSTAT_D")
  
  # First grab peds indicators
  hts_peds <- 
    df %>% 
    filter(indicator %in% tst_peds, 
           trendsfine %in% c("<01", "01-09", "10-14")) %>% 
    sum_indics() %>% 
    mutate(indicator = fct_relevel(paste0(indicator, "_PEDS"), paste0(tst_peds, "_PEDS"))) %>% 
    arrange(indicator)


  # Grab the rest, adding in rows to create space for derived indicators
  # Adding in rows that will be derived variables based on others using lags/leads
  # PLACEMENT MATTERS
  
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
    mutate(category = "TESTING",
           targets = case_when(
             indicator == "HTS_INDEX_KNOWNPOS" ~ 74142,
             indicator == "HTS_INDEX_KNOWNPOS_PEDS" ~  3370,
             indicator == "HTS_RECENT" ~ NA_real_,
             TRUE ~targets),
           ach = case_when(
             indicator == "HTS_INDEX_KNOWNPOS" ~ cumulative/targets,
             indicator == "HTS_INDEX_KNOWNPOS_PEDS" ~  cumulative/targets,
             indicator == "HTS_RECENT" ~ NA_real_,
             TRUE ~ ach)
           )
  

  rm(hts_peds)
    

# TREATMENT ---------------------------------------------------------------

  # Treatment indicators
  trmt_peds <- c("TX_NEW", "TX_CURR", "TX_ML", "TX_RTT")
  
  trmt <- c("TX_NEW", "TX_CURR", "TB_ART", "PMTCT_ART", "TX_ML", "TX_RTT", "CXCA_TX", "CXCA_SCRN_POS", "TX_TB")
  
  trmt_order <- c("TX_NEW", "TX_NEW_PEDS", "TX_CURR", "TX_CURR_PEDS", "TB_ART", "TB_ART_D", 
                  "PMTCT_ART", "PMTCT_ART_D", "TX_ML", "TX_ML_PEDS", "TX_RTT", "TX_RTT_PEDS", 
                  "CXCA_SCRN_POS", "CXCA_TX", "TX_TB_D", "TX_TB")
  

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
                  "TB_PREV_D", "TB_PREV", "AGYW_PREV_D", "AGYW_PREV", "GEND_GBV")
  
  
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
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator", "KeyPop")) %>% 
    mutate(drop_tag = case_when(
      indicator == "KP_PREV" & standardizeddisaggregate == "Total Numerator" ~ 1, 
      TRUE ~ 0
    )) %>% 
    filter(drop_tag != 1) %>% 
    select(-drop_tag) %>% 
    sum_indics() %>% 
    ungroup() %>% 
    bind_rows(., vmmc_age) %>% 
    mutate(indicator = fct_relevel(indicator, prev_order)) %>% 
    arrange(indicator) %>% 
    add_row(fiscal_year = 2021,
            indicator = "VMMC_PERCENT_TARGET_POP",
            .before = 3) %>%  
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

 # Simple lag calculations
 lag_list <- c("HTS_POSITIVITY", "HTS_POSITIVITY_PEDS", "HTS_INDEX_POSITIVITY",
               "HTS_INDEX_POSITIVITY_PEDS", "ANC_KNOWN_STATUS", "PMTCT_EID_POSITIVITY",
               "TB_STAT_PCT", "TB_PREV_PERCENT", "VMMC_PERCENT_TARGET_POP", "TX_TB_PERCENT", 
               "CXCA_TX_PERCENT")

 # MAGIC to create the derived percentages displayed in the table
 
  fy21q2 <- bind_rows(hts, tx, prev, hsys, vl) %>% 
   mutate(order = row_number()) %>% 
     mutate(across(c(qtr1, qtr2, cumulative, targets), 
                 ~ case_when(
                   indicator %in% lag_list ~ lag(.)/lag(., n = 2),
                   indicator == "PCT_POS_FROM_INDEX" ~ lag(., n = 5)/lag(., n = 11),
                   indicator == "PMTCT_POSITIVITY" ~ lag(.)/lag(., n = 3),
                   indicator == "PMTCT_LINKAGE" ~ lead(., n = 18) /lag(., n = 2),
                   indicator == "LINKAGE TO TREATMENT" ~ lag(.) / lag(., n = 29),
                   indicator == "LINKAGE PEDS" ~ lag(.) / lag(., n = 28),
                   indicator %in% c("VLS", "VLS_PEDS") ~ lag(., n = 4)/lag(., n = 2),
                   TRUE ~ .
                   )
                 )
     ) %>% 
     mutate(ach = ifelse(!is.infinite(ach), ach, NA_real_),
            across(c(qtr1, qtr2, cumulative, targets), 
                   ~ifelse(!is.nan(.), ., NA_real_)),
            qtr3 = NA_real_,
            qtr4 = NA_real_) %>% 
    mutate(targets = case_when(
              targets == 0 ~NA_real_,
              TRUE ~ targets)
     ) %>% 
    relocate(qtr3, .after = qtr2) %>% 
    relocate(qtr4, .after = qtr3)

  

# GT tables for production ------------------------------------------------

  pct_list <- c(lag_list, "PCT_POS_FROM_INDEX", "PMTCT_POSITIVITY", "PMTCT_LINKAGE",
                "LINKAGE TO TREATMENT", "LINKAGE PEDS", "VLS", "VLS_PEDS")
  
  
 qtr_tbl <- function(cat_var = "TESTING"){
   
  fy21q2 %>% 
    filter(category == {{cat_var}}) %>% 
    select(-c(fiscal_year, order)) %>% 
    gt(groupname_col  = "category") %>% 
    fmt_missing(everything(), missing_text = "") %>% 
    fmt_percent(
      columns = matches("(qtr|cumul|tar|ach)"), 
      rows = indicator %in% pct_list,
      decimals = 0
    ) %>% 
    fmt_percent(
      columns = matches("ach"), 
      decimals = 0
    ) %>% 
    fmt_percent(
      columns = matches("qtr|cumul|tar"),
      rows = indicator %in% c("HTS_POSITIVITY", "HTS_POSITIVITY_PEDS", 
                               "HTS_INDEX_POSITIVITY_PEDS", "PMTCT_EID_POSITIVITY",
                               "TX_TB_PERCENT"),
      decimals = 2
    ) %>%
    fmt_number(
      columns = matches("(qtr|cumul|tar)"), 
      rows = !indicator %in% pct_list,
      decimals = 0
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "lighter")
      ),
      locations = cells_body(
        columns = everything(),
      )
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "normal")
      ),
      locations = cells_body(
        columns = "ach",
      )
    ) %>% 
    cols_label(
      indicator = "",
      ach = "achievement"
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = trolley_grey_light, alpha = 0.5)
      ),
      locations = cells_body(
        rows = indicator %in% pct_list,
      )
    ) %>% 
     tab_header(
       title = gt::html("<span style='color:#58595b;font-weight:normal'>FY21 QUARTERLY PERFORMACE</span>")
     ) 
  }

 qtr_tbl("TESTING")


 # Iterate and save 
 cat_list <- fy21q2 %>% distinct(category) %>% pull()
 map(cat_list, ~qtr_tbl(.x) %>% gtsave(file.path(images, paste(.x, "QC_FY21Q2.png"))))
 
write_csv(fy21q2, file.path(dataout, "FY21Q2_QC_data.csv"))
 

 
 