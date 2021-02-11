# Purpose: Conduct a DQA on select indicator
# Author: Tim Essam | SI, 
# Date: 2021-02-10
# Notes:

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
    library(readxl)
    
  source("Scripts/Z00_Config.R")
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
    dqa_in <- "Data/DQA"
    
  # Functions  
    get_sites <- function(df) {
        df %>% 
        filter(facility %in% fac_list) %>% 
        left_join(., sites)
    }

    compare_lists <- function(a, b) {
      
      # Purpose: check two character objects, report intersection
      
      # Check if a and b are objects
      stopifnot(is.character(a) & is.character(b))
      
        get_diff <- function(x, y) {
          length(x) - length(y)
        }
        
      # Fetch Differences  
      diff_ab <- setdiff(a, b)
      diff_ba <- setdiff(b,a)
      overlap <- intersect(a, b)
      pct_overlap <- paste0("(", scales::percent(length(overlap)/length(a)), ")")
      
      # Output
      cat("Items", pct_overlap, "appearing in both objects are:\n")
      overlap %>%  sort() %>%  crayon::yellow() %>% writeLines 
      
      cat("\nNumber of item(s) from first list not contained in second:", get_diff(a, overlap), "\n")
      diff_ab %>% sort() %>% crayon::red() %>% writeLines
      
      cat("\nNumber of items(s) from second list not contained in first:", get_diff(b, overlap), "\n")
      diff_ba %>% sort() %>%  crayon::red() %>% writeLines
      
    }  
  
  
  # Indicators of interest
    indic_list <- c("TX_CURR", "TX_PVLS", "TB_PREV", "CXCA_SCRN", 
                   "PrEP_NEW", "PrEP_CURR", "VMMC_CIRC")

# LOAD DATA ============================================================================  
  # Show data
    dir(dqa_in)
    
    file_in <- file.path(dqa_in, "SAFE SmartCare vs. DATIM Data Comparison 1-21-2021.xlsx")
    
    excel_sheets(here(dqa_in, "SAFE SmartCare vs. DATIM Data Comparison 1-21-2021.xlsx")) 
    
    keep_list <- c("6MMD", "CXCA Data", "TB_PREV Data", "TX Data")
    
    safe <- 
      excel_sheets(file_in) %>% 
      set_names() %>% 
      map(., .f = ~read_excel(file_in, sheet = .x, skip = 2)) %>% 
      .[keep_list]
    
  # Glimpse the results  
  map(safe, ~glimpse(.))
    

# MUNGE ============================================================================
  
  # We'll start with treatment data and look back to Q1
  # First, load MER data and extract the 10 sites
    file_msd <- return_latest(folderpath = datim, 
                              pattern = "MER_S.*_Site_IM_FY18-21_\\d{8}_v.*Zambia.*.zip")
    
    
    msd <- 
      read_msd(file_msd) %>%
      filter(fundingagency == "USAID",
                   mech_name == "SAFE", 
             indicator %in% indic_list)
    
   fac_list <- c("Chimwemwe Urban Health Centre", "Ndeke (Kitwe) Urban Health Centre", 
                 "Chipokota Mayamba Urban Health Centre", 
                 "Lubuto Urban Health Centre", "Masala New Urban Health Centre", 
                 "Mumbwa District Hospital", 
                 "Kapiri Urban Health Centre", 
                 "Mahatma Ghandhi Memorial Urban Health Centre", 
                 "Chibefwe Rural Health Centre", "Solwezi Urban Health Centre")
   
   msd %>% distinct(facility) %>% filter(facility %in% fac_list)

   sites <- msd %>% 
     distinct(facility, orgunituid, psnu, snu1, facilityuid) %>% 
     filter(facility %in% fac_list) %>% 
     mutate(sort_excel = case_when(
       str_detect(facility, "Chimwemwe") ~ 1,
       str_detect(facility, "Ndeke") ~ 2,
       str_detect(facility, "Chipokota") ~ 3,
       str_detect(facility, "Lubuto") ~ 4,
       str_detect(facility, "Masala") ~ 5,
       str_detect(facility, "Mumbwa") ~ 6,
       str_detect(facility, "Kapiri") ~ 7,
       str_detect(facility, "Mahatma") ~ 8,
       str_detect(facility, "Chibefwe") ~ 9,
       TRUE ~ 10
     )) %>% 
     arrange(sort_excel)
   sites
   
   sites_df <- 
     msd %>% 
     filter(orgunituid %in% sites$orgunituid, 
            fiscal_year == "2020") %>% 
     reshape_msd(clean = T)

   sites_df %>% count(indicator, standardizeddisaggregate, fiscal_year) %>% prinf()
   
   
  # Munge SMART CARE data extract for each indicator. Want to rectangularlize it and get into a single df
  # So we have TX_CURR & TX_PVLS N | D for periods 1-4. Entire DB has been reshaped, filter below for reproducing DQA  
   safe %>% names()
   
  # This should return 10 facilities 
   safe$`TX Data` %>% 
     distinct(Facility, `Facility Name`) %>% 
     filter(Facility %in% fac_list)
   
   
   tx_smc <- 
     safe$`TX Data` %>% 
     #filter(Facility %in% fac_list) %>% 
     pivot_longer(cols = -c(Province, District, Facility, `Facility Name`),
                  names_to = c("fiscal_year", "period", "indicator", "sex", "age", "type"),
                  #names_sep = " ",
                  names_pattern ="([[:digit:]]{4}) (Q{1}\\w{1}) (TX_CURR|TX_PVLS) (Males|Females) (<15|15\\+)  (Numerator|Denominator)",
                  values_to = "value") %>% 
     mutate(indic_freq = if_else(indicator == "TX_PVLS", "semi-annual", "quarterly"),
            period = paste0("FY20", period)) %>% 
     rename(snu1 = Province,
            psnu = District,
            facility = Facility,
            facility_name = `Facility Name`) 
     
     
  # Merge with the original sites 
   tx_smc_fltr <- get_sites(tx_smc)
   
 
### -- MMD ---
# 6 MO MMD 
   # MMD --
    safe$`6MMD` %>% names()
    safe$`6MMD` %>% 
      distinct(orgunitlevel4, organisationunitname) %>% 
      filter(orgunitlevel4 %in% fac_list)
    
   tx_mmd_smc <-  safe$`6MMD` %>% 
      pivot_longer(cols = -c(orgunitlevel1, orgunitlevel2, orgunitlevel3, orgunitlevel4, organisationunitname, dataname),
                   names_to = c("months", "fiscal_year", "sex", "age"),
                   names_pattern = "(.{10}) ([[:digit:]]{4}) (Males|Females) (<15|15\\+|Unknown)",
                   values_to = "value",
                   values_drop_na = T) %>% 
     mutate(period = case_when(
       months == "Apr to Jun" ~ "FY20Q3",
       TRUE ~ "FY20Q4"
     )) %>% 
     rename(operatingunit = orgunitlevel1, 
            snu1 = orgunitlevel2,
            psnu = orgunitlevel3,
            facility = orgunitlevel4,
            facility_name = organisationunitname) %>% 
     mutate(indicator = "TX_CURR_MDD")
                   

   tx_mmd_smc_fltr <- get_sites(tx_mmd_smc)
   
    
### -- CXCA -- 
  names(safe)
  cxca_smc <- 
    safe$`CXCA Data` %>%
    pivot_longer(cols = `Apr to Jun 2020`:`Jul to Sep 2020`,
                 names_to = c("months", "fiscal_year"),
                 names_pattern = "(.{10}) ([[:digit:]]{4})",
                 values_to = "value") %>% 
    mutate(period = case_when(
      months == "Apr to Jun" ~ "FY20Q3",
      TRUE ~ "FY20Q4"
    )) %>% 
    rename(operatingunit = OU, 
           snu1 = Province,
           psnu = District,
           facility = Facility,
           facility_name = `Facility Name`,
           type = `N/D`, 
           indicator = Indicator)
  
  cxca_smc_fltr <- get_sites(cxca_smc)
  
### --- PREV ---
    safe$`TB_PREV Data` %>% names()
    prev_smc <- 
      safe$`TB_PREV Data` %>%
      pivot_longer(cols = -c(Indicator, OU, Province, District, Facility, `Facility Name`),
                   names_to = c("months", "fiscal_year", "type"),
                   names_pattern = "(.{10}) ([[:digit:]]{4}) (Denominator|Numerator)",
                   values_to = "value") %>% 
      mutate(period = case_when(
        months == "Apr to Jun" ~ "FY20Q3",
        TRUE ~ "FY20Q4"
      )) %>% 
      rename(operatinguint = OU, 
             snu1 = Province,
             psnu = District,
             facility = Facility,
             facility_name = `Facility Name`,
             indicator = Indicator)
     
    prev_smc_fltr <- get_sites(prev_smc)
  
  # Loop through filtered data frames and check disag pattern
    dqa_df_full <- 
      list(tx_smc, tx_mmd_smc, cxca_smc, prev_smc) %>% 
      bind_rows %>% 
    mutate(val_smc = value) %>% 
      select(-value)
    
    dqa_df_fltr <-
      list(tx_smc_fltr, tx_mmd_smc_fltr, cxca_smc_fltr, prev_smc_fltr) %>% 
      bind_rows() %>% 
      mutate(val_smc = value) %>% 
      select(-value)

 
 dqa_df_fltr %>% count(indicator, period, sex, age, type, ) %>% 
   arrange(indicator) %>% spread(indicator, n) %>%
   prinf()
 
 # Notes
 # PrEP, PREV and CXCA variables are numerator and denominator but not sex disaggs
 # These are also semi-annual indicators so they are only available for Q4 comparison
  
 # TX_PVLS has both numerator and denominator requirements plus age/sex disaggs
 
 # Let's start with one indicator and replicate the analysis for Q4
  dqa_df_fltr %>% 
    filter(indicator == "TB_PREV" & period == "FY20Q4")
  
 # Fetch DATIM equivalent vars -- need numerator/denom disaggs
  sites_df %>% 
    filter(indicator == "TB_PREV",
           period == "FY20Q4",
           str_detect(standardizeddisaggregate, ("Denom|Numer"))) %>% 
    select(sitename, orgunituid, indicator, period, val, type = standardizeddisaggregate) %>% 
    mutate(type = str_remove_all(type, "Total ")) %>% 
    left_join(dqa_df_fltr %>% 
                filter(indicator == "TB_PREV" & period == "FY20Q4"), by = c("orgunituid", "type", "period")) %>% 
    relocate(snu1, psnu, sitename, facility, period, val, val_smc, indicator.x, indicator.y, type) %>% 
    arrange(type, sort_excel)

  
   
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================


  