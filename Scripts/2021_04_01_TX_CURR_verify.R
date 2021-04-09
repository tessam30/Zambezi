# PURPOSE: TX_CURR verification post-clean MER dta
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-04-01
# NOTES: 

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
    library(here)
    
    
  
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
  

# LOAD DATA ============================================================================  
  
  msd_file <- return_latest(merdata, pattern = "PSNU_IM_FY19-21_20210319")
    
  msd <- read_msd(msd_file) %>% 
    filter(indicator == "TX_CURR",
    standardizeddisaggregate == "Total Numerator")
    
  
# MUNGE ============================================================================
  
  # Previous request to munge Central Province data to determine the USAID ##s for FY21 Q1
  msd %>% 
    filter(snu1 == "Central Province", fundingagency == "HHS/CDC") %>% 
    group_by(fundingagency, mech_name, mech_code, fiscal_year, psnu) %>% 
    summarise(tx_curr = sum(qtr1, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(tx_curr_overall = sum(tx_curr))
  
  msd %>% group_by(fiscal_year, fundingagency, snu1) %>% 
    summarise(tx_curr = sum(qtr1, na.rm = T)) %>% prinf()
  
  
  # Pull out TX_CURR results 
  msd_long <- 
    msd %>% 
    reshape_msd(clean = T) %>% 
    filter(period_type == "results") %>% 
    group_by(snu1, psnu, mech_code, mech_name, period, period_type, fundingagency) %>% 
    summarise(tx_curr = sum(value, na.rm = T)) %>% 
    ungroup() 
  
  msd_long %>% filter(period == "FY21Q1") %>% 
    count(snu1, fundingagency) %>% spread(fundingagency, n) %>% 
    arrange(USAID)
  
  msd_long %>% 
    group_by(period) %>% 
    summarise(tx_curr = sum(tx_curr))
  
  

# MUNGE and PROJECT TX_NN to TARGETS --------------------------------------

  
  
  msd_long <- 
    msd %>% 
    filter(fundingagency == "USAID") %>% 
    group_by(psnu, psnuuid, snu1, fundingagency, fiscal_year) %>% 
    summarise(across(.cols = matches("qtr|target"), sum, na.rm = T)) %>% 
    reshape_msd(clean = T) %>% 
    spread(period_type, val) %>% 
    mutate(period2 = period) %>% 
    separate(period2, c("FY", "qtr"), sep = "Q") %>% 
    group_by(psnu, FY) %>% 
    fill(targets, .direction = "down") %>% 
    filter(!period %in% c("FY20", "FY21", "FY19")) %>% 
    rename(tx_curr = results) %>% 
    ungroup() %>% 
    group_by(psnu) %>% 
    mutate(pd_flag = ifelse(qtr %in% c(4, 1), 1, NA_integer_),
           target_nn = ifelse(pd_flag == 1, targets - lag(tx_curr, order_by = period), NA_integer_)) %>% 
    group_by(psnu, FY) %>% 
    fill(target_nn, .direction = "up") %>% 
    group_by(period, snu1) %>% 
    mutate(tx_curr_snu1 = sum(tx_curr)) %>% 
    arrange(psnu, period) %>% 
    group_by(psnu) %>% 
    mutate(tx_curr_lag = lag(tx_curr, order_by = period),
           tx_nn = tx_curr - tx_curr_lag,
           tx_nn_growth = (tx_curr - tx_curr_lag)/tx_curr_lag) %>% 
    ungroup() %>% 
    relocate(tx_curr_snu1, .after = last_col()) %>% 
    group_by(snu1, period) %>% 
    mutate(tx_curr_lag_snu1 = sum(tx_curr_lag, na.rm = T),
           tx_nn_snu1 = ifelse(tx_curr_lag_snu1 != 0, tx_curr_snu1 - tx_curr_lag_snu1, NA_integer_)) %>% 
    ungroup() %>% 
    group_by(psnu) %>% 
    mutate(psnu_runs = n()) %>% 
    ungroup() %>% 
    mutate(tx_nn_growth_snu1 = ifelse(tx_curr_lag_snu1 != 0, ((tx_curr_snu1 - tx_curr_lag_snu1) / tx_curr_lag_snu1), NA_real_),
           tx_nn_share = tx_nn / tx_nn_snu1,
           tx_nn_share_psnu_contrib = tx_nn_share * tx_nn_growth_snu1) %>% 
    group_by(snu1, period) %>% 
    mutate(share_check = sum(tx_nn_share, na.rm = T)) %>% 
    arrange(snu1, period) %>% 
    ungroup() %>% 
    clean_psnu()
  
 write_csv(msd_long, file.path(dataout, "TX_NN_analysis.csv"))
  
# VIZ ============================================================================

  #  

  msd_long %>% 
    filter(snu1 == "Muchinga Province", 
           str_detect(period, "FY20|FY21")) %>% 
    mutate(psnu_order = fct_reorder(psnu, tx_nn, .fun = sum, na.rm = T, .desc = T)) %>% 
    ggplot(aes(x = period, y = tx_nn_share_psnu_contrib , group = psnu)) +
    geom_area(fill = trolley_grey_light) +
    geom_line() +
    geom_point(aes(fill = ifelse(tx_nn_share_psnu_contrib  > 0, genoa, old_rose)), shape = 21, size = 3, color = "white") +
    #geom_label(aes(label = paste0(comma(tx_nn, 1), "\n", percent(tx_nn_share_psnu_contrib, 1)), fill = ifelse(tx_nn_share_psnu_contrib  > 0, genoa_light, old_rose_light)), family = "Source Sans Pro") +
    facet_wrap(~psnu_order) +
    scale_fill_identity() +
    scale_y_continuous(labels = percent) +
    si_style_ygrid() +
    coord_cartesian(expand = T, clip = "off") +
    labs(x = NULL, y = NULL, title = "TX_NET_NEW share by PSNU", subtitle = "TX_NN listed as top number, TX_NN district contribution to province ")
    
  
  plot_tx_nn <- function(prov = "Muchinga Province") {
  
    msd_long %>% 
    filter(snu1 == prov, 
           str_detect(period, "FY20|FY21")) %>% 
    mutate(psnu_order = fct_reorder(psnu, tx_nn, .fun = sum, .desc = T)) %>% 
    ggplot(aes(x = period, y = tx_nn_share , group = psnu)) +
    geom_area(fill = trolley_grey_light) +
    #geom_line(aes(y = tx_nn_growth_snu1), color = grey40k, linetype = "dotted") +
    geom_line() + 
    geom_point(aes(fill = ifelse(tx_nn_share  > 0, genoa, old_rose)), shape = 21, size = 3, color = "white") +
    geom_text(aes(label = paste0(comma(tx_nn, 1))),
               family = "Source Sans Pro", 
               vjust = -1) +
    facet_wrap(~psnu_order) +
    scale_fill_identity() +
    scale_y_continuous(labels = percent) +
    si_style_ygrid() +
    coord_cartesian(expand = T, clip = "off") +
    labs(x = NULL, y = NULL, title = "TX_NET_NEW share by PSNU", subtitle = "TX_NN annotated on point. Y-axis reflects psnu share of total TX_NN for province, by period.",
         caption = "Source: FY21 Q1 MSD Post-Clean")  
    
    si_save(file.path(images, glue::glue("{prov}_TX_NN_growth.png")), scale = 1.5, plot = last_plot())
    
  }
      
  msd_long %>% distinct(snu1) %>%  
    filter(str_detect(snu1, "Southern|Western", negate = T)) %>% 
    pull() %>% 
    map(.x = ., .f = ~plot_tx_nn(.x))
    
# SPINDOWN ============================================================================

