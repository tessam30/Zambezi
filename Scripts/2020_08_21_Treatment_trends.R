# Purpose: Munge Genie Extract to look at TX_CURR and TX_NET_NEW trends
# Author: Tim Essam | SI Asc
# Date: 2020_06_10


# Data Notes:
# Site By IM Extract
# DATIM data as of: 06/08/2020 11:03:22 UTC
# Genie report updated: 06/10/2020 10:52:19 UTC
# Current period(s): 2019 Target,  2019 Q1,  2019 Q2,  2019 Q3,  2019 Q4,  2020 Target,  2020 Q1,  2020 Q2
# Operating Unit: Global >Africa >Zambia >
# Indicator: HTS_TST ,HTS_TST_POS ,PrEP_CURR ,PrEP_NEW ,TX_CURR ,TX_NET_NEW ,TX_NEW ,TX_PVLS

# PRELIMS -----------------------------------------------------------------

  library(glitr)
  library(glamr)
  library(tidyverse)
  library(scales)
  library(tidytext)
  library(here)
  library(ICPIutilities)
  library(ggrepel)
  library(patchwork)
  library(extrafont)

# GLOBALS -----------------------------------------------------------------

  data <- "Data"
  data_out <- "Dataout"
  images <- "Images"
  mer_in <- "../../DATIM_Data"


# LOAD and MUNGE ----------------------------------------------------------

  unzip(here(mer_in, "Genie-SiteByIMs-Zambia-Daily-2020-08-21.zip"), exdir = mer_in)
  
  df <- vroom::vroom(here(mer_in, "Genie_SITE_IM_Zambia_Daily_f90e06aa-b294-44b2-a8a1-f1cdeed2429d.txt")) %>% 
    filter(disaggregate == "Total Numerator")
  
  # MSD data
  unzip(file.path(mer_in, "MER_Structured_Datasets_Site_IM_FY18-20_20200605_v1_1_Zambia.zip"), exdir = mer_in)
  
  df_zmb <- read_msd(file.path(mer_in, "MER_Structured_Datasets_Site_IM_FY18-20_20200605_v1_1_Zambia.txt")) 
  
  df_zmb_tn <- df_zmb %>% 
    filter(fundingagency == "USAID", disaggregate == "Total Numerator") %>% 
    reshape_msd()
  # CHANGED LOAD PROCESS


# MUNGE GENIE -------------------------------------------------------------

  df_long <- df %>%  
    reshape_msd() %>% 
    mutate(fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency)) %>% 
    mutate(fy = substr(period, 3, 6))
  
  
  # Reproduce list of indicators for table, first by agency
  df_long %>% 
    filter(str_detect(period, "targets|cumulative")) %>% 
    group_by(fundingagency, indicator, period) %>% 
    summarise(value = sum(val, na.rm = TRUE)) %>% 
    spread(period, value) %>% prinf()
  
  df_long %>% 
    filter(fundingagency == "USAID", str_detect(period, "targets|cumulative")) %>% 
    group_by(indicator, period) %>% 
    summarise(value = sum(val, na.rm = TRUE)) %>% 
    spread(period, value)
  
  
  # Reproduce list of indicators for table, first by agency
  df_long %>% 
    filter(fundingagency == "USAID") %>% 
    filter(str_detect(period, "targets|cumulative", negate = TRUE)) %>% 
    group_by(fundingagency, indicator, period) %>% 
    summarise(value = sum(val, na.rm = TRUE)) %>% 
    group_by(indicator) %>% 
    mutate(change = value - lag(value)) %>% 
    prinf()
  
  
  # pull targets
  tgts <- 
    df_long %>% filter(fundingagency != "Dedpu", indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW", "HTS_TST", "HTS_TST_POS", "HTS_SELF"), str_detect(period, "target")) %>% 
    group_by(fundingagency, indicator, fy) %>% 
    summarise(target = sum(val, na.rm = TRUE))

  # OVerall growth by OU
  df_long %>% filter(str_detect(period, "cumu"), indicator == "TX_CURR") %>% 
    group_by(period) %>% 
    summarise(value = sum(val, na.rm = TRUE)) %>% 
    mutate(growth = (value/lag(value)) -1)
  
  
  # Percent acheivement
  df_long %>% 
    filter(fundingagency != "Dedup", indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW")) %>% 
    filter(str_detect(period, "targ|cum", negate = FALSE), fy == "2020") %>% 
    group_by(fundingagency, indicator, period, fy) %>% 
    summarise(value = sum(val, na.rm = TRUE)) %>% 
    spread(period, value) %>% 
    mutate(ach = fy2020cumulative / fy2020_targets)
    
  
  
# What does overall treatment growth look like for the agencies?
  tx_curr <- 
    df_long %>% 
    filter(fundingagency != "Dedup", indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW")) %>% 
    filter(str_detect(period, "targ|cum", negate = TRUE)) %>% 
    group_by(fundingagency, indicator, period, fy) %>% 
    summarise(value = sum(val, na.rm = TRUE)) %>% 
    group_by(fundingagency) %>% 
    mutate(percent_change = (value/lag(value)) -1)%>% 
    ungroup() %>% 
    mutate(quarter_fill = if_else(str_detect(period, "q3"), grey60k, grey20k),
           agency_order = fct_reorder(fundingagency, value, .desc = TRUE),
           pt_start = if_else(period == "fy2019q3", value, NA_real_),
           pt_end = if_else(period == "fy2020q3", value, NA_real_)) %>% 
    left_join(., tgts)
  
  hts <- 
    df_long %>% 
    filter(fundingagency != "Dedup", indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_SELF")) %>% 
    filter(str_detect(period, "targ|cum", negate = TRUE)) %>% 
    group_by(fundingagency, indicator, period, fy) %>% 
    summarise(value = sum(val, na.rm = TRUE)) %>% 
    group_by(fundingagency) %>% 
    mutate(percent_change = (value/lag(value)) -1)%>% 
    ungroup() %>% 
    mutate(quarter_fill = if_else(str_detect(period, "q3"), grey60k, grey20k),
           agency_order = fct_reorder(fundingagency, value, .desc = TRUE),
           pt_start = if_else(period == "fy2019q3", value, NA_real_),
           pt_end = if_else(period == "fy2020q3", value, NA_real_)) %>% 
    left_join(., tgts)
    

  #Focus on TX_CURR an TX_NET_NEW  
  
   tx_change <- 
     tx_curr %>% filter(indicator == "TX_CURR") %>% 
    ggplot(aes(y = percent_change, x = period)) +
    geom_col(aes(fill = quarter_fill)) + facet_wrap(~agency_order) +
    si_style_ygrid() +
      scale_fill_identity() +
    scale_y_continuous(labels = percent_format()) +
    labs(x = NULL, y = NULL, 
         subtitle = "GROWTH IN TX_CURR BY AGENCY\n",
         caption = "Source: DATIM Genie pull as of 8/21/2020") +
     theme(strip.text.x = element_blank())
    
    tx_level <- 
      tx_curr %>% filter(indicator == "TX_CURR") %>%
      ggplot(aes(y = value, x = period, group = fundingagency)) +
      geom_area(aes(y = target), fill = grey10k, alpha = 0.55) +
      geom_line(aes(y = target), colour = grey30k, linetype = "dotted") +
      geom_area(fill = grey20k, alpha = 0.80) +
      geom_line() + facet_wrap(~agency_order) +
      geom_point(aes(y = value, fill = value), shape = 21, size = 5, na.rm = TRUE, colour = grey90k, stroke = 1) +
      geom_text_repel(aes(y = value, label = comma(value)), segment.colour = NA,
                      family = "Source Sans Pro Light", vjust = 2, force = 10) +
      si_style_ygrid() +
      theme(legend.position = "none",
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      scale_y_continuous(labels = comma_format()) +
      scale_fill_viridis_c(option = "A", direction = -1, begin = .2) +
      labs(x = NULL, y = NULL, 
           title = "TX_CURR LEVELS BY AGENCY AND QUARTER\n")
  
   # Combine graphs 
   tx_summary <-  tx_level / tx_change
    ggsave(here(images, paste0("ZMB_TX_CURR_Summary_", Sys.Date())), plot = tx_summary, 
            scale = 1.4, device = "pdf", useDingbats = FALSE)
  
    si_save(here(images, paste0("ZMB_TX_CURR_Summary_", Sys.Date(), ".png")), plot = tx_summary, 
           scale = 1.4)
    
    
    
    tx_nn_trends <- 
      tx_curr %>% 
        filter(indicator == "TX_NET_NEW") %>%
      ggplot(aes(y = value, x = period, group = fundingagency)) +
      geom_col(aes(fill = if_else(value > 0, "#8ba68a", "#a68a8b"))) + facet_wrap(~agency_order) +
      geom_text_repel(aes(y = value, label = comma(value)), segment.colour = NA,
                      family = "Source Sans Pro Light", vjust = 2, force = 10) +
      si_style_ygrid() +
      theme(legend.position = "none",
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      scale_fill_identity() +
      scale_y_continuous(labels = comma_format()) +
      labs(x = NULL, y = NULL, 
           title = "TX_NET_NEW LEVELS BY AGENCY AND QUARTER\n")

    ggsave(here(images, paste0("ZMB_TX_NET_NEW_Summary_", Sys.Date())), plot = tx_nn_trends,
            scale = 1.25, device = "pdf", useDingbats = FALSE) 
    
    si_save(here(images, paste0("ZMB_TX_NET_NEW_Summary_", Sys.Date(), ".png")), plot = tx_nn_trends,
            scale = 1.25)
  
    
    tx_new <- 
      tx_curr %>% 
      filter(indicator == "TX_NEW") %>%
      ggplot(aes(y = value, x = period, group = fundingagency)) +
      geom_col(aes(fill = if_else(value > 0, "#8ba68a", "#a68a8b"))) + facet_wrap(~agency_order) +
      geom_text_repel(aes(y = value, label = comma(value)), segment.colour = NA,
                      family = "Source Sans Pro Light", vjust = 2, force = 10) +
      si_style_ygrid() +
      theme(legend.position = "none",
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      scale_fill_identity() +
      scale_y_continuous(labels = comma_format()) +
      labs(x = NULL, y = NULL, 
           title = "TX_NEW LEVELS BY AGENCY AND QUARTER\n")
    
    

# Testing -----------------------------------------------------------------

  hts %>% 
      filter(indicator == "HTS_TST") %>% 
      ggplot(aes(y = percent_change, x = period)) +
      geom_col(aes(fill = quarter_fill)) + facet_wrap(~agency_order) +
      si_style_ygrid() +
      scale_fill_identity() +
      scale_y_continuous(labels = percent_format()) +
      labs(x = NULL, y = NULL, 
           subtitle = "GROWTH IN HTS_TST BY AGENCY\n",
           caption = "Source: DATIM Genie pull as of 8/21/2020") +
      theme(strip.text.x = element_blank())
    
    hts %>% 
      filter(indicator == "HTS_TST_POS", fundingagency != "State/AF") %>%
      group_by(fundingagency, indicator) %>% 
      mutate(cumulative = cumsum(value)) %>% 
      ungroup() %>% 
      ggplot(aes(y = value, x = period, group = fundingagency)) +
      geom_area(fill = grey20k, alpha = 0.80) +
      geom_line() + facet_wrap(~agency_order) +
      geom_point(aes(y = value, fill = value), shape = 21, size = 5, na.rm = TRUE, colour = grey90k, stroke = 1) +
      geom_text_repel(aes(y = value, label = comma(value)), segment.colour = NA,
                      family = "Source Sans Pro Light", vjust = 2, force = 10) +
      si_style_ygrid() +
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_y_continuous(labels = comma_format()) +
      scale_fill_viridis_c(option = "A", direction = -1, begin = .2) +
      labs(x = NULL, y = NULL, 
           title = "TESTING LEVELS BY AGENCY AND QUARTER\n")

# MAPS from MSD -----------------------------------------------------------


      
    
    
    