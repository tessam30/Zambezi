# Purpose: Verify FY21 Q1 performance and targets -- TX_PVLS
# Author: Tim Essam | SI
# Date: 2021-02-2
# Notes: COP Work


# GLOBALS -----------------------------------------------------------------

  source("Scripts/Z00_Config.R")
  
  library(ICPIutilities)
  library(glitr)
  library(glamr)
  library(gisr)
  library(tidyverse)
  library(scales)
  library(tidytext)
  library(extrafont)
  library(gt)
  library(googlesheets4)
  library(readxl)
  
  # Latest msd
  file_msd <- return_latest(folderpath = merdata,
                            pattern = "PSNU_IM_FY19-21_20210319_v2_1_Zambia")

# READ and MUNGE ----------------------------------------------------------

  df <- read_msd(file_msd)
  df %>% count(standardizeddisaggregate) %>% prinf()

  df_vl <- 
    df %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
           mech_name != "USAID Health Access for All project"
    ) %>% 
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
    group_by(operatingunit, fiscal_year, indicator, mech_code, mech_name) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    select(-period_type) %>% 
    spread(indicator, value) %>% 
    mutate(mech_name = case_when(
      mech_name == "USAID/District Coverage of Health Services (DISCOVER-H)" ~ "DISCOVER-H",
      TRUE ~ mech_name
    ))
  
  
  # Munge and create lags
  df_vl <- 
    df_vl %>%
    group_by(mech_name) %>%
    mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
           mech_lab = paste0(mech_name, " (", lag(TX_CURR, 2, order_by = period) %>% comma(1), ")")) %>%
    ungroup() %>%
    mutate(VLS = (TX_PVLS/TX_PVLS_D)*VLC,
           notcovered = 1 - VLC) 
  
  
  df_vl %>%
    filter(!period %in% c("FY19Q1", "FY19Q2")) %>% 
    #ggplot(aes(y = fct_reorder(mech_lab, VLC, .desc = TRUE))) +
    ggplot(aes(y = factor(period))) +
    geom_col(aes(x = 1), fill = "gray90") +
    geom_col(aes(x = VLC), fill = genoa_light) +
    geom_col(aes(x = VLS), fill = genoa, alpha = 0.85) +
    geom_text(aes(x = .96, label = percent(notcovered, 1), 
                  color = ifelse(notcovered >=.39, old_rose, "gray30")), 
              size = 3.5, family = "Source Sans Pro") +
    #geom_col(data = . %>% filter(period != "FY21Q1"), aes(x = 1), fill = "white", alpha = 0.25) +
    geom_vline(xintercept = c(.25, .5, .75), linetype = "dashed", color = "gray90") +
    scale_x_continuous(label = percent,expand = c(0.005, 0.005), position = "bottom") +
    scale_color_identity() +
    labs(x = NULL, y = NULL,
         title = "DISCOVER-H HAD THE LARGEST DROP IN COVERAGE/SUPPRESSION IN FY21Q1",
         # subtitle = "USAID | FY20Q3",
         caption = "VLC = TX_PVLS / TX_CURR (2 periods prior); VLS = TX_PVLS / TX_PVLS_D * VLC
       USAID only
       Source: FY21Q1pc MSD") +
    si_style_nolines() +
    theme(axis.text.y = element_text(size = 10)) +
    facet_wrap(~mech_name, scales= "free_y") +
    coord_cartesian(expand = T)
  
  si_save("Images/FY21Q1_Zambia_mech_VLC.png", scale = 1.25)
  
  # Now a table of the raw numbers to make sure things are good

  df_vl_wide <- 
    df_vl %>% 
      mutate(VLS_of_VLC = (TX_PVLS / TX_PVLS_D)) %>% 
    select(period, mech_name, TX_CURR, TX_PVLS, TX_PVLS_D, VLC, VLS, VLS_of_VLC, notcovered) %>%
    pivot_longer(cols = -(1:2),
                 names_to = "indicators",
                 values_to = "value")%>% 
    pivot_wider(names_from = period,
                values_from = value) %>% 
    mutate(indicators = if_else(indicators == "notcovered", "not covered", indicators),
           `Q1 delta` = `FY21Q1` - `FY20Q4`)
  
  # Make a table of the results too
   vl_tbl <- 
     df_vl_wide %>% 
      gt(
      rowname_col = "indicators",
      groupname_col = "mech_name"
    ) %>% 
    fmt_number(columns = 3:12,
      rows = indicators %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
               decimals = 0) %>% 
    fmt_percent(columns = 3:12,
               rows = !indicators %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
               decimals = 0) %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    tab_source_note(source_note = "Source: FY21Q1 pc MSD") %>% 
    tab_source_note(source_note = "VLC = TX_PVLS / TX_CURR (2 periods prior); VLS = TX_PVLS / TX_PVLS_D * VLC; USAID only") %>%  
      tab_options(footnotes.font.size = 8) %>% 
     tab_header("DISCOVER-H DISCOVER-H HAD THE LARGEST DROP IN COVERAGE/SUPPRESSION IN FY21Q1")  %>% 
     tab_style(
       style = cell_fill(color = old_rose, alpha = 0.25),
       locations = cells_body(
         columns = vars(`Q1 delta`),
         rows = `Q1 delta` < 0.21 & !indicators %in% c("not covered", "VLS_of_VLC")
       )
     )
    
    gtsave(vl_tbl, file.path("Images/ZMB_Mech_PVLS_table.png"))
      
 
    