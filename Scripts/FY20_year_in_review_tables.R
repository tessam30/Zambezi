# Purpose: Generate summary table for Zambia Agencies for FY20Q4
# Author: Aaron Chafetz, modified by Tim Essam | SI
# Date: 2020-11-18
# Notes: Based on Genie

# DATIM data as of: 11/13/2020 22:08:05 UTC
# Genie report updated: 11/15/2020 06:03:36 UTC
# Current period(s): 2019 Target,  2019 Q1,  2019 Q2,  2019 Q3,  2019 Q4,  2020 Target,  2020 Q1,  2020 Q2,  2020 Q3,  2020 Q4 


# GLOBALS -----------------------------------------------------------------

  library(tidyverse)
  library(gt)
  library(glitr)
  library(glamr)
  library(gisr)
  library(here)
  library(ICPIutilities)
  library(glue)

  datain <- "Data"
  dataout <- "Dataout"
  
  datim_path <- ("../../DATIM_DATA/")
  agencies <- c("CDC", "DOD", "USAID")
  
  pal <- c(burnt_sienna, golden_sand, genoa, scooter)
  pal <- RColorBrewer::brewer.pal(5, "Spectral")[2:5]

# LOAD AND MUNGE ----------------------------------------------------------

  # Add in 2018 data for net net calculations
  df_old <- list.files(datim_path, "MER_Structured_Datasets_OU_IM_FY18-21_20200918", full.names =  TRUE) %>% 
    read_rds()
  
  df <- read_msd(here(datain, "Genie_OU_IM_Zambia_Daily_2020_11_19.txt"), remove_txt = FALSE)
  
  # Munge 2018 data Q4 for NN adjustments
  df_zmb_old <- df_old %>% 
    filter(operatingunit == "Zambia",
           indicator == "TX_CURR",
           fundingagency != "Dedup",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == 2018) %>% 
    mutate(fundingagency = str_remove(fundingagency, "HHS/")) %>% 
    select(-c(targets:qtr3, cumulative))
  

  df_zmb <- df %>% 
    filter(operatingunit == "Zambia",
           indicator %in% c("TX_CURR", "HTS_TST", "HTS_TST_POS", "TX_NET_NEW", "TX_NEW", "VMMC_CIRC", "PrEP_NEW"),
           fundingagency != "Dedup",
           standardizeddisaggregate == "Total Numerator"
    ) %>% 
    mutate(fundingagency = str_remove(fundingagency, "HHS/"))
  
  # Bind together for NN calcs
  df_agency <- df_zmb %>% 
    bind_rows(df_zmb_old) %>% 
    group_by(fundingagency, indicator, fiscal_year) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    filter(period != "FY21Q1", 
           fundingagency %in% agencies )
  
  df_nn <- df_agency %>% 
    filter(indicator == "TX_CURR" & 
             (str_detect(period, "4") | period_type == "targets")) %>% 
    arrange(fundingagency, period) %>% 
    group_by(fundingagency) %>% 
    mutate(indicator = "TX_NET_NEW",
           val = case_when(period_type == "targets" ~ val - lag(val))) %>% 
    ungroup() %>% 
    filter(period_type == "targets",
           !is.na(val))
  
  
  df_agency <- df_agency %>%
    bind_rows(df_nn) %>% 
    filter(period != "FY18Q4") %>% 
    mutate(pd = case_when(period_type == "results" ~ str_replace(period, "Q", "\nQ"),
                          period_type == "targets" ~ paste(period, "Targets", sep = "\n"),
                          period_type == "cumulative" ~  paste(period, "Total", sep = "\n")),
           fy = str_sub(period, 3,4),
           q = case_when(period_type == "results" ~ str_sub(period, -1),
                         period_type == "cumulative" ~ "5",
                         period_type == "targets" ~ "6"),
           order = as.numeric(fy) + (as.numeric(q)/100),
           pd = fct_reorder(pd, order),
           fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")),
           indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW", "VMMC_CIRC", "PrEP_NEW"))) %>% 
    select(-c(period, period_type, fy, q, order))
  
  # Create the table
  tbl <- df_agency %>% 
    arrange(fundingagency, indicator, pd) %>% 
    pivot_wider(names_from = pd, values_from = val) %>% 
    mutate(`FY19\nAchieved` = `FY19\nTotal` / `FY19\nTargets`,
           `FY20\nAchieved` = `FY20\nTotal` / `FY20\nTargets`) %>% 
    relocate(`FY19\nAchieved`, .after = `FY19\nTargets`) %>% 
    relocate(`FY20\nAchieved`, .after = `FY20\nTargets`) %>% 
    gt(rowname_col = "indicator",
       groupname_col = "fundingagency") %>% 
    fmt_missing(columns = everything(), missing_text = "-")
  
  tbl <- tbl %>% 
    tab_header(title = "Agency Performance") %>% 
    tab_options(table.font.names = "Source Sans Pro") %>% 
    cols_label(fundingagency = "",
               indicator = "") 
    
  
  tbl <- tbl %>% 
    fmt_percent(vars(`FY19\nAchieved`, `FY20\nAchieved`), decimals = 0) %>% 
    fmt_number(vars(`FY19\nQ1`, `FY19\nQ2`, `FY19\nQ3`, `FY19\nQ4`, `FY19\nTotal`, `FY19\nTargets`,
                    `FY20\nQ1`, `FY20\nQ2`, `FY20\nQ3`, `FY20\nQ4`, `FY20\nTotal`, `FY20\nTargets`,
                    `FY21\nTargets`), decimals = 0) 
  
  # Customize fill colors
  tbl <- tbl %>% 
    tab_style(
      style = cell_borders(
        sides = "right",
        weight = px(1.5),
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
        )
      ) %>% 
    tab_style(style = cell_fill(color = pal[4]),
              locations = cells_body(
                columns = vars(`FY19\nAchieved`),
                rows = `FY19\nAchieved` >= 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[3]),
              locations = cells_body(
                columns = vars(`FY19\nAchieved`),
                rows = `FY19\nAchieved` < 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[2]),
              locations = cells_body(
                columns = vars(`FY19\nAchieved`),
                rows = `FY19\nAchieved` < .9)) %>% 
    tab_style(style = cell_fill(color = pal[1]),
              locations = cells_body(
                columns = vars(`FY19\nAchieved`),
                rows = `FY19\nAchieved` < .75)) %>% 
    tab_style(style = cell_fill(color = pal[4]),
              locations = cells_body(
                columns = vars(`FY20\nAchieved`),
                rows = `FY20\nAchieved` >= 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[3]),
              locations = cells_body(
                columns = vars(`FY20\nAchieved`),
                rows = `FY20\nAchieved` < 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[2]),
              locations = cells_body(
                columns = vars(`FY20\nAchieved`),
                rows = `FY20\nAchieved` < .9)) %>% 
    tab_style(style = cell_fill(color = pal[1]),
              locations = cells_body(
                columns = vars(`FY20\nAchieved`),
                rows = `FY20\nAchieved` < .75)) 
  
  # Drop FY19 columns for a fy20 summary table.
  tbl %>% 
    cols_width(
      vars(indicator) ~ px(120),
      everything() ~ px(80))
    #  %>% 
    # cols_hide(
    #   columns = vars(`FY19\nQ1`, `FY19\nQ2`, `FY19\nQ3`, `FY19\nQ4`, `FY19\nTotal`, `FY19\nTargets`,
    #                  `FY19\nAchieved`             
    #   )
    # )
  
  
  #Filter table to just FY20 results
  
  
  
  
# Partner Performance -----------------------------------------------------

  df_ptnr <- 
    df_zmb %>% 
    filter(fundingagency == "USAID") %>% 
    bind_rows(df_zmb_old) %>% 
    rename_official() %>%
    mutate(primepartner = case_when(
      primepartner %in% c("Zambia Center for Communication Programs", "Zambia Centre For Communicati on Programmes") ~ "ZCCP",
      TRUE ~ primepartner
    )) %>% 
    group_by(primepartner, indicator, fiscal_year) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    filter(period != "FY21Q1")  
  
  
  
  df_nn_ptnr <- df_ptnr %>% 
    filter(indicator == "TX_CURR" & 
             (str_detect(period, "4") | period_type == "targets")) %>% 
    arrange(primepartner, period) %>% 
    group_by(primepartner) %>% 
    mutate(indicator = "TX_NET_NEW",
           val = case_when(period_type == "targets" ~ val - lag(val))) %>% 
    ungroup() %>% 
    filter(period_type == "targets",
           !is.na(val))

  
  df_ptnr <- df_ptnr %>%
    bind_rows(df_nn_ptnr) %>% 
    filter(period != "FY18Q4") %>% 
    mutate(pd = case_when(period_type == "results" ~ str_replace(period, "Q", "\nQ"),
                          period_type == "targets" ~ paste(period, "Targets", sep = "\n"),
                          period_type == "cumulative" ~  paste(period, "Total", sep = "\n")),
           fy = str_sub(period, 3,4),
           q = case_when(period_type == "results" ~ str_sub(period, -1),
                         period_type == "cumulative" ~ "5",
                         period_type == "targets" ~ "6"),
           order = as.numeric(fy) + (as.numeric(q)/100),
           pd = fct_reorder(pd, order),
           # fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")),
           indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW", "VMMC_CIRC"))) %>% 
    select(-c(period, period_type, fy, q, order))
  
           
  key_partners <- c("Family Health International", "John Snow, Incorporated", 
                    "JSI Research And Training Institute, INC.", "Pact, Inc.",
                    "PATH", "RIGHT TO CARE", "TBD", "ZCCP")
           
  df_ptnr_grp <- df_ptnr %>% 
    filter(primepartner %in% key_partners)         
           
           
  tbl_ptnr <- df_ptnr_grp %>% 
    arrange(primepartner, indicator, pd) %>% 
    pivot_wider(names_from = pd, values_from = val) %>% 
    mutate(`FY19\nAchieved` = `FY19\nTotal` / `FY19\nTargets`,
           `FY20\nAchieved` = `FY20\nTotal` / `FY20\nTargets`) %>% 
    relocate(`FY19\nAchieved`, .after = `FY19\nTargets`) %>% 
    relocate(`FY20\nAchieved`, .after = `FY20\nTargets`) %>% 
    gt(rowname_col = "indicator",
       groupname_col = "primepartner")
  
  tbl_ptnr <- tbl_ptnr %>% 
    tab_header(title = "USAID Partner Performance") %>% 
    tab_options(table.font.names = "Source Sans Pro") %>% 
    cols_label(primepartner = "",
               indicator = "") 
  
  tbl_ptnr <- tbl_ptnr %>% 
    fmt_percent(vars(`FY19\nAchieved`, `FY20\nAchieved`), decimals = 0) %>% 
    fmt_number(vars(`FY19\nQ1`, `FY19\nQ2`, `FY19\nQ3`, `FY19\nQ4`, `FY19\nTotal`, `FY19\nTargets`,
                    `FY20\nQ1`, `FY20\nQ2`, `FY20\nQ3`, `FY20\nQ4`, `FY20\nTotal`, `FY20\nTargets`,
                    `FY21\nTargets`), decimals = 0) 
  
  tbl_ptnr <- tbl_ptnr %>% 
    fmt_missing(columns = everything(),
                missing_text = "")
  
  tbl_ptnr <- tbl_ptnr %>% 
    tab_style(
      style = cell_borders(
        sides = "right",
        weight = px(1.5),
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )) %>% 
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(
                columns = vars(`FY19\nAchieved`),
                rows = `FY19\nAchieved` == "")) %>%
    tab_style(style = cell_fill(color = pal[4]),
              locations = cells_body(
                columns = vars(`FY19\nAchieved`),
                rows = `FY19\nAchieved` >= 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[3]),
              locations = cells_body(
                columns = vars(`FY19\nAchieved`),
                rows = `FY19\nAchieved` < 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[2]),
              locations = cells_body(
                columns = vars(`FY19\nAchieved`),
                rows = `FY19\nAchieved` < .9)) %>% 
    tab_style(style = cell_fill(color = pal[1]),
              locations = cells_body(
                columns = vars(`FY19\nAchieved`),
                rows = `FY19\nAchieved` < .75)) %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(
                columns = vars(`FY20\nAchieved`),
                rows = `FY20\nAchieved` == "")) %>%
    tab_style(style = cell_fill(color = pal[4]),
              locations = cells_body(
                columns = vars(`FY20\nAchieved`),
                rows = `FY20\nAchieved` >= 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[3]),
              locations = cells_body(
                columns = vars(`FY20\nAchieved`),
                rows = `FY20\nAchieved` < 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[2]),
              locations = cells_body(
                columns = vars(`FY20\nAchieved`),
                rows = `FY20\nAchieved` < .9)) %>% 
    tab_style(style = cell_fill(color = pal[1]),
              locations = cells_body(
                columns = vars(`FY20\nAchieved`),
                rows = `FY20\nAchieved` < .75)) 
  
  
  
  
  tbl_ptnr %>% 
    cols_width(
      vars(indicator) ~ px(120),
      everything() ~ px(80)
    )
  
  

# IP Specific Tables ------------------------------------------------------

  source("Data/2020_self_assessment_table_data.R")

  
  
  ip_tbl <- ip_prf %>% 
    gt(rowname_col = "IP",
       groupname_col = "indicator") %>% 
    fmt_missing(columns = everything(), missing_text = "-")
  
  
  ip_tbl <- 
    ip_tbl %>% 
    tab_header(title = "USAID Implementing Mechanism Performance") %>% 
    tab_options(table.font.names = "Source Sans Pro") %>% 
    fmt_percent(columns = 2:8, decimals = 0) %>% 
    tab_style(style = list(cell_text(weight = 'bold')), 
              locations = cells_body(columns = 1:8, 
                                     rows = `USAID Implementing Mechanism` == "USAID")) 
  
  ip_tbl %>% 
    cols_width(
      everything() ~ px(100)) %>% 
    tab_footnote(
      footnote = md("*No target set for MMD. Goal was to scale up as rapidly as possible*"),
      locations = cells_column_labels(
        columns = contains("MMD")
      )
    )
  
  

# DREAMS Table -------------------------------------------------------------------------
  
  # Interventions to highlight in the table
  grey_out <- c("Social Asset Building (Safe Spaces)", "Condom Distribution", 
                "Financial Literacy Training", "Contraceptive Method Mix", 
                "PrEP")  
  
  dreams_tbl <- 
    dreams %>% 
    gt(rowname_col = "IP",
       groupname_col = "indicator") %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    fmt_percent(columns = "Target Achieved", decimals = 0) %>%
    fmt_number(columns = 3:6, decimals = 0) 
  
  
 #Highlight specific rows
  dreams_tbl <-
    dreams_tbl %>% 
    tab_style(
      style = list(
        cell_fill(color = "#f1f1f1")
      ),
      locations = cells_body(
        columns = everything(), # not needed if coloring all columns
        rows = Intervention %in% grey_out)
    ) %>% 
    tab_header("DREAMS: Increasing services for Adolescent Girls and Young Women") %>% 
    tab_options(table.font.names = "Source Sans Pro") %>% 
      fmt_missing(columns = everything(),
                  missing_text = "-")
  

dreams_tbl %>% 
    cols_width(
      everything() ~ px(160)) 
