# Purpose: Verify FY21 Q1 performance and targets
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

  # Latest MSD PSNU x IM File - Curr release
  file_genie <- return_latest(folderpath = datim, 
                            pattern = "Genie-PSNUByIM")

  cascade_vars <- c("HTS_TST", "HTS_TST_POS", "TX_CURR",
                    "TX_NEW", "VMMC_CIRC", "PREP_NEW", "PREP_CURR")
  
  agencies <- c("USAID", "HHS/CDC")
  
  
  # Bar PLOT
  bar_plot <- function(df, indic) {
    
   p <-  df %>%
      filter(indicator %in% {{indic}}) %>% 
      ggplot(aes(x = factor(quarter), 
                 y = val, 
                 fill = factor(mech_name_abr), 
                 group = mech_name_abr)) +
      geom_col(data = . %>% filter(quarter == "FY21Q1"), 
               aes(x = factor(quarter), 
                   y = FY21), 
               fill = grey10k) +
      geom_col() +
      geom_errorbar(data = . %>% filter(quarter == "FY21Q1"), 
                    aes(ymin = FY21, 
                        ymax = FY21, 
                        colour = target_color), 
                    size = 0.5, 
                    linetype = "dashed") +
      geom_text(data = . %>% filter(quarter == "FY21Q1"),
                aes(label = percent(pct_achieved_FY21Q1, 1)),
                vjust = 1, 
                colour = "white") +
      facet_wrap(indicator ~ mech_name_order, 
                 scales = "free") +
      si_style() +
      scale_color_identity() +
      scale_fill_manual(values = color_list) +
      scale_y_continuous(labels = comma) 
   p
  return(p)
    
  }
  

# LOAD AND MUNGE ----------------------------------------------------------
  
  df <- read_msd(file_genie) %>% 
    reshape_msd(clean = T) 
    
  df_21 <- 
    df %>% 
    filter(str_detect(period, "2020", negate = TRUE))

  
  

  
  df_cade <- 
    df %>% 
    filter(standardizeddisaggregate == "Total Numerator",
                indicator %in% cascade_vars,
                fundingagency %in% agencies,
                period_type != "cumulative",
           fundingagency == "USAID") %>% 
    group_by(indicator, fundingagency, primepartner, period, mech_name, mech_code) %>% 
    summarise(tot = sum(val, na.rm = TRUE)) %>% 
    spread(period, tot) %>% 
    select(-FY20) %>% 
    select(-FY21, everything()) %>% 
    ungroup() %>% 
    mutate(pct_achieved_FY21Q1 = (FY21Q1 / FY21)) %>% 
    pivot_longer(cols = FY20Q1:FY21Q1,
                 names_to = "quarter",
                 values_to = "val") %>% 
    mutate(target_color = if_else(val > FY21, "white", grey90k),
           mech_name_abr = case_when(
             mech_name == "USAID/District Coverage of Health Services (DISCOVER-H)" ~ "DISCOVER-H",
             mech_name == "USAID/Zambia Community HIV Prevention Project (Z-CHPP)"  ~ "Z-CHPP",
             mech_name == "USAID/Stop Gender Based Violence Project (Stop GBV)"     ~ "Stop GBV",
             TRUE ~ mech_name
           ),
           mech_name_order = fct_reorder(mech_name_abr, val, .desc = TRUE))  
  
  gt_df <- 
  df_cade %>% 
    select(indicator, mech_name_order, mech_code, val, FY21, pct_achieved_FY21Q1, quarter) %>% 
    spread(quarter, val) %>% 
    select(-FY21, -pct_achieved_FY21Q1, everything())
  
  
  # Spin up a GT table
  gt_df %>% rename(`FY21 Targets` = FY21,
                   `FY21 Achievement` = `pct_achieved_FY21Q1`) %>% 
    gt(groupname_col = "indicator",
       rowname_col = "mech_name_order") %>% 
    fmt_number(columns = 4:9,
               decimals = 0) %>% 
    fmt_percent(columns = 10,
            decimals = 0) %>% 
    tab_options(table.font.names = "Source Sans Pro") %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    cols_hide(columns = vars(`mech_code`)) %>% 
    tab_style(style = cell_fill(color = genoa_light),
              locations = cells_body(
                columns = vars(`FY21 Achievement`),
                rows = `FY21 Achievement` > 1)
              )
  
  

  
  
  
  
  
    
  df_cade %>% distinct(mech_name_abr) %>% nrow()
  color_list <- c(si_palettes$siei, "#BFDDFF")

  bar_plot(df_cade, c("TX_CURR", "TX_NEW"))
    
  
