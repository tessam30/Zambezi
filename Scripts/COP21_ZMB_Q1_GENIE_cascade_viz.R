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
  library(googlesheets4)
  library(readxl)

  # Latest MSD PSNU x IM File - Curr release
  file_genie <- return_latest(folderpath = datim, 
                            pattern = "Genie-PSNUByIM")

  cascade_vars <- c("HTS_TST", "HTS_TST_POS", "TX_CURR",
                    "TX_NEW", "VMMC_CIRC", "PREP_NEW", "PREP_CURR")
  
  agencies <- c("USAID", "HHS/CDC")
  
  
  cop_data <- "Data/COP21"
  cop_dataout <- "Dataout/COP21"
  
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
  

# READ IN GOOGLESHEET -----------------------------------------------------

  # Table that we are replicating
  excel_sheets(file.path(cop_data, "FY20 Q4 USAID Results Summary Table  11-12-2020.xlsx"))
  
  
  
  # Updated indicator list from Mission
  tst_list <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "HTS_RECENT", "HTS_SELF")
  pmtct_list <- c("PMTCT_ART", "PMTCT_EID", "PMTCT_HEI_POS", "PMTCT_STAT")
  prep_list <- c("PrEP_NEW", "PrEP_CURR")
  tb_list <- c("TB_ART", "TB_STAT")
  tx_list <- c("TX_CURR", "TX_ML", "TX_NEW", "TX_PVLS", "TX_RTT")
  vmmc_list <- c("VMMC_CIRC")
  
  df_indics <- 
    df %>% 
    filter(indicator %in% c(tst_list, tx_list, prep_list, pmtct_list, vmmc_list, tb_list),
                standardizeddisaggregate == "Total Numerator",
                fundingagency == "USAID",
                period != "FY20", period_type != "cumulative") 
  
  
  df_hts_yield <- 
    df_indics %>% filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
    group_by(primepartner, fundingagency, mech_code, mech_name, indicator, period) %>% 
    summarise(total = sum(val, na.rm = TRUE)) %>% 
    spread(indicator, total) %>% 
    ungroup() %>% 
    mutate(HTS_TST_YIELD = HTS_TST_POS / HTS_TST) %>% 
    pivot_longer(cols = HTS_TST:HTS_TST_YIELD,
                 names_to = "indicator",
                 values_to = "total") 
  
  
  df_indic_wide <- 
    df_indics %>% 
    group_by(indicator, fundingagency, primepartner, mech_code, mech_name, period) %>% 
    summarise(total = sum(val, na.rm = TRUE)) %>% 
    full_join(df_hts_yield) %>% 
    spread(period, total) %>% 
    select(-FY21, everything()) %>% 
    mutate(achievement = FY21Q1 / FY21) %>% 
    mutate(indic_flag = case_when(
      indicator == "HTS_TST" ~ 1,
      indicator == "HTS_TST_POS" ~ 2,
      TRUE ~ 3
    )) %>% 
    arrange(mech_name, indicator, indic_flag) 
    
  df_indic_wide %>% filter(primepartner != "TBD") %>% prinf()
    
  # Targets for Local Treatment Partner Need to Map into EQUIP
  write_csv(df_indic_wide, file.path(cop_dataout, "tmp.csv"))
  
  
  
  
# Prep data for gt table and 
gt_df <-  
   df_indic_wide %>%
   ungroup() %>% 
    mutate(mech_name_abr = case_when(
      mech_name == "USAID/District Coverage of Health Services (DISCOVER-H)" ~ "DISCOVER-H",
      mech_name == "USAID/Zambia Community HIV Prevention Project (Z-CHPP)"  ~ "Z-CHPP",
      mech_name == "USAID/Stop Gender Based Violence Project (Stop GBV)"     ~ "Stop GBV",
      TRUE ~ mech_name),
      mech_flag = if_else(mech_name_abr %in% c("Stop GBV", "DISCOVER-H", "Z-CHPP", 
                                       "EQUIP", "Eradicate TB", "SAFE",
                                       "USAID Open Doors"), 1, 0),
      mech_new_name = paste0(mech_name_abr, " (", mech_code, ")"),
      `Q1 Growth` = ((FY21Q1/FY20Q1)-1)) %>%
    #filter(mech_flag == 1) %>% 
   mutate(indicator = fct_relevel(indicator,
                                  "HTS_TST",
                                  "HTS_TST_POS",
                                  "HTS_TST_YIELD",
                                  "HTS_SELF",
                                  "HTS_INDEX",
                                  "HTS_RECENT",
                                  "TX_NEW",
                                  "TX_CURR",
                                  "TX_PVLS",
                                  "TX_ML",
                                  "TX_RTT",
                                  "PMTCT_STAT",
                                  "PMTCT_ART",
                                  "PMTCT_EID",
                                  "PMTCT_HEI_POS",
                                  "PrEP_CURR",
                                  "PrEP_NEW",
                                  "VMMC_CIRC")) %>% 
   arrange(indicator) %>% 
   mutate(indic_type = case_when(
     str_detect(indicator, "HTS") ~ "Testing",
     str_detect(indicator, "(TX|TB)") ~ "Treatment",
     str_detect(indicator, "PMTCT") ~ "Treatment - Peds",
     str_detect(indicator, "(PrEP|VMMC)") ~ "Prevention",
     TRUE ~ NA_character_)
   )

 

# EXPORT EXCEL and CREATE TABLES ------------------------------------
  
 gt_df_export <- gt_df %>% 
   select(IM = mech_new_name,
          `Program Area` = indic_type,
          indicator,
          FY20Q1:FY21,
          `FY21 Achievement`  = achievement,
          `Q1 Growth`
          ) %>% 
   arrange(IM, indicator)
 
 # DO ONLY Once or it will overwrite results
 sheet_id <- "1j6w33jXcRsyrgrZuAHH43XkFi4TWFlYs2RdCdLy27ag"
 
 #sheet_write(gt_df_export, ss = sheet_id, sheet = "Results Table")
 #sheet_write(gt_df_export2, ss = sheet_id, sheet = "Reference Table")
 
 write_csv(gt_df, file.path(cop_dataout, paste0("FY21 Q1 USAID Summary Table ", Sys.Date(), ".csv")))
   
 
 
 im_table <- function(mech_code_num) {
    
   im_name <- gt_df %>% filter(mech_code == {{mech_code_num}}) %>% distinct(mech_new_name) %>% pull(mech_new_name)
   
   gt_df %>% 
     filter(mech_code == {{mech_code_num}}) %>% 
     rename(`FY21 Targets` = FY21,
            `Achievement` = achievement) %>% 
    gt(
      groupname_col = "indic_type",
      rowname_col = "indicator"
    ) %>% 
    cols_hide(columns = vars("fundingagency", "primepartner", "indic_flag", 
                             "mech_code", "mech_name", "mech_flag",
                             "mech_name_abr", "mech_new_name")) %>% 
    fmt_number(columns = (contains("FY")), 
             decimals = 0) %>% 
    fmt_percent(columns = contains("FY"),
                decimals =0,
                rows = (indicator == "HTS_TST_YIELD")) %>% 
    fmt_percent(columns = vars("Achievement", `Q1 Growth`),
                decimals = 0) %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    tab_style(style = cell_fill(color = genoa_light, alpha = 0.44),
              locations = cells_body(
                columns = vars(`Achievement`),
                rows = `Achievement` >= 1 & `Achievement` <= 1.25)
    ) %>% 
    tab_style(style = cell_fill(color = genoa, alpha = 0.44),
              locations = cells_body(
                columns = vars(`Achievement`),
                rows = `Achievement` >= 1.25)
    ) %>% 
    tab_style(style = cell_fill(color = old_rose_light, alpha = 0.33),
              locations = cells_body(
                columns = vars(`Q1 Growth`),
                rows = `Q1 Growth` < 0) 
    ) %>% 
     tab_header(title = paste0(im_name, " TRENDS AND RESULTS AS OF FY21Q1")) %>% 
     tab_source_note("Source: DATIME GENIE pull 2021-02-02")
 }
     
  # Loop over tables and write them to pngs
 gt_df %>% 
   distinct(mech_code, mech_name_abr) %>% 
   pull(mech_code) %>% 
   map(.x, .f = ~im_table(.x) %>% gtsave(file.path(images, paste0(.x, "_FY21 Q1 Results.png"))))
  
  
# LOAD AND MUNGE ----------------------------------------------------------
  
  df <- read_msd(file_genie) %>% 
    reshape_msd(clean = T) 
    
  # Testing modalities
  df %>% filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
                fundingagency == "USAID",
                period != "FY20", period_type != "cumulative") %>% 
    group_by(indicator, fundingagency, modality, period, period_type) %>% 
    summarise(total = sum(val, na.rm = TRUE)) %>% 
    spread(period, total)
    prinf()
  
  
  df %>% 
    filter()
    
    
    
    
  df %>% names()
  
  
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
    
  
