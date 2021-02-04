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

  # Latest GENIE PSNU x IM File - Cur
    file_genie <- return_latest(folderpath = datim, 
                              pattern = "Genie-PSNUByIM")
  
    cascade_vars <- c("HTS_TST", "HTS_TST_POS", "TX_CURR",
                      "TX_NEW", "VMMC_CIRC", "PREP_NEW", "PREP_CURR")
    
    agencies <- c("USAID", "HHS/CDC")
  
  # Folders for reading/writing COP specific data
    cop_data <- "Data/COP21"
    cop_dataout <- "Dataout/COP21"
  
  # Bar PLOT
    bar_plot <- function(df, indic) {
      
      title_label <- df %>%  filter(indicator %in% {{indic}}) %>%
        distinct(indic_type) %>% pull(indic_type)
      
     p <-  df %>%
        filter(indicator %in% {{indic}}) %>%
        ggplot(aes(x = factor(quarter), 
                   y = val, 
                   fill = mech_color, 
                   group = mech_name_abr)) +
        geom_col(data = . %>% filter(quarter == "FY21Q1"), 
                 aes(x = factor(quarter), 
                     y = FY21), 
                 fill = grey10k) +
        geom_col(alpha = 0.75) +
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
        facet_wrap(mech_name_order ~ indicator, 
                   scales = "free") +
        si_style_ygrid() +
        scale_color_identity() +
        scale_fill_identity() +
        scale_y_continuous(labels = comma) +
       labs(caption = "Source: Genie pull as of 2020-02-02",
            subtitle = "Gray bars indicator FY21 annual targets",
            title = paste0(title_label, ": performance summary by mechanism"))
     p
     
     si_save(file.path(images, paste0(title_label, "_FY21Q1_bar_graph_by_mechanism.png")),
            scale = 1.66)
     
    return(p)
    }
  

# READ IN GOOGLESHEET -----------------------------------------------------

  # Table that we are replicating
    excel_sheets(file.path(cop_data, "FY20 Q4 USAID Results Summary Table  11-12-2020.xlsx"))
  
  # Reading in the genie extract from 2020-02-02
    df <- read_msd(file_genie) %>% 
      reshape_msd(clean = T) 
  
  
  # Updated indicator list from Mission
    tst_list <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "HTS_RECENT", "HTS_SELF")
    pmtct_list <- c("PMTCT_ART", "PMTCT_EID", "PMTCT_HEI_POS", "PMTCT_STAT")
    prep_list <- c("PrEP_NEW", "PrEP_CURR")
    tb_list <- c("TB_ART", "TB_STAT")
    tx_list <- c("TX_CURR", "TX_ML", "TX_NEW", "TX_PVLS", "TX_RTT")
    vmmc_list <- c("VMMC_CIRC")
   
  # Stripping down Genie pull to just indicators of interest from Mission
    df_indics <- 
      df %>% 
      filter(indicator %in% c(tst_list, tx_list, prep_list, pmtct_list, vmmc_list, tb_list),
                  standardizeddisaggregate == "Total Numerator",
                  fundingagency == "USAID",
                  period != "FY20", period_type != "cumulative") 
  
  # Calculate yield separately by bring data wide then pivot it back long so we can full join with rest of dataframe
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
  #write_csv(df_indic_wide, file.path(cop_dataout, "tmp.csv"))
  
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

 # Create an Agency level summary table
    df_hts_yield_USAID <- 
      df_indics %>% filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
      group_by(fundingagency, indicator, period) %>% 
      summarise(total = sum(val, na.rm = TRUE)) %>% 
      spread(indicator, total) %>% 
      ungroup() %>% 
      mutate(HTS_TST_YIELD = HTS_TST_POS / HTS_TST) %>% 
      pivot_longer(cols = HTS_TST:HTS_TST_YIELD,
                   names_to = "indicator",
                   values_to = "total") 



  # Create same data frame and groupings for ALL USAID
    df_indic_wide_USAID <- 
      df_indics %>% 
      group_by(fundingagency, indicator, period) %>% 
      summarise(total = sum(val, na.rm = TRUE)) %>% 
      full_join(df_hts_yield_USAID) %>% 
      spread(period, total) %>% 
      select(-FY21, everything()) %>% 
      ungroup() %>% 
      mutate(Achievement = FY21Q1 / FY21,
             `Q1 Growth` = ((FY21Q1/FY20Q1)-1)) %>% 
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

    
  # Setting gt_export of all USAID totals -- This gets appended to mech table
    gt_df_export_USAID <- 
      df_indic_wide_USAID %>% 
      mutate(IM = "ALL USAID") %>% 
      rename(`Program Area` = indic_type,
             `FY21 Achievement` = Achievement) %>% 
      arrange(indicator) %>% 
      select(IM, `Program Area`, indicator, everything(), -fundingagency)
  
    names(gt_df)
    names(gt_df_export_USAID)


# EXPORT EXCEL and CREATE TABLES ------------------------------------
  
  # Toggle filter #filter(mech_flag == 1) %>% # to show all results 
   gt_df_export <- gt_df %>% 
     select(IM = mech_new_name,
            `Program Area` = indic_type,
            indicator,
            FY20Q1:FY21,
            `FY21 Achievement`  = achievement,
            `Q1 Growth`
            ) %>% 
     arrange(IM, indicator) %>% 
    bind_rows(gt_df_export_USAID)

  
 # DO ONLY Once or it will overwrite results
   sheet_id <- "1j6w33jXcRsyrgrZuAHH43XkFi4TWFlYs2RdCdLy27ag"
 
 # Results are written separately with mech_flag filter on. Full results dumped in ref table. 
   #sheet_write(gt_df_export, ss = sheet_id, sheet = "Results Table")
   sheet_write(gt_df_export, ss = sheet_id, sheet = "Reference Table")

  # Writing for review and in case excel version is requested   
  write_csv(gt_df, file.path(cop_dataout, paste0("FY21 Q1 USAID Summary Table ", Sys.Date(), ".csv")))
   

# IM TABLES ---------------------------------------------------------------

  # Function to spit out IM formatted tables
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
  
   # LOAD MSD - Checking MSD for target to make sure they are consistent
     msd <- read_msd(file_msd) %>% filter(fundingagency == "USAID") 
     msd %>% 
       filter(indicator %in% c(tst_list, tx_list, prep_list, vmmc_list, pmtct_list),
              fiscal_year == "2021",
                    standardizeddisaggregate == "Total Numerator",
                   fundingagency == "USAID") %>% 
       group_by(fundingagency, indicator) %>% 
       summarise(targets = sum(targets, na.rm = TRUE))


# PLOTS OF INDICATORS -----------------------------------------------------

  df_cade <- 
    df %>% 
    filter(standardizeddisaggregate == "Total Numerator",
                indicator %in% c(pmtct_list, tst_list, tx_list, vmmc_list, prep_list, tb_list),
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
           mech_name_order = fct_reorder(mech_name_abr, val, .desc = TRUE),
           mech_color = case_when(
             mech_name_abr == "USAID Open Doors" ~ denim,
             mech_name_abr == "SAFE" ~ old_rose,
             mech_name_abr == "DISCOVER-H" ~ moody_blue,
             mech_name_abr == "Z-CHPP" ~ burnt_sienna,
             mech_name_abr == "Eradicate TB" ~ golden_sand,
             mech_name_abr == "EQUIP" ~ scooter,
             mech_name_abr == "Stop GBV" ~ genoa,
             TRUE ~ trolley_grey
           ))  %>% 
       mutate(indic_type = case_when(
         str_detect(indicator, "HTS") ~ "Testing",
         str_detect(indicator, "TX") ~ "Treatment",
         str_detect(indicator, "TB") ~ "Treatment - TB",
         str_detect(indicator, "PMTCT") ~ "Treatment - Peds",
         str_detect(indicator, "PrEP") ~ "Prevention PrEP",
         str_detect(indicator, "VMMC") ~ "Prevention VMMC",
         TRUE ~ NA_character_)
       )
  
  df_cade %>% distinct(mech_name_abr) 
  
  bar_plot(df_cade, tx_list) 
  bar_plot(df_cade, c("HTS_TST", "HTS_TST_POS"))   
  bar_plot(df_cade, prep_list) 
  bar_plot(df_cade, tb_list)
  bar_plot(df_cade, vmmc_list)
  bar_plot(df_cade %>% filter(mech_name_order != "Eradicate TB"), c("PMTCT_STAT", "PMTCT_ART", "PMTCT_EID"))

# EXPERIMENTING WITH GOOGLE SHEETS ----------------------------------------


  # Experimenting with Google Sheets
  tmp_df <- gt_df_export_USAID %>% 
    mutate(row_id = row_number() + 1,
           trend = paste0("=SPARKLINE(E", row_id, ":I", row_id, ",", "{\"color\", \"#7ecfc0\";\"linewidth\", 2})")
    )%>% 
    relocate(trend, .before = "FY20Q1")
  
  tmp_df$trend <- gs4_formula(tmp_df$trend)
  
  sheet_id <-  "181VnbJfbAErkZrnBrvJLI9g5BL3P7O8ILaEV7h80rF0"
  write_sheet(tmp_df, ss = sheet_id, sheet = "sparkline")  
  
