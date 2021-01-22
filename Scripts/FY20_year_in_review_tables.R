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
  library(extrafontdb)
  library(scales)

  datain <- "Data"
  dataout <- "Dataout"
  graphics <- "Graphics"
  
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
           indicator %in% c("TX_CURR", "HTS_TST", "HTS_TST_POS", "TX_NET_NEW", "TX_NEW", "VMMC_CIRC", "PrEP_NEW", "CXCA_SCRN", "TB_PREV"),
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
           indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW", "VMMC_CIRC", "PrEP_NEW", "CXCA_SCRN", "TB_PREV"))) %>% 
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
  
  #Is there any correlation between where those missing children are and where we have coverage? New provinces added in COP20. Mapped out districts and sites to really look at that.
  
  
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
           indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW", "VMMC_CIRC", "PrEP_NEW", "CXCA_SCRN", "TB_PREV"))) %>% 
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

  # Data provided by the mission in tables
  # NOTES - the TB_PREV data seem suspect
  source("Data/2020_self_assessment_table_data.R")
  
  ip_prf <- ip_prf %>%  rename_with( ~gsub(".", " ", .x, fixed = TRUE)) 
  
 tx_tbl <- ip_prf %>% 
    select(1:7) %>% 
    gt(rowname_col = "IP",
       groupname_col = "indicator") %>% 
    fmt_missing(columns = everything(), missing_text = " ") %>% 
    tab_header(title = "TREATMENT: USAID Implementing Mechanism Performance") %>% 
  tab_options(table.font.names = "Source Sans Pro")  %>% 
   fmt_number(columns = 2:7, decimals = 0, rows = 5) %>% 
    fmt_percent(columns = 2:7, decimals = 0, 
                rows = 1:4) %>% 
    tab_style(style = list(cell_text(weight = 'bold')), 
              locations = cells_body(columns = 1:7, 
                                     rows = `USAID Implementing Mechanism` == "USAID")) %>% 
    cols_width(
      everything() ~ px(120)) %>% 
    tab_footnote(
      footnote = md("*No target set for MMD. Goal was to scale up as rapidly as possible*"),
      locations = cells_column_labels(
        columns = contains("MMD")
      )
      )
tx_tbl
  gtsave(tx_tbl, here(graphics, "FY20_self_assessment_summary_table_TREATMENT.pdf"))

  
  tst_tbl <- 
    ip_prf %>% 
    select(1, 8:10) %>% 
    gt(rowname_col = "IP",
       groupname_col = "indicator") %>% 
    fmt_missing(columns = everything(), missing_text = " ") %>% 
    tab_header(title = "PREVENTION: USAID Implementing Mechanism Performance") %>% 
    tab_options(table.font.names = "Source Sans Pro")  %>% 
    fmt_number(columns = 2:4, decimals = 0, rows = 5) %>% 
    fmt_percent(columns = 2:4, decimals = 0, 
                rows = 1:4) %>% 
    tab_style(style = list(cell_text(weight = 'bold')), 
              locations = cells_body(columns = everything(), 
                                     rows = `USAID Implementing Mechanism` == "USAID")) %>% 
    cols_width(
      everything() ~ px(175))
    

gtsave(tst_tbl, here(graphics, "FY20_self_assessment_summary_table_PREVENTION.pdf"))
  

# DREAMS Table -------------------------------------------------------------------------
  
  # Interventions to highlight in the table
  grey_out <- c("Social Asset Building (Safe Spaces)", "Condom Distribution", 
                "Financial Literacy Training", "Contraceptive Method Mix", 
                "PrEP")  
  
  dreams <- dreams %>% rename_with(~gsub(".", " ", .x, fixed = TRUE))
  
  # Verify folks can math
  dreams %>% mutate(test = `2020 Results` /`2020 Targets`)
  
  dreams_tbl <- 
    dreams %>% 
    gt(rowname_col = "IP",
       groupname_col = "indicator") %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    fmt_percent(columns = "Target Achieved", decimals = 0) %>%
    fmt_number(columns = 3:6, decimals = 0) 
  
  
 #Highlight specific rows

    dreams_tbl %>% 
    tab_style(style = list(cell_text(color = genoa, weight = "bold")),
              locations = cells_body(
                columns = vars(`Target Achieved`),
                rows = `Target Achieved` > 1)) %>% 
      tab_style(style = list(cell_text(color = scooter, weight = "bold")), 
                locations = cells_body(columns = 5, 
                                       rows = `Intervention` == "PrEP")) %>% 
    # tab_header("DREAMS: Inter) %>% 
    #   opt_align_table_header(align = c("left")) %>% 
    tab_options(table.font.names = "Source Sans Pro") %>% 
      fmt_missing(columns = everything(),
                  missing_text = "-")
  

# dreams_tbl %>% 
#     cols_width(
#       everything() ~ px(160)) 

# PREP plot ---------------------------------------------------------------

  # Queue up annotation text for a single facet
  ann_text <- data.frame(x = 2.5, y = 16000, lab = "Significant scale-up\nquarter over quarter",
                         indicator = factor(2, levels = c("PrEP_NEW", "PrEP_CURR")))

  prep %>% 
  mutate(color = if_else(indicator == "PrEP_NEW", "#287c6f", "#1e87a5"),
    indicator = factor(indicator, c("PrEP_NEW", "PrEP_CURR"))) %>% 
    ggplot(aes(x = period, y = val, fill = color)) +
    geom_col() + 
    scale_fill_identity() +
    geom_text(aes(label = scales::comma(val)), 
              vjust = 1.5, colour = "white", family = "Source Sans Pro") +
    geom_errorbar(aes(x = period, ymin = targets, ymax = targets), colour = grey10k, size = 0.5, linetype = "dashed" ) +
    facet_wrap(~indicator) +
  glitr::si_style_xline() +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(expand =c(0, 0)) +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank()) 

  si_save(here(graphics, "FY20_PrEP_trends.png"))



# Supply chain MMD --------------------------------------------------------
  library(lemon)
  
  mmd <- mmd %>% 
    mutate(color = case_when(
      mmd_type == "Not reported" ~ "#8980cb",
      mmd_type == "<3 Months (non-MMD)" ~ "#e07653",
      mmd_type == "3-5 Month MMD" ~ "#1e87a5",
      TRUE ~ "#287c6f"
    ))
  
  
  mmd %>% 
    mutate(mmd_type = factor(mmd_type, c("6+ Month MMD", "3-5 Month MMD", "<3 Months (non-MMD)", "Not reported"))) %>% 
    ggplot(aes(period, val, group = mmd_type, fill = color)) +
    geom_col() + 
    facet_rep_wrap(~ mmd_type, repeat.tick.labels = 'bottom') +
  scale_fill_identity() +
    geom_text(aes(label = scales::percent(val, accuracy = 1)), 
              vjust = 1.5, colour = "white", family = "Source Sans Pro") +
    si_style_xline() +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.text.y = element_blank(),
          panel.grid = element_blank()) +
    labs(x = NULL, y = NULL,
         title = "TX_CURR by MMD Duration, 15+ age band") 
  

  si_save(here(graphics, "FY20_MMD_duration_trends.png"), 
          height = 5.82, width = 7.5, dpi = "retina")  
  

# VMMC --------------------------------------------------------------------

  vmmc <- vmmc %>% 
    mutate(color = case_when(
      age_vmmc == "30+" ~ "#e07653",
    age_vmmc == "15-29" ~ "#1e87a5",
    TRUE ~ "#287c6f"
    ),
    age_vmmc = factor(age_vmmc, c("<15", "15-29", "30+")))
  
  vmmc %>% 
    ggplot(aes(period, val, group = age_vmmc, fill = color)) +
    geom_col() + 
    facet_wrap(~age_vmmc, nrow = 1) +
    scale_fill_identity() +
    geom_text(aes(label = scales::percent(val, accuracy = 1)), 
              vjust = 1.5, colour = "white", family = "Source Sans Pro") +
    si_style_xline() +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.text.y = element_blank(),
          panel.grid = element_blank()) +
    labs(x = NULL, y = NULL, 
         title = "VMMC_CIRC AGE BAND TRENDS: ZAMBIA SHIFTED OUT OF UNDER 15 YEAR OLDS IN FY20Q4\n",
         caption = "Quarterly OHA Tableau Dashboard as of 2020-11-25")
    
  si_save(here(graphics, "FY20_VMMC_CIRC_trends.png"), 
          height = 7, width = 10.1, dpi = "retina") 
  

# SUPPLY CHAIN ------------------------------------------------------------

  sc <- 
    sc %>% 
    mutate(regimen_order = fct_reorder(Regimen, share),
           colors = case_when(
             Regimen == "TLE 400" ~ "#287c6f",
             Regimen == "TLD"     ~ "#1e87a5",
             Regimen == "Other"   ~ "#e07653",
             Regimen == "NVP"     ~ "#2057a7",
             TRUE ~ "#c43d4d",
           ),
           label = ifelse(share > 0.04, share, NA)
        )
        
  
  color_order <- c("#287c6f", "#1e87a5", "#e07653", "#2057a7", "#c43d4d")
  
  ggplot(sc, aes(fill = regimen_order, y = Age, x = share)) + 
    geom_bar(position="stack", stat="identity", color = "white") +
    # geom_text(aes(label = paste0(scales::percent(label, accuracy = 1), Regimen)), 
    #           colour = "white", family = "Source Sans Pro",
    #           position = position_stack(vjust = 0.1)) +
    scale_fill_manual(values = rev(color_order)) +
    si_style_void() +
    theme(legend.position = "none")
  
  si_save(here(graphics, "FY20_SC_ARVDISP_share.png"), 
          height = 1, width = 12, dpi = "retina") 

  

# PMTCT -------------------------------------------------------------------

  every_nth = function(n) {
    return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
  }
  
  pmtct_long <- 
    pmtct %>% 
    filter(indicator %in% c("Proxy EID 2mo Coverage", "HEI Linked to ART")) %>% 
    mutate(indicator = fct_inorder(indicator)) %>% 
    pivot_longer(cols = `FY18_Q1`:`FY20_Q4`, 
                 names_to = c("pd"),
                 values_to = "val") %>% 
    mutate(time = factor(pd),
           pd = str_replace(pd, "_", ""),
           fill_color = if_else(indicator %in% c("Proxy EID 2mo Coverage", "HEI Linked to ART"), 
                          "#D2E7ED", grey10k),
           dot_color = if_else(fill_color == "#D2E7ED", scooter, grey60k)) %>% 
    group_by(indicator) %>% 
    ungroup()

  pmtct_long %>% 
    ggplot(aes(x = pd, y = val, group = indicator)) +
    geom_area(aes(fill = fill_color), alpha = 0.65) +
    geom_line(aes(color = dot_color)) +
    geom_point(aes(fill = dot_color),
               shape = 21, color = "white", size = 3) + 
             facet_wrap(~indicator, nrow = 2) +
    scale_y_continuous(limits = c(0, NA), 
                       labels = scales::percent_format()) +
    scale_fill_identity() +
    scale_color_identity() +
    si_style_ygrid() +
    labs(x = NULL, y = NULL, 
         caption = "FY20 Q4 PSNUxIM 2020-11-13, USAID ONLY")
  
  si_save(here(graphics, "FY20_peds_cascade.png"),
          height = 6.39,
          width = 13.29, 
          dpi = "retina")
                       
                       
                       
                       
                      

# PrEP --------------------------------------------------------------------

  prep19 <- readxl::read_excel(here("Data/DISCOVER-Health OHA PrEP Template FY19.xlsx"))  
  prep20 <- readxl::read_excel(here("Data/DISCOVER-Health OHA PrEP Template FY20.xlsx")) 
  
  # PrEP_NEW is the indictor we are looking for to plot over time
  
  prep19_long <-
    prep19 %>%
    pivot_longer(cols = fy19_Oct:fy19_Sept,
                 # names_pattern  = "(....)_(.*)",
                 names_to = c("period"),
                 values_to = "val") %>%
    mutate(period = str_replace_all(period, "fy", "FY"),
      period = fct_inorder(period))

  prep19_long %>%
    filter(indicator == "PrEP_NEW") %>%
    group_by(period, Sex_KP) %>%
    summarise(val = sum(val, na.rm = T)) %>%
    spread(Sex_KP, val) %>%
    prinf()
  
  
  prep20_long <- 
    prep20 %>% select(-c(fy19_Oct:fy19_Sept))%>% 
    pivot_longer(cols = FY20_Oct:FY20_Sept,
                 #names_pattern  = "(....)_(.*)",
                 names_to = ("period"),
                 values_to = "val"
                ) %>% 
    mutate(period = fct_inorder(period))

  # Combine data frames
  prep_all_long <-
    bind_rows(prep19_long, prep20_long)
  
  # Create a dataframe with dates so plot is easy to make w/ a date x-var
  start_date <- as.Date("2018/10/1")
  end_date <- as.Date("2020/09/01")
  dates_fy <- data.frame(date = seq(start_date, end_date, "months")) %>% 
    mutate(date_order = row_number(),
           fy = if_else(date < "2019/10/1", "fy19", "fy20"))

  str(prep_all_long)
  prep_all_long$period %>% levels()

  prep_new <- 
    prep_all_long %>% 
    filter(indicator == "PrEP_NEW",
           Sex_KP != "kvp") %>% 
    group_by(period, Sex_KP) %>% 
    summarise(val = sum(val, na.rm = T)) %>% 
    spread(Sex_KP, val) %>% 
    ungroup()  %>% 
    mutate(total = female + male,
           date_order = row_number(),
           period = str_replace_all(period, "_", "-"),
           n = 1,
           events = if_else(date_order %in% c(6, 9, 11, 13, 15),
                            total, 0),
           period = fct_inorder(period)) %>%  
    left_join(., dates_fy, by = c("date_order"))
  
    
  # Check order of factors
  prep_new$period %>% levels()
  
  prep_new %>% 
    #filter(date_order != 24) %>% 
    mutate(bar = if_else(events == scooter, total, 0)) %>% 
    ggplot(aes(x = date)) + 
    #geom_col(aes(y = events), fill = grey10k, alpha = 0.5, width = 10) +
    geom_area(data = . %>% filter(date_order >= 18), aes(y = total), fill = grey10k, alpha = 0.25) +
    geom_line(aes(y = total), color = scooter, alpha = 0.75, size = 1) +
    geom_line(aes(y = male), color = "#ffd4ac", size = 0.5) +
    geom_line(aes(y = female), color = "#e9ddff", size = 0.5) +
    geom_point(aes(y = total), fill = scooter, shape = 21, size = 4, color = "white") +
      ggrepel::geom_label_repel(aes(y = total, label = scales::comma(total, accuracy = 1), color = scooter),
                            segment.size = 0,
                            point.padding = 0.25,
                            nudge_y = 10,
                            family = "Source Sans Pro",
                            label.size = NA) +
    scale_fill_identity() +
    scale_color_identity() +
    #3200 or 1500 depending on filter above
    scale_y_continuous(limits = c(0, 3200), expand = c(0, 0)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y", 
                 limits = as.Date(c("2018-09-15", "2020-09-15")), 
                 expand = c(0,0)) +
    si_style_xline() +
    labs(x = NULL, y = NULL,
         title = "FY2018 - 2020: DEMAND CREATION FOR PREP YIELDS RESULTS") +
    theme(axis.text.y = element_blank())
  
  si_save(here(graphics, "FY20_PrEP_demand_creation.png"),
          width = 13,
          height = 6, 
          dpi = "retina")
  
  
  # Area graph version
  prep_new %>% 
    pivot_longer(cols = c("male", "female"),
                 names_to = "sex",
                 values_to = "prep_new") %>% 
    ggplot(aes(x = date)) + 
    geom_area(aes(y = prep_new, fill = factor(sex)), alpha = 0.25) +
    geom_line(aes(y = total), color = scooter, alpha = 0.75, size = 1) +
    geom_point(aes(y = total), fill = scooter, shape = 21, size = 4, color = "white") +
    ggrepel::geom_label_repel(data = . %>% filter(sex == "male"),
                              aes(y = total, label = scales::comma(total, accuracy = 1), color = scooter),
                              segment.size = 0,
                              point.padding = 0.25,
                              nudge_y = 10,
                              family = "Source Sans Pro",
                              label.size = NA) +
    scale_color_identity() +
    scale_fill_manual(values = c("male" = "#ffd4ac", "female" = "#e9ddff")) +
    #3200 or 1500 depending on filter above
    scale_y_continuous(limits = c(0, 3200), expand = c(0, 0)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y", 
                 limits = as.Date(c("2018-09-15", "2020-09-15")), 
                 expand = c(0,0)) +
    si_style_xline() +
    labs(x = NULL, y = NULL,
         title = "FY2018 - 2020: DEMAND CREATION FOR PREP YIELDS RESULTS") +
    theme(axis.text.y = element_blank(), legend.position = "none")


  prep_new %>% 
    filter(date_order != 24) %>% 
    summarise(val = sum(total))
  
  

# VLC & VLS ---------------------------------------------------------------

vlc %>% 
    mutate(color = if_else(indicator == "vlc", burnt_sienna, scooter)) %>% 
    ggplot(aes(x = period, y = val, group = indicator))+
    geom_line(aes(color = color)) +
    geom_point(aes(fill = color), shape = 21, size = 4, color = "white")+
    scale_y_continuous(limits = c(0, 1), labels = percent_format()) +
    scale_color_identity() +
    scale_fill_identity() +
    si_style_ygrid() +
    labs(x = NULL, y = NULL,
         caption = "Source: FY20 Q4 PSNUxIM - Initial - 11.13.2020")

  ggsave(here(graphics, "VLS_VLC_peds_scaled.pdf"),
  width = 6.65, height =  5.59,
  device = "pdf",
  dpi = "retina",
  useDingbats = FALSE, scale = 0.75)
  