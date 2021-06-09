# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Target summary
# LICENSE:  MIT
# DATE:     2021-04-30
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(tameDP)
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


# GLOBAL VARIABLES --------------------------------------------------------

  dp_path <- "Data/COP21/2021-06-02 Zambia Datapack Final.xlsx"  
  
  peds_ages <- c("<01","01-04", "05-09", "10-14", 
                 "<15", "02 - 12 Months", "<=02 Months")
  
  age_disaggs <- tribble(
    ~indicator, ~numeratordenom,                              ~standardizeddisaggregate,
    "CXCA_SCRN",           "N",       "Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
    "GEND_GBV",            "N",                                      "Total Numerator",
    "HTS_INDEX_FAC",       "N",                                     "4:Age/Sex/Result",
    "HTS_INDEX_COM",       "N",                                     "4:Age/Sex/Result",
    "HTS_RECENT",          "N",                      "Modality/Age/Sex/RTRI/HIVStatus",
    "HTS_SELF",            "N",                                  "Age/Sex/HIVSelfTest",
    "HTS_TST",             "N",                   "Modality/Age Aggregated/Sex/Result",
    "HTS_TST",             "N",                              "Modality/Age/Sex/Result",
    "HTS_TST_POS",         "N",                   "Modality/Age Aggregated/Sex/Result",
    "HTS_TST_POS",         "N",                              "Modality/Age/Sex/Result",
    "KP_PREV",             "N",                                      "Total Numerator",
    "OVC_HIVSTAT",         "N",                                      "Total Numerator",
    "OVC_SERV",            "N",                                "Age/Sex/ProgramStatus",
    "PMTCT_ART",           "N",                     "Age/NewExistingArt/Sex/HIVStatus",
    "PMTCT_EID",           "N",                                                  "Age",
    "PMTCT_STAT",          "D",                                              "Age/Sex",
    "PMTCT_STAT",          "N",                               "Age/Sex/KnownNewResult",
    "PP_PREV",             "N",                                              "Age/Sex",
    "PrEP_CURR",           "N",                                              "Age/Sex",
    "PrEP_NEW",            "N",                                              "Age/Sex",
    "TB_ART",              "N",                     "Age/Sex/NewExistingArt/HIVStatus",
    "TB_PREV",             "D",                     "Age/Sex/NewExistingArt/HIVStatus",
    "TB_PREV",             "N",                     "Age/Sex/NewExistingArt/HIVStatus",
    "TB_STAT",             "D",                                              "Age/Sex",
    "TB_STAT",             "N",                               "Age/Sex/KnownNewPosNeg",
    "TX_CURR",             "N",                        "Age/Sex/ARVDispense/HIVStatus",
    "TX_CURR",             "N",                         "Age Aggregated/Sex/HIVStatus",
    "TX_CURR",             "N",                                    "Age/Sex/HIVStatus",
    "TX_NEW",              "N",                                    "Age/Sex/HIVStatus",
    "TX_NEW",              "N",                         "Age Aggregated/Sex/HIVStatus",
    "TX_PVLS",             "D",                         "Age/Sex/Indication/HIVStatus",
    "TX_PVLS",             "D",              "Age Aggregated/Sex/Indication/HIVStatus",
    "TX_PVLS",             "N",                         "Age/Sex/Indication/HIVStatus",
    "TX_PVLS",             "N",              "Age Aggregated/Sex/Indication/HIVStatus",
    "TX_TB",               "D", "Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus"
  )


# FUNCTIONS ---------------------------------------------------------------

group_ages <- function(df){
  mutate(df,
         age = case_when(is.na(age) ~ NA_character_,
                         indicator == "OVC_SERV" & age %in% c(peds_ages, "15-17") ~ "<18",
                         indicator == "OVC_SERV" ~ "18+",
                         age %in% peds_ages ~ "<15",
                         TRUE~ "15+"))
}

# clean_partner <- function(df){
#   mutate(df, primepartner = case_when(str_detect(primepartner, "Elizabeth") ~ "EGPAF",
#                                       str_detect(primepartner, "DELOITTE") ~ "Deloitte",
#                                       str_detect(primepartner, "BAYLOR|Baylor") ~ "Baylor",
#                                       str_detect(primepartner, "Family") ~ "Epic",
#                                       str_detect(primepartner, "Pact") ~ "Pact",
#                                       str_detect(primepartner, "TANZANIA HEALTH") ~ "Police and Prisons Activity"))
# }


# IMPORT ------------------------------------------------------------------

  df_dp <- tame_dp(dp_path)
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_msd() 



# MUNGE -------------------------------------------------------------------

  # Get rid of dedpp adjustments
  df_dp_sel <- df_dp %>% 
    filter(mech_code != "00000")

  df_dp_sel <- df_dp_sel %>% 
    filter(indicator == "KP_PREV" | disagg != "KeyPop") %>% 
    count(fundingagency, indicator, numeratordenom, wt = targets, name = "targets") %>% 
    mutate(fiscal_year = 2022)


  lst_trgts <- df_dp_sel %>% 
    distinct(indicator, numeratordenom)

  df_zmb <- df %>% 
    filter(operatingunit == "Zambia",
           fundingagency != "Dedup",
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    semi_join(lst_trgts, by = c("indicator", "numeratordenom")) %>% 
    group_by(fiscal_year, fundingagency, indicator, numeratordenom) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop")


  df_full <- df_zmb %>% 
    bind_rows(df_dp_sel)
  
  df_full <- df_full %>% 
    clean_agency() %>% 
    clean_indicator() %>% 
    select(-numeratordenom)

  df_full <- df_full %>% 
    mutate(achievement = cumulative/targets,
           achievement = na_if(achievement, Inf))
  
  df_ex <- df_full %>% 
    select(-cumulative) %>% 
    pivot_wider(names_from = fiscal_year,
                values_from = c(targets, achievement)) 


write_csv(df_ex, "Dataout/COP21_ZMB_Target_Comp.csv", na = "")

  # Review table
  df_ex %>% 
    group_by(indicator) %>% 
    summarise(all = sum(targets_2022, na.rm = TRUE)) %>%  
    arrange(indicator) %>% 
    prinf()


# HTS ---------------------------------------------------------------------

  df_dp_mods <- df_dp %>% 
    filter(mech_code != "00000",
           indicator == "HTS_TST_POS",
           disagg != "KeyPop") %>% 
    count(fundingagency, indicator, modality, numeratordenom, wt = targets, name = "targets") %>% 
    mutate(fiscal_year = 2022)


  df_zmb_mods <- df %>% 
    filter(operatingunit == "Zambia",
           fundingagency != "Dedup",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result")) %>% 
    group_by(fiscal_year, fundingagency, indicator,numeratordenom, modality) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop")
  
  df_full_mods <- df_zmb_mods %>% 
    bind_rows(df_dp_mods)
  
  
  df_full_mods <- df_full_mods %>% 
    clean_agency() %>% 
    clean_indicator() %>% 
    select(-numeratordenom)
  
  df_full_mods <- df_full_mods %>% 
    mutate(achievement = cumulative/targets,
           achievement = na_if(achievement, Inf))
  
  df_ex_mods <- df_full_mods %>% 
    select(-cumulative) %>% 
    pivot_wider(names_from = fiscal_year,
                values_from = c(targets, achievement))

write_csv(df_ex_mods, "Dataout/COP21_ZMB_Target_Comp_HTS_POS.csv", na = "")




# PARTNER -----------------------------------------------------------------

  df_dp_ptnr <- df_dp %>% 
    filter(fundingagency == "USAID",
           (indicator == "KP_PREV" | disagg != "KeyPop")) %>%
    group_ages() %>% 
    # clean_partner() %>%
    count(primepartner, indicator, age, numeratordenom, wt = targets, name = "targets") %>% 
    mutate(fiscal_year = 2022) %>% 
    arrange(primepartner, indicator, numeratordenom)
  
  
  df_msd_usaid <- df %>% 
    filter(operatingunit == "Zambia",
           fundingagency == "USAID") %>% 
    mutate(trendscoarse = na_if(trendscoarse, "Unknown Age"),
           indicator = case_when(indicator == "HTS_INDEX" & modality == "Index" ~ "HTS_INDEX_FAC",
                                 indicator == "HTS_INDEX" & modality == "IndexMod" ~ "HTS_INDEX_COM",
                                 TRUE ~ indicator)) %>% 
    # clean_partner() %>%
    filter(!is.na(primepartner)) %>% 
    semi_join(age_disaggs, by = c("indicator", "numeratordenom", "standardizeddisaggregate")) %>% 
    group_by(fiscal_year, primepartner, indicator, age = trendscoarse, numeratordenom) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop")
  
  
  df_full_ptnr <- df_msd_usaid %>% 
    bind_rows(df_dp_ptnr)
  
  df_full_ptnr <- df_full_ptnr %>% 
    clean_indicator() %>% 
    select(-numeratordenom)
  
  
  df_full_ptnr <- df_full_ptnr %>% 
    mutate(age = "Total") %>% 
    group_by(fiscal_year, primepartner, indicator, age) %>% 
    summarize(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    bind_rows(df_full_ptnr)
  
  df_full_ptnr <- df_full_ptnr %>% 
    mutate(achievement = cumulative/targets,
           achievement = na_if(achievement, Inf))
  
  df_pntr_ex <- df_full_ptnr %>% 
    filter(age %in% c("Total", "<15")) %>% 
    arrange(primepartner, desc(age), indicator) %>% 
    select(-cumulative) %>% 
    pivot_wider(names_from = fiscal_year,
                values_from = c(targets, achievement))

write_csv(df_pntr_ex, "Dataout/COP21_ZMB_Target_Comp_Partner.csv", na = "")



# SNU1 --------------------------------------------------------------------


  df_dp_ptnrsnu <- df_dp %>% 
    filter(fundingagency == "USAID",
           (indicator == "KP_PREV" | disagg != "KeyPop")) %>%
    mutate(indicator = case_when(indicator == "HTS_INDEX" & modality == "Index" ~ "HTS_INDEX_FAC",
                                 indicator == "HTS_INDEX" & modality == "IndexMod" ~ "HTS_INDEX_COM",
                                 TRUE ~ indicator)) %>% 
    group_ages() %>% 
    # clean_partner() %>%
    count(primepartner, psnu, indicator, age, numeratordenom, wt = targets, name = "targets") %>% 
    mutate(fiscal_year = 2022) %>% 
    arrange(primepartner, indicator, numeratordenom)
  
  
  df_dp_ptnrsnu <- df_dp_ptnrsnu %>% 
    clean_indicator() %>% 
    select(-numeratordenom)
  
  
  df_dp_ptnrsnu <- df_dp_ptnrsnu %>% 
    mutate(age = "Total") %>% 
    group_by(fiscal_year, primepartner, psnu, indicator, age) %>% 
    summarize(across(c(targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    bind_rows(df_dp_ptnrsnu)
  
  ind_lst <- df_dp_ptnrsnu %>% 
    distinct(indicator) %>% 
    arrange(indicator) %>% 
    pull()
  
  df_dp_ptnrsnu_ex <- df_dp_ptnrsnu %>% 
    filter(age %in% c("Total", "<15")) %>% 
    arrange(primepartner, desc(age), psnu, indicator) %>%  
    pivot_wider(names_from = indicator,
                values_from = targets) %>% 
    select(fiscal_year:age, ind_lst)

write_csv(df_dp_ptnrsnu_ex, "Dataout/COP21_ZMB_Target_Comp_SNU.csv", na = "")


# VIZ AND TABLES ----------------------------------------------------------

  df_ex %>% distinct(indicator) %>% pull() %>% cat(sep = "\n")


  tst <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX_COM", "HTS_INDEX_FAC", 
              "HTS_SELF", "HTS_RECENT", "PMTCT_STAT_D", "PMTCT_STAT", 
               "PMTCT_EID", "TB_STAT_D", "TB_STAT", "CXCA_SCRN", "OVC_HIVSTAT")

  trmt <- c("TX_NEW", "TX_CURR", "TB_ART", "PMTCT_ART", "TX_TB_D")

  prev <- c("VMMC_CIRC", "KP_PREV", "OVC_SERV", "PP_PREV", "PrEP_CURR", "PrEP_NEW",
            "TB_PREV", "TB_PREV_D", "GEND_GBV")

  vl <- c("TX_PVLS", "TX_PVLS_D")
  all_cats <- c(tst, trmt, prev, vl)
  
  dp_tbl <- 
    df_ex %>% 
    ungroup() %>% 
    mutate(FY22_delta = targets_2022 - targets_2021,
           FY22_delta_pct = (targets_2022 - targets_2021)/targets_2021,
           category = case_when(
             indicator %in% tst ~ "TESTING", 
             indicator %in% trmt ~ "TREATMENT",
             indicator %in% prev ~ "PREVENTION",
             indicator %in% vl ~ "VIRAL SUPPRESSION",
             TRUE ~ NA_character_
             ),
          indicator = fct_relevel(indicator, all_cats)
    ) %>% 
    arrange(indicator) %>% 
    relocate(c(FY22_delta, FY22_delta_pct), .after = targets_2022)
  
  dp_tbl %>% 
    filter(fundingagency == "USAID") %>% 
    gt(groupname_col = "category") %>% 
    fmt_number(
      columns = matches("targets|delta"), 
      decimals = 0
      ) %>% 
    fmt_percent(
      columns = matches("ach|pct"),
      decimals = 0
    ) %>% 
    fmt_missing(columns = everything(), missing_text = "") %>% 
    tab_style(style = list(
      cell_fill(color = old_rose_light, alpha = 0.25)),
              locations = cells_body(
                columns = vars(FY22_delta),
                rows = FY22_delta < 0)
    ) %>% 
    tab_style(style = list(
      cell_fill(color = genoa_light, alpha = 0.25)),
      locations = cells_body(
        columns = vars(FY22_delta),
        rows = FY22_delta > 0)
    ) %>% 
    cols_hide(
      columns = vars(fundingagency, achievement_2022)
    ) %>% 
    cols_align(
      columns = "indicator", align = "left"
    ) %>% 
    tab_spanner(
      label = "TARGETS",
      columns = matches("target|delt")
    ) %>% 
    tab_spanner(
      label = "ACHIEVEMENT",
      columns = matches("achieve")
    ) %>% 
    cols_label(
      targets_2019 = 2019
      
    )

  