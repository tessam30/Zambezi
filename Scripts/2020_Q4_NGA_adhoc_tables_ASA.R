# Nigeria Achievement table


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
  pal <- RColorBrewer::brewer.pal(10, "Spectral")[7:8]


# DATA PASTA - table ------------------------------------------------------

  nga <- tibble::tribble(
                   ~Indicator, ~FY20.Total, ~FY20.Target, ~FY20.Achieved, ~Agency,
                    "HTS_TST",     3063236,      2982723,     1.02699312, "USAID",
                "HTS_TST_POS",      141740,       148000,   0.9577027027, "USAID",
                     "TX_NEW",      142861,       146558,    0.974774492, "USAID",
                    "TX_CURR",      402521,       440030,   0.9147580847, "USAID",
                     "TX_RTT",       53345,           NA,             NA, "USAID",
                   "OVC_SERV",      579929,       455738,    1.272505255, "USAID",
                   "PrEP_NEW",       21319,         5766,    3.697363857, "USAID",
                    "KP_PREV",      324742,       243709,    1.332499005, "USAID"
                ) %>% 
    rename_with( ~gsub(".", " ", .x, fixed = TRUE)) 
    

  prev <- tibble::tribble(
                 ~Implementing.Mechanism, ~Case.Finding.Yield, ~New.Clients.Initiating.PrEP, ~PREP_NEW.Target, ~PREP_NEW.Achievement, ~Budget.Execution,
                             "KP Care 1",              0.0952,                        5759L,             699L,                  8.24,                 1,
                             "KP Care 2",              0.1163,                         774L,            3369L,                  0.23,              0.95,
                                 "CaTSS",              0.0234,                           NA,               NA,                    NA,                NA,
                                "SIDHAS",              0.0439,                        3177L,               NA,                    NA,              0.97,
                            "TMEC-EpiC*",              0.1363,                        9690L,               NA,                    NA,                NA,
                                  "RISE",              0.0487,                        1276L,             118L,                 10.81,                 1,
                                   "TO1",               0.023,                         140L,             345L,                  0.41,              0.64,
                                   "TO2",              0.0217,                         198L,             580L,                  0.34,              0.95,
                                   "TO3",              0.0186,                         305L,             595L,                  0.51,              0.43
                 ) %>% 
    rename_with( ~gsub(".", " ", .x, fixed = TRUE)) 
  
  
  ovc <- tibble::tribble(
                ~Implementing.Mechanism, ~OVC_SERV, ~OVC_SERV.Target, ~OVC_SERV.Achievement, ~OVC_HIVSTAT, ~OVC_HIVSTAT.Target, ~OVC_HIVSTAT.Achievement, ~Budget.Execution,
                             "ICHSSA 1",   242749L,          195236L,                  1.24,      165991L,             146426L,                     1.13,              0.99,
                             "ICHSSA 2",    86291L,           66498L,                   1.3,       57525L,              49873L,                     1.15,              0.95,
                             "ICHSSA 3",   150945L,               NA,                    NA,       95682L,                  NA,                       NA,              0.76,
                             "ICHSSA 4",    99944L,          194004L,                  0.52,       75436L,             145406L,                     0.52,              0.83
                ) %>% 
    rename_with( ~gsub(".", " ", .x, fixed = TRUE)) 

    
  

  
  tx <- tibble::tribble(
               ~Implementing.Mechanism, ~TX_CURR.Result, ~TX_CURR..Target, ~TX_CURR.Achieved, ~Viral.Load.Suppression.FY20.Q4, ~`%.3.month.MMD`, ~`%6.month.MMD`, ~`TLD.Dispensed.(30-count.equivalent)`, ~TB.Preventative.Therapy,
                           "KP Care 1",           39472,             6175,               6.4,                            0.95,                1,            0.71,                                 195644,                    13576,
                           "KP Care 2",            4600,            11959,               0.4,                             0.9,             0.99,            0.01,                                  25204,                       39,
                              "SIDHAS",          119163,               NA,                NA,                            0.91,             0.98,            0.42,                                 468367,                    80407,
                                "RISE",           62763,            93535,               0.7,                            0.89,             0.98,            0.59,                                 286074,                    17393,
                                 "TO1",           35251,            40162,               0.9,                            0.89,             0.91,             0.3,                                 187266,                     6659,
                                 "TO2",           55882,            71380,               0.8,                            0.92,                0,               0,                                 253297,                     5179,
                                 "TO3",           78958,            89680,               0.9,                            0.93,             0.88,             0.4,                                 435345,                    10865,
                           "TMEC-EpiC",            6432,               NA,                NA,                            0.92,              0.6,               0,                                  18471,                      534
               ) %>% 
    rename_with(~gsub(".", " ", .x, fixed = TRUE)) 

  

# SUMMARY TABLES ----------------------------------------------------------

   sum_tbl <- 
    nga %>% 
    group_by(Agency) %>% 
    gt(rowname_col = "Indicator",
       groupname_col = "Agency") %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    tab_options(table.font.names = "Source Sans Pro")  %>% 
    fmt_number(columns = 2:3, decimals = 0) %>% 
    fmt_percent(columns = 4, decimals = 0) %>% 
    tab_style(style = cell_fill(color = pal[2]),
              locations = cells_body(
                columns = vars(`FY20 Achieved`),
                rows = `FY20 Achieved` >= 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[1]),
              locations = cells_body(
                columns = vars(`FY20 Achieved`),
                rows = `FY20 Achieved` < 1.1)) %>% 
    tab_style(style = cell_fill(color = pal[1]),
              locations = cells_body(
                columns = vars(`FY20 Achieved`),
                rows = `FY20 Achieved` < .9)) %>% 
    cols_width(
      everything() ~ px(180)) 

  sum_tbl  
  
  gtsave(sum_tbl, here(graphics, "NGA_FY20_self_assessment_summary_all.png"))

  

# IP PERFORMANCE PREVENTION -----------------------------------------------

  prev_tbl <- 
    prev %>% 
    gt(rowname_col = "IP",
       groupname_col = "indicator") %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    tab_header(title = "PREVENTION: USAID Implementing Mechanism Performance") %>% 
    tab_options(table.font.names = "Source Sans Pro")  %>% 
    fmt_number(columns = 3:5, decimals = 0) %>% 
    fmt_percent(columns = c(2, 5, 6), decimals = 0) 
    # tab_footnote(
    #   footnote = md("*TIMEC-EpiC centrally funded*"),
    #   locations = cells_column_labels(
    #     columns = contains("Budget Execution")
    #     )
    # )
      
  
  prev_tbl
  gtsave(prev_tbl, here(graphics, "NGA_FY20_self_assessment_im_perf_prev_1.png"))


# OVC ---------------------------------------------------------------------
  
  ovc_tbl <- 
    ovc %>% 
    gt(rowname_col = "IP",
       groupname_col = "indicator") %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    tab_header(title = "TREATMENT: USAID Implementing Mechanism Performance") %>%
    tab_options(table.font.names = "Source Sans Pro")  %>% 
    fmt_number(columns = c(2, 3, 5, 6), decimals = 0) %>% 
    fmt_percent(columns = c(4, 7, 8), decimals = 0) 
  ovc_tbl
  
  gtsave(ovc_tbl, here(graphics, "NGA_FY20_self_assessment_im_perf_prev_2.png"))
  

# IP Performance Treatment ------------------------------------------------
  
  tx_tbl <- 
  tx %>% 
    select(-4) %>% 
    filter(`Implementing Mechanism` != "USAID Total") %>% 
    gt(rowname_col = "IP",
       groupname_col = "indicator") %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    tab_header(title = "TREATMENT: USAID Implementing Mechanism Performance") %>%
    tab_options(table.font.names = "Source Sans Pro") %>% 
    fmt_number(columns = c(2:3, 7:8), decimals = 0) %>% 
    fmt_percent(columns = c(4:6), decimals = 0) 

  tx_tbl
  gtsave(tx_tbl, here(graphics, "NGA_FY20_self_assessment_im_perf_tx.png"))

    

      
    
