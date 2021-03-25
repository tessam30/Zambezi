# Purpose: Munge and Analysis of SI Branch LOE Trackah
# Author: Tim Essam | SI, 
# Date: 2020-02-19
# Notes:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(googlesheets4)  
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
    
  # Functions  
    
  # URL
    url <- "https://docs.google.com/spreadsheets/d/1mea71Z3QcYHLC6bwuWnw0bc93WSRNZX6wUvcjFlEIN4/edit?ts=602fbdd9#gid=0"  


# LOAD DATA ---------------------------------------------------------------

df <- tibble::tribble(
                         ~Name, ~`SIEI.Country.Support/Backstop`, ~SI.Technical.Liaison.Role, ~Core.Analytics, ~Country.Support.Cluster, ~HQ.SI.Techinical.support.Cluster,  ~HFR, ~CIGB, ~Analytics.Standards.WG, ~Capacity.Building.WG, ~Data.Fi, ~DDC, ~`External.engagements.(WHO,.UNAIDS)`, ~ICPI, ~Standing.Meetings, ~`Ad.hoc.Viz.&.Other.Request`, ~SI.Branch.Comm.and.Admin.Support, ~Technical.Leadership, ~`Professional.Development./.Training.(for.staff.w/.USG.req's,.e.g.,.FAITAS.CLP)`, ~SI.Lead.or.Engagement.in.OHA.Working.Groups, ~Please.Variable.add.as.needed,
               "Aaron Chafetz",                             0.07,                         NA,            0.33,                       NA,                                NA, 0.375,    NA,                    0.05,                    NA,       NA, 0.05,                                    NA,  0.01,             0.0375,                          0.08,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
           "Baboyma Kagniniwa",                              0.3,                         NA,             0.3,                       NA,                                NA,   0.2,    NA,                      NA,                    NA,       NA,  0.1,                                    NA,    NA,                0.2,                           0.1,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
           "Catherine Nichols",                              0.2,                        0.1,              NA,                       NA,                                NA,    NA,    NA,                      NA,                    NA,       NA,   NA,                                  0.05,  0.35,                0.2,                           0.1,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
              "Christy Knight",                               NA,                         NA,              NA,                       NA,                                NA,    NA,    NA,                      NA,                    NA,       NA,   NA,                                    NA,    NA,                 NA,                            NA,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
                "Cody Adelson",                             0.25,                       0.35,              NA,                       NA,                              0.02,    NA,    NA,                      NA,                    NA,       NA, 0.08,                                    NA,   0.1,               0.15,                          0.05,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
                "Emily Harris",                               NA,                         NA,              NA,                       NA,                                NA,    NA,    NA,                      NA,                    NA,      0.6,   NA,                                  0.05,    NA,                0.1,                            NA,                                NA,                   0.1,                                                                              0.05,                                           NA,                             NA,
                "Henry Miller",                               NA,                         NA,              NA,                       NA,                                NA,    NA,    NA,                      NA,                    NA,      0.5,   NA,                                    NA,    NA,                 NA,                          0.15,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
                "Jessica Rose",                              0.5,                        0.5,              NA,                       NA,                               0.2,    NA,    NA,                      NA,                    NA,       NA,   NA,                                    NA,    NA,                0.1,                           0.1,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
            "Jessica Stephens",                             0.15,                       0.15,              NA,                       NA,                                NA,    NA,    NA,                    0.05,                  0.05,       NA,   NA,                                    NA,  0.15,               0.15,                            NA,                               0.3,                    NA,                                                                                NA,                                           NA,                             NA,
            "Katya Noykhovich",                              0.5,                        0.2,            0.05,                       NA,                                NA,    NA,  0.05,                    0.05,                    NA,       NA,   NA,                                    NA,    NA,               0.15,                            NA,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
             "Lakeshia Watson",                             0.15,                        0.5,              NA,                       NA,                              0.03,    NA,  0.15,                      NA,                    NA,       NA,   NA,                                    NA,  0.15,               0.02,                          0.02,                                NA,                    NA,                                                                                NA,                                         0.07,                             NA,
          "Madeline Schneider",                              0.5,                        0.3,              NA,                     0.05,                                NA,    NA,    NA,                      NA,                    NA,       NA,   NA,                                    NA,  0.15,                 NA,                            NA,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
                    "Maria Au",                              0.6,                       0.15,              NA,                     0.02,                                NA,    NA,  0.05,                      NA,                    NA,       NA,   NA,                                  0.05,  0.02,               0.11,                            NA,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
             "Nashiva McDavid",                             0.15,                       0.25,              NA,                     0.02,                              0.03,  0.07,  0.15,                      NA,                   0.1,       NA, 0.05,                                    NA,  0.05,               0.08,                          0.05,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
               "Noah Bartlett",                             0.15,                        0.2,             0.3,                       NA,                                NA,    NA,    NA,                      NA,                    NA,       NA,   NA,                                    NA,    NA,               0.15,                           0.2,                                NA,                    NA,                                                                                NA,                                           NA,                             NA,
        "Reshma Bhattacharjee",                              0.5,                       0.25,              NA,                       NA,                                NA,    NA,    NA,                      NA,                   0.1,       NA,   NA,                                  0.05,  0.12,                0.1,                            NA,                                NA,                    NA,                                                                              0.03,                                           NA,                             NA,
                   "Tim Essam",                              0.3,                         NA,            0.33,                       NA,                                NA,  0.15,    NA,                    0.05,                    NA,     0.03,   NA,                                    NA,  0.01,               0.08,                          0.06,                                NA,                    NA,                                                                                NA,                                           NA,                             NA
)
    
    
    
    load_secrets()
  
  id <- 
    gs4_get(url) 

  
 df <-  read_sheet(id$spreadsheet_id)

 cw <- tibble::tribble(
                                                                               ~workstream,              ~wkstrm,
                                                           "SIEI.Country.Support/Backstop",         "OU Support",
                                                                          "Core.Analytics",                "CAC",
                                                                                     "HFR",                "HFR",
                                                                  "Analytics.Standards.WG",         "A-Stnds WG",
                                                                                     "DDC",                "DDC",
                                                                                    "ICPI",               "ICPI",
                                                                       "Standing.Meetings",           "Meetings",
                                                              "Ad.hoc.Viz.&.Other.Request",             "Ad hoc",
                                                               "SI.Technical.Liaison.Role",       "Tech Liaison",
                                                      "External.engagements.(WHO,.UNAIDS)",           "External",
                                                        "HQ.SI.Techinical.support.Cluster",            "HQ Tech",
                                                                                 "Data.Fi",            "Data.Fi",
                                                                    "Technical.Leadership",    "Tech Leadership",
          "Professional.Development./.Training.(for.staff.w/.USG.req's,.e.g.,.FAITAS.CLP)",           "Prof Dev",
                                                                    "Capacity.Building.WG",           "CAP Bldg",
                                                        "SI.Branch.Comm.and.Admin.Support",       "Comm & Admin",
                                                                                    "CIGB",               "CIGB",
                                             "SI.Lead.or.Engagement.in.OHA.Working.Groups",             "OHA WG",
                                                                 "Country.Support.Cluster", "OU Support Cluster"
         )

 
 
# MUNGE ============================================================================
  
  #  Reshape, tally totals for each person
 df_long <- 
   df %>% 
   pivot_longer(-Name,
                names_to = "workstream",
                values_to = "loe",
                values_drop_na = T) %>% 
   group_by(Name) %>% 
   mutate(loe_person = sum(loe, na.rm = T),
          loe_hours = loe * 40) %>% 
   group_by(workstream) %>% 
   mutate(loe_workstream = sum(loe_hours, na.rm = T)) %>%
   ungroup() %>% 
   mutate(total_loe = sum(available_loe = sum(loe_hours)),
          loe_share = loe_workstream / total_loe) %>% 
   left_join(., cw, by = c("workstream")) %>% 
   extract(., Name, c("First", "Last"), "([^ ]+) (.*)") %>% 
   mutate(short_name = paste0(First, " ", substr(Last, 1, 1)))
 
 df_long %>% distinct(wkstrm)
 
 
 
 df_long %>% 
   count(workstream, loe_workstream) %>% 
   arrange(desc(loe_workstream))
 
 df_long %>% 
   count(Name, loe_person) %>% 
   arrange(desc(loe_person))
 
  
# VIZ ============================================================================

  #  Where does most of staff time go?
 df_long %>% 
   group_by(wkstrm) %>% 
   summarise(across(c(loe_workstream, loe_share, total_loe), mean)) %>% 
   ungroup() %>% 
   mutate(workstream = fct_reorder(wkstrm, loe_share)) %>% 
   ggplot(aes(y = workstream, group = wkstrm)) +
   geom_col(aes(x = loe_workstream)) +
   geom_text(aes(x = loe_workstream, label = ifelse(loe_share > 0.01, percent(loe_share, 1), NA_real_)), hjust = 1.1, color = "white") +
   geom_vline(xintercept = c(40, 80, 120, 160), color = "white", linetype = "dotted") +
   si_style_yline() +
   labs(x = "level of effort in hours", y = NULL, title = "SI SPENDS OVER 1/4 OF TOTAL LOE PROVIDING COUNTRY SUPPORT") +
   coord_cartesian(expand = F, clip = "off")
 
 si_save(file.path(images, "SI_workstream_loe.png"))
   
  # Within each workstream, who is devoting the most time?
  df_long %>% 
    mutate(name_order = reorder_within(short_name, loe_hours, workstream),
           workstream = fct_reorder(wkstrm, loe_workstream, .desc = T)) %>% 
    ggplot() +
    geom_col(aes(y = name_order, x = loe_hours)) +
    geom_vline(xintercept = c(8, 16, 24, 32), color = "white", linetype = "dotted") +
    facet_wrap(~workstream, scales = "free_y") +
    scale_y_reordered() +
    si_style_xgrid() +
    labs(x = "level of effort in hours", y = NULL, title = "SI STAFF SPEND MOST OF THEIR TIME SUPPORTING OUS AND TECHNICAL TEAMS") +
    theme(panel.spacing = unit(0.5, "lines"))
  
  si_save(file.path(images, "SI_workstream_staff_summary.png"), scale = 1.45)
   
# Pivot around people  
  df_long %>% 
    mutate(name_order = reorder_within(short_name, loe_hours, workstream),
           workstream = reorder_within(wkstrm, loe_hours, short_name)) %>% 
    ggplot() +
    geom_col(aes(y = workstream, x = loe_hours)) +
    geom_vline(xintercept = c(8, 16, 24, 32), color = "white", linetype = "dotted") +
    facet_wrap(~short_name, scales = "free_y") +
    scale_y_reordered() +
    si_style_xgrid() +
    labs(x = "level of effort in hours", y = NULL, title = "SI STAFF SPEND MOST OF THEIR TIME SUPPORTING OUS AND TECHNICAL TEAMS") +
    theme(panel.spacing = unit(0.5, "lines")) 
 
  si_save(file.path(images, "SI_staff_summary.png"), scale = 1.45)
  
  df_expanded <- expand(df_long, short_name, wkstrm)
  
  df_long %>% 
    right_join(df_expanded) %>% 
    mutate(name_order = reorder_within(short_name, loe_hours, wkstrm),
           name = fct_reorder(short_name, loe_hours, .desc = T),
           workstream = fct_reorder(wkstrm, loe_workstream, .desc = T)) %>% 
    ggplot() +
    geom_tile(aes(y = name, x = workstream, fill = loe_hours), color = "white", size = 0.25) +
    geom_text(aes(y = name, x = workstream, label = percent(loe, 1.0), 
                  color = ifelse(loe > .20, "white", grey90k))) +
    si_style_nolines() +
    scale_x_discrete(guide = guide_axis(n.dodge = 3), position = "top") +
    scale_y_discrete(limits = rev) +
    scale_fill_si(na.value = "#edeeee", discrete = F) +
    scale_color_identity()  +
    labs(x = NULL, y = NULL, title = "MATRIX VIEW OF LOE BY STAFF AND WORKSTREAM",
         subtitle = "Staff LOE share in text. Box color represents total LOE hours.",
         fill = "Level of effort in hours") +
    theme(legend.key.width = unit(3, "line"), legend.key.height = unit(1, "line"),
          legend.justification = "left")
    
  si_save(file.path(images, "SI_staff_summary_matrix.png"), scale = 1.15)
    

  
# SPINDOWN ============================================================================

 write_csv(df_long, "tmp.csv")