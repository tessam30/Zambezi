# Purpose: Munge and Analysis of
# Author: Tim Essam | SI, 
# Date: 2020-02-04
# Notes: Map of PrEP NEW coverage as of Q1 FY21 similar to the attached (left), 
# but for Zambia and for results, not targets. I know maps take a bit of work, 
# but if it's doable, could we compare it to a map on the right showing PrEP NEW
# targets for FY21? Point would be to show progress against targets in a more interesting way.

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    #library(patchwork)
    library(ggtext)
    library(here)
    library(ggnewscale)
    
    source("./Scripts/Z00_Config.R")
    source("./Scripts/Z01_fetch_spdfs.R")
    `%nope%` <- Negate(`%in%`)
    
  
  # Set paths  
    data    <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"  
    copdata <- "Data/COP21"
    cop_out <- "Dataout/COP21"
    
  # Functions  

# LOAD DATA ============================================================================  
  
  # Grab latest genie extract filtered to only PrEP variables
    genie <- 
      read_msd(here(copdata, "Genie_PSNU_IM_Zambia_Daily_31c3786f-40c3-4060-a29b-4ee65c9fdc31.txt")) %>% 
      reshape_msd(clean = T)

# PrEP_CURR SLIDE REPRODUCTION --------------------------------------------

  # Pull out standard disaggs; KeyPop pulled out separately -- they will distort targets and totals
  # Oddly, the stddisag is KeyPop for PrEP_CURR and KeyPopAbr for NEW
  
  # Custom request for PrEP 
  disagg_regroup_m <- c("Age/Sex_Male_25-29", "Age/Sex_Male_30-34",
                        "Age/Sex_Male_35-39", "Age/Sex_Male_40-44",
                        "Age/Sex_Male_45-49", "Age/Sex_Male_50+")
  
  disagg_regroup_f <- c("Age/Sex_Female_25-29", "Age/Sex_Female_30-34",
                        "Age/Sex_Female_35-39", "Age/Sex_Female_40-44",
                        "Age/Sex_Female_45-49", "Age/Sex_Female_50+")

  # PrEP_CURR
  prep_curr_disag_kp <- 
    genie %>% 
      filter(indicator == "PrEP_CURR",
             standardizeddisaggregate %in% c("KeyPop")) %>%
      unite(combo, c(standardizeddisaggregate, otherdisaggregate), remove = F) %>% 
      filter(str_detect(combo, "(FSW|MSM)")) %>% 
      mutate(disags = case_when(
        combo == "KeyPop_FSW" ~ "FSW",
        combo == "KeyPop_MSM" ~ "MSM",
        TRUE ~ NA_character_
      ), 
      core_disag = 0) %>% 
    mutate(core_disag = 0) %>% 
    group_by(disags, period, period_type, core_disag) %>% 
    summarise(total = sum(val, na.rm = TRUE)) %>% 
    ungroup() %>% 
    unite(., "period_combo", c(period, period_type), remove = F)
    # filter(!period_combo %in% c("FY21Q1_results", "FY20Q2_results", "FY20Q4_results"))
  

  prep_curr_disag <- 
    genie %>% 
    filter(indicator == "PrEP_CURR",
           standardizeddisaggregate %in% c("Age/Sex")) %>% 
    unite(combo, c(standardizeddisaggregate, sex, ageasentered), remove = F) %>%
      mutate(disags = case_when(
      combo == "Age/Sex_Female_15-19" ~ "F 15-19",
      combo == "Age/Sex_Male_15-19" ~ "M 15-19",
      combo == "Age/Sex_Female_20-24" ~ "F 20-24",
      combo == "Age/Sex_Male_20-24" ~ "M 20-24",
      combo %in% disagg_regroup_m ~ "M 25-50+",
      combo %in% disagg_regroup_f ~ "F 25-50+",
      TRUE ~ combo)
      ) %>% 
    mutate(core_disag = 1) %>% 
    group_by(disags, period, period_type, core_disag) %>% 
    summarise(total = sum(val, na.rm = TRUE)) %>% 
    ungroup() %>% 
    unite(., "period_combo", c(period, period_type), remove = F) 
    #filter(!period_combo %in% c("FY21Q1_results", "FY20Q2_results", "FY20Q4_results"))
  
  prep_curr_disag_all <- 
    prep_curr_disag %>% 
    bind_rows(prep_curr_disag_kp) %>% 
    mutate(disags = fct_relevel(disags,
                            "M 15-19", 
                            "F 15-19",
                            "M 20-24",
                            "F 20-24",
                            "M 25-50+",
                            "F 25-50+",
                            "FSW",
                            "MSM")
           ) %>% 
    filter(period_type != "cumulative") %>% 
    spread(period_type, total) %>% # Set targets to repeat in same row as results
    mutate(fy = str_extract(period, "\\d{2}")) %>% 
    group_by(disags, fy) %>% 
    fill(., targets, .direction ="down") %>% 
    filter(!is.na(results))


  # prep_curr_disag_all %>% 
  #   group_by(period_combo, disags) %>% 
  #   summarise(total = sum(total, na.rm = TRUE))

 ### PREP_NEW Graph
  prep_new_disag_kp <- 
    genie %>% 
    filter(indicator == "PrEP_NEW",
           standardizeddisaggregate %in% c("KeyPopAbr")) %>%
    unite(combo, c(standardizeddisaggregate, otherdisaggregate), remove = F) %>% 
    filter(str_detect(combo, "(FSW|MSM)")) %>% 
    mutate(disags = case_when(
      combo == "KeyPopAbr_FSW" ~ "FSW",
      combo == "KeyPopAbr_MSM" ~ "MSM",
      TRUE ~ NA_character_
    ), 
    core_disag = 0) %>% 
    mutate(core_disag = 0) %>% 
    group_by(disags, period, period_type, core_disag) %>% 
    summarise(total = sum(val, na.rm = TRUE)) %>% 
    ungroup() %>% 
    unite(., "period_combo", c(period, period_type), remove = F)
  
  prep_new_disag <- 
    genie %>% 
    filter(indicator == "PrEP_NEW",
           standardizeddisaggregate %in% c("Age/Sex")) %>% 
    unite(combo, c(standardizeddisaggregate, sex, ageasentered), remove = F) %>%
    mutate(disags = case_when(
      combo == "Age/Sex_Female_15-19" ~ "F 15-19",
      combo == "Age/Sex_Male_15-19" ~ "M 15-19",
      combo == "Age/Sex_Female_20-24" ~ "F 20-24",
      combo == "Age/Sex_Male_20-24" ~ "M 20-24",
      combo %in% disagg_regroup_m ~ "M 25-50+",
      combo %in% disagg_regroup_f ~ "F 25-50+",
      TRUE ~ combo)
    ) %>% 
    mutate(core_disag = 1) %>% 
    group_by(disags, period, period_type, core_disag) %>% 
    summarise(total = sum(val, na.rm = TRUE)) %>% 
    ungroup() %>% 
    unite(., "period_combo", c(period, period_type), remove = F) 
  
  prep_new_disag_all <- 
    prep_new_disag %>% 
    bind_rows(prep_new_disag_kp) %>% 
    mutate(disags = fct_relevel(disags,
                                "M 15-19", 
                                "F 15-19",
                                "M 20-24",
                                "F 20-24",
                                "M 25-50+",
                                "F 25-50+",
                                "FSW",
                                "MSM")
    ) %>% 
    filter(period_type != "cumulative") %>% 
    spread(period_type, total) %>% # Set targets to repeat in same row as results
    mutate(fy = str_extract(period, "\\d{2}")) %>% 
    group_by(disags, fy) %>% 
    fill(., targets, .direction ="down") %>% 
    filter(!is.na(results))
    

# PrEP VIZ REQUEST -------------------------------------------------------------

  # Hi Tim, could you replicate this with prep curr but streamline with 
  # achievements and targets in the same bar? Q2 fy20 results against 
  # targets one bar , Apr fy20 results against targets another bar, q1 
  # results against targets a third bar? Same age sex disaggs?  
  
  # Set max to prevent y-axis and labels from being truncated
  max <- max(prep_curr_disag_all$targets) + 3000
  
  prep_curr_disag_all %>% 
    #select(-period_combo) %>% 
    #spread(period_type, total) %>% 
    mutate(Achievement = results / targets,
           dotted_line = if_else(Achievement > 1, "white", trolley_grey_light)) %>% 
    ggplot(aes(x = period)) + 
      geom_col(aes(y = targets), fill = trolley_grey_light) +
      geom_col(aes(y = results), 
               fill = genoa, alpha = 1) +
    geom_text(aes(y = results, label = comma(results, accuracy = 1)), vjust = -0.25, 
              color = genoa,
              fontface = "bold", 
              size = 3) +
    facet_wrap(~disags, nrow = 1, strip.position = "bottom") +
    geom_text(data = . %>% filter(fy == "21", disags != "M 25-50+"), 
      aes(y = targets, label = comma(targets)), vjust = -0.5, 
      color = trolley_grey, 
      size = 3) +
    geom_errorbar(aes(ymin = targets, 
                      ymax = targets, 
                      colour = dotted_line), 
                  size = 0.5, 
                  linetype = "dashed") +
        si_style_xline() +
      scale_y_continuous(labels = comma, expand = c(.01, 0),
                         limits = c(0, max)) +
    facet_wrap(~disags, nrow = 1, strip.position = "bottom") +
    scale_color_identity() +
    theme(
      strip.placement = "outside",
      strip.text = element_text(hjust = 0.5, color = trolley_grey),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 9, color = trolley_grey),
      # plot.title = element_markdown(family = "Source Sans Pro Regular", size = 14),
      # plot.subtitle = element_markdown(size = 12, family = "Source Sans Pro"),
      # text = element_text(family = "Source Sans Pro"),
      panel.spacing.x = unit(0.15, "cm"),
      
    ) +
    labs(x = NULL, y = NULL)
    
    # , title = "PrEP_CURR <span style = 'color:#287c6f;'>**RESULTS**</span> AND 
    #      <span style = 'color:#808080;'>**TARGETS**</span> BY AGE BANDS FOR USAID",
    #      caption = "Source: Genie pull as of 2020-02-04",
    #      subtitle = "White dotted lines represent targets")
  
  si_save(here(images, "ZMB_PrEP_CURR_disaggs_summary.png"),
          scale = 1.25, dpi = "retina")
  
  prep_curr_disag_all %>% 
    filter(!disags %in% c("MSM", "FSW")) %>% 
    group_by(period) %>% 
    summarise(total = sum(results, na.rm = TRUE))
  
  
# ------------------------------------------------------------------------------  
  # PREP NEW
  max_new <- max(prep_new_disag_all$targets) + 3000
  
  prep_new_disag_all %>% 
    #select(-period_combo) %>% 
    #spread(period_type, total) %>% 
    mutate(Achievement = results / targets,
           dotted_line = if_else(Achievement > 1, "white", trolley_grey_light)) %>% View()
    ggplot(aes(x = period)) + 
    geom_col(aes(y = targets), fill = trolley_grey_light) +
    geom_col(aes(y = results), 
             fill = genoa, alpha = 1) +
    geom_text(aes(y = results, label = comma(results, accuracy = 1)), vjust = -0.25, 
              color = genoa,
              fontface = "bold", 
              size = 3) +
    facet_wrap(~disags, nrow = 1, strip.position = "bottom") +
    geom_text(data = . %>% filter(fy == "21"), 
              aes(y = targets, label = comma(targets)), vjust = -0.5, 
              color = trolley_grey, 
              size = 3) +
    geom_errorbar(aes(ymin = targets, 
                      ymax = targets, 
                      colour = dotted_line), 
                  size = 0.5, 
                  linetype = "dashed") +
    si_style_xline() +
    scale_y_continuous(labels = comma, expand = c(.01, 0),
                       limits = c(0, max_new)) +
    facet_wrap(~disags, nrow = 1, strip.position = "bottom") +
    scale_color_identity() +
    theme(
      strip.placement = "outside",
      strip.text = element_text(hjust = 0.5, color = trolley_grey),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 9, color = trolley_grey),
      plot.title = element_markdown(family = "Source Sans Pro Regular", size = 14),
      plot.subtitle = element_markdown(size = 12, family = "Source Sans Pro"),
      text = element_text(family = "Source Sans Pro"),
      panel.spacing.x = unit(0.15, "cm"),
    ) +
    labs(x = NULL, y = NULL,
    title = "PrEP_NEW <span style = 'color:#287c6f;'>**RESULTS**</span> AND
    <span style = 'color:#808080;'>**TARGETS**</span> BY AGE BANDS FOR USAID",
              caption = "Source: Genie pull as of 2020-02-04",
              subtitle = "White dotted lines represent targets")
         
  si_save(here(images, "ZMB_PrEP_NEW_disaggs_summary.png"),
          scale = 1.25, dpi = "retina")
  
  # Checking disaggs to make sure they add up
  prep_new_disag_all %>% 
    filter(!disags %in% c("MSM", "FSW")) %>% 
    group_by(period) %>% 
    summarise(total = sum(results, na.rm = TRUE))
  
 
# MUNGE ============================================================================
  
  prep <- 
    genie %>% 
    filter(standardizeddisaggregate == "Total Numerator") %>% 
    clean_psnu()
  
  # Prep data to get results by quarter to show progress  
  prep %>% count(period_type, period, indicator)
  
  # Check overlap
  setdiff(spdf_comm_zmb %>% distinct(psnu) %>% pull(psnu), prep %>% distinct(psnu) %>% pull(psnu))
  
  # Join everything to PEPFAR polygons
  prep_geo <- 
    spdf_comm_zmb %>% 
    full_join(., prep, by = c("uid" = "psnuuid")) %>% 
    select(-contains(".y")) %>% 
    rename_if(endsWith(names(.), ".x"), ~str_remove_all(., ".x"))
  
  prep_geo %>% 
    st_drop_geometry() %>% 
    group_by(period_type, period, indicator, uid, psnu) %>% 
    summarise(total = sum(val, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(period, total) %>% arrange(psnu) %>% prinf()
   
# VIZ ============================================================================
  
  #Crate a map showing PrEP_NEW coverage as of Q1 FY21 for results and show targets on right
  prep_new_21 <- 
    prep_geo %>% 
    filter(indicator == "PrEP_NEW") %>% 
    group_by(period_type, period, indicator, uid, psnu, snu1) %>% 
    summarise(total = sum(val, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(str_detect(period, "FY21"), period_type != "results")
  
  # Let's break this out into two layers with different color codings for each
  prep_new_psnu <- 
    prep_new_21 %>% 
    st_drop_geometry() %>% 
    mutate(targets_sort = ifelse(period_type == "targets", total, NA_real_)) %>% 
    group_by(psnu) %>% 
    fill(., targets_sort, .direction = "updown") %>% 
    mutate(group_count = n()) %>% 
    filter(period_type != "targets", !is.na(psnu)) %>%  
    ungroup() %>% 
    mutate(targets_sort = if_else(is.na(targets_sort), total, targets_sort),
           district_order = reorder_within(psnu, targets_sort, within = snu1),
           dot_color = if_else(total < targets_sort, genoa_light, genoa),
           target_line = if_else(total > targets_sort, "white", "NA"),
           achievement = if_else(total > targets_sort, genoa, genoa_light)) 
  
  # Dot Plot
    ggplot(data = prep_new_psnu) +
    geom_segment(aes(x = total, xend = targets_sort, y = district_order, yend = district_order), 
                 color = trolley_grey_light) +
    geom_point(aes(x = targets_sort, y = district_order),
               shape = 21, color = trolley_grey, size = 3, fill = trolley_grey_light) +
    geom_point(aes(x = total, y = district_order, fill = dot_color),
               shape = 21, size = 3, color = trolley_grey) +
    scale_fill_identity() +
    si_style_xgrid() +
    facet_wrap(~snu1, scales = "free", nrow = 2) +
    scale_y_reordered() +
    labs(x = NULL, y = NULL, title = "PrEP_NEW Results to Targets for FY21")
  
  # Bar graph rotated
  prep_new_psnu_bar <- 
    ggplot(data = prep_new_psnu) +
      geom_col(aes(x = targets_sort, y = district_order), fill = grey10k,
               position = position_dodge2(preserve = c("single"))) +
      geom_col(aes(x = total, y = district_order, fill = achievement),
               position = position_dodge2(preserve = c("single"))) +
      geom_errorbar(aes(xmin = targets_sort,
                        xmax = targets_sort,
                        y = district_order,
                        colour = target_line),
                    size = 0.5,
                    linetype = "dashed") +
      geom_vline(xintercept = 0, color = grey70k, size = 0.5)+
      scale_color_identity() +
      scale_fill_identity() +
      si_style_xgrid() +
      facet_wrap(~snu1, scales = "free", ncol = 2) +
      scale_y_reordered() +
      scale_x_continuous(labels = comma) +
      labs(x = NULL, y = NULL, title = "PrEP_NEW Results to Targets for FY21",
           source = "Genie pull as of 2020-02-04") +
      theme(
        panel.spacing.x = unit(0.05, "cm"),
        panel.spacing.y = unit(0.05, "cm"),
      )
    
  si_save(here(images, "ZMB_PrEP_NEW_psnu_achievement.png"),
          scale = 1.75, dpi = "retina",
          plot = prep_new_psnu_bar,
          height = 5.63,
          width = 4.63)  
    
    
  # Keep geometry, add achievement
    prep_new_psnu_geo <- 
      prep_new_21 %>% 
      mutate(targets_sort = ifelse(period_type == "targets", total, NA_real_)) %>% 
      group_by(psnu) %>% 
      fill(., targets_sort, .direction = "updown") %>% 
      mutate(group_count = n()) %>% 
      filter(!is.na(psnu)) %>%  
      ungroup() %>% 
      mutate(achievement = total/targets_sort) 
  
    
  # There are quite a few psnus that do not have targets
  terr_map +
    geom_sf(data = prep_new_psnu_geo, aes(fill = log(total)), color = "white", size = 0.5) +
    scale_fill_viridis_c(option = "D", direction = -1) +
    facet_wrap(~period_type) 
  
  
  #Admin 1 with labels
  adm1_labels <- left_join(spdf_reg_zmb, 
                           genie %>% distinct(snu1, snu1uid), 
                           by = c("uid" = "snu1uid")) %>% 
    mutate(province = str_remove_all(snu1, " Province")) %>% 
    mutate(
      CENTROID = map(geometry, st_centroid),
      COORDS = map(CENTROID, st_coordinates),
      COORDS_X = map_dbl(COORDS, 1),
      COORDS_Y = map_dbl(COORDS, 2)
    )
  
  achievement_map <- 
  terr_map +
    geom_sf(data = prep_new_psnu_geo %>% filter(period_type == "cumulative"), 
            aes(fill = achievement), color = "white", size = 0.5) +
    geom_sf(data = spdf_ou_zmb, color = grey90k, size = 0.75, fill = NA) +
    geom_sf(data = spdf_reg_zmb, color = grey90k, size = 0.6, fill = NA) +
    ggrepel::geom_label_repel(data = adm1_labels, 
                  aes(label = province,
                      x = COORDS_X,
                      y = COORDS_Y,), 
                  fill = alpha(c("white"), 0.75),
                  force = 6,
                  label.size = NA,
                  label.padding = 0.25,
                  family = "Source Sans Pro Light") +
    scale_fill_si(palette = "genoas", discrete = FALSE, 
                  alpha = 0.75,
                  na.value = grey20k,
                  limits = c(0, 1),
                  labels = percent,
                  oob = scales::oob_squish) +
    si_style_map()
  achievement_map
  
  si_save(here(images, "ZMB_PrEP_NEW_achievement_map.png"), plot = achievement_map, scale = 1.25)
  
  
  library(patchwork)
  achievement_map / prep_new_psnu_bar
  
  
# SPINDOWN ============================================================================


