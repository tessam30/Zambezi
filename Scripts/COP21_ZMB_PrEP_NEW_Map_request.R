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
      read_msd(here(copdata, "Genie-PSNUByIMs-Zambia-Daily-2021-02-04.zip")) %>% 
      reshape_msd(clean = T)
    

# MUNGE ============================================================================
  
  prep <- 
      genie %>% 
      filter(standardizeddisaggregate == "Total Numerator",
             fundingagency == "USAID") %>% 
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
    spread(period, total) %>% prinf()

  
  
  
  # Custom request for PrEP 
  disagg_regroup_m <- c("Age/Sex_Male_25-29", "Age/Sex_Male_30-34",
                        "Age/Sex_Male_35-39", "Age/Sex_Male_40-44",
                        "Age/Sex_Male_45-49", "Age/Sex_Male_50+")
  
  disagg_regroup_f <- c("Age/Sex_Female_25-29", "Age/Sex_Female_30-34",
                        "Age/Sex_Female_35-39", "Age/Sex_Female_40-44",
                        "Age/Sex_Female_45-49", "Age/Sex_Female_50+")

  # Pull out standard disaggs; KeyPop pulled out separately -- they will distort targets and totals
  prep_new_disag_kp <- 
    genie %>% 
      filter(indicator == "PrEP_NEW",
             fundingagency == "USAID",
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
    unite(., "period_combo", c(period, period_type), remove = F) %>% 
    filter(!period_combo %in% c("FY21Q1_results", "FY20Q2_results", "FY20Q4_results"))
  

  prep_new_disag <- 
    genie %>% 
    filter(indicator == "PrEP_NEW",
           fundingagency == "USAID",
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
    unite(., "period_combo", c(period, period_type), remove = F) %>% 
    filter(!period_combo %in% c("FY21Q1_results", "FY20Q2_results", "FY20Q4_results"))
  
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
           )


  prep_new_disag %>% 
    group_by(period, period_type) %>% 
    summarise(total = sum(total, na.rm = TRUE))

  
    
    
  
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
  prep_new_21 %>% 
    st_drop_geometry() %>% 
    mutate(targets_sort = ifelse(period_type == "targets", total, NA_real_)) %>% 
    group_by(psnu) %>% 
    fill(., targets_sort, .direction = "updown") %>% 
    mutate(group_count = n()) %>% 
    ungroup() %>% 
    complete(.,  ) %>% View()
    
    
    
    mutate(targets_sort = if_else(is.na(targets_sort), total, targets_sort),
      district_order = reorder_within(psnu, targets_sort, within = snu1)) %>% 
    ggplot() +
    geom_point(data = . %>% filter(period_type == "targets"), aes(x = total, y = district_order),
               shape = 21, color = grey30k, size = 3, fill = trolley_grey_light) +
    # scale_fill_si(palette = "trolley_greys", discrete = FALSE) +
    # new_scale_fill() +
    geom_point(data = . %>% filter(period_type == "cumulative"), 
               aes(x = total, y = district_order),
               shape = 21, color = grey30k, size = 3, fill = genoa, alpha = 0.75) +
    scale_fill_si(palette = "moody_blues", discrete = FALSE) +
    si_style_xgrid() +
    facet_wrap(~snu1, scales = "free") +
    scale_y_reordered() 

  
  terr_map +
      geom_sf(data = prep_new, aes(fill = total), color = "white", size = 0.5) +  
      scale_fill_si(palette = "denims", 
      discrete = FALSE, 
      na.value = grey10k) +
    facet_wrap(period_type~indicator) 

  
  

# NEW REQUEST -------------------------------------------------------------

  # Hi Tim, could you replicate this with prep curr but streamline with achievements and targets in the same bar? 
  # Q2 fy20 results against targets one bar , Apr fy20 results against targets another bar, q1 results against targets 
  # a third bar? Same age sex disaggs?  
  max <- max(prep_new_disag_all$total)
  
  prep_new_disag_all %>% 
    select(-period_combo) %>% 
    spread(period_type, total) %>% 
    mutate(Achievement = cumulative / targets,
           dotted_line = if_else(Achievement > 1, "white", trolley_grey_light)) %>% 
    ggplot(aes(x = period)) + 
      geom_col(aes(y = targets), fill = grey10k) +
      geom_col(aes(y = cumulative), 
               fill = genoa, alpha = 0.75) +
    geom_text(aes(y = cumulative, label = comma(cumulative)), vjust = -0.25, 
              color = genoa,
              fontface = "bold") +
    geom_text(data = . %>% filter(period == "FY21"),
      aes(y = targets, label = comma(targets)), vjust = -0.5, 
      color = trolley_grey) +
    geom_errorbar(aes(ymin = targets, 
                      ymax = targets, 
                      colour = dotted_line), 
                  size = 0.5, 
                  linetype = "dashed") +
      si_style_xline() +
      scale_y_continuous(labels = comma, expand = c(.01, 0),
                         limits = c(0, max+ 1000)) +
    facet_wrap(~disags, nrow = 1, strip.position = "bottom") +
    scale_color_identity() +
    theme(
      strip.placement = "outside",
      strip.text = element_text(hjust = 0.5),
      axis.text.y = element_blank(),
      plot.title = element_markdown(family = "Source Sans Pro Regular"),
      plot.subtitle = element_markdown(size = 11, family = "Source Sans Pro"),
      text = element_text(family = "Source Sans Pro")
    ) +
    labs(x = NULL, y = NULL, title = "PrEP_NEW <span style = 'color:#287c6f;'>**RESULTS**</span> AND 
         <span style = 'color:#808080;'>**TARGETS**</span> BY AGE BANDS FOR USAID",
         caption = "Source: Genie pull as of 2020-02-04")
         # subtitle = "<span style = 'color:#287c6f;'>**Results in green**</span><span style = 'color:#808080;'>
         # <br>Targets in grey</span style>")
  
  si_save(here(images, "ZMB_PrEP_NEW_disaggs_summary.png"),
          scale = 1.25)
    
    
    prep_new_disag %>% 
      group_by(period, period_type) %>% 
      summarise(total = sum(total, na.rm = TRUE))
      
      

  
  
    
    
  



  

# SPINDOWN ============================================================================


