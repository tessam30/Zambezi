# Purpose: Verify OPU and target shifts across provinces from GENIE
# Author: Tim Essam | SI, 
# Date: 2020-02-08
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
    library(tameDP)  
    library(ICPIutilities)
    library(ggfittext)
    library(gt)
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"  
  
    source("./Scripts/Z00_Config.R")
    source("./Scripts/Z01_fetch_spdfs.R")

      
  # Functions  

# LOAD DATA ============================================================================  
  
  # Load latest genie pull for OVC_SERVE
    genie_file <- return_latest(folderpath = "Data/COP21/", pattern = "2021-02-08_OVC_SERVE.zip")
    dp_file <- return_latest(folderpath = "Data/COP21/", pattern = "Data Pack|$.xlsx")  
      
    opu <- read_msd(genie_file)  
      opu %>% count(mech_code)
  
    df_dp <- import_dp(dp_file)
    df_dp %>% names()
    
# MUNGE ============================================================================
  
  #filter out other indicators; Only look at OVC_SERV and OVC_HIVSTAT
    df_dp_ovc <- 
      df_dp %>% 
      filter(str_detect(indicator_code, "OVC_SERV|OVC_HIVSTAT")) %>% 
      separate(psnu, into = c("psnu","district", "type", "psnuuid"), sep = " ") %>% 
      mutate(type = str_remove_all(type, "\\[|\\]"),
             psnuuid = str_remove_all(psnuuid, "\\[|\\]")) %>% 
      left_join(., spdf_comm_zmb %>% st_drop_geometry(), by = c("psnuuid" = "uid", "psnu" = "psnu"))
    
  
  # Grab All mechs that compare old and new targets
    df_dp_ovc_totals <- df_dp_ovc %>% 
      select(snu1, psnu, indicator_code, age, sex, keypop,
             tot_dedup_rollup_orig = `total deduplicated rollup...6`,
             tot_dedup_rollup =`total deduplicated rollup...92`, 
             mech_85114_old = `85114_dsd...31`, 
             mech_85115_old = `85115_dsd...32`,
             mech_18487_old =`18487_dsd...34`,
             mech_85114_new = `85114_dsd...121`,
             mech_85120_new = `85120_dsd`,
             mech_85121_new = `85121_dsd`,
             mech_85115_new = `85115_dsd...122`,
             mech_18487_new = `18487_dsd...124`
             ) %>% 
      mutate(across(.cols = tot_dedup_rollup_orig:mech_18487_new, as.numeric))
  
    df_dp_ovc %>% distinct(snu1, indicator_code) %>% arrange(snu1) %>% relocate(snu1, indicator_code)
    
    
    df_dp_ovc_totals %>% 
      filter(indicator_code == "OVC_HIVSTAT.N.total.T") %>% 
    write_csv(file.path("Dataout/COP21/ZMB_datapack_OVC_targets.csv"))
      
  
# VIZ ============================================================================

  #  Depict changes from 85114 to mechs 85120 and 85121
  # Show spatial shift as well as distribution by psnus
    tgt_shift_85114 <- 
      df_dp_ovc_totals %>% 
      select(snu1, psnu, 
             indicator_code, age, 
             sex, keypop, 
             tot_dedup_rollup_orig, 
             tot_dedup_rollup, 
             matches("85114|85120|85121")) %>% 
      group_by(snu1, psnu, indicator_code) %>% 
      summarise(across(.cols = mech_85114_old:mech_85121_new, sum, na.rm = T)) %>%
      ungroup() %>% 
      pivot_longer(cols = -c(snu1, psnu, indicator_code),
                   names_to = "mech",
                   values_to = "targets") %>% 
      arrange(mech) %>% 
      mutate(mech_order = fct_relevel(mech, 
                                      "mech_85114_old",
                                      "mech_85114_new",
                                      "mech_85120_new",
                                      "mech_85121_new"),
             indicator = case_when(
               indicator_code == "OVC_HIVSTAT.N.total.T" ~ "OVC_HIVSTAT",
               indicator_code == "OVC_SERV.N.Age_Sex_ProgramStatus.T.Active" ~ "OVC_SERV Active",
               indicator_code == "OVC_SERV.N.Age_Sex_ProgramStatus.T.Graduated" ~ "OVC_SERV Graduated" 
             )
      )
           
  
  # Check totals
  # Print off for verification of the OPU
    tgt_shift_85114 %>% 
      group_by(indicator, mech_order) %>% 
      summarise(targets = sum(targets)) %>% 
      spread(mech_order, targets) %>%
      rowwise() %>% 
      mutate(new_mechs_total = sum(c_across(mech_85114_new:mech_85121_new))) %>% 
      ungroup() %>% 
      mutate(grand_total = sum(new_mechs_total)) %>% 
      gt() %>% 
      fmt_number(columns = 2:7, 
                 decimals = 0)
  
  # Bring in shapefile for mapping
  tgt_shift_85114_geo <- right_join(spdf_comm_zmb, tgt_shift_85114 %>% filter(targets != 0))

  # Checking distribution for applying fill and transformation
  summary(tgt_shift_85114_geo$targets)
  
  # MAP of target shifts for 85114
      terr_map +
      geom_sf(data = tgt_shift_85114_geo, aes(fill = targets), 
              color = "white", stroke = 0.25) +
      scale_fill_viridis_c(option = "A", direction = -1, 
                           #trans = "log",
                           #breaks = c(250, 500, 1000, 2500, 5000, 10000, 30000),
                           labels = comma) +
      facet_wrap(indicator ~ mech_order, nrow = 3) +
      theme(legend.position = "top") +
        guides(fill = guide_colorbar(title.position = 'top', 
                                     title.hjust = 0.5, 
                                     barwidth = unit(20, 'lines'), 
                                     barheight = unit(1, 'lines'))) +
      labs(x = NULL, y = NULL) +
      si_save(file.path(images, "ZMB_OPU_target_shift_85114_map.png"),
              height = 5.625, 
              width = 6.66, 
              dpi = "retina", 
              scale = 1.33)
      
  
  # BAR Graph of target shifts by PSNU to show spatial distribution
    tgt_shift_85114 %>% 
      ungroup() %>% 
      filter(mech_order != "mech_85114_old", targets >0) %>% 
      mutate(psnu_order = reorder_within(psnu, targets, mech_order)) %>% 
      ggplot(aes(x = targets, y = psnu_order, fill = targets)) + 
      geom_col() +
      geom_bar_text(aes(label = comma(targets, 1)), 
                    contrast = T, 
                    min.size = 2) +
      scale_fill_viridis_c(option = "A", direction = -1, 
                           #trans = "log",
                           #breaks = c(250, 500, 1000, 2500, 5000, 10000, 30000),
                           labels = comma) +
      facet_wrap(indicator ~ mech_order, scales = "free") +
      scale_y_reordered() +
      si_style_yline() +
      labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_blank(),
            legend.position = "none",
            text = element_text(size = 8),
            panel.spacing.x = unit(0.05, "cm"),
            panel.spacing.y = unit(0.05, "cm")) +
      coord_cartesian(expand = FALSE) +
      si_save(file.path(images, "ZMB_OPU_target_shift_85114_bar.png"),
              dpi = "retina", scale = 1.25)
    
    # patchwork didn't work due to spacing around map
  
# Second TARGET SHIFT -- 85115
  # Depict changes from 85115 to 18487
   
    tgt_shift_85115 <- 
      df_dp_ovc_totals %>% 
      filter(str_detect(indicator_code, "OVC_SERV|OVC_HIVSTAT")) %>% 
      select(snu1, psnu, indicator_code, 
             age, sex, keypop, 
             tot_dedup_rollup_orig, 
             tot_dedup_rollup, 
             matches("85115|18487")) %>% 
      group_by(snu1, psnu, indicator_code) %>% 
      summarise(across(.cols = mech_85115_old:mech_18487_new, sum, na.rm = T)) %>%
      ungroup() %>% 
      pivot_longer(cols = -c(snu1, psnu, indicator_code),
                   names_to = "mech",
                   values_to = "targets") %>% 
      arrange(mech) %>% 
      mutate(mech_order = fct_relevel(mech, 
                                      "mech_85115_old",
                                      "mech_85115_new",
                                      "mech_18487_old",
                                      "mech_18487_new"),
             indicator = case_when(
               indicator_code == "OVC_HIVSTAT.N.total.T" ~ "OVC_HIVSTAT",
               indicator_code == "OVC_SERV.N.Age_Sex_ProgramStatus.T.Active" ~ "OVC_SERV Active",
               indicator_code == "OVC_SERV.N.Age_Sex_ProgramStatus.T.Graduated" ~ "OVC_SERV Graduated" 
             )
      )
  
  
  # Visualize Target Shifts
  # Throwing into gt for quick formatting and spitting out
    tgt_shift_85115 %>% 
      filter(str_detect(indicator, "OVC_SERV Gra", negate = T)) %>% 
      group_by(indicator, mech_order) %>% 
      summarise(targets = sum(targets)) %>% 
      spread(mech_order, targets) %>% 
      relocate(mech_18487_old, .after = mech_85115_old)%>%
      rowwise() %>% 
      mutate(old_totals = sum(c_across(mech_85115_old:mech_18487_old))) %>% 
      ungroup() %>% 
      relocate(old_totals, .after = mech_18487_old)%>% 
      mutate(grand_total = sum(mech_18487_new)) %>% 
      gt() %>% 
      fmt_number(columns = 2:7, 
                 decimals = 0)
  
  
  # Joining in spatial data for mapping
    tgt_shift_85115_geo <-  right_join(spdf_comm_zmb, tgt_shift_85115)
  
  
  
    terr_map +
    geom_sf(data = tgt_shift_85115_geo %>%
              filter(str_detect(indicator, "(OVC_SERV Active|HIVSTAT)"), 
                     targets > 0), 
            aes(fill = targets), 
            color = "white", 
            stroke = 0.25) +
    scale_fill_viridis_c(option = "A", direction = -1, 
                         #trans = "log",
                         #breaks = c(250, 500, 1000, 2500, 5000, 10000, 30000),
                         labels = comma) +
      facet_wrap(indicator ~ mech_order) +
    theme(legend.position = "none") +
      theme(legend.position = "top") +
      guides(fill = guide_colorbar(title.position = 'top', 
                                   title.hjust = 0.5, 
                                   barwidth = unit(20, 'lines'), 
                                   barheight = unit(1, 'lines'))) +
    labs(x = NULL, y = NULL) +
    si_save(file.path(images, "ZMB_OPU_target_shift_85115_map.png"),
            dpi = "retina", 
            scale = 1.33)
  
    

  # Bar graph to show distribution of shifts for 85115  
    tgt_shift_85115 %>% 
      ungroup() %>% 
      filter(str_detect(mech, "old", negate = T),
             str_detect(indicator, "OVC_SERV Graduated", negate = T),
             targets > 0) %>% 
      mutate(psnu_order = reorder_within(psnu, targets, mech_order)) %>% 
      ggplot(aes(x = targets, y = psnu_order, fill = targets)) + 
      geom_col() +
      geom_bar_text(aes(label = comma(targets, 1)), contrast = T) +
      scale_fill_viridis_c(option = "A", direction = -1, 
                           #trans = "log",
                           #breaks = c(250, 500, 1000, 2500, 5000, 10000, 30000),
                           labels = comma) +
      facet_wrap(indicator ~ mech_order, scales = "free") +
      scale_y_reordered() +
      si_style_yline() +
      labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_blank(),
            legend.position = "none",
            text = element_text(size = 8)) +
      coord_cartesian(expand = FALSE) +
      si_save(file.path(images, "ZMB_OPU_target_shift_85115_bar.png"),
              dpi = "retina", scale = 1.33)

# SPINDOWN ============================================================================

    # NOT USED -- use 
    # Try ggalluvial plot
    
    library(ggalluvial)
    df <- tibble::tribble(
                ~indicator,        ~mech,    ~new_mech, ~target,
             "OVC_HIVSTAT", "mech_85114", "mech_85114",   61360,
             "OVC_HIVSTAT", "mech_85114", "mech_85120",   81277,
             "OVC_HIVSTAT", "mech_85114", "mech_85121",   62048,
             "OVC_SERV A", "mech_85114", "mech_85120",   99566,
             "OVC_SERV A", "mech_85114", "mech_85114",   75162,
             "OVC_SERV A", "mech_85114", "mech_85121",   76005,
             "OVC_SERV G", "mech_85114", "mech_85114",    1536,
             "OVC_SERV G", "mech_85114", "mech_85120",    2034,
             "OVC_SERV G", "mech_85114", "mech_85121",    1557
             )

  # Test ggalluvial package
    is_alluvia_form(df, axes = 1:3, silent = T)

    ggplot(df, aes(y = target, axis3 = mech, axis2 = indicator, axis1 = new_mech)) +
      geom_alluvium(aes(fill = indicator), width = 0, reverse = F)+ 
      geom_stratum(width = 1/10, reverse = F,
                   fill = trolley_grey_light, 
                   color = "white") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
      scale_x_continuous(breaks = 1:3, 
                         labels = c("New Mechs", "Indicator", "Old Mech"),
                         position = "top")+
      coord_cartesian(expand = T, clip = "off") +
      coord_flip() + 
      labs(y = NULL) +
      theme(axis.text.y = element_blank(), 
            legend.position = "none") +
      scale_fill_manual(values = c(genoa_light, moody_blue_light, golden_sand_light)) +
      theme_void()
    ggsave(file.path(images, "ZMB_alluvial_opu_target_shift.svg"),
           width = 10, height = 5.625)
    
    
      geom_stratum(width = 1/12, fill = trolley_grey_light, color = trolley_grey)
    