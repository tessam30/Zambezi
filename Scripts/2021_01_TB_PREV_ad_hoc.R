# PURPOSE:  TB_PREV and TB_ART Global comparison for Zambia
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-03-24
# NOTES: Ad hoc request received on 2021-03-24

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
    library(patchwork)
    library(ggtext)
    library(here)
    library(RColorBrewer)
    
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  
      
    
  # Functions & Objects
    threshold <- 0.9
    
    calc_pct <- function(x, y) {
      ifelse(x > 0.000, x / y, NA)
    }

# LOAD DATA ============================================================================  
    
  msd <- read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY19-21_20210212_v1_1.zip"))
  
  genie <- read_msd(file.path(merdata, "Genie-PSNUByIMs-Zambia-Frozen-2021-04-15.zip"))
  
# MUNGE ============================================================================
  
  # Pull out TB_PREV N / D to replicate PANO TB SCREENING & TPT:GLOBAL View (OU COMP)
    tb_prev <- 
      msd %>% 
        filter(indicator %in% c("TB_PREV"),
               standardizeddisaggregate %in% c("Total Denominator", "Total Numerator"),
               fiscal_year != 2021) %>% 
        group_by(operatingunit, numeratordenom, indicator, fiscal_year) %>% 
        summarise(across(qtr4, sum, na.rm = TRUE)) %>% 
        ungroup() %>% 
        pivot_wider(names_from = c(numeratordenom, fiscal_year),
                    values_from = qtr4) %>% 
        mutate(tpt_2020 = N_2020 / D_2020,
               tpt_2019 = N_2019 / D_2019) %>% 
      #rename(country = countrynamename) %>% 
      mutate(country = if_else(operatingunit == "Democratic Republic of the Congo", "DRC", operatingunit))
    
    tb_art <- 
      msd %>% 
      filter(indicator %in% c("TX_CURR", "TB_ART", "TX_TB_D_POS", "TX_TB_D_NEG", "TX_TB"),
             standardizeddisaggregate %in% c("Total Denominator", "Total Numerator")) %>% 
      
      group_by(operatingunit, indicator, fiscal_year) %>% 
      summarise(across(contains("qtr"), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pivot_longer(cols = contains("qtr"),
                   names_to = "quarter",
                   values_to = "value") %>% 
      pivot_wider(names_from = indicator,
                  values_from = value) %>% 
      mutate(tx_tb = TX_TB, 
             TX_TB = TX_TB_D_POS + TX_TB_D_NEG) %>% 
      mutate(art_pct_screen = calc_pct(TX_TB, TX_CURR),
             art_pct_pos = calc_pct(TX_TB_D_POS, TX_TB),
             period = paste0("FY",substr(fiscal_year, 3, 4), "Q", substr(quarter, 4, 4)))
      
  
# VIZ ============================================================================

  # VIZ for 2020
    x_offset = -1e4/1.5
    
    tb_prev %>% 
      filter(!is.na(D_2020), !is.na(tpt_2020),
             D_2020 > 10000) %>% 
      mutate(ou_order = fct_reorder(country, D_2020),
             num_label = ifelse(N_2020 > 17000, comma(N_2020, 1), NA_character_),
             denom_label = ifelse(N_2020 > 0, comma(D_2020, 1), NA_character_)) %>% 
      ggplot(aes(y = ou_order)) +
      geom_col(aes(x = D_2020), fill = grey10k) +
      geom_col(aes(x = N_2020), fill = grey40k) +
      geom_point(aes(x = x_offset, fill = tpt_2020), size = 12, shape = 21) +
      geom_vline(xintercept = c(5e4, 1e5, 2e5, 3e5), color = "white", linetype = "dotted") +
      geom_vline(xintercept = 0, size = 0.5, color = grey60k) +
      geom_text(aes(x = N_2020, label = num_label), family = "Source Sans Pro", size = 6, hjust = 1.1, color = "white") +
      geom_text(aes(x = D_2020, label = denom_label), family = "Source Sans Pro", size = 6, hjust = -0.1, color = grey60k) +
      geom_text(aes(x = x_offset, label = percent(tpt_2020, 1)), family = "Source Sans Pro", size = 5) +
      si_style_nolines() +
      labs(x = NULL, y = NULL, title = "") +
      scale_fill_si(palette = "carto_teal", discrete = F, limits = c(0, 1)) +
      # scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'Greens'),
      #                      limits = c(0, 1)) +
      coord_cartesian(clip = "on") +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            panel.grid.minor  = element_line(colour = "white")) +
      scale_x_continuous(labels = comma, position = "top",
                         breaks = c(5e4, 1e5, 2e5))
      
      si_save(here(graphs, "GLOBAL_TB_PREV_summmary_fy20.svg"), scale = 1.4)
    
  # TB_ART plot by TB_ART trends
    tb_art %>% 
      mutate(ou_order = fct_reorder(operatingunit, TX_CURR, .desc = T)) %>% 
      # filter(fiscal_year != 2021, !is.na(art_pct_screen)) %>% 
      filter(fiscal_year != 2021) %>% 
      ggplot(aes(x = factor(period), group = operatingunit)) +
      geom_rect(aes(xmin = "FY20Q2", xmax = "FY20Q4", ymin = 0, ymax = Inf), 
                fill = "#F3F4F4") +
      stat_smooth(aes(y = art_pct_screen), color = grey40k) +
      #geom_area(aes(y = art_pct_pos), fill = grey20k) +
      #geom_point(aes(fill = art_pct_screen), shape = 21, size = 3, color = "white") + 
      geom_point(aes(y = art_pct_screen, fill = art_pct_screen), shape = 21, size = 3, color = "white") + 
      facet_wrap(~ou_order) +
      si_style_ygrid() +
      scale_fill_si(palette = "carto_teal", discrete = F, limits = c(0, 1)) +
      theme(panel.margin = unit(0.5, "lines")) +
      scale_y_continuous(labels = percent)
    
  # We need to generate a different scale for South Africa due to it's wonkiness
    tb_art %>% 
      filter(fiscal_year != 2021, operatingunit != "Malawi") %>% 
      mutate(ou_order = fct_reorder(operatingunit, TX_CURR, .desc = T),
             ymax = ifelse(operatingunit == "South Africa", 4e6, 1.4e6),
             ymin = 0) %>% 
      group_by(ou_order) %>% 
      mutate(tx_curr_min = min(TX_CURR, na.rm = T)) %>% 
      ungroup() %>% 
      filter(tx_curr_min > 5e5) %>% 
      ggplot(aes(x = factor(period))) +
      geom_rect(aes(xmin = "FY20Q2", xmax = "FY20Q4", ymin = 0, ymax = Inf), 
                fill = "#F3F4F4", alpha = 0.25) +
      geom_col(aes(y = TX_CURR), fill = grey10k) +
      geom_col(aes(y = TX_TB, fill = art_pct_screen)) +
      geom_point(aes(y = ifelse(TX_TB>0, TX_TB*.98, NA_integer_)), fill = "white", shape = 22, size = 10) +
      geom_text(aes(y = TX_TB*.98, label = percent(art_pct_screen, 1)), 
                 family = "Source Sans Pro", 
                 vjust = 1.4, 
                 size = 3) +
      geom_blank(aes(y = ymin)) +
      geom_blank(aes(y = ymax)) +
      geom_hline(yintercept = c(5e5, 1e6), color = "white", linetype = "dotted")+
      facet_wrap(~ou_order, scales = "free_y", nrow = 2) +
      si_style_xline() +
      scale_fill_si(palette = "carto_teal", discrete = F, limits = c(0, 1.1)) +
      scale_y_continuous(labels = comma) +
      theme(panel.spacing = unit(0.5, "lines"), legend.position = "none")+
      coord_cartesian(expand = T, clip = "off") +
      labs(x = NULL, y = NULL, title = "") +
      scale_x_discrete(labels = c("", "FY19Q2", "", "FY19Q4", "", "FY20Q2", "", "FY20Q4"))
      
      
    si_save(here(graphs, "GLOBAL_tb_art_screen_summmary_fy20.svg"), scale = 1.25) 
      

    

# 2021-04-15 REQUEST ------------------------------------------------------

  # NOTES: 
  # TB_STAT to show change in notifications. This would include the numerator and denominator over a three year period i.e. 2018 through 2020. 
  # Annual data would be fine.
    
  # TB_PREV denominator and numerator analysis to show coverage against TX_CURR. 
  # Again annual data over a three year period. We basically want to see the trend of 
  # coverage against the TX_CURR. We fully understand the TB data is not the most accurate 
  # in terms of tracking cohorts but it would be useful to look into such an analysis.    
    
    
# SPINDOWN ============================================================================

    source <- "Source: DATIM Genie frozel data pull 2021/04/15"
    
    tb_stat <- 
      genie %>% 
      filter(indicator == "TB_STAT",
             #fundingagency %in% c("USAID", "HHS/CDC"),
             standardizeddisaggregate %in% c("Total Denominator", "Total Numerator")) %>% 
      group_by(standardizeddisaggregate, indicator, snu1, fiscal_year) %>% 
      summarise(across(contains("qtr"), sum, na.rm = TRUE)) %>% 
      reshape_msd(clean = T) %>% 
      pivot_wider(names_from = c(standardizeddisaggregate),
                  values_from = val) %>% 
      mutate(TB_STAT = `Total Numerator` / `Total Denominator`) %>% 
      group_by(period) %>% 
      mutate(across(contains("Total"), sum, na.rm = T, .names = "Nat {.col}"),
             TB_STAT_NAT = `Nat Total Numerator` / `Nat Total Denominator`) %>% 
      ungroup() %>% 
      group_by(snu1) %>% 
      mutate(ave_tb = mean(TB_STAT)) %>% 
      ungroup() %>% 
      mutate(snu_order = fct_reorder(paste0(snu1, "\n"), ave_tb, .desc = T))
    

    tb_stat %>% 
      filter(period != "FY18Q2") %>% 
      ggplot(aes(x = period, group = snu1)) +
      geom_line(aes(y = TB_STAT_NAT), size = 1.25, color = grey20k) +

      geom_line(aes(y = TB_STAT), size = 1, color = grey80k) +
      geom_point(aes(y = TB_STAT, fill = TB_STAT), size = 3, shape = 21, color = grey80k) +
      facet_wrap(~snu_order) +
      si_style_ygrid() +
      scale_y_continuous(labels = scales::percent_format(1)) +
      scale_fill_viridis_c(option = "A") +
      scale_x_discrete(labels = c("FY18Q4", "", "FY19Q2", "", "FY19Q4", "", "FY20Q2", "", "FY20Q4", "")) +
      theme(legend.position = 'none') +
      labs(x = NULL, y = NULL, title = "TB_STAT PERCENT COVERAGE TRENDS",
           caption = source)
    
    si_save(file.path(images, "ZMB_TB_STAT_coverage_historical_fixed_y.png"), scale = 1.25)
    
    
    
    
    # Pull out TB_PREV N / D to replicate PANO TB SCREENING & TPT:GLOBAL View (OU COMP)
    tb_prev <- 
      genie %>% 
      filter(indicator %in% c("TB_PREV", "TX_CURR"),
             standardizeddisaggregate %in% c("Total Denominator", "Total Numerator")) %>% 
      group_by(snu1, numeratordenom, indicator, fiscal_year) %>% 
      summarise(across(contains("qtr"), sum, na.rm = TRUE)) %>%
      reshape_msd(clean = T) %>% 
      ungroup() %>% 
      pivot_wider(names_from = c(numeratordenom, indicator),
                  values_from = val) %>% 
      mutate(fy = str_sub(period, 1, 4)) %>% 
      group_by(snu1, fy) %>% 
      mutate(across(contains("_TB"), sum, na.rm = T, .names = "annual {.col}"),
             coverage = D_TB_PREV / N_TX_CURR,
             tb_prev_cov = N_TB_PREV / D_TB_PREV) %>% 
      ungroup() %>% 
      mutate(snu_order = fct_reorder(paste0(snu1, "\n"), `annual D_TB_PREV`, .desc = T))
      
      
    tb_prev %>% 
      ggplot(aes(x = period, group = snu1)) +
      geom_col(aes(y = N_TX_CURR), fill = grey10k) +
      geom_col(aes(y = D_TB_PREV), fill = scooter, alpha = 0.75) +
      geom_text(aes(y = D_TB_PREV, label = percent(coverage, 1)), family = "Source Sans Pro", size = 9/.pt, vjust = -1) +
      facet_wrap(~snu_order, scales = "free_y") +
      scale_y_continuous(labels = comma) +
      si_style_ygrid() +
      labs(x = NULL, y = NULL, title = "TX_PREV_D RELATIVE TO TX_CURR", 
           caption = source) +
      scale_x_discrete(labels =c("", "FY18Q2", "", "FY18Q4", "", "FY19Q2", "", "FY19Q4", "", "FY20Q2", "", "FY20Q4", ""))
    
    si_save(file.path(images, "ZMB_TB_PREV_TX_CURR_historical_free_y.png"), scale = 1.5)
    
    
    tb_prev %>% 
      ggplot(aes(x = period, group = snu1)) +
      geom_col(aes(y = D_TB_PREV), fill = grey20k) +
      geom_col(aes(y = N_TB_PREV), fill = genoa, alpha = .9) +
      geom_text(aes(y = N_TB_PREV, label = percent(tb_prev_cov, 1)), family = "Source Sans Pro", size = 9/.pt, vjust = -1) +
      facet_wrap(~snu_order, scales = "free_y") +
      scale_y_continuous(labels = comma) +
      si_style_ygrid() +
      labs(x = NULL, y = NULL, title = "TX_PREV COVERAGE (TX_PREV_N / TX_PREV_D)",
           caption = source) +
      scale_x_discrete(labels =c("", "FY18Q2", "", "FY18Q4", "", "FY19Q2", "", "FY19Q4", "", "FY20Q2", "", "FY20Q4", ""))
    
    si_save(file.path(images, "ZMB_TB_prev_coverage_historical_free_y.png"), scale = 1.5)
    
    tb_prev %>% 
      filter(str_detect(period, "(Q1|Q3)", negate = T)) %>% 
      ggplot(aes(x = period, y = tb_prev_cov, group = snu1)) +
      geom_area(fill = grey10k, alpha = 0.75) +
      geom_line(size = 1, color = grey30k) +
      geom_point(fill = genoa, shape = 21, color = "white", size = 3) +
      geom_text(aes(label = percent(tb_prev_cov, 1)), family = "Source Sans Pro", size = 9/.pt, vjust = -1) +
      facet_wrap(~snu_order) +
      scale_y_continuous(labels = comma) +
      si_style_ygrid() +
      labs(x = NULL, y = NULL, title = "TX_PREV COVERAGE (TX_PREV_N / TX_PREV_D) TRENDS",
           caption = source) +
      #scale_x_discrete(labels =c("", "FY18Q2", "", "FY18Q4", "", "FY19Q2", "", "FY19Q4", "", "FY20Q2", "", "FY20Q4", "")) +
      coord_cartesian(clip = "off") +
      theme(axis.text.y = element_blank())
      
    si_save(file.path(images, "ZMB_TB_PREV_coverage_pct_historical_fixed_y.png"), scale = 1.5)
      
      
      
    