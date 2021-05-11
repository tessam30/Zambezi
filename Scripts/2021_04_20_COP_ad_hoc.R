# Purpose: Munge and Analysis of Data for COP Presentation
# Author: Tim Essam | SI, 
# Date: 2020-04-19
# Notes: Request from Megan regarding fine age band targets

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
    library(ggnewscale)
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
    
  # Functions  
    merdata <- glamr::si_path("path_msd")
    
    calc_pct <- function(x, y) {
      ifelse(y > 0.000, x / y, NA)
    }
  

# LOAD DATA ============================================================================  
  
  msd <- read_msd(file.path(merdata, "MER_Structured_Datasets_PSNU_IM_FY19-21_20210319_v2_1_Zambia.zip"))
  msd_ou <- read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY19-21_20210319_v2_1.zip"))
  subnat <- read_msd(file.path(merdata, "MER_Structured_Datasets_NAT_SUBNAT_FY15-21_20210319_v2_1.zip"))
    
  genie <- read_msd(file.path(merdata, "Genie-PSNUByIMs-MultipleOUs-Daily-2021-04-19.zip")) 
  genie_2 <- read_msd(file.path(merdata, "ZMB", "Genie-OUByIMs-Zambia-Daily-2021-05-11.zip"))
    
# MUNGE ============================================================================
  
  #  munge 15 - 29 year old age band targets
 msd %>% 
    filter(trendsfine %in% c("15-19", "20-24", "25-29"),
           fiscal_year == "2021",
           standardizeddisaggregate == "Age/Sex",
           indicator == "VMMC_CIRC") %>% 
      group_by(fiscal_year) %>% 
      summarise(targets = sum(targets, na.rm = T), 
                q1 = sum(qtr1, na.rm = T)) %>% 
      mutate(q1/targets)
  
  
  msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", 
                 fiscal_year == "2021",
                 indicator %in% c("PrEP_CURR", "PrEP_NEW", "GEND_GBV")) %>% 
    group_by(indicator) %>% 
    summarise(targets = sum(targets, na.rm = T), 
              q1 = sum(qtr1, na.rm = T)) %>% 
    mutate(pct = q1 / targets)
  
  # Check if any preliminary targets exist
  genie %>% 
    filter(fiscal_year == "2021", 
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(indicator) %>% 
    summarise(qtr2 = sum(qtr2, na.rm = T))
  
  
  # HTS Request
 hts_qtr <-  
   msd_ou %>% 
    filter(operatingunit == "Zambia", 
           indicator %in% c("HTS_TST_POS", "HTS_TST"),
           sex == "Male", 
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    reshape_msd(clean = T) %>% 
    spread(period_type, value) %>% 
    group_by(period, indicator, ageasentered) %>% 
    summarise(results = sum(results, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(fy = str_sub(period, 1, 4)) %>% 
    spread(indicator, results) %>% 
    filter(!period %in% c("FY19", "FY20", "FY21"))
  
 hts <- 
   hts_qtr %>% 
    rename(age = ageasentered) %>% 
    mutate(positivity = HTS_TST_POS/HTS_TST) %>% 
    group_by(period) %>% 
    mutate(across(contains("HTS"), sum, na.rm = T, .names = "Tot_{.col}")) %>% 
    ungroup() %>% 
    mutate(tot_pos = Tot_HTS_TST_POS/Tot_HTS_TST, 
           tot_share = HTS_TST_POS / Tot_HTS_TST_POS) %>% 
    arrange(age, period) 
  
  hts %>% 
    filter(age %in% c("20-24", "50+")) %>% 
    mutate(age_color = ifelse(age %in% c("20-24", "50+"), burnt_sienna, trolley_grey_light)) %>% 
    group_by(age, period) %>% 
    ggplot(aes(x = period, group = age)) +
    geom_line(aes(y = tot_pos), color = grey10k, size = 1.5) +
    geom_line(aes(y = positivity), color = grey50k) +
    geom_point(aes(y = positivity, fill = age_color, size = tot_share), shape = 21, color = grey80k) +
    #geom_text(aes(y = positivity, label = scales::percent(positivity, 0.01))) +
    # geom_col(aes(y = HTS_TST), fill = grey20k) +
    # geom_col(aes(y = HTS_TST_POS), fill = genoa_light) +
    facet_wrap(~(paste0(age, "\n"))) +
    si_style_ygrid() +
      scale_fill_identity() +
      scale_y_continuous(labels = percent_format(1), limits = c(0, .15)) +
    labs(x = NULL, y = NULL, title = "CASE FINDING FOR 20-24 YEAR OLD MALES CONSISTENTLY TRAILS OVERALL CASE FINDING TRENDS FOR ALL MALES",
         caption = "Source: MSD FY21Q1 Post-Clean") +
    coord_cartesian(clip = "off") +
    scale_x_discrete(labels = c("FY19Q1", "", "FY19Q3", "", "FY20Q1", "", "FY20Q3", "", "FY21Q1")) +
    scale_size_binned() +
    theme(legend.position = "none")
    
    
  ggsave(file.path(images, "ZMB_positivity_rate_men_age_disags_subset.png"), 
         width = 12, height = 5.75, dpi = 320, 
         scale = 1.25)
  
  
  hts_ach <-  
    msd_ou %>% 
    filter(operatingunit == "Zambia", 
           indicator %in% c("HTS_TST_POS", "HTS_TST"),
           sex == "Male", 
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    reshape_msd(clean = T) %>% 
    filter(period_type != "results") %>% 
    spread(period_type, value) %>% 
    group_by(period, indicator, ageasentered) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = T)) %>% 
    filter(period %in% c("FY19", "FY20", "FY21")) %>% 
    mutate(ach = calc_pct(cumulative, targets))
  
  
  hts_ach %>% 
    rename(age = ageasentered) %>% 
    filter(!age %in% c("Unknown Age", "40-49"), indicator == "HTS_TST_POS") %>% 
    mutate(target_color = if_else(targets < cumulative, "white", grey90k),
           age_color = ifelse(age %in% c("20-24", "50+"), burnt_sienna, grey30k)) %>% 
    ggplot(aes(x = period, group = indicator)) +
    geom_col(aes(y = targets), fill = grey10k)+
    geom_col(aes(y = cumulative, fill = age_color)) +
    geom_errorbar(aes(ymin = targets, ymax = targets, color = target_color), linetype = "dotted", size = 0.75) + 
    facet_wrap(~paste0(age, "\n"), scales = "free_y") +
    geom_label(aes(y = cumulative, label = percent(ach, 1)), 
               size = 9/ .pt,
               family = "Source Sans Pro")+
    si_style_ygrid() +
    scale_y_continuous(labels = comma) +
                 scale_color_identity() +
  scale_fill_identity() +
    labs(x = NULL, y = NULL, title = "HTS_TST_POS ACHIEVEMENT FOR FINE AGE BANDS",
         caption = "Source: MSD FY21Q1 Post-Clean") +
    coord_cartesian(clip = "off") +
    theme(panel.spacing = unit(0.5, "lines"))
    
  ggsave(file.path(images, "ZMB_hts_pos_men_age_disags.png"), 
         width = 5, height = 4.75, dpi = 320, 
         scale = 1.5)
  
  # Contribution to case finding
  # HTS Request

 hts_index <- 
   msd_ou %>% 
   filter(operatingunit == "Zambia", 
          sex == "Male", 
          standardizeddisaggregate == "Age/Sex/Result") %>% 
    reshape_msd(clean = T) %>% 
    spread(period_type, value) %>% 
    group_by(period, indicator, ageasentered) %>% 
    summarise(results = sum(results, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(fy = str_sub(period, 1, 4)) %>% 
    spread(indicator, results) %>% 
    filter(!period %in% c("FY19", "FY20", "FY21"),
           ageasentered != "Unknown Age") %>% 
   mutate(positivity = (HTS_INDEX_NEWPOS/(HTS_INDEX_NEWPOS+HTS_INDEX_NEWNEG)))
 
  hts_index %>% 
    filter(ageasentered %in% c("20-24", '25-29', '30-34', '35-39', '40-44', '45-49', '50+')) %>%
    group_by(period) %>% 
    mutate(across(contains("INDEX"), sum, na.rm = T, .names = "tot_{.col}"),
           overall_pos = (tot_HTS_INDEX_NEWPOS/(tot_HTS_INDEX_NEWPOS + tot_HTS_INDEX_NEWNEG))) %>%
    ggplot(aes(x = period, group = ageasentered)) +
    geom_line(aes(y = overall_pos), color = grey30k, size = 1) +
    geom_line(aes(y = positivity)) +
    geom_point(aes(fill = HTS_INDEX_NEWPOS, size = HTS_INDEX_NEWPOS, y = positivity), shape = 21) +
    facet_wrap(~ageasentered) +
    si_style_ygrid() 

  hts_index %>% 
    filter(ageasentered %in% c("20-24", '25-29', '30-34', '35-39', '40-44', '45-49', '50+')) %>%
    group_by(period) %>% 
    mutate(across(contains("INDEX"), sum, na.rm = T, .names = "tot_{.col}"),
           overall_pos = (tot_HTS_INDEX_NEWPOS/(tot_HTS_INDEX_NEWPOS + tot_HTS_INDEX_NEWNEG))) %>% 
    filter(ageasentered %in% c("20-24", '50+')) %>%
    ggplot(aes(x = factor(period), group = ageasentered)) +
    geom_line(aes(y = overall_pos), color = grey10k, size = 1.25) +
    #geom_text(aes(y = overall_pos, label = percent(overall_pos, 1)), color = grey90k) +
    geom_line(aes(y = positivity)) +
    geom_point(aes(size = HTS_INDEX_NEWPOS, y = positivity), shape = 21, fill = burnt_sienna) +
    facet_wrap(~ageasentered) +
    si_style_ygrid() +
    scale_y_continuous(labels = percent, limits = c(0, .31)) +
    scale_x_discrete(labels = c("FY19Q1", "", "FY19Q3", "", "FY20Q1", "", "FY20Q3", "", "FY21Q1")) +
    scale_size_binned() +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL, title = "HTS_INDEX CASE IDENTIFICATION LAGS FOR 20-24 AND 50+ COMPARED TO \nOVERALL POSITIVITY FOR 20-50+",
         subtitle = "Circle size corresponds to HTS_INDEX_NEWPOS volume",
         caption = "Source: MSD FY21Q1 Post-Clean; INDEX TESTING")
  
  ggsave(file.path(images, "ZMB_hts_pos_index_men_age_disags.png"), 
         width = 7, height = 3.5, dpi = 320, 
         scale = 1.5)
  
  hts_index %>% count(period)

# TX_CURR request ---------------------------------------------------------
  
  # Pull in both custom coverage and gap data
  source("Data/COP21/cop21_tx_coverage.R")
  

  # TX_CURR trends Request
  tx_qtr <-  
    msd %>% 
    filter(operatingunit == "Zambia", 
           indicator %in% c("TX_CURR"),
           sex == "Male", 
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    reshape_msd(clean = T) %>% 
    spread(period_type, value) %>% 
    group_by(period, indicator, ageasentered) %>% 
    summarise(results = sum(results, na.rm = T), 
              targets = sum(targets, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(fy = str_sub(period, 1, 4),
           tgt = ifelse(targets > 0, targets, NA_integer_)) %>% 
    group_by(ageasentered, fy) %>% 
    fill(tgt, .direction = "updown") %>% 
    filter(!period %in% c("FY"))
  
  # TX_CURR Request
  tx_qtr <-  
    genie_2 %>% 
    filter(operatingunit == "Zambia", 
           indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           fiscal_year == "2021") %>% 
    reshape_msd(clean = T) %>% 
    spread(period_type, value) %>% 
    group_by(period, indicator, ageasentered, sex) %>% 
    summarise(results = sum(results, na.rm = T), 
              targets = sum(targets, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(fy = str_sub(period, 1, 4),
           tgt = ifelse(targets > 0, targets, NA_integer_)) %>% 
    group_by(ageasentered, fy) %>% 
    fill(tgt, .direction = "updown") %>% 
    filter(!period %in% c("FY21")) %>%
    ungroup() %>% 
    filter(period == 'FY21Q2')
  
  
  # Recalculate gap
  tx_qtr_gap <- 
    tx_qtr %>% 
    select(-c(targets, fy)) %>% 
    left_join(tx_gap, by = c("ageasentered" = "age", "sex")) %>% 
    select(period:`PLHIV.2021`) %>% 
    mutate(Gap = results - `PLHIV.2021`,
           coverage = results/`PLHIV.2021`) %>% 
      mutate(age_color = ifelse(ageasentered == "20-24" & sex == "Male", burnt_sienna, grey20k))
  
  male_tx <- 
    tx_qtr_gap %>% 
    filter(sex == "Male") %>% 
      ggplot(aes(y = ageasentered, x = coverage)) +
      geom_col(aes(fill = age_color), alpha = 0.85) +
      scale_fill_identity() +
      new_scale_fill() +
      geom_point(aes(x = -0.05, fill = results), size = 17, shape = 21, color = grey90k) +
      geom_vline(xintercept = c(0.25, 0.5, 0.75), color = "white")+
      geom_text(aes(label = percent(coverage, 1)), 
                hjust = 1.1, 
                size = 9/.pt)+
      geom_text(aes(label = comma(results), 
                    color = ifelse(results > 40000, "white", "black"),
                    x = -0.05), 
                size = 9/.pt) +
      facet_wrap(~paste0(sex,  "s FY21", "\n", "TX_CURR   |    Coverage"), scales = "free_y") +
      scale_fill_si(palette = "moody_blues", discrete = F) +
      scale_color_identity() +
      si_style_xgrid() +
      labs(x = NULL, y = NULL, title = "") +
      theme(axis.text.x = element_blank(),
            legend.position = "none" ) +
      scale_y_discrete(limits = rev) +
      coord_cartesian(clip = "off")
      
    si_save(file.path(images, "ZMB_age_disaggs_coverage_disags.png"), scale = 1.33, 
            width = 13, height = 5.79) 
  
  # GAP
 male_tx_gap <- 
   tx_qtr_gap %>% 
    filter(sex == "Male") %>% 
    ggplot(aes(y = ageasentered)) +
    geom_col(aes(x = Gap), fill = grey20k) +
    geom_vline(xintercept = c(seq(-4e4, -1e4)), color = "white")+
    geom_point(aes(x = 2500, fill = results), size = 17, shape = 21, color = grey90k) +
    geom_text(aes(x = 2500, label = comma(results),
              color = ifelse(results > 40000, "white", "black")),
              size = 9/.pt) +
    geom_text(aes(x = Gap, label = comma(Gap, accuracy = 1L)), hjust = 1.05, size = 9/.pt)+

    facet_wrap(~sex, scales = "free_y") +
    si_style_nolines() +
    labs(x = NULL, y = NULL, title = "") +
    theme(legend.position = "none" ) +
    scale_y_discrete(limits = rev, position = "right") +
    coord_cartesian(clip = "off") +
    scale_fill_si(palette = "moody_blues", discrete = F) +
    new_scale_fill() +
    scale_color_identity() +
    scale_x_continuous(limits = c(-5e4, 2500)) +
    theme(axis.text.x = element_blank(),
          strip.text = element_blank())
  
 male_tx + male_tx_gap + si_save(file.path(images, "ZMB_age_tx_gap_combo_plot.png"), 
                                   scale = 1.33)
 
 si_save(file.path(images, "ZMB_age_plhiv_treatment_gap_coverage_disags.png"), scale = 1.33, 
          width = 13, height = 5.79) 
    

  # TX_CURR for just 20-24 and 50+
 tx_qtr_19 <-  
   msd %>% 
   filter(operatingunit == "Zambia", 
          indicator %in% c("TX_CURR"),
          sex == "Male", 
          standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
   reshape_msd(clean = T) %>% 
   spread(period_type, value) %>% 
   group_by(period, indicator, ageasentered) %>% 
   summarise(results = sum(results, na.rm = T), 
             targets = sum(targets, na.rm = T)) %>% 
   ungroup() %>% 
   mutate(fy = str_sub(period, 1, 4),
          tgt = ifelse(targets > 0, targets, NA_integer_)) %>% 
   group_by(ageasentered, fy) %>% 
   fill(tgt, .direction = "updown") %>% 
   filter(!period %in% c("FY19", "FY20", "FY21", "FY22")) %>% 
   filter(str_detect(period, "FY19"))
 
 
 tx_qtr <-  
    genie_2 %>% 
    filter(operatingunit == "Zambia", 
           indicator %in% c("TX_CURR"),
           sex == "Male", 
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    reshape_msd(clean = T) %>% 
    spread(period_type, value) %>% 
    group_by(period, indicator, ageasentered) %>% 
    summarise(results = sum(results, na.rm = T), 
              targets = sum(targets, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(fy = str_sub(period, 1, 4),
           tgt = ifelse(targets > 0, targets, NA_integer_)) %>% 
    group_by(ageasentered, fy) %>% 
    fill(tgt, .direction = "updown") %>% 
    filter(!period %in% c("FY19", "FY20", "FY21", "FY22"))
 
 tx_qtr_19_21 <- bind_rows(tx_qtr, tx_qtr_19)
 
 
  
 tx_qtr_25_34 <- 
   tx_qtr_19_21  %>% 
    ungroup() %>% 
    filter(ageasentered %in% c("25-29", "30-34", "50+")) %>%
    mutate(age_group = case_when(
      ageasentered %in% c("25-29", "30-34") ~ "25-34", 
      TRUE ~ "50+"
    )) %>% 
    group_by(age_group, indicator, period) %>% 
    summarise(across(c(tgt, results), sum, na.rm = T)) %>% 
   rename(ageasentered = age_group)
  
    
 tx_qtr_20_34 <- 
   tx_qtr_19_21 %>% 
   ungroup() %>% 
   filter(ageasentered %in% c("25-29", "30-34", "50+"))
  
  
 tx_qtr_20_34 %>% 
  mutate(error_bar = ifelse(results > tgt, "white", grey90k)) %>% 
    ggplot(aes(x = period, y = results, group = ageasentered)) + 
    geom_col(aes(y = tgt), fill = grey10k)+
    geom_col(aes(fill = ifelse(results/tgt > 1, genoa, grey30k)), alpha = 0.85) + 
    geom_hline(yintercept = c(seq(0, 100000, 25000)), color = "white", size = 0.1) +
    geom_errorbar(aes(ymin = tgt, ymax = tgt, color = error_bar), linetype = "dotted") +
    geom_text(aes(label = comma(results, 1),
                  color = ifelse(results/tgt > 1, "white", grey90k)),
              size = 9/.pt, vjust = 1.5) +
    geom_label(aes(label = percent(results/tgt, 1)), 
                   size = 9/.pt, color = grey90k, nudge_y = 2000) +
    facet_wrap(~paste0("Males ", str_to_upper(ageasentered), "\n"), scales = "fixed") +
    si_style_nolines() +
    #scale_y_continuous(breaks = comma(c(seq(25000, 100000, 25000))), position = "right", breaks = ) +
    scale_y_continuous(label = label_number_si(), position = "right") +
    theme(axis.text.y = element_blank(),
          strip.text = element_text(size = 14, face = "bold")) +
    scale_x_discrete(labels = c("FY19Q1", "", "FY19Q3", "", "FY20Q1", "", "FY20Q3", "", "FY21Q1", "")) +
    labs(x = NULL, y = NULL, title = "",
         caption = "Source: Genie OU X IM 2021-05-11") +
    scale_fill_identity()+
    scale_color_identity() +
    coord_cartesian(expand = F, clip = "off")
    
    ggsave(file.path(images, "ZMB_TX_curr_men_age_disags_25_34.png"), 
           width = 12.5, height = 5.75, dpi = "retina", 
           scale = 1.25)


    # VIZ ============================================================================

  # Can we get a graph that compares HTS TST and HTS POS among the different age bands of 
  # males to demonstrate continued low uptake of services among men 20-24 and men 50+? COP19 – COP20 Q1 

# SPINDOWN ============================================================================

