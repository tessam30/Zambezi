# Purpose: Validate Spectrum output files
# Author: Tim Essam | SI
# Date: 2021-01-26
# Notes: 



# GLOBALS -----------------------------------------------------------------

  library(readxl)
  library(glitr)
  library(glamr)
  library(gisr)
  library(tidyverse)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(here)
  library(patchwork)

  #Data folder
  cop_data <- "Data/COP21"
  images <- "Images"
  dataout <- "Dataout/COP21"
  
  # Filter and spread count of quarters
  count_qtrs <- function(df) {
    df %>% 
      count(calendar_quarter, sex, age) %>% 
      spread(sex, n) %>% 
      prinf()
  }
  
  compare_lists <- function(df1, df2) {
    
    dist1 <- df1 %>% 
      distinct(district) %>% 
      pull()
    
    dist2 <- df2 %>% 
      distinct(district) %>% 
      pull
    
    print(setequal(dist1, dist2))
    
  }
  
  calendar_plot <- function(df) {
    df %>% 
    ggplot(aes(x = calendar_quarter, y = sex, fill = factor(n))) +
      geom_tile(color = "white", alpha = 0.55, size = 0.5) +
      geom_text(aes(label = n)) +
      facet_wrap(~age, scales = "free_y", nrow = 2) +
      si_style_xline() +
      theme(legend.position = "none")+
      scale_fill_manual(values = c("116" = scooter, "232" = old_rose),
                        na.value = trolley_grey_light) 
  }
  
  
  
# LOAD AND MUNGE ----------------------------------------------------------

  # Review sheets in file
    excel_sheets(here(cop_data, "ART Main file.xlsx"))
    art_main <- read_xlsx(here(cop_data, "ART Main File.xlsx"))
  
  #names(art_main_long) %>% str_extract_all(., "(Both|Female|Male)")

  # Load data, pivot longer so it can be compared with long art_unaids file
    art_main_long <- 
      art_main %>% 
      #rename_with(~str_replace(., "(<15|15+)$", "\\1  Both")) %>% 
      pivot_longer(cols = `Mar 2018 <15`:`Sep 2020 15+  Females`,
                   #names_to = c("a", "c", "d", "e"),
                   #names_sep = " ",
                   #names_pattern ="([[:alpha:]]{3}) ([[:digit:]]{4}) (<15|15\\+) (Both|Female|Male)",
                   names_to = "categorycombo",
                   values_to = "art_est") %>% 
      separate(., col = "categorycombo", 
               into = c("month", "year", "age", "sex"), 
               sep = " ", convert = TRUE, 
               remove = FALSE, 
               fill = "right") %>% 
      mutate(sex = stringi::stri_extract_last_words(categorycombo) %>% str_replace_all(., "15", "both"),
             sex = str_to_lower(sex)) %>%    
      rename(district = "District") %>% 
      mutate(calendar_quarter = case_when(
              month == "Mar" ~ paste0("CY", year, "Q1"),
              month == "Jun" ~ paste0("CY", year, "Q2"),
              month == "Sep" ~ paste0("CY", year, "Q3"), 
              TRUE ~  paste0("CY", year, "Q4")),
        
            age_group = case_when(
              sex == "both" & age == "<15" ~ "Y000_014",
              TRUE ~ "Y015_999" 
            ), 
            sex = if_else(sex == "females", "female", sex)
        )
  
  # Collapse down to the district level so it matches up with UNAIDS size
    art_dist <- 
      art_main_long %>% 
      group_by(district, calendar_quarter, sex, age, age_group) %>% 
      summarise(art_est = sum(art_est, na.rm = TRUE)) %>% 
      ungroup()
  
  
  # Load and harmonize UNAIDS data so it will easily merge with art_main file output
    art_unaids <- read_csv(here(cop_data, "20210124t18-00utc-zambia-moh-art-unaids-art-program-data.csv")) %>% 
      mutate(age = case_when(
        age_group == "Y000_014" ~ "<15",
        TRUE ~ "15+"
      )) %>%
      rename(district = "area_name") %>% 
      arrange(calendar_quarter)
  
  # Check that both data frames are using same district names
    compare_lists(art_unaids, art_dist)
    
    
  # Something is a little funky with the counts for combo categories
  # Not sure how/why age groups + sex are included multiple timesin CY2018Q3/both, CY2018Q4/female/male
    count_qtrs(art_unaids)
    count_qtrs(art_dist)
    
  # So lets make a visualization of the problem
    
  art_moh_plot <-  
    art_unaids %>% 
      full_join(., art_dist %>% select(calendar_quarter, age, sex, district)) %>% 
      count(calendar_quarter, age, sex, age_group) %>% 
      mutate(n = ifelse(is.na(age_group), NA, n)) %>% 
    calendar_plot() +
      labs(x = NULL, y = NULL,
           title = "THE ZAMBIA MOH ART UNAIDS DATA IS MISSING DATA FOR CY2019Q1",
           subtitle ="Data appears to have been entered twice in CY2018Q3 & CY2018Q4",
           caption = "Source: 20210124t18-00utc-zambia-moh-art-unaids-art-program-data.csv")
      
  art_main_plot <- 
    art_dist %>% 
    count(calendar_quarter, age, sex) %>% 
    calendar_plot() +
    labs(x = NULL, y = NULL,
         title = "THE ZAMBIA ART MAIN FILE IS NOT MISSING ANY QUARTERS",
         caption = "ART Main file.xlsx")
    
  # patchwork the files together
  art_moh_plot / art_main_plot  
  si_save(here(images, "ZMB_ART_validation_plot.png"), scale = 1.5)
    
    
  art_unaids %>% count(calendar_quarter, age, sex) %>% prinf()
  
  art_unaids %>% filter(calendar_quarter == "CY2018Q4") %>% 
    count(calendar_quarter, age, sex, district) %>% 
    spread(sex, n) %>% prinf()
  
  
  # Merge moh-unaids with art_main --> preserving art_var from art_main to compare
  art_joined <- 
    art_dist %>% 
    full_join(., art_unaids, by = c("calendar_quarter", "age", "sex", "age_group", "district")) %>% 
    mutate(art_validated = art_est - art_current, 
           art_issue_flag = art_validated == 0)
  
  art_joined_range <-
    art_joined %>% 
    arrange(district, calendar_quarter) %>% 
    mutate(group_num = row_number(),
           facet_num = group_num %/% 1044) %>%
    group_by(sex, age, district, calendar_quarter) %>% 
    mutate(tot = sum(art_validated)) %>% 
    ungroup() %>% 
    filter(art_issue_flag == "FALSE")
  
  max <- max(abs(art_joined_range$tot))
  
    art_joined_range %>% 
    ggplot(aes(x = calendar_quarter, y = district, fill = (tot))) +
    #geom_point(shape = 22, size = 3, color = grey30k, stroke = 0.25) +
    geom_tile(color = "white", size = 0.25) +
    facet_wrap(~ facet_num, scales = "free_y") +
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'PiYG'),
                          limits = c(-1 * max/10, max/10), 
                          breaks = breaks_pretty(n = 6),
                          labels = scales::comma,
                         oob = squish) +
    si_style_xline() +
      scale_y_discrete(limits = rev) +
      scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL,
         title = "ART CURRENT NUMBERS FROM THE ZAMBIA MOH/UNAIDS DATASET\nDIFFERED FROM THE ART MAIN FILE ESTIMATES IN THREE TIME PERIODS",
         fill = "Deviation from \nART Main data",
         subtitle = 
           "<span style = 'color:#c51b7d;'>**Dark pink boxes**</span> 
           indicate much lower values compared with ART main data<br> and <span style = 'color:#4d9221;'>**dark green boxes**</span> are much higher </span>",
         caption = "Source: Comparison of 20210124t18-00utc-zambia-moh-art-unaids-art-program-data.csv and ART Main file.xlsx") +
    theme(strip.text = element_blank(),
          legend.key.width = unit(1.5, "cm"),
          text = element_text(family = "Source Sans Pro"),
          plot.subtitle = element_markdown(size = 11, lineheight = 1.2))

  si_save(here(images, "ZMB_ART_validation_summary.png"),
          height = 11.5, 
          width = 8, scale = 1.1)


# EDA and MAPPING ---------------------------------------------------------

  
  
# WRITE and CLOSE OUT -----------------------------------------------------

  write_csv(art_dist, here(dataout, "ART_main_file_long_district.csv"))
  write_csv(art_joined, here(dataout, "ZMB_ART_joined_validated.csv"))
  