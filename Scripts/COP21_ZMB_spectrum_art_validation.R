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

  library(gt)
  library(gtsummary)

  #Data folder
  cop_data <- "Data/COP21"
  images <- "Images"
  dataout <- "Dataout/COP21"
  docs <- "Documents"
  
  # Filter and spread count of quarters
  count_qtrs <- function(df) {
    df %>% 
      count(calendar_quarter, sex, age) %>% 
      spread(sex, n) %>% 
      prinf()
  }
  
  # Function will return list of districts that are not equal
  compare_districts <- function(df1, df2, compare_var) {
    
    dist1 <- df1 %>% 
      distinct({{compare_var}}) %>% 
      pull()
    
    dist2 <- df2 %>% 
      distinct({{compare_var}}) %>% 
      pull
    
    print(setequal(dist1, dist2))
    
  }
  
  # Plot the calendar quarter frequencies as a heatmap
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
      ungroup() %>% 
      mutate(district = if_else(district == "Mushindano", "Mushindamo", district))
  
  
  # Load and harmonize UNAIDS data so it will easily merge with art_main file output
    art_unaids <- read_csv(here(cop_data, "20210124t18-00utc-zambia-moh-art-unaids-art-program-data.csv")) %>% 
      mutate(age = case_when(
        age_group == "Y000_014" ~ "<15",
        TRUE ~ "15+"
      )) %>%
      rename(district = "area_name") %>% 
      arrange(calendar_quarter) %>% 
      mutate(district = if_else(district == "Mushindano", "Mushindamo", district))
  
  # Check that both data frames are using same district names
    compare_districts(art_unaids, art_dist, district)
    
    
  # Something is a little funky with the counts for combo categories
  # Not sure how/why age groups + sex are included multiple timesin CY2018Q3/both, CY2018Q4/female/male
    count_qtrs(art_unaids)
    count_qtrs(art_dist)
    
  # So lets make a visualization of the problem for each dataset and then stitch together with patchwork
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
    
  # Printed table of the issue we are seeing
  # For under 15s data in cq CY2018Q3 is doubled, same issue for CY2018Q4 and male/females
  art_unaids %>% 
    count(calendar_quarter, age, sex) %>% 
    spread(sex, n) %>%  
    prinf()
  
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
    filter(is.na(art_issue_flag)|art_issue_flag != "TRUE", )
  
    # Grab max to make sure legend/fill is symmetric
    max <- max(abs(art_joined_range$tot), na.rm = T)

  
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
                         oob = squish, 
                         na.value = trolley_grey_light) +
    si_style_xline() +
      scale_y_discrete(limits = rev) +
      scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL,
         title = "<span style = 'font-size:14pt;'>ART CURRENT NUMBERS FROM THE ZAMBIA MOH/UNAIDS DATASET<br>DIFFERED FROM THE ART MAIN FILE ESTIMATES IN FOUR TIME PERIODS</span>",
         fill = "Deviation from \nART Main data",
         subtitle = 
           "<span style = 'color:#c51b7d;'>**Dark pink boxes**</span> 
           indicate much lower values compared with ART main data and <span style = 'color:#4d9221;'>**dark green boxes**</span> are much higher.</span>
         <br><span style = 'color:#808080;'>**Light gray boxes are**</span> missing data.</span>",
         caption = "Source: Comparison of 20210124t18-00utc-zambia-moh-art-unaids-art-program-data.csv and ART Main file.xlsx", 
         color = "No data") +
    theme(strip.text = element_blank(),
          legend.key.width = unit(1.5, "cm"),
          plot.title = element_markdown(family = "Source Sans Pro Regular"),
          plot.subtitle = element_markdown(size = 11, family = "Source Sans Pro"),
          axis.text = element_markdown(family = "Source Sans Pro Light"))

  si_save(here(images, "ZMB_ART_validation_summary.png"),
          height = 11.5, 
          width = 8, scale = 1.1)


# EDA and MAPPING ---------------------------------------------------------

  source("./Scripts/Z01_fetch_spdfs.R")
  
  districts <- art_unaids %>% distinct(district) %>% pull()
  districts_geo <- spdf_comm_zmb %>% distinct(psnu) %>% pull()
  setdiff(districts, unique(spdf_comm_zmb$psnu))
  
  art_geo <- 
    left_join(spdf_comm_zmb, art_dist, by = c("psnu" = "district"))

   terr_map +
    geom_sf(data = art_geo %>% 
              filter(sex != "both", str_detect(calendar_quarter, "CY2020")),
            aes(fill = art_est), color = "white") +
    facet_wrap(sex~calendar_quarter) +
    scale_fill_si(palette = "denims", discrete = FALSE, trans = "log", labels = comma) +
    si_style_map()
  
   art_dist %>% 
     filter(sex != "both", str_detect(calendar_quarter, "CY2020")) %>% 
     group_by(district, calendar_quarter, sex) %>% 
     summarise(tot = sum(art_est)) %>% 
     spread(calendar_quarter, tot)
   
   # Create the full sized data frame to fix the MOD data column for complete calendar_quarters
  cq_full <-  art_dist %>% distinct(calendar_quarter, sex, age, age_group, district)
   
  
  # Attempt fix -- assumption is that 2nd entry belongs in CY2019Q1 
  art_unaids_flytrap <- 
    cq_full %>% 
    left_join(art_unaids) %>% 
    group_by(district, calendar_quarter, age_group, sex) %>% 
    mutate(group_count = n(), 
           row_num = row_number()) %>% 
    group_by(district, sex, age_group) %>% 
    mutate(lag_count = row_number()) %>% 
    mutate(art_proposed = case_when(
      # Fix under 15s
      calendar_quarter == "CY2019Q1" & sex == "both" ~ lead(art_current, n = 1),
      calendar_quarter == "CY2019Q2" & sex == "both" ~ lag(art_current, n = 4),
      
      calendar_quarter == "CY2019Q1" & sex %in% c("male","female") ~ lead(art_current, n = 1),
      calendar_quarter == "CY2019Q2" & sex %in% c("male","female") ~ lag(art_current, n = 2),
      TRUE ~ art_current)
    ) %>% 
    ungroup %>% 
    mutate(keep_row_flag = case_when(
      calendar_quarter == "CY2018Q3" & sex == "both" & lag_count == 3 ~ 0,
      calendar_quarter == "CY2018Q4" & sex %in% c("male","female") & lag_count == 5 ~ 0,
      TRUE ~ 1)
      ) %>% 
    filter(keep_row_flag == 1)
    
  # This is the "fixed" MOH data
  art_joined_flytrap <- 
    art_dist %>% 
    left_join(., art_unaids_flytrap, by = c("calendar_quarter", "age", "sex", "age_group", "district")) %>% 
    mutate(art_current_validated = art_est - art_current,
           art_proposed_validated = art_est - art_proposed,
           art_issue_flag_orig = art_current_validated == 0,
           art_issue_flag_prop = art_proposed_validated == 0)

   
# TABLES AND SPARKLINES ---------------------------------------------------

   # TODO - Generate Tables for each Province and stitch together into PDF booklet
   
   #ggplot for the sparkline
   # Add in Regions so we can loop over them 
   art_dist_region <- 
   art_dist %>% 
   left_join(., spdf_comm_zmb, by = c("district" = "psnu")) %>% 
    select(-c(uid, operatingunit, operatingunituid, geometry))
 
 
  art_unaids_region <- 
    cq_full %>% 
    left_join(art_unaids) %>% 
    group_by(district, calendar_quarter, age_group, sex, age, area_id) %>% 
    summarise(art_summed = sum(art_current, na.rm = T)) %>% 
    ungroup() %>% 
    left_join(., spdf_comm_zmb, by = c("district" = "psnu")) %>% 
    select(-c(uid, operatingunit, operatingunituid, geometry, area_id)) %>% 
    rename(art_est = "art_summed")
    
  
   
   plot_spark <- function(df) {
     df %>% 
       mutate(
         art_start = if_else(calendar_quarter == "CY2018Q1", art_est, NA_real_),
         art_end = if_else(calendar_quarter == "CY2020Q3", art_est, NA_real_),
       ) %>% 
       group_by(district, sex) %>% 
       fill(., art_start, .direction = "down") %>% 
       fill(., art_end, .direction = "up") %>% 
       ungroup() %>% 
       mutate(color = if_else(art_end - art_start < 0, old_rose, scooter)) %>% 
       ggplot(aes(x = calendar_quarter, y = art_est, color = color, group = paste0(district, "-", sex)))+
       geom_line(size = 15) +
       si_style_void() +
       scale_color_identity() +
       theme(legend.position = "none")
   }
   
   # Nest the ggplots in a dataframe
   
    # Wrap Below in a function to loop over provinces and to allow for age filter
   # Ensure that key variables remain for table
   
   art_table <- function(df, province) {
    
    # Generates nested sparklines to be inserted into table cells  
    spark_lines <- 
      df %>% 
     filter(snu1 == {{province}}) %>% 
     #filter(str_detect(calendar_quarter, "(CY2019|CY2020)")) %>% 
     arrange(sex, district) %>% 
     select(-age_group) %>% 
     mutate(colvar = district,
            disag = sex) %>% 
     nest(art = c(calendar_quarter, art_est, district, sex)) %>% 
     mutate(plot = map(art, plot_spark)) 
   
   # Reshape data wide for table plot -- data frame appearance will be table appearance
   art_wide <-  
     df %>% 
     filter(snu1 == {{province}}) %>% 
     select(-age_group) %>% 
     #filter(str_detect(calendar_quarter, "(CY2019|CY2020)")) %>% 
     pivot_wider(names_from = calendar_quarter, values_from = art_est) %>% 
     mutate(`Difference` = CY2020Q3 - CY2018Q1,
            `Percent Change` = (CY2020Q3/CY2018Q1) - 1) %>% 
     rename(Age = "age") %>% 
     rename_if(startsWith(names(.), "CY"), ~str_remove_all(., "CY"))
   
   # Pass wide data to gt then use text_transform() to embed sparkline
    art_gt <- 
     art_wide %>% 
     mutate(ggplot = NA) %>% 
     gt(
       rowname_col = "district",
       groupname_col = "sex"
     ) %>% 
     text_transform(
       locations = cells_body(vars(ggplot)),
       fn = function(x){
         map(spark_lines$plot, ggplot_image, height = px(15), aspect_ratio = 4)
       }
     ) %>% 
     cols_width(vars(ggplot) ~ px(100)) %>% 
     cols_label(
       ggplot = "Trend"
     ) %>% 
       cols_hide(columns = vars(snu1)) %>% 
     #fmt_number(5:12, decimals = 0) %>% 
     fmt_number(3:13, decimals = 0) %>% 
      fmt_percent(15, decimals = 0) %>% 
     tab_options(row_group.background.color = trolley_grey_light) %>% 
       tab_header(title = paste0("ART estimates for ", {{province}}, " Province")) %>% 
      tab_style(
        style = cell_fill(color = old_rose_light, alpha = 0.25),
        locations = cells_body(
          columns = vars(`Percent Change`),
          rows = `Percent Change` < 0
        )
      ) %>% 
      tab_source_note(source_note = "Source: ART Main file.xlsx")
   
      art_gt 
  
       gtsave(art_gt, here(docs,  paste0("MOH ART estimates for ", {{province}}, " Province.png")))
     }
   
  # Loop over provinces and export full table    
   art_dist_region %>% 
     distinct(snu1) %>% 
     pull() %>% 
     map(., .f = ~art_table(.x))
   
  art_table(art_unaids_region, "Central")
   
   

# WRITE and CLOSE OUT -----------------------------------------------------

  write_csv(art_dist, here(dataout, "ART_main_file_long_district.csv"))
  write_csv(art_joined, here(dataout, "ZMB_ART_joined_validated.csv"))
  write_csv(art_joined_flytrap, here(dataout, "ZMB_ART_join_validated_fixed_moh.csv"))
  