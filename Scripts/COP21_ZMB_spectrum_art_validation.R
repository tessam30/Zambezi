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
  library(scales)
  library(ggtext)
  library(here)

  #Data folder
  cop_data <- "Data/COP21"
  images <- "Images"
  dataout <- "Dataout/COP21"
  
  
  

# LOAD AND MUNGE ----------------------------------------------------------

  excel_sheets(here(cop_data, "ART Main file.xlsx"))
  art_main <- read_xlsx(here(cop_data, "ART Main File.xlsx"))
  
  names(art_main_long) %>% str_extract_all(., "(Both|Female|Male)")

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
           sex = str_to_lower(sex)) %
   rename(district = "District")
  
  
  art_unaids <- read_csv(here(cop_data, "20210124t18-00utc-zambia-moh-art-unaids-art-program-data.csv")) %>% 
    mutate(age = case_when(
      age_group == "Y000_014" ~ "<15",
      TRUE ~ "15+"
    )) %>% 
    rename(district = "area_name")
  