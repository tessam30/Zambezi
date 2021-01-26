# Purpose: COP21 Spectrum Data Review
# Author: Tim Essam
# Date: 2021-01-25
# Notes:


# GLOBALS -----------------------------------------------------------------

  library(glitr)
  library(glamr)
  library(gisr)
  library(readxl)
  library(purrr)
  library(here)
  library(extrafont)
  library(scales)
  library(openxlsx)
  library(tidyverse)
  library(ggtext)
  
  #Data folder
  cop_data <- "Data/COP21"
  images <- "Images"
  dataout <- "Dataout/COP21"
  
  

# READ and MUNGE ----------------------------------------------------------

  pop_file <- "Population age groups.xlsx"
  excel_sheets(here(cop_data, pop_file))
  
  # DATA NOTES: Regional totals are included in the annual pop totals, need to remove these
  # Regional values are at position 1, 14, 25, 
  
  # National Stats Office Data
  
  zmb_pop_nso <- read_xlsx(here(cop_data, pop_file), sheet = "zmb_population_nso")
  
  
  # provinces 
    prov_list <- c("Central", "Copperbelt", "Eastern", "Luapula", "Lusaka", 
                   "Muchinga", "North-Western", "Northern", "Southern", "Western")
  
  
  # First tackle reading in 2020 - 2018 data as this seems to have same structure and skip pattern
    year_list <- list("2020", "2019", "2018")
  
    pop_year <- 
      year_list %>% 
      set_names() %>% 
      map(.f = ~read_excel(path = here(cop_data, pop_file), 
                           sheet = .x, col_names = TRUE, skip = 2) %>% 
            mutate(year = .x, 
                   prov_flag = if_else(`Row Labels` %in% prov_list, 1, 0))) %>% 
      bind_rows()
  
    
  # # Generate a vector of names to be pre_pended repeated 17 times
  #   names_stub <- rep(c("f_", "m_"), each = 17)
  #   
  # # Create vector of new names that will be applied
  #   new_names <- pop_year %>% 
  #     select(-c(`Row Labels`, `Total Population`, `...37`, `year`)) %>% 
  #     names() %>% str_c(names_stub, .)
    
    
  # Things we need to do 
  # 1) rename applying the appropriate male/female stubs
  # 2) reshape long separating out stubs to categories
  # 3) Filter out the provinces, use the fact that Lusaka province will be taking filter_flag = 1, 3, or 5
  # 4) But before throwing out provinces, we need to fill the provinces up and down
    
  # Rename applying the new vector
    pop_year_df <- 
      pop_year %>% 
      rename_at(.,  2:18, ~paste0("f", "_", .x)) %>% 
      rename_at(., 19:35, ~paste0("m", "_", .x)) %>% 
      rename(district = `Row Labels`,
             district_totals = `Total Population`) %>% 
      select(-`...37`) %>% 
      filter(district != "Grand Total") %>% 
      rename_with(., ~gsub("\\...*", "", .)) %>% 
      mutate(province = if_else(prov_flag == 1, paste0(district, " Province"), NA_character_)) %>% 
      select(province, district, everything()) %>% 
      group_by(province) %>% 
      mutate(filter_flag = row_number()) %>% 
      ungroup() %>% 
      fill(province) %>% 
      mutate(prov_flag = ifelse(district == "Lusaka" & filter_flag %in% c(2, 4, 6), 0, prov_flag)) 
    
    # Extract regional values to verify
    pop_year_region <- 
      pop_year_df %>% 
      filter(prov_flag == 1) %>% 
      select(-c(filter_flag, prov_flag)) %>% 
      pivot_longer(.,
                   cols = -c(province, district, district_totals, year),
                   names_to = c("sex", "age"),
                   names_pattern = "(.)_(.*)" ,
                   values_to = "pop") %>% 
      mutate(sex = if_else(sex == "f", "female", "male")) %>% 
      group_by(year) %>% 
      mutate(pop_by_year = sum(pop)) %>% 
      ungroup() %>% 
      rename(province_totals = district_totals) %>% 
      mutate(sex_age_band_sh = pop / province_totals)
    
    pop_year_region %>% count(year, pop_by_year)
    
  
  # Reshape and validate totals   
    pop_year_district <- 
      pop_year_df %>%
      filter(prov_flag != 1) %>% 
      select(-c(filter_flag, prov_flag)) %>% 
      pivot_longer(.,
                   cols = -c(province, district, district_totals, year),
                   names_to = c("sex", "age"),
                   names_pattern = "(.)_(.*)" ,
                   values_to = "pop") %>% 
      mutate(sex = if_else(sex == "f", "female", "male"), 
             year = as.numeric(year)) %>% 
      group_by(year) %>% 
      mutate(pop_by_year = sum(pop)) %>% 
      group_by(year, province) %>% 
      mutate(pop_by_prov_year = sum(pop)) %>%
      group_by(district, year) %>% 
      mutate(pop_by_district_year = sum(pop)) %>% 
      ungroup()
    
    # Check that totals match
    pop_year_district %>% 
      mutate(flag = near(district_totals, pop_by_district_year)) %>% 
      count(flag)
    

# EXPLORE TRENDS  ---------------------------------------------------------

  # What do the shares look like across provinces?
    
    pop_year_region %>% 
      mutate(share = if_else(sex == "female", -sex_age_band_sh, sex_age_band_sh),
             color_sex = if_else(sex == "female", moody_blue_light, burnt_sienna_light)) %>% 
      filter(year == 2020) %>% 
      ggplot(aes(x = share, y = age, group = sex, fill = color_sex)) +
      geom_col() +
      geom_vline(xintercept = seq(from = -0.075, to =0.075, by = 0.025), colour = "white", 
                 alpha = 0.75) +
      geom_vline(xintercept = 0, colour = grey50k) +  
      ggrepel::geom_text_repel(aes(label = scales::percent(round(sex_age_band_sh, 4), 0.01)), size = 2) +
      facet_wrap(~province, nrow = 2) +
      si_style_xline() +
      scale_fill_identity() +
      scale_x_continuous(labels = c("10%", "5%", "0", "5%", "10%")) +
      labs(x = NULL, y = NULL, 
      title = "<span style = 'font-size:14pt; font-family:SourceSansPro-SemiBold;'>POPULATION PYRAMID BY PROVINCE</span><br>
      <span style = 'color:#dfd3ff;'>      FEMALES</span> 
      | <span style = 'color:#ffcaa2;'>MALES</span>") +
      theme(
        text = element_text(family = "Source Sans Pro"),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 11, lineheight = 1.2))
           
    si_save(here(images, "ZMB_prov_pop_pyramid.png"), scale = 1.75)
    
# DOCUMENT and EXPORT -----------------------------------------------------

  varnames <- c(names(pop_year_region), names(pop_year_district)) %>% unique()
    
  var_desc <- 
      varnames %>% 
      as_tibble_col(column_name = "variable") %>% 
      mutate(description = case_when(
        variable == "province" ~ 'Province',
        variable == "district" ~ 'District',
        variable == "province_totals" ~ 'Province population totals by year from original data',
        variable == "district_totals" ~ 'District population totals by year from original data',
        variable == "year" ~ 'Year',
        variable == "sex" ~ 'Sex',
        variable == "age" ~ 'Age band',
        variable == "pop" ~ 'estimated population',
        variable == "sex_age_band_sh" ~ 'share of total province population by age / sex / year',
        variable == "pop_by_year" ~ 'estimated population by year for all Zambia',
        variable == "pop_by_prov_year" ~ 'estimated population by province by year',
        variable == "pop_by_district_year" ~ 'estimated population by district by year'
      ))
  
    # NOTES: Had issues with openxlsx writing readable excel file, so csvs and manually combined  
    #create workbook
    wb <- createWorkbook()
    
    #add instructions tab
    add_data_excelwb <- function(df, sheet_name) {
      
      addWorksheet(wb, sheetName = {{sheet_name}}, gridLines = FALSE)
      writeDataTable(wb, sheet = {{sheet_name}}, x = df, withFilter = TRUE)
      setColWidths(wb, sheet = {{sheet_name}}, widths = 18, cols = 1:length(names(df)))
                   
    }
    
    df_list <- list(pop_year_district, pop_year_region, zmb_pop_nso, var_desc)
    df_list_names <- list("Population by district_sex_age", "Population by province_sex_age",
                          "Zambia Population Estimates NSO", "variable codebook")
    
    # Write them all as CSVs
    purrr::map2(.x = df_list, 
                .y = df_list_names,
                .f = ~write.csv(.x, here(dataout, paste0(.y, ".csv"))))

    
    # add_data_excelwb(df = pop_year_district, sheet_name = "Population by district/sex/age")
    # add_data_excelwb(df = pop_year_region, sheet_name = "Population by province/sex/age")
    # add_data_excelwb(df = zmb_pop_nso, sheet_name = "zmb_pop_nso")
    # add_data_excelwb(df = var_desc, sheet_name = "variable codebook")
    
    
    #save
    saveWorkbook(wb, here(dataout, "Population_age_groups_tidy.xlsx"), overwrite = TRUE)  

    
  
  
  