## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  visualizing completeness of reporting
## DATE:     2020-03-13
## UPDATED:  2020-11-19


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(lubridate)
  library(vroom)
  library(scales)
  library(extrafont)
  library(ggtext)
  library(extrafont)
  library(glamr)
  library(glitr)
  library(COVIDutilities)
  library(ISOcodes)

# GLOBAL VARIABLES --------------------------------------------------------

  out_folder <- "Dataout"
  viz_folder <- "Images"
  
  pal <- viridis_pal()(6) # %>% show_col()
  color_hv_sites <- pal[1]
  color_ref <- "gray30" # "#C8C8C8"
  color_all_sites <- "#D3D3D3"
  period <- "Q4"
  
  # quarter starts
  qtrs <- as_date(c("2019-10-01", "2020-01-01", "2020-04-01", "2020-07-01", "2020-09-30"))
  
  # review slide background
  bckgrnd <- "#cfcdc9"
  
  # Stringency Index API url - start/end date
  ox_start <- "2020-01-01"
  ox_end <- today()
  url_ox <- paste("https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range",
    ox_start, ox_end,
    sep = "/"
  )
  rm(ox_end, ox_start)
  
  start_date <- "2019-12-30"
  weeks <- 90
  
  # DATIM base
  baseurl <- "https://final.datim.org/"
  source("../../../Documents/funny_things.R")

# IMPORT ------------------------------------------------------------------

  # myuser <- "" #do not save; see above in source
  iso_map <- identify_levels(username = username, password = mypwd(username)) %>%
    rename(iso = countryname_iso)

# COVID CALENDAR ----------------------------------------------------------

  covid <- who_pandemic() %>% pull(date)
  
  fy20_dates <- seq.Date(as_date("2019-10-01"), as_date("2021-04-28"), length.out = 365)
  
  df <- tibble(date = fy20_dates) %>%
    mutate(
      value = ifelse(date < "2020-07-01", 1, 0),
      post_who = date > covid
    )

# COVID DATA PULLS --------------------------------------------------------

  # COVID Restrictions (HDX)
  df_gov_measures <- extract_excel_data(
    hdx_govmes_url,
    hdx_govmes_linkid,
    "Dataset", "xlsx"
  )
  
  # Government Response (Oxford - https://covidtracker.bsg.ox.ac.uk/about-api)
  json <- url_ox %>%
    jsonlite::fromJSON(flatten = TRUE)

  # COVID cases (JHU)
  df_covid <- pull_jhu_covid()
  
  df_covid <- ISO_3166_1 %>%
    dplyr::select(Name, iso = Alpha_3) %>%
    mutate(Name = recode(Name,
      "Congo, The Democratic Republic of the" = "Congo (Kinshasa)",
      "Myanmar" = "Burma",
      "C?te d'Ivoire" = "Cote d'Ivoire",
      "Lao People's Democratic Republic" = "Laos",
      "Tanzania, United Republic of" = "Tanzania",
      "Viet Nam" = "Vietnam"
    )) %>%
    left_join(df_covid, ., by = c("countryname" = "Name")) %>%
    mutate(countryname = recode(countryname,
      "Congo (Kinshasa)" = "Democratic Republic of the Congo"
    ))
  
  # filter to just PEPFAR countries
  df_covid_pepfar <- df_covid %>%
    filter(iso %in% iso_map$iso)
  
  df_covid_pepfar <- df_covid_pepfar %>%
    filter(cases >= 10)

  df_covid_pepfar_top <- df_covid_pepfar %>%
    filter(countryname %in% top_tx) %>%
    group_by(countryname) %>%
    mutate(lab = case_when(date == max(date) ~ iso)) %>%
    ungroup()


# MUNGE OXFORD DATA -------------------------------------------------------
  
  # covert from json to dataframe
  df_stringency <- json %>%
    unlist() %>%
    enframe()
  
  # clean up table
  df_stringency <- df_stringency %>%
    rowwise() %>%
    mutate(
      parts = length(unlist(str_split(name, "[.]"))),
      tbl = first(unlist(str_split(name, "[.]"))),
      tbl = gsub("\\d", "", tbl)
    ) %>%
    filter(parts == 4) %>% # Keep the data, section with the longest parts
    separate(name,
      into = c("name", "date", "iso", "variable"),
      sep = "[.]"
    ) %>% # Separate column into multiple parts
    dplyr::select(date:value) %>% # Get rid of extra columns
    filter(date != value, iso != value) %>% # Exclude repetition
    mutate(date = ymd(date), value = as.numeric(value)) %>%
    spread(variable, value) %>%
    dplyr::select(-contains("legacy"))
  
  # filter to PEPFAR countries
  df_stringency <- df_stringency %>%
    filter(iso %in% iso_map$iso)
  
  rm(json)

# COVID CALENDAR ----------------------------------------------------------

  # Aling HFR to COVID stingency so we can merge by date and OU
  df_stringency_date <-
    ISO_3166_1 %>%
    dplyr::select(Name, iso = Alpha_3) %>%
    mutate(Name = recode(Name,
      "Congo, The Democratic Republic of the" = "Congo (Kinshasa)",
      "Myanmar" = "Burma",
      "C?te d'Ivoire" = "Cote d'Ivoire",
      "Lao People's Democratic Republic" = "Laos",
      "Tanzania, United Republic of" = "Tanzania",
      "Viet Nam" = "Vietnam"
    )) %>%
    left_join(df_stringency) %>%
    rename(operatingunit = Name) %>%
    mutate(operatingunit = recode(operatingunit,
      "Congo (Kinshasa)" = "Democratic Republic of the Congo"
    )) %>%
    filter(iso %in% iso_map$iso) %>%
    mutate(covid_val = -0.15)

  # Adding custom colors to stringency index for plotting. Collapsing down to week to aling with HFR
  # so plots line up.
  df_stringency <-
    df_stringency_date %>%
    mutate(
      bins = case_when(
        is.na(stringency) ~ "NA",
        stringency < 1 ~ "<1",
        stringency < 25 ~ "1-24",
        stringency < 50 ~ "25-49",
        stringency < 75 ~ "50-74",
        stringency < 85 ~ "75-84",
        TRUE ~ "85-100"
      ),
      color = case_when(
        is.na(stringency) ~ "#D9CDC3",
        stringency < 1 ~ "#D3E8F0",
        stringency < 25 ~ "#FAE1AF",
        stringency < 50 ~ "#FDAC7A",
        stringency < 75 ~ "#F6736B",
        stringency < 85 ~ "#DA3C6A",
        TRUE ~ "#A90773"
      )
    ) %>%
    mutate(
      bins = factor(bins, c("NA", "<1", "1-24", "25-49", "50-74", "75-84", "85-100")),
      color = factor(color, c("#D9CDC3", "#D3E8F0", "#FAE1AF", "#FDAC7A", "#F6736B", "#DA3C6A", "#A90773"))
    )

  # Join COVID and STringency data together
  df_covid_stringe <- df_stringency %>% left_join(df_covid, by = c("iso", "date"))


# PLOTS -------------------------------------------------------------------


  # Define filters for various plots
  top_tx_ous <- c(
    "South Africa", "Nigeria", "Mozambique", "Zimbabwe",
    "Uganda", "Nigeria", "Kenya", "Malawi", "Zambia", "Democratic Republic of Congo",
    "Eswatini")
  select_ous <- c("Kenya", "Nigeria", "Zambia", "Zimbabwe")
  single_ou <- c("Zambia")
  
  min_date <- as.Date("2020-04-01")
  max_date <- as.Date("2021-05-01")
  caption <- "Source: JHU COVID-19 feed + stringecy index from Blavatnik School of Government at Oxford University"
  
  # Without the 14-day moving average line
  df_covid_stringe %>%
    filter(operatingunit %in% top_tx_ous) %>%
    mutate(sort_var = fct_reorder(operatingunit, daily_cases, .desc = T)) %>%
    ggplot(aes(x = date), group = operatingunit) +
    geom_vline(xintercept = as.Date(c("2020-04-01", "2020-07-01", "2020-10-01", 
                                    "2021-01-01", "2021-04-01")), 
               size = 0.5, color = grey20k) +
    # geom_line(aes(y = if_else(cases> 1, daily_cases, NA_real_))) +
    geom_area((aes(y = if_else(cases > 0, daily_cases, NA_real_))), fill = grey40k, alpha = 0.85) +
    geom_hline(yintercept = -2, size = 3, color = "white") +
    geom_col(aes(y = -50, fill = (color)), alpha = 1) +
    facet_wrap(~ paste0(sort_var, "\n"), scales = "free_y") +
    scale_fill_identity() +
    scale_x_date(
      limits = c(min_date, max_date),
      date_labels = "%b %y", date_breaks = "1 months"
    ) +
    scale_y_continuous(labels = comma)+
    # scale_y_log10() +
    si_style_yline() +
    labs(
      title = "DAILY COVID-19 CASES CONTINUE TO CYLCE",
      caption = caption,
      x = NULL, y = NULL
    )

  # Zambia Specific plots but can change with filtering the OU
  # Adding in a 14-day lag to match visual approach used by NYTimes
  library(zoo) # for rolling mean

  covid_plot_df <- 
    df_covid_stringe %>%
    filter(operatingunit %in% top_tx_ous) %>%
    arrange(date) %>%
    group_by(operatingunit) %>%
    mutate(
      max_cases = max(cases, na.rm = T),
      seven_day = zoo::rollmean(daily_cases, 7, fill = NA, align = c("right")),
      fourteen_day = zoo::rollmean(daily_cases, 14, fill = NA, align = c("right")),
      covid_max = max(cases, na.rm = T),
      yaxis_offset = (-covid_max * 0.001),
      yaxis_offset_lim = yaxis_offset - (0.10 * yaxis_offset)
    ) %>% 
    ungroup() %>%
    mutate(sort_var = fct_reorder(operatingunit, max_cases, .desc = T),
           sa_flag = ifelse(operatingunit == "South Africa", 1, 0)) %>% 
    group_by(sa_flag) %>% 
    mutate(axis_max = max(daily_cases, na.rm = T),
           axis_min = 0) %>% 
    ungroup()

    
    
    # Use a geom_blank to ensure South Africa gets its own axis if included.
  covid_plot_df %>% 
    ggplot(aes(x = date), group = operatingunit) +
    annotate("rect", xmin = as.Date("2021-01-01"), xmax = as.Date("2021-04-01"), ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
    # geom_line(aes(y = if_else(cases> 1, daily_cases, NA_real_))) +
    geom_col((aes(y = if_else(cases > 0, daily_cases, NA_real_))), fill = grey30k, alpha = 0.85) +
    geom_area(aes(y = fourteen_day), fill = "#f7c5c4", alpha = 0.75) +
    geom_line(aes(y = fourteen_day), color = "#d73636", size = 0.75, alpha = 0.75) +
    geom_blank(aes(y = axis_max)) +
    geom_blank(aes(y = axis_min)) +
    # geom_line(aes(y = zoo::rollmean(daily_cases, 14, fill = grey20k, align = c("right")))) +
    # geom_hline(yintercept = -5, size = 2, color = "white") +
    # geom_col(aes(y = yaxis_offset, fill = (color)), alpha = 0.85) +
    # geom_col(aes(y = yaxis_offset_lim), fill = "white") +
    facet_wrap(~ sort_var, scales = "free_y") +
    scale_fill_identity() +
    scale_y_continuous(labels = comma)+
    scale_x_date(
      limits = c(min_date, max_date),
      date_labels = "%b %y", date_breaks = "1 months"
    ) +
    # scale_y_log10() +
    si_style_ygrid() +
    labs(
      title = "DAILY COVID-19 CASES ON THE DECLINE IN FY21Q1",
      caption = "Source: JHU COVID-19 feed + stringecy index from Blavatnik School of Government at Oxford University",
      x = NULL, y = NULL
    )


  # Save using dimensions of google slides and setting dingbats = F to render fonts in .AI
  ggsave(file.path(viz_folder, "2020_COVID_rise.pdf"),
    plot = last_plot(), useDingbats = F,
    width = 10, height = 5.625, dpi = "retina"
  )



