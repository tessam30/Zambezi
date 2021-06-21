# PURPOSE: Munge and Analysis of FY21 Q2 MD tables
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-05-20
# NOTES: New take on the tables

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
library(gt)
library(fontawesome)


# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

merdata <- si_path(type = "path_msd")


# Key indicators for the base tables
indics <- c("PrEP_NEW", "OVC_SERV", "VMMC_CIRC", 
            "HTS_TST", "HTS_TST_POS",
            "TX_NEW", "TX_CURR")


# Mechs that need to be filtered for whatever reason

# Agency order throughout
# Use the long order b/c of the varying nature of coverage by diff agencies
agency_order_shrt <- c("USAID", "ALL OTHER AGENCIES")


# call required functions
source("../selfdestructin5/Scripts/add_achv_colors_tbl.R")
source("../selfdestructin5/Scripts/MD_tables_reboot_funs.R")


# Indicator Definitions -- THESE MAY CHANGE DEPENENT ON INDICS above
indic_def <- 
  tibble::tribble(
    ~indic_category,    ~indicator,        ~indicator_plain,
    "prevention",       "PrEP_NEW",       "Newly enrolled on antiretroviral pre-exposure prophylaxis",
    "prevention",       "OVC_SERV",       "Beneficiaries of OVC programs for children/families affected by HIV",
    "prevention",       "VMMC_CIRC",      "Voluntary medical male circumcision for HIV prevention",
    "testing",          "HTS_TST",        "Received HIV testing service and results",
    "testing",          "HTS_TST_POS",    "Received HIV testing service and positive results",
    "treatment",        "TX_NEW",         "Newly enrolled on antiretroviral therapy",
    "treatment",        "TX_CURR",        "Currently receiving antiretroviral therapy"
  )

# LOAD DATA ============================================================================  

ou_im <- 
  si_path() %>% 
  return_latest("PSNU_IM_FY19-21_20210514_v1_1_Zambia") %>% 
  read_msd() %>% 
  filter(fiscal_year %in% c(2020, 2021)) 


# HELPER FUNCTIONS --------------------------------------------------------

# KEEP ONLY USAID AND ALL OTHER AGENCIES
# Helper to do a bit of repetitive munging
clean_and_aggregate <- function(df){
  df %>% 
    filter(indicator %in% indics,
           standardizeddisaggregate %in% c("Total Numerator")) %>% 
    clean_agency() %>% 
    mutate(agency = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES"),
           # Lump factors at 3 then apply long agency order b/c of varying nature
           # mutate(agency = fct_lump(fundingagency, n = 2, other_level = "ALL OTHER AGENCIES"),
           agency = fct_relevel(agency, agency_order_shrt)) %>% 
    group_by(fiscal_year, agency, indicator) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop")
}           



# SHAPE BASE TABLE --------------------------------------------------------
# Shape the base dataframe from which the table is derived
#@description shape the msd to wide with key indicators  
#@param df - base msd from which all manipulations are done
#@param country_col either countryname or operating unit, depending on table desired
#@param ou countryname or operating unit

shape_md_tbl <- function(df, country_col, ou) {
  
  # Filter the data down to key indicators defined in indics object
  # Collapsing down to the agency level
  ou_tbl <- 
    df %>% 
    filter({{country_col}} %in% ou) %>% 
    clean_and_aggregate()
  
  
  # Clean up and add up down flags, these will be used in version 1.0   
  md_tbl <- 
    ou_tbl %>% 
    reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
    group_by(agency, indicator) %>% 
    mutate(value_run = row_number(),
           gap = targets - results_cumulative,
           gap_denom = (4 - (substr(period, 6, 6) %>% as.numeric)),
           gap_pace = gap_calc(gap, gap_denom),
           APR = denom_share(results_cumulative, targets)) %>% 
    ungroup() %>% 
    arrange(agency, indicator, period) %>% 
    group_by(agency, indicator) %>% 
    mutate(
      value_yr_lag = lag(results_cumulative, n = 4),
      q2q_comp = q2q_compare(results_cumulative, value_yr_lag),
      change_dir = if_else(q2q_comp > 0, "increase", "decrease")
    ) %>% 
    ungroup() %>% 
    mutate(indicator = fct_relevel(indicator, indics)) %>% 
    calc_achv(., APR, period) %>% 
    group_by(agency)
  
  # Old table layout
  md_tbl_old <- 
    md_tbl %>% 
    filter(period %in% c("FY20Q4", "FY21Q2")) %>% 
    select(period, agency, indicator, targets, results = cumulative, APR) %>% 
    mutate(period = str_sub(period, 1, 4)) %>%
    pivot_wider(names_from = period, 
                names_glue = "{period}{.value}",
                values_from = c(targets, results, APR),
                names_sort = TRUE) %>% 
    left_join(., indic_def) %>% 
    ungroup() %>% 
    mutate(indicator2 = ifelse(agency == "USAID", paste(indicator, indicator_plain), paste(indicator)),
           indicator = fct_relevel(indicator, indics)) %>% 
    arrange(agency, indicator)
  
  md_tbl_old <- 
    md_tbl_old %>% 
    relocate(indicator2, .before = indicator) %>% 
    select(-indic_category, -indicator_plain) %>% 
    select(agency, indicator2, indicator, sort(tidyselect::peek_vars())) %>% 
    group_by(indicator) %>% 
    mutate(across(c(FY21results), sum, .names = "overall_q2")) %>% 
    ungroup() %>% 
    mutate(overall_share = FY21results / overall_q2) 
  
  return(md_tbl_old)
}

# Test function above
df_tbl <-  shape_md_tbl(df = ou_im, country_col = operatingunit, ou = "Zambia")



# Fix column names
fix_col_names <-function(md_tbl_old) {  
  
  tbl_col_names <- 
    head(md_tbl_old, 1) %>% 
    ungroup() %>% 
    mutate_all(as.character) %>% 
    pivot_longer(everything(), names_to = "column", values_to = "value") %>% 
    select(-value) %>% 
    mutate(label = ifelse(str_detect(column, "FY"), str_sub(column, 5, -1), ""),
           label = ifelse(label == "APR", "achievement", label)) %>% 
    deframe()
  
  return(tbl_col_names)
}

col_names <-  fix_col_names(shape_md_tbl(ou_im, operatingunit, "Zambia"))  

# Create a bar graph with share
spark_plot <- function(df){
  df %>% 
    ggplot(aes(y = indicator2)) +
    geom_col(aes(x = 1), fill = grey10k) +
    geom_col(aes(x = share, fill = ifelse(fundingagency == "USAID", genoa, genoa_light))) + 
    scale_fill_identity() +
    si_style_void() +
    theme(legend.position = "none")
}

# Create data frame to pass to spark_plot
md_spark <- 
  df_tbl %>% 
  mutate(fundingagency = agency, 
         indicator2 = indicator,
         share = overall_share) %>% 
  # nest(spark_nest = c(period, results_cumulative, fundingagency, indicator, spark_color)) %>% 
  # mutate(plot = map(spark_nest, spark_plot))
  nest(spark_nest = c(share, fundingagency, indicator2)) %>% 
  mutate(plot = map(spark_nest, spark_plot))


# Create table

df_tbl %>% 
  filter(agency == "USAID") %>% 
  mutate(ggplot = NA) %>%
  mutate(space = NA, .after = FY21targets) %>% 
  mutate(circle = FY21APR, .before = FY21APR) %>% 
  gt(groupname_col = "agency") %>% 
  cols_hide(matches("FY20")) %>%
  cols_hide(indicator) %>% 
  text_transform(
    locations = cells_body(columns = c(ggplot)),
    fn = function(x){
      map(md_spark$plot, ggplot_image, height = px(15), aspect_ratio = 4)
    }
  ) %>% 
  # Format numbers
  fmt_percent(
    columns = matches("APR|share"), 
    decimal = 0
  ) %>% 
  fmt_number(
    columns = matches("targ|result|q2"),
    decimal = 0
  ) %>% 
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  ) %>% 
  fmt_missing(
    columns = space,
    missing_text = ""
  ) %>% 
  cols_align(
    align = c("left"),
    columns = "indicator"
  ) %>% 
  tab_options(
    row_group.font.weight = "bold"
  ) %>% 
  opt_all_caps(
    all_caps = TRUE,
    locations = c("row_group")
  ) %>% 
  cols_label(
    indicator2 = "",
    FY21APR = "apr",
    FY21results = "results",
    FY21targets = "targets",
    space = "",
    overall_q2 = "all results",
    overall_share = "usaid share",
    ggplot = "",
    circle = ""
  ) %>% 
  text_transform( 
    locations = cells_body(
      columns = c(indicator2),
      rows = (agency == "USAID")
    ),
    fn = function(x){
      name <- word(x, 1)
      name2 <- word(x, 2, -1)
      glue::glue(
        "<div style='line-height:10px'<span style='font-weight:regular;font-variant:small-caps;font-size:13px'>{name}</div>
        <div><span style='font-weight:regular;font-size:11px'>{name2}</br></div>"
      )
    }
  ) %>%   
  tab_spanner(
    label = md("**FY21 Q2 OVERALL**"),
    columns = matches("overall|ggplot")
  ) %>% 
  tab_spanner(
    label = md("**FY21 Q2 USAID**"),
    columns = matches("FY21|circle")
  ) %>% 
  tab_style(
    style = list("font-variant: small-caps;"),
    locations = cells_column_labels(columns = everything()
    )
  ) %>% 
  tab_header(
    title = glue::glue("ZAMBIA PERFORMANCE SUMMARY")
  ) %>%
  opt_align_table_header(align = c("center")) %>% 
  # add_achv_colors() %>% 
  tab_source_note(
    source_note = paste("Produced on ",Sys.Date(), " using PEPFAR FY21Q2i MSD released on 2021-05-14.")
  ) %>% 
  # tab_source_note(
  #   source_note = md("*ALL OTHER AGENCIES* based on aggregates excluding de-duplication.")
  # ) %>% 
  tab_options(
    source_notes.font.size = 8,
    table.font.size = 12
  ) %>% 
  # cols_width(
  #   indicator2 ~ px(340),
  # ) %>% 
  add_achv_shapes() %>% 
  gtsave("Images/ZMB_USAID_Q2_Summary.png")



# OU AGAINST TARGETS, USAID AGAINST TARGETS, USAID CONTRIBUTION -----------

  df_ou <- ou_im %>% 
  filter(indicator %in% indics,
         standardizeddisaggregate %in% c("Total Numerator")) %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
  group_by(indicator) %>% 
  mutate(value_run = row_number(),
         gap = targets - results_cumulative,
         gap_denom = (4 - (substr(period, 6, 6) %>% as.numeric)),
         gap_pace = gap_calc(gap, gap_denom),
         APR = denom_share(results_cumulative, targets)) %>% 
  ungroup() %>% 
  mutate(indicator = fct_relevel(indicator, "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR",
                                 "PrEP_NEW", "OVC_SERV", "VMMC_CIRC"))

  add_space <- function(x) {
    paste0(x, "\n")
  }
  
  # Standard bar graph plot of ahcievement by indicator and time
  df_usaid %>% 
    mutate(apr_fill = if_else(APR >= 1, genoa, genoa_light),
           targ_line = if_else(results_cumulative > targets, "white", grey80k)) %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = targets), fill = grey10k) +
    geom_col(aes(y = results_cumulative, fill = apr_fill)) +
    geom_errorbar(aes(ymin = targets, ymax = targets, color = targ_line), linetype = "dotted") +
    geom_text(aes(y = results_cumulative, label = percent(APR, 1)), size = 8/.pt, vjust = -0.5) +
  facet_wrap(~indicator, scales = "free_y", nrow = 2,
             labeller = labeller(indicator = add_space)) +
  si_style_ygrid() +
    scale_y_continuous(labels = comma) +
    scale_color_identity() +
    scale_fill_identity() +
    labs(x = NULL, y = NULL, caption = paste0("Produced on ",Sys.Date(), " using PEPFAR FY21Q2i MSD released on 2021-05-14.")) +
    theme(axis.text.x = element_text(size = 8))
  
  ggsave("Images/ZMB_FY21Q2_USAID_summary.png", height = 6, width = 12.6, scale = 1.25)
    
  # USAID ONLY
  df_usaid <- ou_im %>% 
    filter(indicator %in% indics,
           standardizeddisaggregate %in% c("Total Numerator"),
           fundingagency == "USAID") %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
    group_by(indicator) %>% 
    mutate(value_run = row_number(),
           gap = targets - results_cumulative,
           gap_denom = (4 - (substr(period, 6, 6) %>% as.numeric)),
           gap_pace = gap_calc(gap, gap_denom),
           APR = denom_share(results_cumulative, targets)) %>% 
    ungroup() %>% 
    mutate(indicator = fct_relevel(indicator, "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR",
                                   "PrEP_NEW", "OVC_SERV", "VMMC_CIRC"))

  df_usaid_sh <- 
    df_usaid %>% 
    rename(usaid_cumulative = results_cumulative) %>% 
    select(period, indicator, usaid_cumulative) %>% 
    right_join(df_ou) %>% 
    mutate(USAID_share = usaid_cumulative / results_cumulative) 
  
 df_usaid_sh %>% 
   mutate(alpha_value = if_else(period == "FY21Q2", 100, 50)) %>% 
   ggplot(aes(y = period, alpha = alpha_value)) +
   geom_col(aes(x = results_cumulative), fill = denim_light) +
   geom_col(aes(x = usaid_cumulative), fill = denim) +
   geom_text(aes(x = usaid_cumulative, label = percent(USAID_share, 1)), hjust = -0.1, size = 10/.pt)+
   facet_wrap(~indicator, nrow  = 2, scales = "free_x",
              labeller = labeller(indicator = add_space)) +
   scale_x_continuous(labels = comma) +
   scale_alpha(range = c(0.55, 1))+
   si_style_xgrid() +
   labs(x = "\nOU results by quarter", y = NULL, title = "",
        caption = paste0("Produced on ",Sys.Date(), " using PEPFAR FY21Q2i MSD released on 2021-05-14.")) +
   coord_cartesian(expand = F, clip = "off") +
   theme(legend.position = "none")
   
   
 ggsave("Images/ZMB_FY21Q2_USAID_share_summary.png", height = 6, width = 12.6, scale = 1.25)
    

# Data Visualization 
# aggregating USAID IM results for the quarter against targets for 4 indicators (HTS,HTS_POS, TX_NEW, TX_CURR); 
#  the example from our Health Office Portfolio is included as a placeholder for now in the google slides

# So, basically need to pull out major USAID IPs and repeat this for select indicators

usaid <- ou_im %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW"), 
         standardizeddisaggregate == "Total Numerator", 
         fundingagency == "USAID", 
         fiscal_year == 2021) %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
  ungroup() %>% 
  mutate(achv = cumulative / targets)

usaid %>% 
  mutate(line_color = if_else(cumulative > targets, "white", grey80k),
         indicator = fct_relevel(indicator, 
                                 "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")) %>% 
  ggplot(aes(x = indicator)) +
  geom_col(aes(y = targets), fill = grey10k) +
  geom_col(aes(y = cumulative), fill = genoa, alpha = 0.8) +
  geom_errorbar(aes(ymin = targets, ymax = targets, color = line_color), linetype = "dotted") +
  geom_text(aes(y = targets, label = comma(targets)), vjust = -0.5, size = 14/.pt)+
  geom_text(aes(y = cumulative, label = paste0(comma(cumulative), " (", percent(achv, 1), ")")), vjust = 1.25, color = "white",
            size = 14/.pt)+
  scale_color_identity() +
  si_style_xline() +
  scale_y_continuous(labels = comma, limits  = c(0, 700000)) +
  coord_cartesian(expand = F, clip = "on") +
  labs(x = NULL, y = NULL, title = "USAID PERFORMANCE ON TREATMENT CASCADE") +
  theme(axis.text.y = element_blank())

si_save("Images/USAID_performance_on_treatment.png", width = 9.35, height  = 6.47, scale = 0.90)

# SAME BUT BY IMS
denom_share <- function(x, y) {
  ifelse(y > 0.000, (x / y), NA_real_)
}


ims <- ou_im %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW"), 
         standardizeddisaggregate == "Total Numerator", 
         fundingagency == "USAID", 
         fiscal_year == 2021) %>% 
  mutate(mech_code = case_when(
    mech_code == "82075" ~ "18304",
    TRUE ~ mech_code
  ),
    mech_name = case_when(
      mech_name == "Local Treatment Partner" ~ "EQUIP", 
      TRUE ~ mech_name
    )) %>% 
  group_by(fiscal_year, indicator, mech_name, mech_code) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
  ungroup() %>% 
  mutate(FY21APR = denom_share(cumulative, targets))

ims_tbl <- 
  ims %>% mutate(indicator = fct_relevel(indicator, 
                                         "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
                 mech_code = fct_relevel(mech_code,
                                         "17413", "17399", 
                                         "17422", "17400",
                                         "17410", "18487", 
                                         "18304", "82075",
                                         "85117"))  %>% 
  #filter(mech_code %in% c("18304", "17399", "17413", "17400", "17410", "17422")) %>% 
  mutate(mech_name = case_when(
    str_detect(mech_name, "District Coverage") ~ "DISCOVER-Health",
    str_detect(mech_name, "Open Doors") ~ "Open Doors",
    str_detect(mech_name, "Zambia Community") ~ "ZCHPP",
    TRUE ~ mech_name),
    mech_name = fct_relevel(mech_name,
                            "EQUIP", "DISCOVER-Health", "SAFE",
                            "Eradicate TB", "ZCHPP", "Open Doors")
  ) %>% 
  select(-matches("qtr|fisc")) %>% 
  rename(results = cumulative) %>% 
  relocate(results, .before = targets) %>% 
  relocate(mech_code, .before = indicator) %>% 
  arrange(mech_code, indicator) %>% 
  mutate(gap = if_else(targets>0, targets - results, NA_real_)) %>% 
  mutate(circle = FY21APR, .after = FY21APR, 
         line_color = if_else(results > targets, "white", grey80k),
         max = pmax(results, targets),
         ach_max = if_else(FY21APR >= 1.25, 1.25, FY21APR))


ims_tbl %>% 
  #filter(str_detect(mech_name, "EQUIP|DISCOVER|SAFE")) %>% 
  filter(str_detect(mech_name, "ZCH|Open|Stop|Erad")) %>% 
  ggplot(aes(x = indicator)) +
  geom_col(aes(y = targets), fill = grey10k) +
  geom_col(aes(y = results), fill = "#047491", alpha = 0.85) +
  facet_wrap(~mech_name, labeller = labeller(mech_name = add_space), nrow = 1) +
  si_style_ygrid() +
  scale_y_continuous(labels = comma, position = "right") +
  geom_errorbar(aes(ymin = targets, ymax = targets, color = line_color), linetype = "dotted") +
  geom_text(aes(y = results, label = paste0(comma(results))), vjust = 1, color = "white",
            size = 10/.pt)+
  # geom_point(aes(y = max, fill = ach_max ), size = 10, shape = 21,) +
  geom_label(aes(y = max, label = percent(FY21APR, 1), fill = ach_max,
                       color = if_else(FY21APR >0.5, "white", grey80k)), size = 8/.pt)+
  # geom_text(aes(y = max, label = percent(FY21APR, 1)), size = 8/.pt, vjust = -1)+
  scale_color_identity() +
  scale_fill_si(palette = "scooters", discrete = F) +
  labs(x = NULL, y = NULL, title = "",
       caption = "Source FY21Q2i MSD released on 2021-05-14") +
  coord_cartesian(expand = F, clip = "off") +
  theme(legend.position = "off")


ggsave("Images/USAID_IP_tranch2_performance_on_treatment.svg", width = 13, height  = 5.5, scale = 1.25, dpi = "retina")




# OVC INDICATORS ----------------------------------------------------------
  ims_ovc <- ou_im %>% 
    filter(indicator %in% c("OVC_SERV", "OVC_SERV_UNDER_18"),
           standardizeddisaggregate == "Total Numerator", 
           fundingagency == "USAID", 
           fiscal_year == 2021) %>% 
    mutate(mech_code = case_when(
      mech_code == "82075" ~ "18304",
      TRUE ~ mech_code
    ),
    mech_name = case_when(
      mech_name == "Local Treatment Partner" ~ "EQUIP", 
      TRUE ~ mech_name
    )) %>% 
    group_by(fiscal_year, indicator, mech_name, mech_code) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
    ungroup() %>% 
    mutate(FY21APR = denom_share(cumulative, targets)) %>% 
  filter(qtr2 > 0) %>% 
  mutate(line_color = if_else(cumulative > targets, "white", grey80k),
         max = pmax(cumulative, targets))

ims_ovc %>% 
  ggplot(aes(x = indicator)) +
  geom_col(aes(y = targets), fill = grey10k) +
  geom_col(aes(y = cumulative), fill = "#047491", alpha = 0.85) +
  facet_wrap(~mech_name, labeller = labeller(mech_name = add_space), nrow = 1) +
  si_style_ygrid() +
  scale_y_continuous(labels = comma, position = "right") +
  geom_errorbar(aes(ymin = targets, ymax = targets, color = line_color), linetype = "dotted") +
  geom_text(aes(y = cumulative, label = paste0(comma(cumulative))), vjust = 1, color = "white",
            size = 10/.pt)+
  # geom_point(aes(y = max, fill = ach_max ), size = 10, shape = 21,) +
  geom_label(aes(y = max, label = percent(FY21APR, 1), fill = FY21APR,
                 color = if_else(FY21APR >0.5, "white", grey80k)), size = 8/.pt)+
  # geom_text(aes(y = max, label = percent(FY21APR, 1)), size = 8/.pt, vjust = -1)+
  scale_color_identity() +
  scale_fill_si(palette = "scooters", discrete = F, limits = c(0, 1)) +
  labs(x = NULL, y = NULL, title = "",
       caption = "Source FY21Q2i MSD released on 2021-05-14") +
  coord_cartesian(expand = F, clip = "off") +
  theme(legend.position = "off") 
  
ggsave("Images/USAID_IP_tranch3_OVC_performance_on_treatment.svg", width = 13, height  = 5.5, scale = 1.25, dpi = "retina")

# CXCA TRENDS -------------------------------------------------------------

  ou_im %>% 
  filter(indicator %in% c("CXCA_SCRN", "CXCA_SCRN_POS", "CXCA_TX"), 
                   standardizeddisaggregate == "Total Numerator", 
                   fundingagency == "USAID") %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
  ungroup() %>% 
  mutate(FY21APR = denom_share(cumulative, targets))
