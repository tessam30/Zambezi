# Pull in Zambia BOB data to get monthly updates on indicators
# Author: Tim Essam
# Date: 2020_06_30




# LIBRARIES ---------------------------------------------------------------

  library(tidyverse)
  library(glamr)
  library(glitr)
  library(gisr)
  library(googledrive)
  library(readxl)
  library(here)
  library(Wavelength)



# GLOBALS -----------------------------------------------------------------

  fetch_in <- "BOBs"
  data_out <- "Dataout"
  images <- "Images"
  folder_setup(list("BOBs"))

  
  read_bob <- function(filename, sheet = "monthly_bob", skip = 4) {
    
  mech_name_tmp <- filename %>% str_extract(., pattern = regex(".+?(?=_BOB)"))
      
   df <- read_excel(
     here(fetch_in, filename),
     sheet = sheet,
     skip = skip
   )  %>% 
     mutate(mech_name = mech_name_tmp,
            mech_code = as.character(mech_code))
   
   print(df %>% names())

  return(df)    

  }
  
  
# READ in HFR
  hfr_file <- "HFR__FY20_PD9_Zambia_Partners6Data_20200625_a.xlsx"
  output_folder <- "Dataout"
  
  #process Excel file for Saturn
  hfr_process_template(here(fetch_in, hfr_file), folderpath_output = data_out)
  
  #process Excel file for Saturn with full validation
  datim_folder <- "~/Datim"
  hfr_process_template(path, output_folder, datim_path = datim_folder)
  

# FETCH DATA -----------------------------------------------------------
  #Data stored on gdrive, retrieve

  drive_auth(email = "")

  # Setup bob id to use as an argument in drive functions; makes fetching eaiser
  bob_folder <- "17kLeOKEOZB-L5mxk53apC3srvDsyMW17"

  # List the files of interest so we know what to fetch
  drive_ls(as_id(bob_folder))
  bob_ids <- drive_ls(as_id(bob_folder), pattern = "BOB") %>% pull(name)

  #download
  walk(bob_ids, ~ import_drivefile(bob_folder, .x, folderpath = fetch_in, zip = "FALSE"))

# CLEAN UP  ---------------------------------------------------------------
  
  # Let's document process of reshaping/tidying, see if we can functionalize it
  bobs <- list.files(path = fetch_in, pattern = "BOB")
  
  # Review the excel sheets for hidden tabs
  map(bobs, ~excel_sheets(path = here(fetch_in, .)))
  
  
  #EQUIP is missing tidy headings, need to modify file to add them in
  equip   <- read_bob("EQUIP_BOB_May_2020_tidy.xlsx") %>% 
    mutate(mech_code = if_else(mech_code == "18305", "18304", mech_code))
  
  discover<- read_bob("DISCOVER_BOB_May 2020.xlsx")
  
  odoors <- read_bob("Open_Doors_BOB_May_2020.xlsx") %>% 
    filter(!is.na(mech_code)) 
  
  safe    <- read_bob("SAFE_BOB_May_2020.xlsx") 
  
  stopgbv <- read_bob("Stop_GBV_BOB_May_2020.xlsx") %>% 
    filter(!is.na(mech_code)) 
  
  Zchpp <- read_bob("Z-CHPP_BOB_May_2020.xlsx", sheet = "monthly_bob_With DREAMS") %>% 
    filter(!is.na(mech_code))
  
  # Rename DISCOVER cervical cancer screening disaggregates
  # [82] "cxca_first_15+...82"     "cxca_rescreen_15+...83"  "cxca_follow_up_15+...84"
  # [85] "cxca_first_15+...85"     "cxca_rescreen_15+...86"  "cxca_follow_up_15+...87"
  # [88] "cxca_follow_up_15+...88
  
  discover <- 
    discover %>% 
    rename(cxca_first_o15      = `cxca_first_15+...82`,
           cxca_rescreen_o15   = `cxca_rescreen_15+...83`,
           cxca_follow_up_o15  = `cxca_follow_up_15+...84`,
           cxca_via_neg_result = `cxca_first_15+...85`,
           cxca_via_pos_result = `cxca_rescreen_15+...86`,
           cxca_scrn_cancer    = `cxca_follow_up_15+...87`,
           cxca_scrn_thermo    =`cxca_follow_up_15+...88`)
  
  equip <- equip %>% 
    rename(cxca_first_o15      = `...82`,
           cxca_rescreen_o15   = `...83`,
           cxca_follow_up_o15  = `...84`,
           cxca_via_neg_result = `...85`,
           cxca_via_pos_result = `...86`,
           cxca_scrn_cancer    = `...87`,
           cxca_scrn_thermo    = `...88`,
           cxca_tx_leep_o15    = `...89`) %>% 
    dplyr::select(-contains("..."))
  
  # NOTE: SAFE switches order of cxca_via_pos / cxca_via_neg results in raw data --- be aware
  safe <- safe %>% 
    rename(cxca_first_o15      = `cxca_first_15+`,
           cxca_rescreen_o15   = `cxca_rescreen_15+`,
           cxca_follow_up_o15  = `cxca_follow_up_15+`,
           cxca_via_neg_result = `cxca_scrn_negative_15+`,
           cxca_via_pos_result = `cxca_scrn_positive_15+`,
           cxca_scrn_cancer    = `cxca_scrn_suspected_cancer_15+`,
           cxca_scrn_thermo    = `cxca_tx_cryotherapy_thermocoagulation_15+`)
  
  mech_list <- list(discover, equip, odoors, safe, stopgbv, Zchpp)


  map(mech_list, ~ (.) %>% count(mech_name, mech_code))  
  
# Get a site count for each mechanism

  
  
# To process split out HFR files 
  
  devtools::load_all()
  
  #files to import
  (file <- list.files("ou_submissions/", full.names = TRUE))
  
  #ensure all files have at least one tab to be imported (need to have HFR in tab name)
  purrr::walk(file, is_hfrtab)
  
  #run validations to check if there are any issues
  purrr::walk(file,
              hfr_process_template)
  
  #if okay, run full validation and output file
  purrr::walk(file,
              hfr_process_template, round_hfrdate = TRUE, 
              # hfr_pd_sel = 8,
              folderpath_output = "out/processed",
              datim_path = "out/DATIM")
  
  #output for late/fixes in the HI tracker (CLIPR > Value to clipboard )
  list.files("out/processed") %>%
    tibble::tibble(filename =.) %>%
    tidyr::separate(filename, c(NA, "pd", "iso", "mech", NA, "date_processed"), sep = "_") %>%
    dplyr::mutate(date_processed = stringr::str_remove(date_processed, "\\..*") %>% lubridate::as_date(),
                  date_added = lubridate::today()) %>%
    dplyr::select(date_added, everything())
  
  