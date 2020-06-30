# Pull in Zambia BOB data to get monthly updates on indicators
# Author: Tim Essam
# Date: 2020_06_30




# LIBRARIES ---------------------------------------------------------------

  library(tidyverse)
  library(glamr)
  library(glitr)
  library(gisr)
  library(googledrive)



# GLOBALS -----------------------------------------------------------------

  fetch_in <- "BOBs"
  data_out <- "Dataout"
  images <- "Images"


# FETCH DATA -----------------------------------------------------------
  #Data stored on gdrive, retrieve

  drive_auth()

  # Setup bob id to use as an argument in drive functions; makes fetching eaiser
  bob_folder <- "17kLeOKEOZB-L5mxk53apC3srvDsyMW17"

  # List the files of interest so we know what to fetch  
  drive_ls(as_id(bob_id))
  bob_ids <- drive_ls(as_id(bob_id), pattern = "BOB") %>% pull(name)

  #download
  walk(bob_ids, ~ import_drivefile(bob_folder, .x, folderpath = fetch_in, zip = "FALSE"))
