# Fetch and download all HFR
# Author: T.Essam
# 2020-07-02


# LIBRARIES ---------------------------------------------------------------

  library(googledrive)
  library(tidyverse)



# GLOBALS -----------------------------------------------------------------


# FETCH HFR and DOWNLOAD --------------------------------------------------

  folder_id <- "1nIBZCDNHmKRDAp7qABDfYp2y_7z14WwZ"

  # Search through folder for all Zambia files
  drive_auth()
  
  drive_ls(path = folder_id, pattern = "Zambia")
