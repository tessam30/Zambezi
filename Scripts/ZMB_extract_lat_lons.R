# Purpose: Pull Zambia lat / lon
# Author: Tim Essam
# Date: 2021-01-27
# Notes: PULLS late longs for Zambia



  # Levels
  df_lvls <- glamr::identify_levels(datim_user(), datim_pwd())
  
  # Comm level
  comm_lvl <- df_lvls %>%
    filter(operatingunit == country) %>%
    pull(community)
  
  # Orgs
  df_locs <- gisr::extract_locations(country, datim_user(), datim_pwd())
  
  # Community uids
  comm_uids <- df_locs %>%
    filter(level == comm_lvl) %>%
    pull(id)
