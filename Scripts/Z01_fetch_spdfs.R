#Purpose: PUll Zambia levels, save admin0-admin2 polygons and generate lat/lons

  library(tidyverse)
  library(glamr)


# GLOBALS -----------------------------------------------------------------
  # Load configs
  source("./Scripts/Z00_Config.R")
  
  # Function to pull level number
  get_spdf <- function(geo_level, cntry = "Zambia") {
      
    # Grab the level of geography from df_lvls table
    lvls <- df_lvls %>%
        dplyr::filter(operatingunit == {{cntry}}) %>%
        pull({{geo_level}})
    
    # Pull out the ids for each row corresponding to geometry
    lvl_id <- df_locs %>%
      dplyr::filter(level == lvls) %>%
      pull(id)
    
    # Filter the PEPFAR polygon shapefile and return result
    spdf <- 
      spdf_pepfar %>% 
      dplyr::filter(uid %in% lvl_id)
    
    return(spdf)
    
  }
    
  # Geodata
  file_shp <- 
    list.files(
      path = shpdata, 
      pattern = "VcPepfarPolygons.shp$",
      recursive = TRUE,
      full.names = TRUE) %>% 
    sort()  %>% 
    dplyr::last()  
  
  # Grab DATIM info
  
  # PEPFAR Geodata
  spdf_pepfar <- file_shp %>% sf::read_sf()
  
  # OUs
  df_ous <- glamr::identify_ouuids(datim_user(), datim_pwd())
  
  # Levels
  df_lvls <- glamr::identify_levels(datim_user(), datim_pwd()) 
  
  # Orgs & Printing out Zambia output for quick review
  df_locs <- gisr::extract_locations(
    country,
    level = NULL,
    add_geom = TRUE,
    username = NULL,
    password = NULL
  )
  df_lvls %>% dplyr::filter(countryname == "Zambia")
  
  # Region/Province IDS live df_locs dataframe under level 4 (not in df_lvls)
  reg_ids <- df_locs %>% 
    dplyr::filter(level == 4) %>% 
    pull(id)
 
  # Return Administrative uids (ou, )
  spdf_comm_zmb <- get_spdf("community")
  spdf_ou_zmb <- get_spdf("country_lvl")

    prov_cw <- tibble::tribble(
         ~province,          ~uid,
         "Western", "ZP7sUNCemnF",
         "Central", "glHv5VLHKi0",
        "Northern", "IJ4fAvJ0HJf",
         "Eastern", "KL9KJYtIrPo",
         "Luapula", "Vx5hU3RgmjC",
          "Lusaka", "fKHv5qcp1nN",
      "Copperbelt", "ujvP4QAHDoM",
        "Muchinga", "QPIMzDJei82",
        "Southern", "ecXXey5nCpd",
    "NorthWestern", "LuBA2MsPgGd",
        "Military", "y0cygdhumHI"
    )

    spdf_reg_zmb <- 
      spdf_pepfar %>% 
      dplyr::filter(uid %in% reg_ids) %>%
      left_join(., prov_cw)
   
    df_locs %>% 
    gisr::extract_facilities()
  
  # Get a terrain map for Zambia
  terr_map <- gisr::terrain_map(country, adm1 = spdf_reg_zmb,
                          adm0 = spdf_ou_zmb, terr = rasdata, mask = T)
  
  # Add in snu1 and psnu names to spdf_Comm_zmb for fuzzy joins
  # Latest MSD PSNU x IM File - Curr release
  file_msd <- return_latest(folderpath = datim, 
                            pattern = "MER_S.*_PSNU_IM_FY18-21_\\d{8}_v.*Zambia.*.zip")
  
  msd_geo <- 
    ICPIutilities::read_msd(file_msd) %>% 
    distinct(psnuuid, psnu, operatingunit, operatingunituid, snu1)
  
  spdf_comm_zmb <-  
    left_join(spdf_comm_zmb, msd_geo, by = c("uid" = "psnuuid")) %>% 
    glamr::clean_psnu() %>% 
    dplyr::mutate(snu1 = str_remove_all(snu1, " Province"))
    
  # Clean up workspace.
  remove(df_locs, df_lvls, df_ous, spdf_pepfar)  


  
 