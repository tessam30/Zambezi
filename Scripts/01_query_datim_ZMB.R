## PROJECT:  find your beach
## AUTHOR:   A.Chafetz | USAID
## LICENSE:  MIT
## PURPOSE:  pull coordinates for USAID sites
## NOTE:     drawing heavily from USAID-OHA-SI/right_size/pull_datim
## DATE:     2020-04-09
## UPDATED:  2020-04-29


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)
library(fs)


# GLOBAL VARIABLES --------------------------------------------------------

myuser <- "tessam"


# DATIM API FUNCTION ------------------------------------------------------

  query_datim <- function(ou_uid, org_lvl, type, username, password, baseurl = "https://final.datim.org/"){
    
    print(paste("running ", ou_uid, " ... ", Sys.time()))
    
    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             "dimension=bw8KHXzxd9i:NLV6dy7BE2O&" #Funding Agency -> USAID
             )
    
    if(type == "TX"){
      type_url <- 
        paste0("dimension=pe:2019Oct&", #period
               "dimension=IeMmjHyBUpi:W8imnja2Owd&", #Targets/Results - Targets W8imnja2Owd,Jh0jDM5yQ2E
               "dimension=LxhLO68FcXm:MvszPTQrUhy&", #technical area, TX_CURR
               "filter=RUkVjD3BsS1:PE5QVF0w4xj&" #Top Level  - Numerator
               )
    } else if (type == "HTS"){
      type_url <- 
        paste0("dimension=pe:2019Oct&", #period
               "dimension=IeMmjHyBUpi:W8imnja2Owd&", #Targets/Results - Targets W8imnja2Owd,Jh0jDM5yQ2E
               "dimension=LxhLO68FcXm:f5IPTM7mieH;wdoUps1qb3V;BTIqHnjeG7l;rI3JlpiuwEK;CUblPgOMGaT&&", #technical area,  HTS_TST, HTS_INDEX, PMTCT_STAT, TB_STAT, VMMC_CIRC
               "filter=bDWsPYyXgWP:mSBg9AZx1lV;viYXyEy7wKi;awSDzziN3Dn;EvyNJHbQ7ZE&", #HIV Test Status (Specific)
               "filter=Jm6OwL9IqEa:FVCb4OUYkiG;NcerbGnPdIc;oQmZ3urcBbP;xmYVJTekCtl;NwSG4Qshiu1;H6bOQCcwrBV;INrbTYREjOX;d7vYERohPfS;lRAOEGldPtj;sZWARAOoyvZ;qdFQQc5dCbH;OAwcN9zCmLM;t7WRCjHNaEU;K4CBGfivWfg;ML7FROoRi6r;v5AuS5Aw9Ks&" #HTS Modality (USE ONLY for FY19 Results/FY20 Targets)
        )
    } else if(type == "LAB"){
      type_url <- 
        paste0("dimension=pe:2019Q3&", #period
               "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets/Results - Results W8imnja2Owd,Jh0jDM5yQ2E
               "dimension=LxhLO68FcXm:scxfIjoA6nt&", #technical area, LAB_PTCQI
               "dimension=HWPJnUTMjEq:T7Z0TtiWqyu;SG4w1HBS23B;MC7Q6BN0Xw9;hfaBo0nrQok;oBbMk5GjX4a;PJEPs8sHAk5&" #Disaggregation Type = Lab/CQI, Lab/PT, Lab/TestVolume, POCT/CQI, POCT/PT, POCT/TestVolume"
        )
      
    } else if(type == "SC_STOCK"){
      type_url <- 
        paste0("dimension=pe:2019Q3&", #period
               "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets/Results - Results W8imnja2Owd,Jh0jDM5yQ2E
               "dimension=LxhLO68FcXm:Wcg6Zu3y7OE&" #technical area, SC_STOCK
               #categoryoption combo for components
               # "dimension=dx:FDlLTtGvb6d.M19pNu5afz5;FDlLTtGvb6d.ghnHGxQGzsU;FDlLTtGvb6d.f1MsjgHEA1F;FDlLTtGvb6d.FsVyz3CsO23;FDlLTtGvb6d.xQzCIqxQrOD;FDlLTtGvb6d.zpAeF87eVrG;FDlLTtGvb6d.hEBJzAkrIVn;qQ7UK8XhpHm.M19pNu5afz5;",
               #   "qQ7UK8XhpHm.ghnHGxQGzsU;qQ7UK8XhpHm.f1MsjgHEA1F;qQ7UK8XhpHm.FsVyz3CsO23;qQ7UK8XhpHm.xQzCIqxQrOD;qQ7UK8XhpHm.zpAeF87eVrG;qQ7UK8XhpHm.hEBJzAkrIVn;x0qTD4eEKoS.M19pNu5afz5;x0qTD4eEKoS.ghnHGxQGzsU;x0qTD4eEKoS.f1MsjgHEA1F;",
               #   "x0qTD4eEKoS.FsVyz3CsO23;x0qTD4eEKoS.xQzCIqxQrOD;x0qTD4eEKoS.zpAeF87eVrG;x0qTD4eEKoS.hEBJzAkrIVn;zNY7jEoat5w.M19pNu5afz5;zNY7jEoat5w.ghnHGxQGzsU;zNY7jEoat5w.f1MsjgHEA1F;zNY7jEoat5w.FsVyz3CsO23;zNY7jEoat5w.xQzCIqxQrOD;zNY7jEoat5w.zpAeF87eVrG;zNY7jEoat5w.hEBJzAkrIVn&"
        )
      
    } 
    
    end_url <- "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true"
    
    full_url <- paste0(core_url, type_url, end_url)
    
    df <- get_datim_targets(full_url, username, password)
  
  return(df)
}




# IDENTIFY INPUTS FOR API -------------------------------------------------

  #identify site level in each ou
    df_lvls <- identify_levels(username = myuser, password = mypwd(myuser)) %>% 
      filter(name3 == "Zambia")
  
  #pull orgunit uids
    df_uids <- identify_ouuids(username = myuser, password = mypwd(myuser)) %>% 
      filter(displayName == "Zambia")
  
  #table for API use
    ctry_list <- left_join(df_lvls, df_uids, by = c("country_name" = "displayName")) %>% 
      select(operatingunit = name3, operatingunituid = id, countryname = country_name, 
             psnu_lvl = prioritization, site_lvl = facility)
    
    rm(df_lvls)


# PULL DATA ---------------------------------------------------------------


  #run API across all countries
    df_tx <- map2_dfr(.x = ctry_list$operatingunituid, 
                      .y = ctry_list$site_lvl, 
                      .f = ~ query_datim(.x, .y, "TX", myuser, mypwd(myuser)))
      
    df_hts <- map2_dfr(.x = ctry_list$operatingunituid, 
                      .y = ctry_list$site_lvl, 
                      .f = ~ query_datim(.x, .y, "HTS", myuser, mypwd(myuser)))
    
      
    df_lab <- map2_dfr(.x = ctry_list$operatingunituid, 
                       .y = ctry_list$site_lvl, 
                       .f = ~ query_datim(.x, .y, "LAB", myuser, mypwd(myuser)))
    
    df_stk <- map2_dfr(.x = ctry_list$operatingunituid, 
                       .y = ctry_list$site_lvl, 
                       .f = ~ query_datim(.x, .y, "SC_STOCK", myuser, mypwd(myuser)))
    


# PULL COORDINATES --------------------------------------------------------

  #pull hierarchy
    df_orgs <- purrr::map_dfr(.x = df_uids$id,
                              .f = ~ pull_hierarchy(.x, myuser, mypwd(myuser))) 
    

# MUNGE AND APPEND DATA ---------------------------------------------------

  #combine all technical areas into HTS (modalities are derived from VMMC, TB, PMTCT, INDEX)
    df_hts <- df_hts %>% 
      mutate(`Technical Area` = "HTS_TST") %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.double, sum, na.rm = TRUE) %>% 
      ungroup()
    
  #remove disaggs from lab
    df_lab <- df_lab %>% 
      spread(`Disaggregation Type`, Value) %>% 
      rowwise() %>% 
      mutate(Value = sum(`Lab/TestVolume`, `POCT/TestVolume`, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by_if(is.character) %>% 
      summarise_at(vars(Value), sum, na.rm = TRUE) %>% 
      ungroup()
    
  #append data together
    df_full <- bind_rows(df_tx, df_hts, df_lab, df_stk)

  #limit output variables
    df_sel <- df_full %>% 
      select(fundingagency = `Funding Agency`,
             resulttarget = `Targets / Results`,
             indicator = `Technical Area`,
             period = Period,
             orgunituid, 
             region = orglvl_2,
             orglvl_3, orglvl_4,
             value = Value)
    
  #merge with hierarchy/coordinates
    df_sites <- left_join(df_sel, df_orgs)
  
  
  #add iso codes
    df_sites <- df_sites %>% 
      left_join(iso_map, by = c("countryname" = "operatingunit"))  %>% 
      select(-regional) %>% 
      select(countryname, iso, everything())
    
  #keep long version  
    df_sites_lng <- df_sites %>% 
      select(countryname, iso, region, orgunituid, latitude, longitude, indicator, value)
      
  #reshape for mapping
    df_sites_wide <- df_sites %>% 
      select(countryname, iso, region, orgunituid, latitude, longitude, indicator) %>% 
      mutate(exists = "X") %>% 
      spread(indicator, exists)

    
  
    
# EXPORT ------------------------------------------------------------------

  write_csv(df_sites_wide, "Data/SBU_PEPFAR_USAID_Site_Coordinates_v3_SBU.csv", na = "")    
  write_csv(df_sites_lng, "Data/SBU_PEPFAR_USAID_Site_Coordinates_v3_long_SBU.csv", na = "")    

    
    

    # #checks
    # df_lng <- df_sites %>%
    #   gather(indicator, reported, HTS_TST, LAB_PTCQI, SC_STOCK, TX_CURR, na.rm = TRUE)
    # 
    # df_distinct <- df_sites %>%
    #   distinct(countryname, orgunituid) %>%
    #   count(countryname, name = "total")
    # 
    # 
    # df_lng %>%
    #   count(region, countryname, indicator) %>%
    #   spread(indicator, n) %>%
    #   left_join(df_distinct) %>%
    #   select(region, countryname, total, HTS_TST, TX_CURR, LAB_PTCQI, SC_STOCK) %>%
    #   arrange(region, countryname) %>%
    #   janitor::adorn_totals()
    #   print(n = Inf)

      
      
        