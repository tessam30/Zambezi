# Purpose: Look at net new at a site level
# Author: Tim Essam
# Date: 2021-01-19
# Notes: KP_prev map of where all IPs are working


# GLOBALS -----------------------------------------------------------------

  # Source basemap layers and check rendering
  #source("Scripts/00_setup_maps.R")

  library(ICPIutilities)
  library(glamr)
  library(here)
  library(glitr)
  library(gisr)
  library(scales)
  library(sf)
  library(rgdal)
  library(raster)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(tidyverse)

  

  # Load configs & retrieve pepfar polygons
  source("./Scripts/Z01_fetch_spdfs.R")


  # Set Filters
  indics <- c("KP_PREV")
  caption <- paste0("Source: DATIM MSD PSNU_IM_FY18-21_20201218 | Created on: ", Sys.Date())
  
  # Target function - generate aggregates for FY21
    sum_targets <- function(.df, ...) {
    
        .df %>% 
        filter(fiscal_year == "2021") %>% 
        group_by(...) %>% 
        summarise(targets = sum(targets, na.rm = TRUE)) %>% 
        ungroup()
    }
    
    
    # Latest MSD PSNU x IM File - Curr release
    file_msd <- return_latest(folderpath = datim, 
                              pattern = "MER_S.*_PSNU_IM_FY18-21_\\d{8}_v.*Zambia.*.zip")
    
    # Geodata
    file_shp <- list.files(
          path = shpdata, 
          pattern = "VcPepfarPolygons.shp$",
          recursive = TRUE,
          full.names = TRUE
          ) %>% 
      sort() %>% 
      last()
    
    # Levels
    df_lvls <- glamr::identify_levels(datim_user(), datim_pwd())
    
  
# LOAD AND MUNGE MER DATA -------------------------------------------------

  # First, load geodata
    # Pull in unique id for all psnus in zambia
    msd_geo <- 
      read_msd(file_msd) %>% 
      select(psnuuid, psnu, operatingunit, operatingunituid, snu1)
    
  # Unique psnuuids/operatingunituids needed to filter the PEPFAR global dataset and admin0 boundary
    uids <- 
      msd_geo %>% 
      distinct(psnuuid, operatingunituid, psnu, snu1, snu1uid)
    
    # Load Zambia Shapefile and filter to psnus
      zmb_geo <- 
        st_read(file_shp) %>% 
        filter(uid %in% uids$psnuuid) %>% 
        rename(psnuuid = uid)
    
      zmb_admin0 <- 
        st_read(file_shp) %>% 
        filter(uid %in% uids$operatingunituid) 

      amb_admin1 <- 
        st_read(file_shp) %>% 
        filter(uid %in% uids$snu1uid)
      
    remove(msd_geo)
 
  # Lad datim data for munging
    msd <- read_msd(file_msd) %>% 
      filter(indicator %in% indics, standardizeddisaggregate == "Total Numerator") %>% 
      mutate(fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency))
  
  # Reproduce 2020 DATIM Results -- 90,026
    msd %>% 
      group_by(indicator, fiscal_year) %>% 
      summarise(total = sum(cumulative, na.rm = TRUE))
    
  # Check the geographic distribution of partners by district
    msd %>% 
      filter(fiscal_year == "2021") %>% 
      count(psnu, fundingagency, primepartner)%>% 
      spread(fundingagency, n) %>% 
      prinf()
    
  # Merge shapefile to msd for plotting
  # Need the coords for ggrepel and labeling
    msd_geo <- left_join(zmb_geo, 
                         msd %>% filter(fiscal_year == "2021"), 
                         by = c("psnuuid")) %>% 
      mutate(no_kp = if_else(is.na(operatingunit), 0, 1),
             fill_color = case_when(
               fundingagency == "CDC" ~ scooter, 
               fundingagency == "USAID" ~ old_rose,
               TRUE ~ "NA"
             )) %>% 
      group_by(psnuuid) %>% 
      mutate(
        CENTROID = map(geometry, st_centroid),
        COORDS = map(CENTROID, st_coordinates),
        COORDS_X = map_dbl(COORDS, 1),
        COORDS_Y = map_dbl(COORDS, 2)
      )
  
    msd_geo_totals <- sum_targets(msd, fundingagency, psnu, psnuuid)
    msd_geo_partner_totals <- sum_targets(msd, fundingagency, psnu, psnuuid, primepartner)  
    
    # Join in geographic information so they can be mapped
    msd_geo_totals <- left_join(zmb_geo, msd_geo_totals)   
    msd_geo_partner_totals_sf <- left_join(zmb_geo, msd_geo_partner_totals)

# MAP & PLOT --------------------------------------------------------------

  # Make bounding box in case map needs to be cropped to msd_geo range
    mapRange <- c(range(st_coordinates(msd_geo)[, 1]), range(st_coordinates(msd_geo)[, 2]))
    
  # Terrain map
   terr_map <-  terrain_map("Zambia",
                terr_path = rasdata,
                adm0 = zmb_admin0,
                adm1 = zmb_admin1)
    
  # Basic filled map showing where CDC and USAID have KP_PREV coverage  
      kp_cov_map <- 
        terr_map +
         geom_sf(data = zmb_geo, fill = grey10k, colour = grey20k, alpha = 0.5) +
         geom_sf(data = msd_geo %>% filter(fundingagency != "NA") %>% distinct(psnuuid, fundingagency, fill_color), 
                 aes(fill = fill_color), color = grey90k, size = 0.75, alpha = 0.75) +
         geom_sf(data = zmb_admin1, fill = "NA", colour = grey70k, size = 0.5, linetype = "dotted") +
         geom_sf(data = zmb_admin0, fill = "NA", colour = grey70k, size = 1) +
         ggrepel::geom_label_repel(data = msd_geo %>% filter(fundingagency != "NA") %>%
                                    distinct(psnu, COORDS_X, COORDS_Y, fundingagency),
                      aes(x = COORDS_X,
                          y = COORDS_Y,
                          label = str_replace_all(psnu, " District", "")),
                      force = 6,
                      fill = alpha(c("white"), 0.75),
                      label.size = NA,
                      label.padding = 0.1,
                      family = "Source Sans Pro Light") +
         facet_wrap(~fundingagency) +
         scale_fill_identity() +
         coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])  +
         si_style_void() +
         theme(panel.grid = element_blank(),
               strip.text = element_text(size = 16)) +
         labs(title = "KP_PREV COVERAGE BY DISTRICT FOR FY21",
              subtitle = "Filled districts indicate FY21 targets greater than 0 \n",
              caption = caption)
      
      kp_cov_map
   
   #si_save(here(images, "ZMB_KP_PREV_COVERAGE_BY_DISTRICT_AGENCY_labels.png"), scale = 1.4)
    si_save(here(images, "ZMB_KP_PREV_COVERAGE_BY_DISTRICT_AGENCY_no_labels.png"), plot = kp_cov_map,
           scale = 1.4)   
     
     
    
  # Plot target volumes by psnu 
    map_targets <-  
      terr_map +
       geom_sf(data = zmb_geo, fill = grey10k, colour = grey20k, alpha = 0.5) +
       geom_sf(data = msd_geo_totals %>% filter(fundingagency != "NA"), 
               aes(fill = targets), alpha = 0.85) + 
      geom_sf(data = zmb_admin1, fill = "NA", colour = grey70k, size = 0.5, linetype = "dotted") +
       geom_sf(data = zmb_admin0, fill = "NA", colour = grey70k, size = 1) +
      # ggrepel::geom_label_repel(data = msd_geo %>% filter(fundingagency != "NA") %>%
      #                             distinct(psnu, COORDS_X, COORDS_Y, fundingagency),
      #                           aes(x = COORDS_X,
      #                               y = COORDS_Y,
      #                               label = str_replace_all(psnu, " District", "")),
      #                           force = 6,
      #                           fill = alpha(c("white"), 0.75),
      #                           label.size = NA,
      #                           family = "Source Sans Pro Light") +
      facet_wrap(~paste0(fundingagency, " FY21 KP_PREV TARGETS"), ncol = 1) +
       scale_fill_si(palette = "scooters", discrete = FALSE, trans = "log") +     
       coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])  +
       si_style_void() +
       theme(panel.grid = element_blank(),
             strip.text = element_text(size = 16)) +
       labs(x = NULL, y = NULL) +
       theme(legend.position = "none")
    
    map_targets
     
   
    bar_targets <-  
      msd_geo_totals %>% 
       filter(fundingagency != "NA") %>% 
       mutate(psnu_order = reorder_within(str_replace_all(psnu, " District", ""), targets, fundingagency)) %>% 
       ggplot(aes(x = targets, y = psnu_order, fill = (targets))) +
       geom_col() +
         facet_wrap(~fundingagency, scales = "free_y", ncol = 1
                    ) +
       scale_y_reordered() +
       scale_fill_si(palette = "scooters", discrete = FALSE, trans = "log", alpha = 0.85) +
       si_style_xgrid() +
       scale_x_continuous(labels = comma) +
       theme(legend.position = "none", 
             strip.text = element_blank()) +
       labs(x = NULL, y = NULL)
    bar_targets

    map_targets + bar_targets +
      plot_annotation(title = "KP_PREV TARGETS BY DISTRICT FOR FY21",
                      subtitle = "Dark colors (longer bar) indicates higher targets.",
                      caption = caption,
                      theme = theme(text = element_text("Source Sans Pro"))) + 
      plot_layout(widths = c(1, 1))
    
    si_save(here(images, "KP_PREV_FY21_TARGETS_BY_AGENCY_no_labels_.png"), scale = 1.75)


  # Map by IPs
     map_partner <- 
       terr_map +
        geom_sf(data = zmb_geo, fill = grey10k, colour = grey20k, alpha = 0.5) +
        geom_sf(data = msd_geo_partner_totals_sf %>% filter(fundingagency != "NA") %>% 
                  mutate(agency_partner = paste0(fundingagency, ": ", primepartner, "\n")), 
                aes(fill = agency_partner), alpha = 0.9) +
        geom_sf(data = zmb_admin0, fill = "NA", colour = grey70k, size = 1) +
        # ggrepel::geom_label_repel(data = msd_geo %>% filter(fundingagency != "NA") %>%
        #                             distinct(psnu, COORDS_X, COORDS_Y, fundingagency),
        #                           aes(x = COORDS_X,
        #                               y = COORDS_Y,
        #                               label = str_replace_all(psnu, " District", "")),
        #                           force = 6,
        #                           fill = alpha(c("white"), 0.75),
        #                           label.size = NA,
        #                           family = "Source Sans Pro Light") +
        facet_wrap(~agency_partner) +
        scale_fill_si(palette = "siei") +     
        coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])  +
        si_style_void() +
        theme(panel.grid = element_blank(),
              strip.text = element_text(size = 10)) +
        labs(x = NULL, y = NULL) +
        theme(legend.position = "none")
 
 # bar_targets_partner 
     bar_partner <-  msd_geo_partner_totals %>% 
        filter(fundingagency != "NA") %>% 
        mutate(agency_partner = paste0(fundingagency, ": ", primepartner, "\n"),
               psnu_order = reorder_within(str_replace_all(psnu, " District", ""), targets, agency_partner)) %>% 
        ggplot(aes(x = targets, y = psnu_order, group = primepartner, fill = agency_partner)) +
        geom_col() +
        facet_wrap(~ agency_partner, scales = "free_y", nrow= 1,
                   labeller = labeller(agency_partner = label_wrap_gen(40))) +
        scale_y_reordered() +
        scale_fill_si(palette = "siei", alpha = 0.9) +
        si_style_xgrid() +
        scale_x_continuous(labels = comma) +
        theme(legend.position = "none", 
              strip.text = element_text(size = 8),
              axis.text.x = element_text(size = 8)) +
      labs(x = NULL, y = NULL, 
           subtitle = "FY21 KP_PREV targets by partner")
     
   map_partner + 
     bar_partner +
     plot_layout(nrow = 2, heights = c(4, 1)) +
     plot_annotation(title = "KP_PREV COVERGE BY DISTRICT AND PRIME PARTNER FOR FY21",
                     subtitle = "Coverage based on FY21 targets greater than 0",
                     caption = caption,
                     theme = theme(text = element_text("Source Sans Pro"))) 

  si_save(here(images, "KP_PREV_FY21_TARGETS_BY_PARTNER_ANDAGENCY_no_labels_.png"), scale = 1.75)
 
 
  # Map IPS + Bar graph + CDC on LHS + USAID on RHS
  # Functions for agency calls
     agency_map <- function(agency)   {
         map_partner <- terr_map +
           # geom_tile(data = filter(spdf, SR_LR < 210), aes(x = x, y = y, alpha = SR_LR)) +
           # scale_alpha(name = "", range = c(0.6, 0), guide = F) +
           # theme(legend.position = "none") + 
           geom_sf(data = zmb_geo, fill = grey10k, colour = grey20k, alpha = 0.5) +
           geom_sf(data = msd_geo_partner_totals_sf %>% 
                     filter(fundingagency %in% {{agency}}) %>% 
                     mutate(agency_partner = paste0(fundingagency, ": ", primepartner, "\n")), 
                   aes(fill = agency_partner), alpha = 0.9, stroke = 0) +
           geom_sf(data = zmb_admin0, fill = "NA", colour = grey70k, size = 0.75) +
           facet_wrap(~agency_partner, ncol = 1,
                      labeller = labeller(agency_partner = label_wrap_gen(40))) +
           coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])  +
           si_style_void() +
           theme(panel.grid = element_blank(),
                 strip.text = element_text(size = 10)) +
           labs(x = NULL, y = NULL) +
           theme(legend.position = "none",
                 strip.text = element_text(color = "white"))
     
     return(map_partner)
     }
   
   agency_bar <- function(agency) {
     bar_partner <-  msd_geo_partner_totals %>% 
       filter(fundingagency != "NA") %>% 
       mutate(agency_partner = paste0(fundingagency, ": ", primepartner, "\n"),
              psnu_order = reorder_within(str_replace_all(psnu, " District", ""), 
                                          targets, agency_partner)) %>% 
       filter(fundingagency %in% {{agency}}) %>% 
       ggplot(aes(x = targets, 
                  y = psnu_order, 
                  group = primepartner, 
                  fill = agency_partner)) +
       geom_col() +
       facet_wrap(~ agency_partner, scales = "free_y", ncol = 1,
                  labeller = labeller(agency_partner = label_wrap_gen(40))) +
       scale_y_reordered() +
       si_style_xgrid() +
       scale_x_continuous(labels = comma) +
       theme(legend.position = "none", 
             strip.text = element_text(size = 8),
             axis.text.x = element_text(size = 8)) +
       labs(x = NULL, y = NULL) 
       return(bar_partner)
   }
   
     cdc_map <- 
       agency_map(c("CDC")) + 
       scale_fill_manual(values = c(denim, old_rose, burnt_sienna))
     
     usaid_map <- 
       agency_map(c("USAID")) + 
       scale_fill_manual(values = c(scooter, genoa, trolley_grey))

    cdc_bar <- 
      agency_bar(c("CDC"))  + 
      scale_fill_manual(values = c(denim, old_rose, burnt_sienna))
    
    usaid_bar <- 
      agency_bar(c("USAID")) + 
      scale_fill_manual(values = c(scooter, genoa, trolley_grey))
  
 # Put it all together to have CDC on lhs and USAID and rhs
   cdc_bar + cdc_map + usaid_map + usaid_bar +
    plot_layout(nrow = 1, 
                widths = c(1, 1, 1, 1)) +
    plot_annotation(
      title = "KP_PREV FY21 COVERAGE BY IMPLEMENTING PARTNERS",
      subtitle = "Coverage determined by FY21 Targets being greater than 0",
      caption = caption,
      theme = theme(text = element_text("Source Sans Pro")))
  
  si_save(here(images, "KP_PREV_FY21_TARGETS_BY_PARTNER_AND_AGENCY_combo_.png"),
          scale = 1.5)
    
    
    
    
    