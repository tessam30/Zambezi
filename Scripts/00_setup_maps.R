# Purpose: Setup basemap and geographic matches to plot Zambia data
# Author: Tim Essam | SI Asc
# Date: 2020_06_10
# NOTES: For inner glow?! https://www.katiejolly.io/blog/2020-03-06/inner-glow

# PRELIMS -----------------------------------------------------------------

  library(rnaturalearthdata)
  library(tidyverse)
  library(raster)
  library(rgeos)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthhires)
  library(extrafont)
  library(ggspatial)
  library(ggtext)
  library(ggsflabel)
  library(glitr)
  library(glamr)
  library(googledrive)
  library(here)


# GLOBALS -----------------------------------------------------------------

  dem_url <- "https://drive.google.com/drive/u/0/folders/1M02ToX9AnkozOHtooxU7s4tCnOZBTvm_"

  data <- "Data"
  data_out <- "Dataout"
  images <- "Images"
  gis <- "../../GEODATA/RASTER"

  # 4326 CRS is the WGS default (Mercator)
  crs <- 4326
  buffer_size <- 50000 #meter based buffer (5 Km)
  

# FUNCTIONS ---------------------------------------------------------------

  # Function to return neighbors of a given contry
  geo_neighbors <- function(countries = list("Zambia")) {
    
    # Arrange the bounaries and rasters needed
    world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") 
    
    #stopifnot({{country}} %in% world$sovereignt)
    # if(!{{countries}} %in% world$sovereignt) {
    #   stop('Country name is not the Natural Earth reference dataset. Please enter a name from dataset.')
    # }
    
    focus_country <-  rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
      dplyr::filter(sovereignt %in% {{countries}})
    
    # Filter based on neighbors touched by polygons of interest
    get_neighbors <- world %>% 
      sf::st_transform(., crs = st_crs(crs)) %>% 
      filter(lengths(st_touches(., focus_country)) > 0)
    
    return(get_neighbors)
  }
  
  
 # Return admin0 bondary for a given country
  get_admin0 <- function(countries = list("Zambia"))  {
    
    admin0 <- 
      rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
      dplyr::filter(sovereignt %in% {{countries}}) %>% 
      sf::st_transform(., crs = st_crs(crs))
    
    return(admin0)    
  }
  
  
# Return admin1 boundary for a given country
  get_admin1 <- function(countries = list("Zambia")) {
    admin1 <- 
      rnaturalearth::ne_states(country = {{countries}}, returnclass = "sf") %>% 
      sf::st_transform(., crs = st_crs(crs))
    
    return(admin1)  
  }
  
# Return terrain for the country's bounding box, add some 
  get_terrain <- function(country = list("Zambia, Malawi")) {
    
    # Grab terrain for a selected area of interest (AOI)
    
    #TODO: CHECK IF SR_LR file exists, if not download it with google drive
    #drive_download(dem_url)
    
    ne_geo <- 
      raster(file.path(gis, "SR_LR.tif")) 
    
    #tmp <- elevatr::get_elev_raster(zmb_admin0, z = 5) 
    
    chop <- get_admin0({{country}}) %>% st_crop(., st_bbox(.))
    spdf <- crop(ne_geo, chop)  %>% 
      as(., "SpatialPixelsDataFrame") %>% as.data.frame(.)  
    
    return(spdf)
  }
  
  
  terrain_map <- function(country = "Zambia") {
    
    admin0 <- get_admin0({{country}})
    admin1 <- get_admin1({{country}})
    ngbhrs <- geo_neighbors({{country}})
    
    # Set the map range for centering the map in the ggplot call
    mapRange <- c(range(st_coordinates(admin0)[, 1]), range(st_coordinates(admin0)[, 2]))
    
    spdf <- get_terrain({{country}})
    
    p <- ggplot() +
      geom_tile(data = filter(spdf, SR_LR < 210), aes(x = x, y = y, alpha = SR_LR)) +
      scale_alpha(name = "", range = c(0.6, 0), guide = F) +
      theme(legend.position = "none") + 
      geom_sf(data = ngbhrs, fill = "#d9d9d9", alpha = 0.35, size = 0.25, colour = grey70k) +
      #geom_sf_text(data = zmb_neighbors, aes(label = sovereignt), family = "Source Sans Pro" ) +
      geom_sf(data = admin0, colour = "white", fill = "grey93", size = 2, alpha = 0.25) +
      geom_sf(data = admin0, colour = "black", fill = "NA") +
      geom_sf(data = admin1, fill = "NA", linetype = "dotted") +
      #geom_sf_label_repel(data = ne_cities, aes(label = name), alpha = 0.90, family = "Source Sans Pro Light") +
      si_style() +
      coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
      theme_void()
    
    return(p)
  }
  
# PLOTS -------------------------------------------------------------------

 zmb_base <- terrain_map(list("Zambia"))
  
 # terrain_map(list("Mali", "Burkina Faso", "Nigeria", "Togo", "Benin", "Ghana", "Niger"))
 # terrain_map(list("Mali", "Ethiopia")) 
 # 
 # terrain_map("United Republic of Tanzania")
 # terrain_map("Democratic Republic of the Congo")
 # terrain_map("Malawi")
 # terrain_map("Zambia")
 # terrain_map("Angola")


   
    
