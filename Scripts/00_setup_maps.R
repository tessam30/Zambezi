# Purpose: Setup basemap and geographic matches to plot Zambia data
# Author: Tim Essam | SI Asc
# Date: 2020_06_10


# PRELIMS -----------------------------------------------------------------

library(rnaturalearthdata)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(extrafont)
library(ggspatial)
library(ggtext)
library(ggsflabel)


# GLOBALS -----------------------------------------------------------------


  data <- "Data"
  data_out <- "Dataout"
  images <- "Images"
  gis <- "GIS"



# BOUNDARIES NEEDED -----------------------------------------------------

  
  # Arrange the bounaries and rasters needed
  
  zmb_neighbors <- ne_countries(scale = "large", returnclass = "sf") %>% 
    filter(sovereignt %in% c("Malawi", "Mozambique", "Botswana", "Namibia",
      "Democratic Republic of the Congo", "Angola", "United Republic of Tanzania", "Zimbabwe"))
  
  ne_geo <- raster(file.path(data, "SR_LR", "SR_LR.tif")) 
  new_proj <- crs(ne_geo)
  
  zmb_admin0 <- ne_countries(scale = "large", returnclass = "sf") %>% 
    filter(sovereignt == "Zambia") 
  
  # Set the map range for centering the map in the ggplot call
  mapRange <- c(range(st_coordinates(zmb_admin0)[, 1]), range(st_coordinates(zmb_admin0)[, 2]))
    
  ne_cities <- st_read(file.path(gis, "ne_10m_populated_places_simple", "ne_10m_populated_places_simple.shp")) %>% 
    filter(sov0name == "Zambia", pop_max > 1e5)
  
  # Note that the order for the bbox is xmin, xmax, ymin, ymax not same as st_bbox()
  bbox <- st_bbox(zmb_admin0)
  new_bbox <- c(bbox[1] + 1, bbox[3] + 1, bbox[2] - 1 , bbox[4] - 1) 
  
  zmb_admin1 <- ne_states(country = "Zambia", returnclass = "sf") 
  
  
  zmb_admin0_chop <- st_crop(zmb_admin0, xmin = 21, ymin = -19, xmax = 35, ymax = -8)
  ne_geo_chop <- crop(ne_geo, zmb_admin0_chop)
  spdf <- as(ne_geo_chop, "SpatialPixelsDataFrame") %>% as.data.frame(.)
  
  

 zmb_base <- 
   ggplot() +
    geom_tile(data = filter(spdf, SR_LR < 210), aes(x = x, y = y, alpha = SR_LR)) +
    scale_alpha(name = "", range = c(0.6, 0), guide = F) +
    theme(legend.position = "none") + 
  geom_sf(data = zmb_neighbors, fill = "#d9d9d9", alpha = 0.35, size = 0.25, colour = "#969696") +
  #geom_sf_text(data = zmb_neighbors, aes(label = sovereignt), family = "Source Sans Pro" ) +
    geom_sf(data = zmb_admin0, colour = "white", fill = "grey93", size = 4, alpha = 0.25) +
    geom_sf(data = zmb_admin0, colour = "black", fill = "NA") +
  geom_sf(data = zmb_admin1, fill = "NA") +
   geom_sf_label_repel(data = ne_cities, aes(label = name), alpha = 0.90, family = "Source Sans Pro Light") +
  si_style() +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
  theme_void()
   
    
