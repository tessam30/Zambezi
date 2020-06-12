# Purpose: Setup basemap and geographic matches to plot Zambia data
# Author: Tim Essam | SI Asc
# Date: 2020_06_10


# PRELIMS -----------------------------------------------------------------

library(rnaturalearthdata)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)



# GLOBALS -----------------------------------------------------------------


data <- "Data"
data_out <- "Dataout"
images <- "Images"



# BOUNDARIES NEEDED -----------------------------------------------------

  world <- ne_countries(scale = "large", returnclass = "sf")
  
  ne_geo <- raster(file.path(data, "SR_LR", "SR_LR.tif")) 
  new_proj <- crs(ne_geo)
  
  zmb_admin0 <- ne_countries(scale = "large", returnclass = "sf") %>% 
    filter(sovereignt == "Zambia")
    
    
  ne_cities <- st_read(file.path(gispath, "ne_10m_populated_places_simple", "ne_10m_populated_places_simple.shp"))
  
  # Note that the order for the bbox is xmin, xmax, ymin, ymax not same as st_bbox()
  bbox <- st_bbox(zmb_admin0)
  new_bbox <- c(bbox[1] + 1, bbox[3] + 1, bbox[2] - 1 , bbox[4] - 1) 
  
  zmb_admin1 <- ne_states(country = "Zambia", returnclass = "sf") %>% 
    st_transform(new_proj)
  
  ne_ocean_chop <- st_crop(ne_ocean, xmin = -83, ymin = -5.5, xmax = -65, ymax = 15)
  ne_geo_chop <- crop(ne_geo, ne_ocean_chop)
  
  
  zmb_admin0_chop <- st_crop(zmb_admin0, xmin = bbox[1], ymin = new_bbox[3], xmax = new_bbox[2], ymax = new_bbox[4])
  ne_geo_chop
  spdf <- as(ne_geo_chop, "SpatialPixelsDataFrame") %>% as.data.frame(.)
  
  
  terrain <- ggplot() +
    geom_tile(data = filter(spdf, SR_LR < 210), aes(x = x, y = y, alpha = SR_LR)) +
    scale_alpha(name = "", range = c(0.6, 0), guide = F) +
    theme(legend.position = "none")
