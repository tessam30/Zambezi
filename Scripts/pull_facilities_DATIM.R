#Pull Zambia coordinates using new api parameters
# Author: TE | AC | BK
# Date: 2020-07-01



# LIBRARIES ---------------------------------------------------------------

library(Wavelength)
library(glamr)
library(tidyverse)



# GLOBALS -----------------------------------------------------------------

username <- ""
password <- glamr::mypwd(username)
baseurl = "https://final.datim.org/"
zmbuid <- get_outable(username, mypwd(username)) %>% 
  filter(operatingunit == "Zambia") %>% 
  pull(operatingunit_uid)



#lZsCb6y0KDX - malawi
#compile url
url <- paste0(baseurl,
              "api/organisationUnits?filter=path:like:", zmbuid, #Zambia UID
              "&fields=id,name,path,level,geometry&paging=false")

#pull data from DATIM
df <- url %>%
  httr::GET(httr::authenticate(username,password)) %>%
  httr::content("text") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("organisationUnits") %>%
  tibble::as_tibble() %>%
  mutate(
    geom_type = geometry$type,          # NA or Geometry Type Value
    coordinates = geometry$coordinates  # NA or list of 2 or more
  ) %>%
  dplyr::select(-geometry)

sites <- df %>%
  filter(geom_type == "Point" | is.na(geom_type)) %>%
  select(-geom_type) %>%
  unnest_wider(coordinates, names_sep = "_") %>%
  rename(lon = "coordinates_...1", lat = "coordinates_...2")

# Combine with 00_setup_maps and get a production quality map out of box
zmb_base + 
  geom_point(data = sites, aes(lon, y = lat), 
                      shape = 21,  fill = USAID_red, colour = "white",
                      size = 3, alpha = 0.25) + 
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL, title = "DATIM Health facilities for Zambia",
       source = paste0("Source: DATIM API pull as of ", Sys.Date()))
