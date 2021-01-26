##  PROJECT: Q3 Review
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of VL
##  LICENCE: MIT
##  DATE:    2020-09-08




# Libraries
library(tidyverse)
library(sf)
library(extrafont)
library(glitr)
library(gisr)
library(here)
library(scales)
library(patchwork)
library(ICPIutilities)
library(rnaturalearthhires)
library(rgdal)


# Globals
dir_data <- here("Data")
dir_dataout <- here("Dataout")

# dir_geo <- "../../Boundaries"
dir_terr <- "../../GEODATA/RASTER"
# dir_merdata <- "../../DATIM"

# datim_path <- "../../DATIM_Data"

dir_img <- here("Images")
dir_graphs <- here("Graphics")


# MER Data
file_psnu_im <- (list.files(
  path = dir_merdata,
  pattern = ("PSNU_IM_FY18-20_20200814_v1_1_Zambia"),
  recursive = TRUE,
  full.names = TRUE
))

df <- read_msd(file_psnu_im)

# GEO Data
gis_5_sfc <- list.files(dir_geo, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

gis_4_sfc <- list.files(dir_geo, pattern = ".*_4_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

zam1 <- get_adm_boundaries("ZMB", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)



# MER Data Munge ---------------------------------------------------------------------------------

# u15
df_VL_u15 <- df %>%
  filter(
    fiscal_year == "2020",
    fundingagency == "USAID",
    indicator %in% c("TX_PVLS", "TX_CURR"),
    standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
    operatingunit %in% c("Zambia")
  ) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
  group_by(fiscal_year, operatingunit, psnuuid, psnu, trendscoarse, indicator) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  spread(indicator, val)



df_VL_u15 <- df_VL_u15 %>%
  group_by(operatingunit, psnuuid, psnu, trendscoarse) %>%
  mutate(
    VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
    ou_lab = paste0(psnu, " (", lag(TX_CURR, 2, order_by = period) %>% comma(), ")")
  ) %>%
  ungroup() %>%
  mutate(VLS = (TX_PVLS / TX_PVLS_D) * VLC) %>%
  mutate(Not_Cov = case_when(
    VLC > 1 ~ 0,
    TRUE ~ 1 - VLC
  )) %>%
  filter(
    period == "FY20Q3",
    trendscoarse == "<15"
  ) %>%
  mutate(shortname = str_remove(psnu, "District")) %>%
  mutate(lab_psnu = case_when(Not_Cov > .7 ~ shortname))

# all ages
df_VL <- df %>%
  filter(
    fiscal_year == "2020",
    fundingagency == "USAID",
    indicator %in% c("TX_PVLS", "TX_CURR"),
    standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
    operatingunit %in% c("Zambia")
  ) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
  group_by(fiscal_year, operatingunit, psnuuid, psnu, indicator) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  spread(indicator, val)



df_VL <- df_VL %>%
  group_by(operatingunit, psnuuid, psnu) %>%
  mutate(
    VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
    ou_lab = paste0(psnu, " (", lag(TX_CURR, 2, order_by = period) %>% comma(), ")")
  ) %>%
  ungroup() %>%
  mutate(VLS = (TX_PVLS / TX_PVLS_D) * VLC) %>%
  mutate(Not_Cov = case_when(
    VLC > 1 ~ 0,
    TRUE ~ 1 - VLC
  )) %>%
  filter(period == "FY20Q3") %>%
  mutate(shortname = str_remove(psnu, "District")) %>%
  mutate(lab_psnu = case_when(Not_Cov > .7 ~ shortname))


# GEO Data Joins
zam_geo_u15 <- st_as_sf(gis_5_sfc$Zambia) %>%
  left_join(df_VL_u15, by = c("uid" = "psnuuid"))

zam_geo_all <- st_as_sf(gis_5_sfc$Zambia) %>%
  left_join(df_VL, by = c("uid" = "psnuuid"))


# VIZ All AGES------------------------------------------------------------------------------------
zam_map_all <-
  terrain_map(countries = "Zambia", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zam_geo_all %>% filter(!is.na(Not_Cov)), aes(fill = VLC), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(
    option = "magma", alpha = 0.9, direction = -1,
    breaks = c(0, .25, .50, .75, 1.00),
    limits = c(0, 1.00),
    labels = percent
  ) +
  si_style_map() +
  theme(
    legend.position = "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")
  ) +
  ggtitle(
    label = "",
    subtitle = "Viral Load Coverage - All Ages"
  )



vls_map_all <- terrain_map(countries = "Zambia", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zam_geo_all %>% filter(!is.na(VLS)), aes(fill = VLS), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(
    option = "viridis", alpha = 0.9, direction = -1,
    breaks = c(0, .25, .50, .75, 1.00),
    limits = c(0, 1.00),
    labels = percent
  ) +
  si_style_map() +
  theme(
    legend.position = "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")
  ) +
  ggtitle(
    label = "",
    subtitle = "Viral Load Suppression - All Ages"
  )


# VIZ - UNDER 15 -------------------------------------------------------------------------------------------
zam_map_u15 <- terrain_map(countries = "Zambia", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zam_geo_u15 %>% filter(!is.na(Not_Cov)), aes(fill = VLC), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(
    option = "magma", alpha = 0.9, direction = -1,
    breaks = c(0, .25, .50, .75, 1.00),
    limits = c(0, 1.00),
    labels = percent
  ) +
  si_style_map() +
  theme(
    legend.position = "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")
  ) +
  ggtitle(
    label = "",
    subtitle = "Viral Load Coverage - <15"
  )



zam_vls_mapu15 <- terrain_map(countries = "Zambia", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zam_geo_u15 %>% filter(!is.na(VLS)), aes(fill = VLS), lwd = .2, color = grey10k) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(
    option = "viridis", alpha = 0.9, direction = -1,
    breaks = c(0, .25, .50, .75, 1.00),
    limits = c(0, 1.00),
    labels = percent
  ) +
  si_style_map() +
  theme(
    legend.position = "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")
  ) +
  ggtitle(
    label = "",
    subtitle = "Viral Load Suppression - <15"
  )


# VIZ COMBINED & SAVED ---------------------------------------------------------------------------------

(zam_map_all | vls_map_all) / (zam_map_u15 | zam_vls_mapu15) +
  plot_layout(ncol = 1) +
  plot_annotation(
    title = "Zambia | FY20Q3i",
    caption = "Source: FY20Q3i MSD - USAID Only,
VLC = TX_PVLS / TX_CURR (2 periods prior)
VLS = (TX_PVLS_N/TX_PVLS_D)*VLC"
  )




ggsave(here("Graphics", "FY20Q3_ViralLoad_Zambia_4map.png"),
  scale = 1.2, dpi = 310, width = 10, height = 7, units = "in"
)


ggsave(here("Graphics", "FY20Q3_ViralLoad_Zambia_4map.pdf"),
  scale = 1.2, dpi = 310, width = 10, height = 7, units = "in",
  useDingbats = FALSE
)
