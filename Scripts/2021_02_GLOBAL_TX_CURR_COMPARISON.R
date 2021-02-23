# Purpose: Plot TX_CURR across all outs by FY20 / FY21
# Author: Tim Essam | SI, 
# Date: 
# Notes:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(readxl)
    library(here)
    library(gt)
    
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
    
  # Functions  
  

# LOAD DATA ============================================================================  
  
    dir(data)
    excel_sheets(here(data, "Treatment_Global.xlsx"))
    
   df <-  excel_sheets(here(data, "Treatment_Global.xlsx")) %>% 
     set_names() %>% 
     purrr::map_dfr(.f = ~read_excel(here(data, "Treatment_Global.xlsx"), sheet = .x) %>% 
                      mutate(year = .x %>% as.numeric())
     )
  
   
   dft <- 
     df %>% 
     mutate(metric = paste0(`Metrics...3`, " ", year)) %>% 
     select(-`Metrics...3`, -year) %>% 
     pivot_wider(
       names_from = metric,
       values_from = `Metrics...4`
     ) %>% 
     arrange(desc(`Target 2021`)) %>%
     
     select(
       Indicator,
       `Operating Unit`,
       `FY20\n Results` = `Result. 2020`,
       `FY20\n Targets` = `Target 2020`,
       `FY20\n Achievement` = `% Achievement. 2020`,
       `FY21 Results` = `Result. 2021`,
       `FY21 Targets` = `Target 2021`,
       `FY21 Achievement` = `% Achievement. 2021`
     ) %>% 
     mutate(rank = dense_rank(-`FY21 Achievement`))
   

   tx_tbl <- 
     dft %>% 
       gt(groupname_col = "Indicator") %>% 
       fmt_number(c(3:4, 6:7),
                  decimals = 0) %>% 
       fmt_percent(c(5, 8),
                   decimals = 0) %>% 
       tab_style(
         style = cell_fill(color = grey10k, alpha = 0.5),
         locations = cells_body(
           rows = `FY21 Targets` > 3e5 & `Operating Unit` != "Zambia"
         )
       ) %>% 
       tab_style(
         style = cell_fill(color = genoa, alpha = 0.55),
         locations = cells_body(
           rows = `Operating Unit` == "Zambia"
           )
         ) %>% 
       fmt_missing(columns = everything(), missing_text = "-") %>% 
       cols_hide(columns = vars(rank)) %>%
       tab_header("ZAMBIA HAS BEST TX_CURR ACHIEVEMENT FOR OUS WITH MORE THAN 300K TARGETS") %>% 
       tab_source_note(source_note = "Source: PANORAMA Treatement Global Dossier 2021-02-23") %>% 
     tab_options(
       footnotes.font.size = 8
     )
     
     gtsave(tx_tbl, here(images, "TX_CURR GLOBAL COMPARISON.png"))
     
     dft %>% 
       filter(!is.na(`FY21 Targets`)) %>% 
       mutate(ou = fct_reorder(`Operating Unit`, `FY21 Targets`),
              ou_color = if_else(`FY21 Targets` > 3e5, grey40k, grey10k),
              error_color = if_else(`FY21 Results` > `FY21 Targets`, "white", grey10k),
              group = if_else(`FY21 Targets` > 3e5, "Over 300K targets", "Under 300K targets")
              ) %>% 
       ggplot(aes(y = ou)) +
       geom_col(aes(x = `FY21 Targets`), fill = grey10k) +
       geom_col(aes(x = `FY21 Results`), fill = trolley_grey, alpha = 0.5) +
       geom_col(data = . %>% filter(`Operating Unit` == "Zambia"), aes(x = `FY21 Results`), fill = genoa) +
       geom_errorbar(aes(xmin = `FY21 Targets`, xmax = `FY21 Targets`, color = error_color), size = 1.15) +
       geom_text(aes(x = `FY21 Results`, label = paste0(" ", percent(`FY21 Achievement`, 1))), 
                 hjust = 0,
                 size = 3,
                 color = grey80k) +
       scale_color_identity() +
       facet_wrap(~group, scales = "free") +
       scale_x_continuous(labels = comma) +
       coord_cartesian(expand = FALSE, clip = 'off') +
       si_style_xgrid() +
       labs(x = NULL, y = NULL, title = "ZAMBIA HAS BEST FY21 TX_CURR ACHIEVEMENT FOR OUS WITH MORE THAN 300K TARGETS")
     
     si_save(file.path(images, "TX_CURR_Achievement_FY21_GLOBAL"), scale = 1.25)
     
         


# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

