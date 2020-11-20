# PURPOSE: AD Hoc look at TX_CURR targets for FY20 & FY21
# Author: Tim Essam
# Date: 2020-11-18


genie <- 
  vroom::vroom("/Users/tim/Downloads/Genie_OU_IM_Zambia_Daily_cbd05a94-14b5-43a3-80f5-dc5934152597.txt")

  df_long <- genie %>% 
    filter(disaggregate == "Total Numerator") %>% 
    reshape_msd() %>% 
    mutate(fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency)) %>% 
    mutate(fy = substr(period, 3, 6))
  
  tx_targets <- df_long %>% 
    filter(indicator == "TX_CURR", period %in% c("fy2020_targets" , "fy2021_targets")) %>% 
    group_by(fundingagency, period) %>% 
    summarise(targets = sum(val, na.rm = TRUE)) %>% 
    spread(period, targets)


# If PSNU is kept in w/ a reorder_within needed in a mutate above
  tx_targets %>% 
    ggplot(aes(x = psnu, y = targets)) + 
    geom_col() + 
    facet_wrap(~fundingagency, scales = "free") + 
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(labels = comma) +
    si_style_xgrid()
