## PROJECT: COVID MOBILITY REPORTS
## AUTHOR:  A Chafetz | USAID
## PURPOSE: compare mobility drops in TZA
## LICENSE: MIT
## DATE:    2020-08-07

library(tidyverse)
library(extrafont)
library(glitr)
library(glamr)
library(scales)

#data source: https://www.google.com/covid19/mobility/

mobility <- read_csv("../../../Downloads/Global_Mobility_Report.csv")

mobility <- mobility %>% 
  gather(ind, value, retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline) %>% 
  mutate(value = value /100)


mobility_zmb <- mobility %>% 
  filter(country_region == "Zambia") %>% 
  mutate(ind = str_remove(ind, "_percent_change_from_baseline") %>% 
           str_replace("_", " ") %>% 
           str_to_sentence()) %>% 
  group_by_at(vars(-value)) %>% 
  mutate(n = n(), 
         absflag = max(abs(value)),
         row_flag = if_else(abs(value) == absflag, 1, 0)) %>% 
  filter(row_flag == 1) %>% 
  group_by_at(vars(-value, -n, -absflag, -row_flag)) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()


mobility_zmb %>% 
  arrange(date) %>% 
  filter(country_region == "Zambia",
         is.na(sub_region_1)) %>% 
  ggplot(aes(date, value)) +
  geom_vline(xintercept = as.Date(c("2020-04-01", "2020-07-01")), color = "gray20") +
  geom_area(color = si_blue,
            fill = si_lblue, 
            alpha = 0.75) +
  facet_wrap(~str_c(ind, "\n")) +
  scale_y_continuous(label = percent) +
  labs(x = NULL, y = NULL,
       title = "ZAMBIA MOBILITY CHANGES",
       subtitle = "change from baseline",
       caption = "Source: Google Mobility Report") +
  si_style() 
