## PROJECT: COVID MOBILITY REPORTS
## AUTHOR:  T Essam | USAID
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


mobility_zmb %>% group_by(country_region) %>% filter(date == min(date)) %>% pull(date)


mobility_zmb %>% 
  arrange(date) %>% 
  filter(country_region == "Zambia",
         is.na(sub_region_1)) %>% 
  group_by(ind) %>% 
  mutate(seven_day = zoo::rollmean(value, 7, fill = NA, align = c("right")),
         fourteen_day = zoo::rollmean(value, 14, fill = NA, align = c("right")),
         seven_color = if_else(seven_day >= 0, "#0eccff", "#d73636")) %>% 
  ungroup() %>% 
  ggplot(aes(date)) +
  annotate("rect", xmin = as.Date("2020-02-16"), xmax = as.Date("2020-04-01"), ymin = -Inf, ymax = Inf, fill = grey10k, alpha = 0.55) +
  annotate("rect", xmin = as.Date("2020-07-01"), xmax = as.Date("2020-08-27"), ymin = -Inf, ymax = Inf, alpha = 0.55, fill= grey10k) +
  #geom_vline(xintercept = as.Date(c("2020-04-01", "2020-07-01")), color = grey20k) +
  geom_area(aes(y =  seven_day), color = "white",
            alpha = 0.75, 
           fill = grey20k) +
  geom_line(aes(y = seven_day, color = seven_color, group = ind)) +
facet_wrap(~str_c(ind %>% str_replace("_", " "), "\n")) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_y_continuous(label = percent) +
  scale_x_date(date_labels = "%b", date_breaks = "1 months")
  labs(x = NULL, y = NULL,
       title = "ZAMBIA MOBILITY CHANGES",
       subtitle = "Change from baseline",
       caption = "Source: Google Mobility Report") +
  si_style_ygrid() 

ggsave(file.path("Images", "ZMB_mobility_trend_2.pdf"),
       plot = last_plot(), useDingbats = F,
       width = 10, height = 5.625, dpi = "retina")
