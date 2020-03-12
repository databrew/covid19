library(covid19)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

day0 = 150
time_before = -5
pd <- df %>%
  filter(country %in% c('China',
                        'Italy',
                        'Spain',
                        'Germany',
                        'France',
                        'US', 
                        'Switzerland')) %>%
  filter(!district %in% 'Hubei') %>%
  mutate(district = ifelse(country == 'China', district, country)) %>%
  group_by(date, district, country) %>%
  summarise(confirmed_cases = sum(confirmed_cases)) %>%
  mutate(day = date) %>%
  mutate(country = ifelse(country == 'China', 'Provinces of China', country)) %>%
  ungroup %>%
  group_by(district) %>%
  mutate(first_case = min(date[confirmed_cases >= day0])) %>%
  ungroup %>%
  mutate(days_since_first_case = date - first_case) %>%
  filter(days_since_first_case >= time_before)


cols <- colorRampPalette(brewer.pal(n = 8, 'Set1'))(length(unique(pd$country)))
cols[4] <- adjustcolor('grey', alpha.f = 0.6)



ggplot() +
  theme_simple() +
  geom_line(data = pd,
            aes(x = days_since_first_case,
                y = confirmed_cases,
                color = country,
                group = district),
            lwd = 1) +
  scale_y_log10() +
  scale_color_manual(name = '',
                     values = cols) +
  labs(x = 'Days since first case in country/province',
       y = 'Cumulative cases (log scale)',
       title = 'COVID-19: Western countries vs. (non-Hubei) Chinese provinces',
       subtitle = '"DAY 0": First day with > 150 cases. Grey lines: Chinese provinces',
       caption = 'Data from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data\nChart by Joe Brew, @joethebrew, www.databrew.cc') +
  xlim(-5, 20) +
  geom_hline(yintercept = day0, lty = 2, alpha = 0.7) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7) +
  geom_point(data = tibble(days_since_first_case = 0,
                           confirmed_cases = day0),
             aes(x = days_since_first_case,
                 y = confirmed_cases),
             color = 'red', 
             pch = 1,
             size = 20) 
