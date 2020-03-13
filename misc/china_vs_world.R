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
ggsave('../data-raw/isglobal/img/china_comparison.png')
ggsave('../data-raw/isglobal/img/china_comparison.jpg')
ggsave('../data-raw/isglobal/img/china_comparison.pdf')



# Italy comparison
day0 = 150
time_before = -5
pd <- df_country %>%
  filter(country %in% c('Italy',
                        'Spain')) %>%
  group_by(date, country) %>%
  summarise(confirmed_cases = sum(confirmed_cases)) %>%
  mutate(day = date) %>%
  ungroup %>%
  group_by(country) %>%
  mutate(first_case = min(date[confirmed_cases >= day0])) %>%
  ungroup %>%
  mutate(days_since_first_case = date - first_case) %>%
  filter(days_since_first_case >= time_before)


cols <- c('black', 'red')

ggplot() +
  theme_simple() +
  geom_line(data = pd,
            aes(x = days_since_first_case,
                y = confirmed_cases,
                color = country,
                group = country),
            lwd = 1) +
  scale_y_log10() +
  scale_color_manual(name = '',
                     values = cols) +
  labs(x = 'Days since first case in country/province',
       y = 'Cumulative cases (log scale)',
       title = 'COVID-19: Spain and Italy',
       subtitle = 'Data as of 12 March 2020',
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
ggsave('../data-raw/isglobal/img/italy_comparison.png')
ggsave('../data-raw/isglobal/img/italy_comparison.jpg')
ggsave('../data-raw/isglobal/img/italy_comparison.pdf')


library(covid19)
library(ggplot2)
library(lubridate)

date_vec <- as.POSIXct(seq(as.Date('2020-03-02'),
                           as.Date('2020-03-12'),
                           by = 2))
date_lab <- format(date_vec, '%b\n%d')

ggplot(data = esp,
       aes(x = date_time,
           y = value)) +
  geom_area(color = 'black', fill = 'red', alpha = 0.7) +
  facet_wrap(~ccaa, scales = 'free_x') +
  theme_simple() +
  labs(x = '', y = 'Casos') +
  geom_text(data = esp %>% filter(date_time == max(date_time)),
            aes(label = value,
                x = date_time - hours(50),
                y = value + 50),
            size = 3,
            alpha = 0.6,
            color = 'red') +
  labs(title = 'Casos confirmados por CCAA',
       subtitle = 'Datos: 17:00 12 de marzo 2020') +
  ylim(0, 1600) +
  theme(strip.text = element_text(size = 15)) +
  scale_x_datetime(breaks = date_vec,
                   labels = date_lab,
                   limits = c(min(date_vec), max(date_vec)))

ggsave('../data-raw/isglobal/img/ccaa.png')
ggsave('../data-raw/isglobal/img/ccaa.jpg')
ggsave('../data-raw/isglobal/img/ccaa.pdf')


day0 = 50
time_before = -5
pd = esp %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(date, ccaa) %>%
  filter(date_time == max(date_time)) %>%
  ungroup %>%
  group_by(date, ccaa) %>%
  summarise(confirmed_cases = sum(value)) %>%
  mutate(day = date) %>%
  ungroup %>%
  group_by(ccaa) %>%
  mutate(first_case = min(date[confirmed_cases >= day0])) %>%
  ungroup %>%
  mutate(days_since_first_case = date - first_case) %>%
  filter(days_since_first_case >= time_before)
# 
ggplot(data = pd,
       aes(x = days_since_first_case,
           y = confirmed_cases,
           group = ccaa,
           color = ccaa)) +
  geom_line()

# World comparison
# day0 = 150
# time_before = -5
# pd <- df_country %>%
#   filter(country %in% c('Italy',
#                         'Spain',
#                         'Iran',
#                         'Denmark',
#                         'Germany',
#                         'France',
#                         'US', 
#                         'Switzerland',
#                         'Japan', 
#                         'Korea, South')) %>%
#   group_by(date, country) %>%
#   summarise(confirmed_cases = sum(confirmed_cases)) %>%
#   mutate(day = date) %>%
#   ungroup %>%
#   group_by(country) %>%
#   mutate(first_case = min(date[confirmed_cases >= day0])) %>%
#   ungroup %>%
#   mutate(days_since_first_case = date - first_case) %>%
#   filter(days_since_first_case >= time_before)
# 
# 
# cols <- colorRampPalette(brewer.pal(n = 8, 'Set1'))(length(unique(pd$country)))
# 
# ggplot() +
#   theme_simple() +
#   geom_line(data = pd,
#             aes(x = days_since_first_case,
#                 y = confirmed_cases,
#                 color = country,
#                 group = country),
#             lwd = 1) +
#   scale_y_log10() +
#   scale_color_manual(name = '',
#                      values = cols) +
#   labs(x = 'Days since first case in country/province',
#        y = 'Cumulative cases (log scale)',
#        title = 'COVID-19: cumulative cases by country',
#        subtitle = 'Data as of 12 March 2020',
#        caption = 'Data from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data\nChart by Joe Brew, @joethebrew, www.databrew.cc') +
#   xlim(-5, 20) +
#   geom_hline(yintercept = day0, lty = 2, alpha = 0.7) +
#   geom_vline(xintercept = 0, lty = 2, alpha = 0.7) +
#   geom_point(data = tibble(days_since_first_case = 0,
#                            confirmed_cases = day0),
#              aes(x = days_since_first_case,
#                  y = confirmed_cases),
#              color = 'red', 
#              pch = 1,
#              size = 20) 
# ggsave('../data-raw/isglobal/img/world_comparison.png')
# ggsave('../data-raw/isglobal/img/world_comparison.jpg')
# ggsave('../data-raw/isglobal/img/world_comparison.pdf')


