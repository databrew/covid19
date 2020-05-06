library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(covid19)

# GOOGLE
mobility <- read_csv('Global_Mobility_Report.csv')
esp <- mobility %>%
  filter(country_region == 'Spain') %>%
  filter(!is.na(sub_region_1),
         is.na(sub_region_2)) %>%
  dplyr::select(-sub_region_2) %>%
  gather(key, value, contains('baseline')) %>%
  # filter(sub_region_1 %in% c(
  #   "Community of Madrid",
  #   "Catalonia"
  # )) %>%
  filter(date >= '2020-03-01') %>%
  mutate(key = gsub('_from_baseline',
                    '', key)) %>%
  mutate(key = gsub('_',
                    ' ', key)) %>%
  mutate(key = Hmisc::capitalize(key))

ggplot(data = esp,
       aes(x = date,
           y = value)) +
  geom_line(aes(color = key)) +
  facet_wrap(~sub_region_1) +
  theme_simple() +
  theme(legend.position = 'top') +
  scale_color_manual(name = '',
                     values = RColorBrewer::brewer.pal(n = length(unique(esp$key)), 'Set1')) +
  geom_hline(yintercept = 0.3, lty = 2, alpha = 0.7) +
  labs(title = 'Mobility in Spain',
       subtitle = 'According to Google Community Mobility Reports',
       caption = 'Data from https://www.google.com/covid19/mobility/; chart: @joethebrew',
       x = '',
       y = '% above/below baseline') +
  theme(axis.text.x = element_text(size = 6))

# Workplaces only
wp <- esp %>%
  filter(key %in% c('Transit stations percent change',
                    'Workplaces percent change')) %>%
  mutate(week = format(date, '%W')) %>%
  filter(!weekdays(date) %in% c('Saturday', 'Sunday')) %>%
  group_by(key, week, sub_region_1) %>%
  summarise(value = mean(value, na.rm = T),
            start_date = min(date)) %>%
  ungroup %>%
  filter(!sub_region_1 %in% c('Melilla', 'Ceuta')) %>%
  filter(start_date >= '2020-03-15')

confinament <- 
  tibble(xmin =  as.Date(c('2020-03-29')),
         xmax =  as.Date(c('2020-04-12')),
         ymax = 0,
         ymin = min(wp$value))

ggplot(data = wp,
       aes(x = start_date,
           y = value,
           color = key)) +
  # geom_rect(data = confinament,
  #           aes(xmin = xmin,
  #               ymin = ymin,
  #               xmax = xmax,
  #               ymax = ymax,
  #               # x = NA,
  #               # y = NA,
  #               group = NA,
  #               color = NA),
  #           color = 'grey') +
  # geom_bar(stat = 'identity') 
  geom_point() +
  geom_line() +
  # geom_area() +
  facet_wrap(~sub_region_1, scales = 'free_x') +
  # facet_grid(key ~ sub_region_1) +
  theme_simple() +
  theme(legend.position = 'top') +
  # geom_hline(yintercept = 0.3, lty = 2, alpha = 0.7) +
  labs(title = 'Average weekday (Mon-Fri) mobility for transit stations / workplaces',
       subtitle = 'According to Google Community Mobility Reports',
       caption = 'Data from https://www.google.com/covid19/mobility/; chart: @joethebrew',
       x = 'First day of week',
       y = '% above/below baseline') +
  theme(axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 14)) +
  geom_text(aes(y = value + 3,
                label = round(value)),
            alpha = 0.6,
            show.legend = F) +
  scale_color_manual(name = '',
                     values = c('darkorange', 'brown')) +
  scale_x_date(breaks = sort(unique(wp$start_date)),
               labels = format(sort(unique(wp$start_date)), '%b\n%d')) +
  geom_vline(xintercept = as.Date(c('2020-03-28',
                                    '2020-04-08')),
             alpha = 0.7,
             lty = 2)




# APPLE
mobility <- read_csv('applemobilitytrends-2020-04-12.csv')
df <- mobility %>%
  gather(key, value, `2020-01-13`:`2020-04-12`) %>%
  mutate(date = as.Date(key))

mob <- df %>%
  filter(region %in% c('Italy', 'Spain'))# %>% 
           # c('Barcelona', 'Madrid', 'Milan', 'Rome', 'New York City')) %>%
  # filter(transportation_type == 'transit')
# cata <- esp %>% filter(region == 'Barcelona')

# Get the disease data
right <- covid19::df_country %>% filter(country %in% c('Italy', 'Spain'))
mob$country <- mob$region
joined <- mob %>% left_join(right)
ggplot(data = mob,
       aes(x = date,
           y = value,
           color = region)) +
  geom_line() +
  facet_wrap(~transportation_type)

x = joined %>%
  filter(cases > 0)
pd <- x %>% 
  filter(transportation_type == 'transit') %>%
  dplyr::select(cases = cases_non_cum, value, country, date) %>%
  group_by(country) %>%
  mutate(reduce_75 = min(date[value < 75]),
         reduce_cases_75 = dplyr::first(cases[value <75]),
         reduce_50 = min(date[value < 50]),
         reduce_cases_50 = dplyr::first(cases[value <50])) %>%
  gather(key, value, cases:value) %>%
  mutate(key = ifelse(key == 'cases',
                      'Incident cases',
                      'Apple maps transit direction requests (% of normal)'))

options(scipen = '999')
ggplot(data = pd,
       aes(x = date,
           y = value,
           color = key)) +
  geom_point(size = 3, alpha = 0.7) +
  # geom_smooth(se = FALSE) +
  # geom_point(data = pd, aes(x = reduce_75, y = reduce_cases_75),
  #            color = 'black') +
  facet_wrap(~country) +
  # geom_hline(yintercept = c(75), lty = 2, alpha = 0.6) 
  theme_simple() +
  scale_y_log10(limits = c(5, max(pd$value))) +
  scale_color_manual(name = '', values = c('black', 'red')) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 26, hjust = 0.5)) +
  labs(x = 'Date',
       y = 'Value',
       title = 'COVID-19 cases and mobility',
       subtitle = '(As measured in relative volume of "transit" directions requests via Apple Maps)',
       caption = 'Mobility data from Apple: https://www.apple.com/covid19/mobility. COVID-19 data from Johns Hopkins University. Chart: own.')
ggsave('~/Desktop/plot.png')
