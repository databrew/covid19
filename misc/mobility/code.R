library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(covid19)
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
