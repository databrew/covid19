library(covid19)
library(dplyr)
library(ggplot2)
a = plot_day_zero(countries = c('Italy', 'Spain'),
                           day0 = 150,
                           cumulative = TRUE,
                           time_before = 0,
                           max_date = as.Date('2020-03-09'))
a
ggsave('~/Desktop/isglobal/a.png')

b = plot_day_zero(countries = c('Italy', 'Spain'),
              day0 = 150,
              cumulative = TRUE,
              time_before = -10,
              max_date = as.Date('2020-03-09'),
              add_markers = T)
b
ggsave('~/Desktop/isglobal/b.png')



cc = plot_day_zero(countries = c('Italy', 'Spain'),
              day0 = 150,
              cumulative = TRUE,
              time_before = -10,
              add_markers = T)
cc
ggsave('~/Desktop/isglobal/c.png')


d = plot_day_zero(countries = c('Italy', 'Spain', 'France', 'Germany',
                            'US', 'UK'),
              day0 = 150,
              cumulative = TRUE,
              time_before = -10,
              add_markers = T)
d
ggsave('~/Desktop/isglobal/d.png')


e = plot_day_zero(countries = c('Italy', 'Spain', 'France', 'Germany',
                            'US', 'UK', 'Switzerland'),
              day0 = 150,
              cumulative = TRUE,
              time_before = -10,
              add_markers = T)
e
ggsave('~/Desktop/isglobal/e.png')


day0 = 150
time_before = -5
pd <- df %>%
  filter(country %in% c('China',
                        'Italy',
                        'Spain',
                        'Germany',
                        'France',
                        'US', 
                        'UK',
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


library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(n = 8, 'Set1'))(length(unique(pd$country)))
cols[4] <- adjustcolor('grey', alpha.f = 0.6)



f = ggplot() +
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
f
ggsave('~/Desktop/isglobal/f.png')


g = plot_day_zero(countries = c('Italy', 'Spain', 'France', 'Germany',
                            'US', 'UK', 'Switzerland', 'Qatar',
                            'Japan', 'Korea, South', 'Singapore'),
              day0 = 150,
              cumulative = TRUE,
              time_before = -10,
              add_markers = T)
g
ggsave('~/Desktop/isglobal/g.png')


pdf('~/Desktop/isglobal/all.pdf',
    height = 33,
    width = 24)
Rmisc::multiplot(a, b, cc, d, e, f, g,
                       cols = 2)
dev.off()
