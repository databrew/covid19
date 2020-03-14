library(covid19)
library(ggplot2)
library(lubridate)
library(dplyr)
Sys.timezone()

date_vec <- (seq(as.Date('2020-03-02'),
                           as.Date('2020-03-12'),
                           by = 2))
date_lab <- format(date_vec, '%b\n%d')


esp = covid19::esp %>% # %>% filter(!ccaa %in% 'Melilla') %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(ccaa, date) %>%
  filter(date_time == max(date_time)) %>%
  ungroup %>%
  dplyr::select(-date_time)
ggplot(data = esp,
       aes(x = date,
           y = value,
           group = ccaa)) +
  geom_line(color = 'black') +
  facet_wrap(~ccaa, ncol = 3) +
  theme_simple() + 
  scale_y_log10() +
  labs(x = '', y = 'Casos') +
  geom_text(data = esp %>% filter(date == max(date)),
            aes(label = value,
                x = date - 2,
                y = value + 150),
            size = 5,
            alpha = 0.6) +
  labs(title = 'Casos confirmados por CCAA',
       subtitle = 'Datos: 18:00 13 de marzo 2020') +
  # ylim(0, 1600) +
  theme(strip.text = element_text(size = 15)) +
  scale_x_date(breaks = date_vec,
                   labels = date_lab)


day0 = 50
esp <- covid19::esp
esp <- esp %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(ccaa, date) %>%
  filter(date_time == max(date_time)) %>%
  dplyr::select(-date_time) %>%
  ungroup %>%
  group_by(ccaa) %>%
  mutate(start_date = as.Date(min(date[value >= day0]))) %>%
  ungroup %>%
  mutate(days_since_first_case = date - start_date) %>%
  filter(ccaa %in% c('Cataluña', 'Madrid'))

cols <- c('black', 'red')
# cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, 'Set1'))(length(unique(esp$ccaa)))

ggplot(data = esp,
       aes(x = days_since_first_case,
           y = value,
           color = ccaa)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  xlim(-5, 10) +
  scale_color_manual(name = '', values = cols) +
  theme_simple() +
  theme(legend.position = 'top') +
  geom_hline(yintercept = day0, lty = 2, alpha = 0.7) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7) +
  # geom_point(data = tibble(days_since_first_case = 0,
  #                          confirmed_cases = day0),
  #            aes(x = days_since_first_case,
  #                y = confirmed_cases),
  #            color = 'darkgrey', 
  #            pch = 1,
  #            size = 20) +
  labs(x = 'Dies des del primer dia amb > 50 casos',
       y = 'Casos acumulatius (escala logarítmica)',
       title = 'Trajectòria del brot, ajustat per temps',
       subtitle = 'Dia 0: primer dia amb 50 o més casos acumulatius') +
  theme(legend.text = element_text(size = 20))
ggsave('~/Desktop/cat.png')


# ALL CCAA
day0 = 50
esp <- covid19::esp
esp <- esp %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(ccaa, date) %>%
  filter(date_time == max(date_time)) %>%
  dplyr::select(-date_time) %>%
  ungroup %>%
  group_by(ccaa) %>%
  mutate(start_date = as.Date(min(date[value >= day0]))) %>%
  ungroup %>%
  filter(!is.na(start_date),
         is.finite(start_date)) %>%
  mutate(days_since_first_case = date - start_date) 

# cols <- c('black', 'red')
cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, 'Set1'))(length(unique(esp$ccaa)))

ggplot(data = esp,
       aes(x = days_since_first_case,
           y = value,
           color = ccaa)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  xlim(0, 10) +
  scale_color_manual(name = '', values = cols) +
  theme_simple() +
  theme(legend.position = 'top') +
  geom_hline(yintercept = day0, lty = 2, alpha = 0.7) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7) +
  # geom_point(data = tibble(days_since_first_case = 0,
  #                          confirmed_cases = day0),
  #            aes(x = days_since_first_case,
  #                y = confirmed_cases),
  #            color = 'darkgrey', 
  #            pch = 1,
  #            size = 20) +
  labs(x = 'Dies des del primer dia amb > 50 casos',
       y = 'Casos acumulatius (escala logarítmica)',
       title = 'Trajectòria del brot, ajustat per temps',
       subtitle = 'Dia 0: primer dia amb 50 o més casos acumulatius') +
  theme(legend.text = element_text(size = 20))
ggsave('~/Desktop/cat.png')
