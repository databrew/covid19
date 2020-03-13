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
  geom_area(color = 'black', fill = 'orange') +
  facet_wrap(~ccaa, scales = 'free_x') +
  theme_simple() +
  labs(x = '', y = 'Casos') +
  geom_text(data = esp %>% filter(date_time == max(date_time)),
            aes(label = value,
                x = date_time - hours(50),
                y = value + 150),
            size = 5,
            alpha = 0.6) +
  labs(title = 'Casos confirmados por CCAA',
       subtitle = 'Datos: 17:00 12 de marzo 2020') +
  # ylim(0, 1600) +
  theme(strip.text = element_text(size = 15)) +
  scale_x_datetime(breaks = date_vec,
                   labels = date_lab,
                   limits = c(min(date_vec), max(date_vec)))
