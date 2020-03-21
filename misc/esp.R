library(covid19)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggplot2)
Sys.timezone()


# Doubling time
n_cases_start = 150
countries = c('Italy', 'Spain', 'France', 'Germany', 'Italy', 'Switzerland', 'Denmark', 'US', 'United Kingdom', 'Norway')
# countries <- sort(unique(df_country$country))
out_list <- curve_list <-  list()
counter <- 0
for(i in 1:length(countries)){
  message(i)
  this_country <- countries[i]
  sub_data <-df_country %>% filter(country == this_country)
  # Only calculate on countries with n_cases_start or greater cases,
  # starting at the first day at n_cases_start or greater
  ok <- max(sub_data$confirmed_cases, na.rm = TRUE) >= n_cases_start
  if(ok){
    counter <- counter + 1
    pd <- sub_data %>%
      filter(!is.na(confirmed_cases)) %>%
      mutate(start_date = min(date[confirmed_cases >= n_cases_start])) %>%
      mutate(days_since = date - start_date) %>%
      filter(days_since >= 0) %>%
      mutate(days_since = as.numeric(days_since))
    fit <- lm(log(confirmed_cases) ~ days_since, data = pd) 
    # plot(pd$days_since, log(pd$cases))
    # abline(fit)
    ## Slope
    # curve <- fit$coef[2]
    
    # Predict days ahead
    fake <- tibble(days_since = seq(0, max(pd$days_since) + 5, by = 1))
    fake <- left_join(fake, pd %>% dplyr::select(days_since, confirmed_cases, date))
    fake$predicted <- exp(predict(fit, newdata = fake))
    
    # Doubling time
    dt <- log(2)/fit$coef[2]
    out <- tibble(country = this_country,
                  doubling_time = dt)
    out_list[[counter]] <- out
    curve_list[[counter]] <- fake %>% mutate(country = this_country)
  }
}
done <- bind_rows(out_list)
curves <- bind_rows(curve_list)
# Get curves back in exponential form
# curves$curve <- exp(curves$curve)

# Join doubling time to curves
joined <- left_join(curves, done)

# Get rid of Italy future (since it's the "leader")
joined <- joined %>%
  filter(country != 'Italy' |
           date <= (Sys.Date() -1))


# Make long format
long <- joined %>% 
  dplyr::select(date, days_since, country, confirmed_cases, predicted, doubling_time) %>%
  tidyr::gather(key, value, confirmed_cases:predicted) %>%
  mutate(key = Hmisc::capitalize(gsub('_', ' ', key))) %>%
  mutate(key = ifelse(key == 'Predicted', 'Predicted (based on current doubling time)', key))



cols <- c('red', 'black')
ggplot(data = long,
       aes(x = days_since,
           y = value,
           lty = key,
           color = key)) +
  geom_line(data = long %>% filter(key != 'Confirmed cases'),
            size = 1.5, alpha = 0.8) +
  geom_point(data = long %>% filter(key == 'Confirmed cases')) +
  geom_line(data = long %>% filter(key == 'Confirmed cases'),
            size = 0.8) +
  facet_wrap(~paste0(country, '\n',
                     '(doubling time: ', 
                     round(doubling_time, digits = 1), ' days)'), scales = 'free') +
  theme_simple() +
  scale_linetype_manual(name ='',
                        values = c(1,2)) +
  scale_color_manual(name = '',
                     values = cols) +
  theme(legend.position = 'top') +
  labs(x = 'Days since first day at >150 cumulative cases',
       y = 'Cases',
       title = 'COVID-19 case trajectories (assuming no changes in doubling time)',
       caption = 'Data from Johns Hopkins. Processing: Joe Brew @joethebrew. Code: github.com/databrew/covid19',
       subtitle = '(Doubling time calculated since first day at >150 cumulative cases)')


# Overlay Italy
ol1 <- joined %>% filter(!country %in% 'Italy')
ol2 <- joined %>% filter(country == 'Italy') %>% dplyr::rename(Italy = confirmed_cases) %>%
  dplyr::select(Italy, days_since)
ol <- left_join(ol1, ol2) %>%
  dplyr::select(days_since, date, country, confirmed_cases, predicted, Italy,doubling_time)
ol <- tidyr::gather(ol, key, value, confirmed_cases: Italy) %>%
  mutate(key = Hmisc::capitalize(gsub('_', ' ', key))) %>%
  mutate(key = ifelse(key == 'Predicted', 'Predicted (based on current doubling time)', key))



cols <- c('red', 'blue', 'black')
ggplot(data = ol,
       aes(x = days_since,
           y = value,
           lty = key,
           color = key)) +
  geom_line(data = ol %>% filter(key != 'Confirmed cases'),
            size = 1.2, alpha = 0.8) +
  geom_point(data = ol %>% filter(key == 'Confirmed cases')) +
  geom_line(data = ol %>% filter(key == 'Confirmed cases'),
            size = 0.8) +
  facet_wrap(~paste0(country, '\n',
                     '(doubling time: ', 
                     round(doubling_time, digits = 1), ' days)'), scales = 'free') +
  theme_simple() +
  scale_linetype_manual(name ='',
                        values = c(1,2, 3)) +
  scale_color_manual(name = '',
                     values = cols) +
  theme(legend.position = 'top') +
  labs(x = 'Days since first day at >150 cumulative cases',
       y = 'Cases',
       title = 'COVID-19 case trajectories (assuming no changes in doubling time)',
       caption = 'Data from Johns Hopkins. Processing: Joe Brew @joethebrew. Code: github.com/databrew/covid19',
       subtitle = '(Doubling time calculated since first day at >150 cumulative cases)') +
  theme(strip.text = element_text(size = 15))



pd <- df %>%
  mutate(cases = confirmed_cases) %>%
  arrange(date) %>%
  group_by(country) %>%
  mutate(start_date = min(date[cases >= n_cases_start])) %>%
  ungroup %>%
  mutate(days_since = date - start_date) %>%
  filter(days_since > 0) %>%
  filter(country %in% this_country)
fit <- lm(log(cases) ~ days_since, data = pd) 
plot(pd$days_since, log(pd$cases))
abline(fit)
# Slope
fit$coef[2] 
# Doubling time
log(2)/fit$coef[2] 

# Latest in Spain
pd <- esp_df %>%
  filter(date == max(date)) %>%
  mutate(p = deaths / sum(deaths) * 100)
text_size <- 12

# deaths
ggplot(data = pd,
       aes(x = ccaa,
           y = deaths)) +
  geom_bar(stat = 'identity',
           fill = 'black') +
  theme_simple() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = '',
       y = 'Deaths | Muertes',
       title = 'COVID-19 deaths in Spain',
       subtitle = 'Data as of evening of Sunday, March 15',
       caption = 'github.com/databrew/covid19 | joe@databrew.cc') +
  theme(legend.position = 'top',
        legend.text = element_text(size = text_size * 2),
        axis.title = element_text(size = text_size * 2),
        plot.title = element_text(size = text_size * 2.3),
        axis.text.x = element_text(size = text_size * 1.5)) +
  geom_text(data = pd %>% filter(deaths > 0),
            aes(x = ccaa,
                y = deaths,
                label = paste0(deaths, '\n(',
                               round(p, digits = 1), '%)')),
            size = text_size * 0.3,
            nudge_y = 15) 
ggsave('~/Desktop/spain.png')



# Madrid vs Lombardy deaths
n_death_start <- 5
text_size <- 12
pd <- esp_df %>%
  filter(ccaa == 'Madrid') %>%
  dplyr::select(date, ccaa, cases, deaths) %>%
  bind_rows(ita %>%
              filter(ccaa == 'Lombardia') %>%
              dplyr::select(date, ccaa, cases, deaths)) %>%
  arrange(date) %>%
  group_by(ccaa) %>%
  mutate(first_n_death = min(date[deaths >= n_death_start])) %>%
  ungroup %>%
  mutate(days_since_n_deaths = date - first_n_death) 

cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, 'Spectral'))(length(unique(pd$ccaa)))
cols <- c('black', 'red')
ggplot(data = pd,
       aes(x = days_since_n_deaths,
           y = deaths,
           color = ccaa)) +
  geom_line(size = 1.5) +
  theme_simple() +
  scale_y_log10() +
  scale_color_manual(name = '',
                     values = cols) +
  xlim(0, 15) +
  labs(x = paste0('DAYS\n(day 0 = first day with >', n_death_start, ' cumulative deaths)'),
       y = 'CUMULATIVE DEATHS\n(logarithmic scale)',
       title = 'COVID-19 deaths. Lombardy and Madrid',
       subtitle = paste0('Time-adjusted for first day with >', n_death_start, ' cumulative deaths'),
       caption = 'github.com/databrew/covid19 | joe@databrew.cc') +
  theme(legend.position = 'top',
        legend.text = element_text(size = text_size * 2),
        axis.title = element_text(size = text_size * 2),
        plot.title = element_text(size = text_size * 2.3))
ggsave('~/Desktop/italy.png')


plot_day_zero(countries = c('Italy', 'Spain'),ylog = T)


date_vec <- (seq(as.Date('2020-03-02'),
                           Sys.Date(),
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
  # geom_line(color = 'black') +
  geom_area( fill = 'darkorange', color = 'black') +
  facet_wrap(~ccaa, ncol = 3) +
  theme_simple() + 
  # scale_y_log10() +
  labs(x = '', y = 'Casos') +
  geom_text(data = esp %>% filter(date == max(date)),
            aes(label = value,
                # x = date - 2,
                # y = value + 100
                x = as.Date('2020-03-08'),
                y = 2500),
                # ),
            size = 7,
            alpha = 0.6) +
  labs(title = 'Casos confirmados por CCAA',
       subtitle = 'Datos: 17:00 14 de marzo 2020') +
  # ylim(0, 3500) +
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
  # filter(ccaa %in% c('Navarra' )) %>%
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
cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, 'Set2'))(length(unique(esp$ccaa)))
ccaas <- sort(unique(esp$ccaa))
cols[which(ccaas == 'Madrid')] <- 'red'
# cols[2] <- 'black'

ggplot(data = esp,
       aes(x = days_since_first_case,
           y = value,
           color = ccaa)) +
  geom_line(lwd = 2) +
  # geom_point() +
  scale_y_log10(
    limits = c(50, 3000)
    ) +
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
  labs(x = paste0('Días desde el primer día con >', day0, ' casos'),
       y = 'Casos acumulatius (escala logarítmica)',
       title = 'Trayectoria del brote, ajustado por día de "inicio"',
       subtitle = paste0('Día 0: primer día con ', day0,' o más casos acumulativos')) +
  theme(legend.text = element_text(size = 20)) #+
  # geom_hline(yintercept = seq(100, 3000, 100), alpha = 0.1)
ggsave('~/Desktop/esp.png')
