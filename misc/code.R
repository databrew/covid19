library(tidyverse)
library(covid19)
library(ggplot2)
library(databrew)


# Datasets at https://github.com/CSSEGISandData/COVID-19
pd <- df_country %>%
  # mutate(country = ifelse(country != 'Mainland China', 'Other', 'China')) %>%
  arrange(date, country) %>%
  filter(country %in% c(
    'Italy', 
    'Germany',
    'US',
    'France',
    # 'UK',
    'South Korea',
    'Iran',
    'Germany',
    # 'Japan',
    'Norway',
    'Switzerland',
    'South Korea',
    'Spain')) %>% #, 'France', 'US',

  group_by(country, date) %>%
  summarise(confirmed_cases = sum(confirmed_cases)) %>%
  ungroup %>%
  group_by(country) %>%
  mutate(first_case = min(date[confirmed_cases > 150])) %>%
  ungroup %>%
  mutate(days_since_first_case = date - first_case)# %>%
  # filter(days_since_first_case >= 0)

cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, 'Set1'))(length(unique(pd$country)))

selfy <- function(x){abs(x)}
ggplot(data = pd,
       aes(x = as.numeric(days_since_first_case),
           y = confirmed_cases)) +
  geom_line(aes(group = country,
                color = country),
            size = 2,
            alpha = 0.85) + 
  # geom_point(aes(group = country,
  #                color = country),
  #            size = 2) +
  geom_hline(yintercept = 150, lty = 2, alpha = 0.7) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7) +
  geom_point(data = tibble(days_since_first_case = 0,
                           confirmed_cases = 150),
             aes(x = days_since_first_case,
                 y = confirmed_cases),
             color = 'red', 
             pch = 1,
             size = 20) +
  scale_y_log10() +
  theme_simple() +
  scale_color_manual(name = '', values = cols) +
  labs(x = 'Days until/after reaching "critical mass" (150 cases)',
       y = 'Confirmed cases\n(cumulative, logarithmic scale)',
       title = 'Cumulative cases by country',
       subtitle = 'Day 0 = first day at which the country had > 150 cases',
       caption = 'Data from https://github.com/CSSEGISandData/COVID-19\nChart by www.databrew.cc') +
  scale_x_continuous(sec.axis = sec_axis(~ . + 0,
                                         breaks = c(-20, 10),
                                         labels = c('Before "critical mass"',
                                                    'After "critical mass"')))


ggplot(data = pd %>% filter(country == 'US'),
       aes(x = date,
           y = confirmed_cases)) +
  geom_point() +
  geom_line() +
  theme_simple() +
  xlim(as.Date('2020-01-15'),
       Sys.Date()) +
  labs(x = 'Date',
       y = 'Confirmed cases',
       title = 'COVID-19 cases and Donald Trump\'s statements on the disease')


# Confirmed cases
download.file(url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv',
              destfile = 'data-raw/confirmed_cases.csv')

# Deaths
download.file(url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv',
              destfile = 'data-raw/deaths.csv')

# Recovered
download.file(url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv',
              destfile = 'data-raw/recovered.csv')

# Read in 
confirmed_cases <- read_csv('data-raw/confirmed_cases.csv')
deaths <- read_csv('data-raw/deaths.csv')
recovered <- read_csv('data-raw/recovered.csv')

# Define function for cleaning up
clean_up <- function(ts, value_name = 'deaths'){

    # Make wide
  ts <- ts %>%
    gather(key = date,
           value = value_name,
           names(ts)[!is.na(as.Date(names(ts), format = '%m/%d/%y'))])
  
  # Clean up columns and names
  names(ts) <- c('district', 'country', 'lat', 'lng', 'date', value_name)
  ts$date <- as.Date(ts$date, format = '%m/%d/%y')
  return(ts)
  
}

# Clean up the datasets
deaths <- clean_up(deaths, value_name = 'deaths')
confirmed_cases <- clean_up(confirmed_cases, value_name = 'confirmed_cases')
recovered <- clean_up(recovered, value_name = 'recovered')

# Join all together
df <- full_join(x = confirmed_cases,
                y = deaths) %>%
  full_join(recovered)

# Italy vs Spain
pd <- df %>%
  # mutate(country = ifelse(country != 'Mainland China', 'Other', 'China')) %>%
  arrange(date, country) %>%
  filter(country %in% c(
    'Italy', 
    'Germany',
    # 'US',
    'France',
    # 'UK',
    # 'South Korea',
    # 'Iran',
    'Germany',
    # 'Japan',
    'Norway',
    'Switzerland',
    # 'South Korea',
    'Spain')) %>% #, 'France', 'US',
                        # 'South Korea',
                        # 'Iran',
                        # 'Germany',
                        # 'Japan',
                        # 'Norway',
                        # 'Switzerland',
                        # # 'Mainland China',
                        # 'UK')) %>%
  group_by(country, date) %>%
  summarise(confirmed_cases = sum(confirmed_cases)) %>%
  ungroup %>%
  group_by(country) %>%
  mutate(first_case = min(date[confirmed_cases > 150])) %>%
  ungroup %>%
  mutate(days_since_first_case = date - first_case) %>%
  filter(days_since_first_case >= 0)

cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8,
                                                  name = 'Set1'))(length(unique(pd$country)))
# cols <- c('darkorange', 'blue')
# cols <- c( 'lightblue', 'red', 'darkorange', 'blue')
cols[which(sort(unique(pd$country)) %in% c('Italy', 'Spain'))] <- c('darkorange', 'blue')
cols[which(sort(unique(pd$country)) %in% c('Norway'))] <- c('grey')
# cols[which(sort(unique(pd$country)) %in% c('US'))] <- c('blue')


# cols[which(sort(unique(pd$country)) %in% c('Japan'))] <- c('lightblue')

library(databrew)
library(extrafont)
library(ggplot2)
# loadfonts()
ggplot(data = pd,
       aes(x = as.numeric(days_since_first_case),
           y = confirmed_cases)) +
  geom_line(aes(color = country),  alpha = 1, size = 1) +
  geom_point(aes(color = country), size = 3, alpha = 0.6) +
  theme_bw() +
  scale_color_manual(name = '',
                     values = cols) +
  scale_y_log10(breaks = c(150, 300, 500, 1000, 3000, 10000)) +
  # scale_y_log10() +
  labs(x = 'Days since country\'s first day with > 150 cases',
       y = 'Cumulative number of confirmed cases\n(Logarithmic scale)',
       title = 'COVID-19 cases since country\'s first day at > 150 cases',
       subtitle = 'Data as of March 9',
       caption = 'Raw data from Johns Hopkins University: https://github.com/CSSEGISandData/COVID-19\nData processing and visualization: Joe Brew') +
  theme_bw() +
  geom_hline(yintercept = 1000, lty = 2, alpha = 0.3) + 
  geom_vline(xintercept = 6, lty = 2, alpha = 0.3) +
  theme(
    text = element_text(family = "Ubuntu light", color = "grey20", size = 14),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    panel.grid.major = element_blank(), #element_line(colour="grey70",size=0.15),
    panel.grid.minor = element_line(colour="grey70",size=0.15),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'grey20', size = 0.25),
    plot.margin=unit(rep(0.5, 4),"cm"),
    legend.position="right",
    legend.text = element_text(size = 18),
    plot.caption=element_text(hjust=1,size=9,colour="grey30"),
    plot.subtitle=element_text(face="italic",size=12,colour="grey40"),
    plot.title=element_text(size=22,face="bold")) 
ggsave('~/Desktop/y.png', width = 11, height = 6)
#   # Doubling times
# start <- 1
# vec <- c(start, rep(NA, 10))
# text_list <- list()
# for (i in 0:10){
#   iup <- i + 1
#   this_day <- 4 * i
#   this_val <- vec[iup]
#   vec[iup+1] <- 4 * this_val
#   text_list[iup] <- paste0('Day ', this_day, ': ',format(this_val, big.mark = ','))
# }
# text_list <- unlist(text_list)
# cat(paste0(text_list, collapse = '\n'))

spain = pd %>% filter(country == 'Spain')
x = rep(NA, nrow(spain))
x[1] <- 165
for(i in 2:nrow(spain)){
  x[i] <- x[i-1] * 1.41425
}

cbind(x, spain$confirmed_cases)

