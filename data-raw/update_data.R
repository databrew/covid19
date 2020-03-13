library(dplyr)
library(readr)
library(tidyr)

# Datasets at https://github.com/CSSEGISandData/COVID-19
if(!dir.exists('jhu')){
  dir.create('jhu')
}

# Confirmed cases
download.file(url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv',
              destfile = 'jhu/confirmed_cases.csv')

# Deaths
download.file(url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv',
              destfile = 'jhu/deaths.csv')

# Recovered
download.file(url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv',
              destfile = 'jhu/recovered.csv')

# Read in 
confirmed_cases <- read_csv('jhu/confirmed_cases.csv')
deaths <- read_csv('jhu/deaths.csv')
recovered <- read_csv('jhu/recovered.csv')

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

# # Keep only States for the US
# # (otherwise, double-counts certain things)
# library(maps)
# states <- map('state')$names
# states <- unlist(lapply(states, function(x){strsplit(x, ':', fixed = TRUE)[1]}))
# df <- df %>%
#   filter(country != 'US' |
#            tolower(district) %in% states)
# Beginning on March 10, the data format changes for the US - reporting states and
# sub-state entities
# we want to keep all US entries through March 9 and then beginning on the 10th,
# only keep those with commas (the states)
df <- df %>%
  filter(country != 'US' |
           date < '2020-03-10' |
           !grepl(', ', district))

# Manual update for Spain (JHU data behind)
df$confirmed_cases[df$country == 'Spain' & df$date == '2020-03-12'] <- 3000
df$deaths[df$country == 'Spain' & df$date == '2020-03-12'] <- 84
# df$recovered[df$country == 'Spain' & df$date == '2020-03-12']

# Decumulate too
df <- df %>%
  ungroup %>%
  arrange(country, district, date) %>%
  group_by(country, district, lat, lng) %>%
  mutate(confirmed_cases_non_cum = confirmed_cases - lag(confirmed_cases, default = 0),
         deaths_non_cum = deaths - lag(deaths, default = 0),
         recovered_non_cum = recovered - lag(recovered, default = 0)) %>%
  ungroup


# Join all together but by country
df_country <- df %>%
  group_by(country, date) %>%
  summarise(lat = mean(lat),
            lng = mean(lng),
            confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE)) %>%
  ungroup %>%
  # # # Weird March 11 correction
  # mutate(confirmed_cases = ifelse(country == 'US' & date == '2020-03-10', 1050, confirmed_cases)) %>%
  # mutate(deaths = ifelse(country == 'US' & date == '2020-03-10', 29, deaths)) %>%
  # mutate(recovered = ifelse(country == 'US' & date == '2020-03-10', 7, recovered)) %>%
  group_by(country, lat, lng) %>%
  mutate(confirmed_cases_non_cum = confirmed_cases - lag(confirmed_cases, default = 0),
         deaths_non_cum = deaths - lag(deaths, default = 0),
         recovered_non_cum = recovered - lag(recovered, default = 0)) %>%
  ungroup


usethis::use_data(df, overwrite = T)
usethis::use_data(deaths, overwrite = T)
usethis::use_data(confirmed_cases, overwrite = T)
usethis::use_data(recovered, overwrite = T)
usethis::use_data(df_country, overwrite = T)


# # Italy vs Spain
# pd <- df %>%
#   # mutate(country = ifelse(country != 'Mainland China', 'Other', 'China')) %>%
#   arrange(date, country) %>%
#   filter(country %in% c(
#     'Italy', 'Spain')) %>% #, 'France', 'US',
#                         # 'South Korea',
#                         # 'Iran',
#                         # 'Germany',
#                         # 'Japan',
#                         # 'Norway',
#                         # 'Switzerland',
#                         # # 'Mainland China',
#                         # 'UK')) %>%
#   group_by(country, date) %>%
#   summarise(confirmed_cases = sum(confirmed_cases)) %>%
#   ungroup %>%
#   group_by(country) %>%
#   mutate(first_case = min(date[confirmed_cases > 0])) %>%
#   ungroup %>%
#   mutate(days_since_first_case = date - first_case) %>%
#   filter(days_since_first_case >= 0)
# 
# cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8,
#                                                   name = 'Set1'))(length(unique(pd$country)))
# cols <- c('red', 'darkorange')
# library(databrew)
# library(extrafont)
# loadfonts()
# ggplot(data = pd,
#        aes(x = as.numeric(days_since_first_case),
#            y = confirmed_cases)) +
#   geom_line(aes(color = country), size = 2, alpha = 1) +
#   theme_bw() +
#   scale_color_manual(name = '',
#                      values = cols) +
#   # scale_y_log10() +
#   labs(x = 'Days since first case (0 = day of first confirmation)',
#        y = 'Cumulative number of confirmed cases\n(Logarithmic scale)',
#        title = 'COVID-19: growth in cases since zone\'s first confirmed case',
#        subtitle = 'Data as of March 6',
#        caption = 'Raw data from Johns Hopkins University: https://github.com/CSSEGISandData/COVID-19\nData processing and visualization: Joe Brew') +
#   theme_bw()+
#   theme(
#     text = element_text(family = "Ubuntu light", color = "grey20", size = 16),
#     strip.background = element_blank(),
#     strip.text = element_text(hjust = 0),
#     panel.grid.major = element_blank(), #element_line(colour="grey70",size=0.15),
#     panel.grid.minor = element_line(colour="grey70",size=0.15),
#     panel.background = element_blank(),
#     panel.border = element_blank(),
#     axis.line = element_line(color = 'grey20', size = 0.25),
#     plot.margin=unit(rep(0.5, 4),"cm"),
#     legend.position="right",
#     legend.text = element_text(size = 18),
#     plot.caption=element_text(hjust=1,size=9,colour="grey30"),
#     plot.subtitle=element_text(face="italic",size=12,colour="grey40"),
#     plot.title=element_text(size=22,face="bold")) 
# 
# #   # Doubling times
# # start <- 1
# # vec <- c(start, rep(NA, 10))
# # text_list <- list()
# # for (i in 0:10){
# #   iup <- i + 1
# #   this_day <- 4 * i
# #   this_val <- vec[iup]
# #   vec[iup+1] <- 4 * this_val
# #   text_list[iup] <- paste0('Day ', this_day, ': ',format(this_val, big.mark = ','))
# # }
# # text_list <- unlist(text_list)
# # cat(paste0(text_list, collapse = '\n'))
