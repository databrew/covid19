library(dplyr)
library(readr)
library(tidyr)

# Consider replacing pipeline with: https://cowid.netlify.com/data/new_deaths.csv

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
df$confirmed_cases[df$country == 'Spain' & df$date == '2020-03-12'] <- 3050
df$deaths[df$country == 'Spain' & df$date == '2020-03-12'] <- 84
# df$recovered[df$country == 'Spain' & df$date == '2020-03-12']

# Add a Spain row for March 13 (updating manually)
if(length(df$confirmed_cases[df$country == 'Spain' & df$date == '2020-03-13']) == 0){
  new_row <- tibble(district = NA,
                    date = as.Date('2020-03-13'),
                    country = 'Spain',
                    lat = df$lat[df$country == 'Spain'][1],
                    lng = df$lng[df$country == 'Spain'][1],
                    confirmed_cases = 4231,
                    deaths = 121,
                    recovered = 189)
  df <- df %>% bind_rows(new_row)
}

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

if(!dir.exists('isglobal')){
  dir.create('isglobal')
}
write_csv(df, 'isglobal/world_region_data.csv')
write_csv(df_country, 'isglobal/world_data.csv')


# Spain
spain_file <- 'spain/spain.txt'
con <- file(spain_file,open="r")
spain_lines <- readLines(con)
close(con)

# Process line function for Spain
out_list <- list()
date_time <- Sys.time()
counter <- 0
for(i in 1:length(spain_lines)){
  message(i)
  this_line <- spain_lines[i]
  if(this_line != ''){
    # Split at the spaces
    line_split <- strsplit(this_line, split = ' ', fixed = TRUE)
    line_split <- unlist(line_split)
    # See if date

    is_date <- substr(line_split[1], 1, 4) == '2020'
    if(is_date){
      this_date_time <- paste0(line_split[1], ' ',
                               line_split[2], ' CET')
      date_time <- this_date_time
    } else {
      counter <- counter + 1
      cases <- as.numeric(line_split[length(line_split)])
      ccaa <- paste0(line_split[1:(length(line_split)-1)], collapse = ' ')
      out <- tibble(date_time = date_time,
                    ccaa = ccaa,
                    value = cases)
      out_list[[counter]] <- out
    }
  }

}
esp <- bind_rows(out_list)
esp$date_time <- as.POSIXct(esp$date_time,tz='Europe/Madrid')

write_csv(esp, 'spain/ccaa.csv')
usethis::use_data(esp, overwrite = T)

# Write a map
