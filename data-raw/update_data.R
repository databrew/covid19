library(dplyr)
library(readr)
library(tidyr)
library(readxl)

# Get world populationd data
world_pop <- read_csv('unpop/API_SP.POP.TOTL_DS2_en_csv_v2_866861.csv',
                      skip = 4)
world_pop <- world_pop %>%
  dplyr::select(country = `Country Name`,
                iso = `Country Code`,
                pop = `2018`)
usethis::use_data(world_pop, overwrite = TRUE)

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

# Get most recent Spanish ministry data
library(gsheet)
url <- 'https://docs.google.com/spreadsheets/d/15UJWpsW6G7sEImE8aiQ5JrLCtCnbprpztfoEwyTNTEY/edit#gid=810081118'
esp_df <- gsheet::gsheet2tbl(url)
right <- esp_df %>%
  group_by(date) %>%
  summarise(confirmed_cases = sum(cases, na.rm = TRUE),
            deaths  = sum(deaths, na.rm = TRUE))

# Overwrite Spanish data with more accurate ministry data
overwrite_spain = TRUE
if(overwrite_spain){
  dfx <- df %>% filter(country == 'Spain' & date >= '2020-03-16') 
  dfy <- df %>% filter(country != 'Spain' | date < '2020-03-16')
  dfz <- dfx %>% dplyr::select(date, district, country, lat, lng, recovered) %>%
    # dont get ahead of ministry
    filter(date <= max(right$date))
  
  dfz <- left_join(dfz, right)
  df <- bind_rows(dfz, dfy)
  df <- df %>% arrange(country, district, date)  
}


# # Add a Spain row for March 17 (updating manually)
# if(length(df$confirmed_cases[df$country == 'Spain' & df$date == '2020-03-17']) == 0){
#   new_row <- tibble(district = NA,
#                     date = as.Date('2020-03-17'),
#                     country = 'Spain',
#                     lat = df$lat[df$country == 'Spain'][1],
#                     lng = df$lng[df$country == 'Spain'][1],
#                     confirmed_cases = 11178,
#                     deaths = 491,
#                     recovered = NA) #?
#   df <- df %>% bind_rows(new_row)
# }

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

# Get the iso code for each country
countries <- sort(unique(df$country))
country_codes <- tibble(country = countries)
library(passport)
country_codes$iso <- parse_country(x = countries, to = 'iso3c')
df <- left_join(df, country_codes)


usethis::use_data(df, overwrite = T)
usethis::use_data(deaths, overwrite = T)
usethis::use_data(confirmed_cases, overwrite = T)
usethis::use_data(recovered, overwrite = T)
usethis::use_data(df_country, overwrite = T)

# Make spreadsheet for ISGlobal
isglobal <- df_country %>%
  filter(date == max(date)) %>%
  dplyr::select(date, 
                country, 
                cases = confirmed_cases,
                deaths) %>%
  left_join(country_codes)


if(!dir.exists('isglobal')){
  dir.create('isglobal')
}
write_csv(isglobal, 'isglobal/isglobal.csv')

write_csv(df, 'isglobal/world_region_data.csv')
write_csv(df_country, 'isglobal/world_data.csv')


### Now managing Spain data through 
# google sheets
# Code left here (commented out) for reproducibility purposes only
# # Spain
# spain_file <- 'spain/spain.txt'
# con <- file(spain_file,open="r")
# spain_lines <- readLines(con)
# close(con)
# 
# # Process line function for Spain
# out_list <- list()
# date_time <- Sys.time()
# counter <- 0
# for(i in 1:length(spain_lines)){
#   message(i)
#   this_line <- spain_lines[i]
#   if(this_line != ''){
#     # Split at the spaces
#     line_split <- strsplit(this_line, split = ' ', fixed = TRUE)
#     line_split <- unlist(line_split)
#     # See if date
# 
#     is_date <- substr(line_split[1], 1, 4) == '2020'
#     if(is_date){
#       this_date_time <- paste0(line_split[1], ' ',
#                                line_split[2], ' CET')
#       date_time <- this_date_time
#     } else {
#       counter <- counter + 1
#       cases <- as.numeric(line_split[length(line_split)])
#       ccaa <- paste0(line_split[1:(length(line_split)-1)], collapse = ' ')
#       out <- tibble(date_time = date_time,
#                     ccaa = ccaa,
#                     value = cases)
#       out_list[[counter]] <- out
#     }
#   }
# 
# }
# esp <- bind_rows(out_list)
# esp$date_time <- as.POSIXct(esp$date_time,tz='Europe/Madrid')
# 
# # Get in a format more similar to the country dataset
# esp <- esp %>%
#   mutate(confirmed_cases = value)
# 
# # Create a day (rather than time-date) value
# esp_df <- esp %>%
#   mutate(date = as.Date(date_time)) %>%
#   arrange(ccaa, date) %>%
#   group_by(ccaa, date) %>%
#   filter(date_time == max(date_time)) %>%
#   ungroup
# 
# # Read in deaths
# esp_deaths <- read_csv('https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv')
# esp_deaths <- gather(esp_deaths,
#                      date, deaths,
#                      `03/03/2020`:`15/03/2020`)
# esp_deaths <- esp_deaths %>%
#   dplyr::rename(ccaa = CCAA) %>%
#   filter(ccaa != 'Total') %>%
#   mutate(ccaa = ifelse(ccaa == 'Castilla y León', 'CyL',
#                        ifelse(ccaa == 'Castilla-La Mancha', 'CLM',
#                               ccaa)))
# esp_deaths$date <- as.Date(esp_deaths$date,
#                            format = '%d/%m/%Y')
# # Join deaths and cases
# esp_df <- left_join(esp_df, esp_deaths)
# esp_df$deaths <- ifelse(is.na(esp_df$deaths), 0,
#                         esp_df$deaths)
# # # write to google docs and manage from there
# # from now on
# write_csv(esp_df, '~/Desktop/esp.csv')

# # Read UCI data
# esp_uci <- read_csv('https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_uci.csv')
# esp_uci <- gather(esp_uci,
#                      date, uci,
#                      `04/03/2020`:`16/03/2020`)
# esp_uci <- esp_uci %>%
#   dplyr::rename(ccaa = CCAA) %>%
#   filter(ccaa != 'Total') %>%
#   mutate(ccaa = ifelse(ccaa == 'Castilla y León', 'CyL',
#                        ifelse(ccaa == 'Castilla-La Mancha', 'CLM',
#                               ccaa)))
# esp_uci$date <- as.Date(esp_uci$date,
#                            format = '%d/%m/%Y')
# usethis::use_data(esp_uci, overwrite = TRUE)

# Read population data
esp_pop <- tibble(ccaa = c("Andalucía",
                           "Aragón",
                           "Asturias",
                           "Baleares",
                           "C. Valenciana",
                           "Canarias",
                           "Cantabria",
                           "Cataluña",
                           "Ceuta",
                           "CLM",
                           "CyL",
                           "Extremadura",
                           "Galicia",
                           "La Rioja",
                           "Madrid",
                           "Melilla",
                           "Murcia",
                           "Navarra",
                           "País Vasco"),
                  pop = c(8414240,#  "Andalucía",
                            1319291,#"Aragón",
                            1022800,#"Asturias",
                            1149460,#"Baleares",
                            5003769,#"C. Valenciana",
                            2153389,#"Canarias",
                            581078,#"Cantabria",
                            7675217,#"Cataluña",
                            84777,#"Ceuta",
                          2032863, #"CLM",
                            2399548,#"CyL",
                            1067710,#"Extremadura",
                            2699499,#"Galicia",
                            316798,#"La Rioja",
                            6663394,#"Madrid",
                            86487,#"Melilla",
                            1493898,#"Murcia",
                            654214,#"Navarra",
                            2207776))#"País Vasco"))
usethis::use_data(esp_pop, overwrite = TRUE)




# # Join with uci
# esp_df <- left_join(esp_df, esp_uci)

# Process the Spain data a bit more
esp_df <- esp_df %>%
  mutate(value = cases,
         confirmed_cases = cases)

# Get non cumulative cases
esp_df <- esp_df %>%
  ungroup %>%
  arrange(ccaa, date) %>%
  group_by(ccaa) %>%
  mutate(confirmed_cases_non_cum = confirmed_cases - lag(confirmed_cases, default = 0),
         deaths_non_cum = deaths - lag(deaths, default = 0),
         uci_non_cum = uci - lag(uci, default = 0)) %>%
  ungroup

# Spot corrections
esp_df <- esp_df %>%
  mutate(confirmed_cases_non_cum = ifelse(confirmed_cases_non_cum < 0,
                                          0, 
                                          confirmed_cases_non_cum)) 


# Look into using France / Italy data too
# https://code.montera34.com:4443/numeroteca/covid19
ita <- read.delim("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",sep = ",")  

# Process data
library(dplyr)
ita$date <-  as.Date(ita$data)
ita <- ita %>%
  dplyr::select(ccaa = denominazione_regione,
                date,
                cases = totale_casi,
                deaths = deceduti)
# De-cumulate
ita <- ita %>%
  ungroup %>%
  arrange(ccaa, date) %>%
  group_by(ccaa) %>%
  mutate(cases_non_cum = cases - lag(cases, default = 0),
         deaths_non_cum = deaths - lag(deaths, default = 0)) %>%
  ungroup

# By province
# https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/edit#gid=0

usethis::use_data(ita, overwrite = T)

# Get Italian populations
ita_pop <- tibble(
  ccaa = c('Abruzzo',
           'Basilicata',
           'Calabria',
           'Campania',
           'Emilia Romagna',
           'Friuli Venezia Giulia',
           'Lazio',
           'Liguria',
           'Lombardia',
           'Marche',
           'Molise',
           'P.A. Bolzano',
           'P.A. Trento',
           'Piemonte',
           'Puglia',
           'Sardegna',
           'Sicilia',
           'Toscana',
           'Umbria',
           'Valle d\'Aosta',
'Veneto'),
pop = c(
  1315000,#'Abruzzo',
  567118,#'Basilicata',
  1957000,#'Calabria',
  5827000,#'Campania',
  4453000,#'Emilia Romagna',
  1216000,#'Friuli Venezia Giulia',
  5897000,#'Lazio',
  1557000,#'Liguria',
  10040000,#'Lombardia',
  1532000,#'Marche',
  308493,#'Molise',
  520891,#'P.A. Bolzano',
  538223,#'P.A. Trento',
  4376000,#'Piemonte',
  4048000,#'Puglia',
  1648000,#'Sardegna',
  5027000,#'Sicilia',
  3737000,#'Toscana',
  884640,#'Umbria',
  126202,#'Valle d\'Aosta',
  4905000#'Veneto'
)
)
usethis::use_data(ita_pop, overwrite = T)

library(readr)
write_csv(esp_df, 'spain/ccaa_day.csv')
write_csv(esp_df, 'isglobal/ccaa_day.csv')
write_csv(esp_pop, 'isglobal/esp_pop.csv')
write_csv(world_pop, 'isglobal/world_pop.csv')
write_csv(ita_pop, 'isglobal/ita_pop.csv')

# write_csv(esp_uci, 'isglobal/esp_uci.csv')

usethis::use_data(esp_df, overwrite = T)
# Write a map

# Cognoms
cognoms <- read_csv('cognoms/cognoms.csv', skip = 7, locale = readr::locale(encoding = "latin1"))
cognoms <- cognoms$Nombre[1:1000]
surnames <- read_csv('cognoms/surnames.csv')
surnames <- surnames$name[1:1000]
usethis::use_data(cognoms, overwrite = T)
usethis::use_data(surnames, overwrite = T)
