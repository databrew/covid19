# Prior to running this, one should
# Clone the repo at https://github.com/CSSEGISandData/COVID-19/ into the same level as the covid19 directory (same level, not same directory)

# Pull from JHU
system('cd ../../COVID-19; pwd; git pull;')

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

# Add regions
iso_regions <- read_csv('isoregions.csv') %>%
  dplyr::select(iso = `alpha-3`,
                region,
                sub_region = `sub-region`)
world_pop <- left_join(world_pop, iso_regions)
usethis::use_data(world_pop, overwrite = TRUE)


# Get chinese pop by region
chi_pop <- tibble(
  ccaa = c('Anhui',
           'Beijing',
           'Chongqing',
           'Fujian',
           'Gansu',
           'Guangdong',
           'Guangxi',
           'Guizhou',
           'Hainan',
           'Hebei',
           'Heilongjiang',
           'Henan',
           'Hong Kong',
           'Hubei',
           'Hunan',
           'Inner Mongolia',
           'Jiangsu',
           'Jiangxi',
           'Jilin',
           'Liaoning',
           'Macau',
           'Ningxia',
           'Qinghai',
           'Shaanxi',
           'Shandong',
           'Shanghai',
           'Shanxi',
           'Sichuan',
           'Tianjin',
           'Tibet',
           'Xinjiang',
           'Yunnan',
           'Zhejiang'),
  pop = c(63.24, #'Anhui',
          21.54, #'Beijing',
          31.02, #'Chongqing',
          39.41, #'Fujian',
          26.37, #'Gansu',
          113.46, #'Guangdong',
          49.26, #'Guangxi',
          36, #'Guizhou',
          9.34, #'Hainan',
          75.56, #'Hebei',
          37.73, #'Heilongjiang',
          96.05, #'Henan',
          7.39, #'Hong Kong',
          59.17, #'Hubei',
          68.99, #'Hunan',
          25.34, #'Inner Mongolia',
          80.51, #'Jiangsu',
          46.48, #'Jiangxi',
          27.04, #'Jilin',
          43.59, #'Liaoning',
          0.62, #'Macau',
          6.88, #'Ningxia',
          6.03, #'Qinghai',
          38.64, #'Shaanxi',
          100.47, #'Shandong',
          24.24, #'Shanghai',
          37.18, #'Shanxi',
          83.41, #'Sichuan',
          15.6, #'Tianjin',
          3.44, #'Tibet',
          24.87, #'Xinjiang',
          48.3, #'Yunnan',
          57.37)) #'Zhejiang')
# Multiply by 1 million
chi_pop$pop <- chi_pop$pop * 1000000
usethis::use_data(chi_pop, overwrite = TRUE)

# Johns Hopkins data
# Define the files at https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_daily_reports
reports_dir <- '../../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/'
files <- dir(reports_dir)
files <- files[grepl('.csv', files, fixed = T)]

# Read in all files
out_list <- list()
for(i in 1:length(files)){
  this_file <- files[i]
  this_date <- as.Date(substr(this_file, 1, 10), format = '%m-%d-%Y')
  this_data <- read_csv(paste0(reports_dir, this_file)) 
  # There was a change in formatting, so have to do this in two different ways
  if('Admin2' %in% names(this_data)){
    this_data <-this_data %>%
      dplyr::select(district = Province_State,
                    country = Country_Region,
                    time_stamp = Last_Update,
                    cases = Confirmed,
                    deaths = Deaths,
                    recovered = Recovered)
  } else {
    this_data <-this_data %>%
      dplyr::rename(district = `Province/State`,
                    country = `Country/Region`,
                    time_stamp = `Last Update`,
                    cases = Confirmed,
                    deaths = Deaths,
                    recovered = Recovered)
  }
  
  
  out_list[[i]] <- this_data %>% mutate(time_stamp = as.character(time_stamp)) %>%
    mutate(date = this_date)
}
df <- bind_rows(out_list) %>% dplyr::select(-Latitude, -Longitude, -recovered)

# Define which countries we have sub-national population data for
have_pop <- c('US', 'Italy', 'Spain', 'China', 'Canada')

# For China, get all on one
df <- df %>% mutate(country = ifelse(country == 'Mainland China',
                                       'China',
                                       country)) %>%
  mutate(country = ifelse(country == 'Korea, South', 'South Korea',
                          ifelse(country == 'Hong Kong SAR', 'Hong Kong',
                                 ifelse(country == 'Republic of Korea', 'North Korea',
                                        ifelse(country == 'Macao SAR', 'Macao',
                                               ifelse(country == 'Czech Republic', 'Czechia',
                                                      ifelse(country == 'Republic of Moldova', 'Moldova',
                                                             ifelse(country == 'Russian Federation',
                                                                    'Russia',
                                                                    ifelse(country == 'Iran (Islamic Republic of)',
                                                                           'Iran', country))))))))) %>%
  mutate(country = ifelse(country == 'Gambia, The', 'Gambia', country)) %>%
  mutate(district = ifelse(country %in% have_pop,
                                        district,
                                        NA)) %>%
  group_by(country, district, date) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE)) %>%
  ungroup
  

# For US
# Up until March 9, data shows by sub-states but not States
# Beginning on March 10, shows states, not sub-states
usiso2 <- readLines('usiso2/usiso2.txt')
iso2 <- unlist(lapply(strsplit(usiso2, split = '\t', fixed = T), function(x){x[1]}))
district <- unlist(lapply(strsplit(usiso2, split = '\t', fixed = T), function(x){x[2]}))
usiso2 <- tibble(district, iso2)

us_old <- df %>% filter(country == 'US', date <= '2020-03-09')
us_new <- df %>% filter(country == 'US', date >= '2020-03-10')

# In us_old, keep only the sub-states, since that's where the data is
us_old <- us_old %>% filter(grepl(',', district))
# Do the opposite in us_new (ie, keep only the states)
us_new <- us_new %>% filter(!grepl(',', district))

# In us_old, get the state
us_old$iso2 <- unlist(lapply(strsplit(us_old$district, ', '), function(x){x[length(x)]}))
# now bring in state name
us_old <- left_join(us_old %>% dplyr::select(-district), usiso2) %>% dplyr::select(-iso2)
us_old <- us_old %>%
  group_by(country, date, district) %>%
  summarise(cases = sum(cases, na.rm = T),
            deaths = sum(deaths, na.rm = TRUE)) %>% ungroup
# bring it all together now
df <- df %>%
  filter(country != 'US') %>%
  bind_rows(us_old,
            us_new)

# Get most recent Spanish ministry data
library(gsheet)
url <- 'https://docs.google.com/spreadsheets/d/15UJWpsW6G7sEImE8aiQ5JrLCtCnbprpztfoEwyTNTEY/edit#gid=810081118'
esp_df <- gsheet::gsheet2tbl(url)
right <- esp_df %>%
  group_by(date) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            deaths  = sum(deaths, na.rm = TRUE))

# Get Italy regional data

# https://code.montera34.com:4443/numeroteca/covid19
ita <- read.delim("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",sep = ",")  

# Process data
ita$date <-  as.Date(ita$data)
ita <- ita %>%
  dplyr::select(ccaa = denominazione_regione,
                date,
                cases = totale_casi,
                uci = terapia_intensiva,
                hospitalizations = totale_ospedalizzati,
                deaths = deceduti)

# Get the district-level data for Spain and Italy in (since JHU doesn't have it)
do_italy_spain <- TRUE
if(do_italy_spain){
  # Italy
  left <- ita %>% 
    dplyr::rename(district = ccaa) %>%
    mutate(country = 'Italy')
  right <- df %>% filter(country == 'Italy') %>%
    dplyr::select(date,country)
  add_these <- df %>% filter(country == 'Italy') %>%
    filter(!date %in% left$date,
           date < max(left$date)) # remove any obs AHEAD of ministry data
  joined <- left_join(left, right) %>% bind_rows(add_these)
  df <- df %>%
    mutate(flag = (country == 'Italy' & date %in% joined$date) | (country == 'Italy' & date > max(ita$date))) %>%
    filter(!flag) %>% dplyr::select(-flag) %>%
    bind_rows(joined)
  
  # Spain
  left <- esp_df %>% 
    dplyr::rename(district = ccaa) %>%
    mutate(country = 'Spain') %>%
    dplyr::select( -comment)
  right <- df %>% filter(country == 'Spain') %>%
    dplyr::select(date,country)
  add_these <- df %>% filter(country == 'Spain') %>%
    filter(!date %in% left$date,
           date < max(left$date)) # remove any obs AHEAD of ministry data
  joined <- left_join(left, right) %>% bind_rows(add_these)
  df <- df %>%
    mutate(flag = (country == 'Spain' & date %in% joined$date) | (country == 'Spain' & date > max(esp_df$date))) %>%
    filter(!flag) %>% dplyr::select(-flag) %>%
    bind_rows(joined)
}

# Decumulate
df <- df %>%
  ungroup %>%
  arrange(country, district, date) %>%
  group_by(country, district) %>%
  mutate(cases_non_cum = cases - dplyr::lag(cases, default = 0),
         deaths_non_cum = deaths - dplyr::lag(deaths, default = 0),
         uci_non_cum = uci - dplyr::lag(uci, default = 0)) %>%
  ungroup

# Deal with errors in US data
df$cases[df$district == 'Nevada' & df$country == 'US' & df$date == '2020-03-17'] <- 50
df$cases[df$district == 'Utah' & df$country == 'US' & df$date == '2020-03-19'] <- 77
df$cases[df$district == 'Washington' & df$country == 'US' & df$date == '2020-03-17'] <- 1000
df$cases[df$district == 'Grand Princess' & df$country == 'US' & df$date == '2020-03-16'] <- 21
df$deaths[df$district == 'Oregon' & df$country == 'US' & df$date == '2020-03-22'] <- 5

# Join all together but by country
df_country <- df %>%
  group_by(country, date) %>%
  dplyr::summarise(cases = sum(cases, na.rm = TRUE),
                        deaths = sum(deaths, na.rm = TRUE),
                        uci = sum(uci, na.rm = TRUE),
                        hospitalizations = sum(hospitalizations, na.rm = TRUE),
                        cases_non_cum = sum(cases_non_cum, na.rm = TRUE),
                        deaths_non_cum = sum(deaths_non_cum, na.rm = TRUE),
                        uci_non_cum = sum(uci_non_cum, na.rm = TRUE))

# Get the iso code for each country
countries <- sort(unique(df$country))
country_codes <- tibble(country = countries)
library(passport)
country_codes$iso <- parse_country(x = countries, to = 'iso3c')
df <- left_join(df, country_codes)
df_country <- left_join(df_country, country_codes)

usethis::use_data(df, overwrite = T)
usethis::use_data(df_country, overwrite = T)

# Make spreadsheet for ISGlobal
isglobal <- df_country %>%
  filter(date == max(date)) %>%
  dplyr::select(date, 
                country, 
                cases,
                deaths) %>%
  left_join(country_codes)


if(!dir.exists('isglobal')){
  dir.create('isglobal')
}
write_csv(isglobal, 'isglobal/isglobal.csv')

write_csv(df, 'isglobal/world_region_data.csv')
write_csv(df_country, 'isglobal/world_data.csv')


# Read population data for Spain's CCAA
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


# Interpret missing UCI cases
esp_df <- esp_df %>%
  arrange(date) %>%
  group_by(ccaa) %>%
  mutate(uci = zoo::na.approx(uci, na.rm = F))

# Get non cumulative cases
esp_df <- esp_df %>%
  ungroup %>%
  arrange(ccaa, date) %>%
  group_by(ccaa) %>%
  mutate(cases_non_cum = cases - lag(cases, default = 0),
         deaths_non_cum = deaths - lag(deaths, default = 0),
         uci_non_cum = uci - lag(uci, default = 0)) %>%
  ungroup

# Spot corrections
esp_df <- esp_df %>%
  mutate(cases_non_cum = ifelse(cases_non_cum < 0,
                                          0, 
                                cases_non_cum)) 


# De-cumulate Italy
ita <- ita %>%
  ungroup %>%
  arrange(ccaa, date) %>%
  group_by(ccaa) %>%
  mutate(cases_non_cum = cases - lag(cases, default = 0),
         deaths_non_cum = deaths - lag(deaths, default = 0),
         uci_non_cum = uci - lag(uci, default = 0),
         hospitalizations_non_cum = hospitalizations - lag(hospitalizations, default = 0)) %>%
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

# Create a regions pop for Italy, China, Spain, US
usa_pop <- readxl::read_excel('usapop/usapop.xlsx', 
                              skip = 3)
canada_pop <- 
  tibble(ccaa = c('Alberta',
         'British Columbia',
         'Grand Princess',
         'Manitoba',
         'New Brunswick',
         'Newfoundland and Labrador',
         'Northwest Territories',
         'Nova Scotia',
         'Ontario',
         'Prince Edward Island',
         'Quebec',
         'Saskatchewan'),
         pop = c(4413146,
                 5110917,
                 NA,
                 1377517,
                 779993,
                 521365,
                 44904,
                 977457,
                 14711827,
                 158158,
                 8537674,
                 1181666))
names(usa_pop)[c(1,13)] <- c('ccaa', 'pop')
usa_pop <- usa_pop %>% dplyr::select('ccaa', 'pop')
usa_pop <- usa_pop[6:56,]
usa_pop$ccaa <- gsub('.', '', usa_pop$ccaa, fixed = TRUE)
regions_pop <- 
  bind_rows(esp_pop %>% mutate(country = 'Spain', iso = 'ESP'),
            ita_pop %>% mutate(country = 'Italy', iso = 'ITA'),
            chi_pop %>% mutate(country = 'China', iso = 'CHN'),
            usa_pop %>% mutate(country = 'US', iso = 'USA'),
            canada_pop %>% mutate(country = 'Canada', iso = 'CAN'))
usethis::use_data(regions_pop, overwrite = T)

setwd('..')
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))
devtools::install()
setwd('data-raw/')
