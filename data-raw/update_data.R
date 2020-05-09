# Prior to running this, one should
# Clone the repo at https://github.com/CSSEGISandData/COVID-19/ into the same level as the covid19 directory (same level, not same directory)



# Pull from JHU
system('cd ../../COVID-19; pwd; git pull;')

# Pull from ibesora's webscraping app
system('cd ../../covid-19-data; pwd; git pull;')

# Pull from data for French departments
# Consider replacing with this: Updated map (data through 31 March
system('cd ../../FRANCE-COVID-19; git pull')

# Pull data for Portugese regions
system('cd ../../covid19pt-data; git pull')

library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(rgdal)
library(sp)
library(raster)
library(rgdal)

# Testing data from our world in data
testing <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv')
usethis::use_data(testing, overwrite = T)

# Pull from transpariencia catalunya
# https://analisi.transparenciacatalunya.cat/Salut/Registre-de-casos-de-COVID-19-realitzats-a-Catalun/qwj8-xpvk/data
if(!dir.exists('trans_cat/')){
  dir.create('trans_cat/')
}
trans_cat <- read_csv('https://analisi.transparenciacatalunya.cat/api/views/qwj8-xpvk/rows.csv?accessType=DOWNLOAD&sorting=true')
trans_cat <- trans_cat %>%
  mutate(date = as.Date(TipusCasData, format = '%d/%m/%Y'))
write_csv(trans_cat, 'trans_cat/trans_cat.csv')
usethis::use_data(trans_cat, overwrite = T)


# Province level data for all of Spain
provinces <- read_csv('https://github.com/montera34/escovid19data/blob/master/data/output/covid19-provincias-spain_consolidated.csv?raw=true')
usethis::use_data(provinces, overwrite = T)

# Get catalunya regions sanitarias
library(rgdal)
regions_sanitaries <- readOGR('regions_sanitaries/', 'RegionsS_2018', encoding = "latin1")
usethis::use_data(regions_sanitaries, overwrite = T)



# Pull from Spanish ministry
if(!dir.exists('isciii')){
  dir.create('isciii')
}
isc <- read_csv('https://cnecovid.isciii.es/covid19/resources/agregados.csv',
                col_names = c("CCAA", "FECHA", "CASOS", "PCR+", "TestAc+", "Hospitalizados", "UCI", "Fallecidos", "Recuperados"),
                col_types = c('ccddddddd')) %>%
  filter(!is.na(CCAA))
remove_from <- which(grepl('NOTA', isc$CCAA))[1]
isc <- isc[1:(remove_from-1),]
joiner <- tibble(CCAA = c('AN',
                          'AR',
                          'AS',
                          'CB',
                          'CE',
                          'CL',
                          'CM',
                          'CN',
                          'CT',
                          'EX',
                          'GA',
                          'IB',
                          'MC',
                          'MD',
                          'ML',
                          'NC',
                          'PV',
                          'RI',
                          'VC'),
                 ccaa = c('Andalucía',
                          'Aragón',
                          'Asturias',
                          'Cantabria',
                          'Ceuta',
                          'CyL',
                          'CLM',
                          'Canarias',
                          'Cataluña',
                          'Extremadura',
                          'Galicia',
                          'Baleares',
                          'Murcia',
                          'Madrid',
                          'Melilla',
                          'Navarra',
                          'País Vasco',
                          'La Rioja',
                          'C. Valenciana'))
isc <- left_join(isc, joiner)
# Deal with the PCR and Ac columns
isc$CASOS <- ifelse(is.na(isc$CASOS),
                    ifelse(!is.na(isc$`PCR+`),
                           isc$`PCR+`,
                           0) + 
                      ifelse(!is.na(isc$`TestAc+`),
                             isc$`TestAc+`,
                             0),
                    isc$CASOS)
isc <- isc %>%
  mutate(CASOS = ifelse(is.na(CASOS), 0, CASOS))

isc <- isc %>%
  dplyr::select(date = FECHA,
                ccaa,
                cases = CASOS,
                uci = UCI,
                deaths = Fallecidos)
isc$date <- as.Date(isc$date, format = '%d/%m/%Y')
isc <- isc %>% filter(!is.na(date))
message('MAX DATE IN SPAIN IS ', max(isc$date))
write_csv(isc, 'isciii/raw.csv')

# Get French data
cases <- read_csv('../../FRANCE-COVID-19/france_coronavirus_time_series-confirmed.csv')
deaths <- read_csv('../../FRANCE-COVID-19/france_coronavirus_time_series-deaths.csv')
# Clean up
clean_france <- function(x){
  x$Date <- as.Date(x$Date, format = '%d/%m/%Y')
  x$Total <- NULL
  x <- x %>% gather(key, value, names(x)[2:ncol(x)])
  x <- x %>% dplyr::rename(date = Date,
                           ccaa = key)
  return(x)
}
cases <- clean_france(cases) %>% dplyr::rename(cases = value)
deaths <- clean_france(deaths) %>% dplyr::rename(deaths = value)
fra_df <- left_join(cases, deaths)
# fra_df <- fra_df %>%
#   mutate(cases = ifelse(is.na(cases), 0, cases),
#          deaths = ifelse(is.na(deaths), 0, deaths))
fra_df <- fra_df %>% arrange(date, ccaa)
# Flag non-updated areas 
fra_df <- fra_df %>%
  group_by(ccaa) %>%
  mutate(flag_cases = cases < dplyr::lag(cases, 1),
         flag_deaths = deaths < dplyr::lag(deaths, 1)) %>%
  ungroup %>%
  mutate(cases = ifelse(flag_cases, NA, cases),
         deaths = ifelse(flag_deaths, NA, deaths)) %>%
  dplyr::select(-flag_deaths,
                -flag_cases)
cases <- fra_df %>% dplyr::select(date, ccaa, cases) %>% filter(!is.na(cases))
deaths <- fra_df %>% dplyr::select(date, ccaa, deaths) %>% filter(!is.na(deaths))
left <- expand.grid(date = sort(unique(c(cases$date, deaths$date))),
                    ccaa = sort(unique(c(cases$ccaa, deaths$ccaa))))
joined <- left_join(left, cases) %>% left_join(deaths)
# Interpolate missing
library(zoo)
fra_df <- joined %>%
  arrange(date) %>%
  group_by(ccaa) %>%
    mutate(cases = na.approx(cases, maxgap = 3, rule = 2),
           deaths = na.approx(deaths, maxgap = 3, rule = 2))

# Get french population
fra_pop <- 
  tibble(ccaa = c("Auvergne-Rhône-Alpes",
                  "Bourgogne-Franche-Comté",
                  "Bretagne",
                  "Centre-Val de Loire",
                  "Corse",
                  "Grand Est",
                  "Guadeloupe",
                  "Guyane",
                  "Hauts-de-France",
                  "Île-de-France",
                  "La Réunion",
                  "Martinique",
                  "Mayotte",
                  "Normandie",
                  "Nouvelle-Aquitaine",
                  "Nouvelle-Calédonie",
                  "Occitanie",
                  "Pays de la Loire",
                  "Provence-Alpes-Côte d'Azur",
                  "Saint-Barthélémy",
                  "Saint-Martin"),
         pop = c(
           8032377, #"Auvergne-Rhône-Alpes",
           2783039, #"Bourgogne-Franche-Comté",
           3340379, #"Bretagne",
           2559073, #"Centre-Val de Loire",
           344679, #"Corse",
           5511747, #"Grand Est",
           395700, #"Guadeloupe",
           290691, #"Guyane",
           5962662, #"Hauts-de-France",
           12278210, #"Île-de-France",
           859959, #"La Réunion",
           376480, #"Martinique",
           270372, #"Mayotte",
           3303500, #"Normandie",
           5999982, #"Nouvelle-Aquitaine",
           280460, #"Nouvelle-Calédonie",
           5924858, #"Occitanie",
           3801797, #"Pays de la Loire",
           5055651, #"Provence-Alpes-Côte d'Azur",
           9131, # "Saint-Barthélémy",
           32125# "Saint-Martin"
         ))
# Get french map
map_fra <- raster::getData(country = 'FRA', level = 1)
# Get Spanish map
map_esp <- raster::getData(country = 'ESP', level = 1)
# Get Italian map
map_ita <- raster::getData(country = 'ITA', level = 1)
ccaas <- map_ita@data$NAME_1
ccaas <- ifelse(ccaas == "Apulia", 'Puglia',
                # ifelse(ccaas == "Emilia-Romagna", 'Emilia Romagna',
                       ifelse(ccaas == "Friuli-Venezia Giulia", 'Friuli Venezia Giulia',
                              ifelse(ccaas == "Sicily", 'Sicilia',
                                            ccaas)))
map_ita@data$ccaa <- ccaas
usethis::use_data(map_ita, overwrite = T)
# Andorra map
map_and <- raster::getData(country = 'AND', level = 0)
usethis::use_data(map_and, overwrite = T)
# Fix names
names_df <- tibble(NAME_1 = c('Andalucía',
                              'Aragón',
                              'Cantabria',
                              'Castilla-La Mancha',
                              'Castilla y León',
                              'Cataluña',
                              'Ceuta y Melilla',
                              'Comunidad de Madrid',
                              'Comunidad Foral de Navarra',
                              'Comunidad Valenciana',
                              'Extremadura',
                              'Galicia',
                              'Islas Baleares',
                              'La Rioja',
                              'País Vasco',
                              'Principado de Asturias',
                              'Región de Murcia',
                              'Islas Canarias'),
                   ccaa = c('Andalucía',
                            'Aragón',
                            'Cantabria',
                            'CLM',
                            'CyL',
                            'Cataluña',
                            'Melilla',
                            'Madrid',
                            'Navarra',
                            'C. Valenciana',
                            'Extremadura',
                            'Galicia',
                            'Baleares',
                            'La Rioja',
                            'País Vasco',
                            'Asturias',
                            'Murcia',
                            'Canarias'))
map_esp@data <- left_join(map_esp@data, names_df)

usethis::use_data(map_esp, overwrite = TRUE)
usethis::use_data(fra_pop, overwrite = TRUE)
usethis::use_data(fra_df, overwrite = TRUE)
usethis::use_data(map_fra, overwrite = TRUE)

# Get Portuguese data
#https://github.com/dssg-pt/covid19pt-data
# Shpaefiles
map_por <- readOGR('../../covid19pt-data/extra/mapas/portugal/', 'portugal')
# raw data
portugal <- read_csv('../../covid19pt-data/data.csv')
# Chop down variables
places <- c('_arscentro','_arslvt','arsalentejo','arsalgarve','acores','madeira','estrangeiro', 'arsnorte')
places <- paste0(places, collapse = '|')
keep_vars <- which(grepl(places, names(portugal)) & !grepl('recuperados', names(portugal)))
portugal <- portugal[,c(1, keep_vars)]
portugal$data <- as.Date(portugal$data, format = '%d-%m-%Y')
portugal <- gather(portugal, key, value, names(portugal)[2:ncol(portugal)])
ccaa <- unlist(lapply(strsplit(portugal$key, '_'), function(x){x[2]}))
key <- unlist(lapply(strsplit(portugal$key, '_'), function(x){x[1]}))
portugal$key <- ifelse(key == 'obitos', 'deaths', 'cases')
portugal$ccaa <- ccaa
deaths <- portugal %>% filter(key == 'deaths') %>% dplyr::select(-key) %>% dplyr::rename(deaths = value)
cases <- portugal %>% filter(key == 'cases') %>% dplyr::select(-key) %>% dplyr::rename(cases = value)
joined <- full_join(cases, deaths)
joined <- joined %>%
  mutate(ccaa = ifelse(ccaa == 'acores', 'Açores',
                       ifelse(ccaa == 'arsalgarve', 'Algarve',
                              ifelse(ccaa == 'arsalentejo', 'Alentejo',
                                     ifelse(ccaa == 'arslvt', 'RLVT',
                                            ifelse(ccaa == 'madeira', 'Madeira',
                                                   ifelse(ccaa == 'arsnorte', 'Norte',
                                                          ifelse(ccaa == 'arscentro', 'Centro', 'Estrangeiro'))))))))
por_df <- joined
por_df <- por_df %>%
  mutate(cases = ifelse(is.na(cases), 0, cases),
         deaths = ifelse(is.na(deaths), 0, deaths))
por_df <- por_df %>% dplyr::rename(date = data)
por_df <- por_df %>% arrange(date, ccaa)
# Portugal population
por_pop <- tibble(
  ccaa = c('Açores',
           'Alentejo',
           'Algarve',
           'Madeira',
           'Norte',
           'Centro',
           'RLVT'),
  pop = c(242846,
          705478,
          438864,
          253945,
          3572583,
          2327000,
          3447173)
)
usethis::use_data(por_pop, overwrite = TRUE)
usethis::use_data(por_df, overwrite = TRUE)
usethis::use_data(map_por, overwrite = TRUE)


# Get ibesora's data (from acquas application)
the_dir <- '../../covid-19-data/'
csvs <- dir(the_dir)
csvs <- csvs[grepl('.csv', csvs, fixed = T)]
if(file.exists('catalonia.csv')){
  data_list <- list()
  for(i in 1:length(csvs)){
    data <- read_csv(paste0(the_dir, csvs[i])) %>%
      mutate(file_name = gsub('.csv', '', csvs[i], fixed = TRUE))
    data_list[[i]] <- data
  }
}

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
have_pop <- c('US', 'Italy', 'Spain', 'China', 'Canada', 'France', 'Portugal')

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
  mutate(country = ifelse(country == 'UK', 'United Kingdom', country)) %>%
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
# url <- 'https://docs.google.com/spreadsheets/d/15UJWpsW6G7sEImE8aiQ5JrLCtCnbprpztfoEwyTNTEY/edit#gid=810081118'
# esp_df <- gsheet::gsheet2tbl(url)
na_to_zero <- function(x){ifelse(is.na(x), 0, x)}
esp_df <- isc %>% mutate(cases = na_to_zero(cases),
                         uci = na_to_zero(uci),
                         deaths = na_to_zero(deaths))
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
ita$ccaa <- as.character(ita$ccaa)
ita <- ita %>%
  mutate(ccaa = ifelse(ccaa %in% c('P.A. Bolzano', 'P.A. Trento'), 'Trentino-Alto Adige',
                       ccaa)) %>%
  group_by(ccaa, date) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            uci = sum(uci, na.rm = TRUE),
            hospitalizations = sum(hospitalizations, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE))

# Get the district-level data for Spain and Italy Portugal, France too in (since JHU doesn't have it)
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
    mutate(country = 'Spain')
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
  
  # Portugal
  left <- por_df %>% 
    dplyr::rename(district = ccaa) %>%
    mutate(country = 'Portugal')
  right <- df %>% filter(country == 'Portugal') %>%
    dplyr::select(date,country)
  add_these <- df %>% filter(country == 'Portugal') %>%
    filter(!date %in% left$date,
           date < max(left$date)) # remove any obs AHEAD of ministry data
  joined <- left_join(left, right) %>% bind_rows(add_these)
  df <- df %>%
    mutate(flag = (country == 'Portugal' & date %in% joined$date) | (country == 'Portugal' & date > max(por_df$date))) %>%
    filter(!flag) %>% dplyr::select(-flag) %>%
    bind_rows(joined)
  
  # # France (special case because JHU has some)
  # NOT DOING DUE TO INCOMPLETE DATA FOR FRANCE
  # left <- fra_df %>% 
  #   dplyr::rename(district = ccaa) %>%
  #   mutate(country = 'France')
  # right <- df %>% filter(country == 'France') %>%
  #   dplyr::distinct(date,country)
  # add_these <- df %>% filter(country == 'France') %>%
  #   group_by(date, country) %>% mutate(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>%
  #   mutate(district = NA) %>%
  #   filter(!date %in% left$date,
  #          date < max(left$date)) # remove any obs AHEAD of ministry data
  # joined <- left_join(left, right) %>% bind_rows(add_these)
  # df <- df %>%
  #   mutate(flag = (country == 'France' & date %in% joined$date) | (country == 'France' & date > max(fra_df$date))) %>%
  #   filter(!flag) %>% dplyr::select(-flag) %>%
  #   bind_rows(joined)
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

# De-cumulate france
fra_df <- fra_df %>%
  ungroup %>%
  arrange(ccaa, date) %>%
  group_by(ccaa) %>%
  mutate(cases_non_cum = cases - lag(cases, default = 0),
         deaths_non_cum = deaths - lag(deaths, default = 0)) %>%
  ungroup

# De-cumulate portugal
por_df <- por_df %>%
  ungroup %>%
  arrange(ccaa, date) %>%
  group_by(ccaa) %>%
  mutate(cases_non_cum = cases - lag(cases, default = 0),
         deaths_non_cum = deaths - lag(deaths, default = 0)) %>%
  ungroup


# By province
# https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/edit#gid=0

usethis::use_data(ita, overwrite = T)
usethis::use_data(por_df, overwrite = T)
usethis::use_data(fra_df, overwrite = T)

# Get Italian populations
ita_pop <- tibble(
  ccaa = c('Abruzzo',
           'Basilicata',
           'Calabria',
           'Campania',
           'Emilia-Romagna',
           'Friuli Venezia Giulia',
           'Lazio',
           'Liguria',
           'Lombardia',
           'Marche',
           'Molise',
           'Trentino-Alto Adige',
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
  1070000, # trentino alto
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

# Create a regions pop for Italy, China, Spain, US, Portugal, France
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
            fra_pop %>% mutate(country = 'France', iso = 'FRA'),
            por_pop %>% mutate(country = 'Portugal', iso = 'PRT'),
            canada_pop %>% mutate(country = 'Canada', iso = 'CAN'))
usethis::use_data(regions_pop, overwrite = T)

# Write to the covidcount directory
write_csv(df_country, '../../covidcount/data/jhu.csv')
write_csv(world_pop, '../../covidcount/data/world_pop.csv')
setwd('..')
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))
devtools::install()
setwd('data-raw/')
