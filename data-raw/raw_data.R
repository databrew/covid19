## code to prepare `raw_data` dataset goes here

# usethis::use_data("raw_data")

library(sp)
library(rgdal)
world <- readOGR('geo/world_small/', 'TM_WORLD_BORDERS_SIMPL-0.3')

# Get a country variable to match with JHU's
geo_countries <- sort(unique(world@data$NAME))
jhu_countries <- sort(unique(df$country))
jhu_countries[!jhu_countries %in% geo_countries]
# geo_countries[!geo_countries %in% jhu_countries]

# Recode
world@data <- world@data %>%
  mutate(country = as.character(NAME)) %>%
  mutate(country = dplyr::recode(
    country,
    'Brunei Darussalam' = 'Brunei',
    'Iran (Islamic Republic of)' = 'Iran',
    'China' = 'Mainland China',
    'Republic of Moldova' = 'Moldova',
    'The former Yugoslav Republic of Macedonia' = 'North Macedonia',
    'Korea, Republic of' = 'South Korea',
    'Saint Martin' = 'St. Martin',
    'United Kingdom' = 'UK',
    'United States' = 'US',
    'Holy See (Vatican City)' = 'Vatican City',
    'Viet Nam' = 'Vietnam'
    
  ))

usethis::use_data(world, overwrite = T)

