library(tidyverse)
library(readxl)
library(countrycode)

#### Import

df_cities <- read_excel("global-city-population-estimates.xls", sheet = "CITIES-OVER-300K")
df_countries <- read_csv("countries_pop_worldbank/5602213b-aad1-43d2-8ea5-b5434491cd6d_Data.csv")

# Unify country names
df_countries$country_name <- countrycode(sourcevar = df_countries$`Country Code`, origin = "wb", destination = "country.name")
df_countries <- df_countries %>% filter(str_detect(`Series Name`, "Popul"))
df_countries$country_name[is.na(df_countries$country_name)] <- df_countries$`Country Name`[is.na(df_countries$country_name)]
df_cities$country_name <- countrycode(sourcevar = df_cities$`Country Code`, origin = "iso3n", destination = "country.name")

# Drop unnecessary columns
df_countries <- df_countries %>% select(-all_of(colnames(df_countries)[1:4]))
df_cities <- df_cities %>% select(- `Country Code`, -`Country or area`, -`City Code`, -Note, -Latitude, -Longitude)
colnames(df_cities)[1] <- "city"
colnames(df_countries) <- c(paste(1960:2021),"country_name")

df_cities <- df_cities %>% pivot_longer(
  cols = `1950`:`2030`,
  names_to = "year",
  values_to = "population"
) %>% mutate(population = 1000*population)

df_countries <- df_countries %>% pivot_longer(
  cols = `1960`:`2021`,
  names_to = "year",
  values_to = "population_country"
) %>% mutate(population_country = as.numeric(population_country))

df_cities <- left_join(df_cities, df_countries, by = c("year", "country_name"))
df_cities <- df_cities %>% mutate(pop_share = population/population_country)
df_cities$year <- as.numeric(df_cities$year)
df_cities <- df_cities %>% mutate(
  logpop = log(population),
  logpopcountry = log(population_country)
) %>% group_by(city) %>% arrange(year) %>%
  mutate(
    lastpop = lag(population),
    lastlogpop = lag(logpop),
    lastpopcountry = lag(population_country),
    lastlogpopcountry = lag(logpopcountry),
    lastyear = lag(year),
    growthrate = (logpop - lastlogpop)/(year - lastyear),
    sharechange = pop_share - lag(pop_share)
  )






