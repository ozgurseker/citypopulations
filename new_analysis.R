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
df_countries <- df_countries %>% select(-`Series Name`,-`Series Code`, -`Country Code`, -`Country Name`)
df_cities <- df_cities %>% select(- `Country Code`, -`Country or area`, -`City Code`, -Note, -Latitude, -Longitude)
colnames(df_cities)[1] <- "city"
colnames(df_countries) <- c(paste(1960:2021),"country_name")
