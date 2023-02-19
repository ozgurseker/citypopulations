{
library(tidyverse)
library(readxl)
library(countrycode)
}
#### Import ####

df_cities <- read_excel("global-city-population-estimates.xls", sheet = "CITIES-OVER-300K")
df_countries <- read_csv("countries_pop_worldbank/5602213b-aad1-43d2-8ea5-b5434491cd6d_Data.csv")

#### Income Groups Investigate ####
cgroups <- read_csv("data-XHzgJ.csv") 

for(year in 1987:2020){
  cgroups[[paste(year)]] <- as.numeric(cgroups[[paste(year)]])
}
cgroups %>% group_by(`Income group`) %>% summarise(max = max(`2020`, na.rm = TRUE),
                                                   min = min(`2020`, na.rm = TRUE),
                                                   mean = mean(`2020`, na.rm = TRUE))
incomethresholds <- c(1000,4000,12500)
incomelevels <- c("Low","LowMid", "HighMid", "High")
#### Unify country names ####
df_countries$country_name <- countrycode(sourcevar = df_countries$`Country Code`, origin = "wb", destination = "country.name")
df_countries <- df_countries %>% filter(str_detect(`Series Name`, "Popul"))
df_countries$country_name[is.na(df_countries$country_name)] <- df_countries$`Country Name`[is.na(df_countries$country_name)]
df_cities$country_name <- countrycode(sourcevar = df_cities$`Country Code`, origin = "iso3n", destination = "country.name")

cgroups <- cgroups %>% mutate(Country = countrycode(
  sourcevar = Country,
  origin = "country.name", destination = "country.name"
))

#### Drop unnecessary columns and pivot longer ####

df_countries <- df_countries %>% select(-all_of(colnames(df_countries)[1:4]))
df_cities <- df_cities %>% select(- `Country Code`, -`Country or area`, -`City Code`, -Note, -Latitude, -Longitude)
colnames(df_cities)[1] <- "city"
colnames(df_countries) <- c(paste(1960:2021),"country_name")
cgroups <- cgroups %>% select(-`Lending category`, -`Income group`)

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

cgroups <- cgroups %>% pivot_longer(
  cols = `1987`:`2020`,
  names_to = "year",
  values_to = "income_country"
) 

#### Combine dataframes and modify ####

df_countries <- left_join(df_countries, cgroups, by = c("year"="year","country_name"="Country")) %>% 
  mutate(
  income_level = ifelse(income_country < incomethresholds[1], incomelevels[1], NA),
  income_level = ifelse(income_country < incomethresholds[2] & income_country >= incomethresholds[1], incomelevels[2], income_level),
  income_level = ifelse(income_country < incomethresholds[3] & income_country >= incomethresholds[2], incomelevels[3], income_level),
  income_level = ifelse(income_country >= incomethresholds[3], incomelevels[4], income_level)
) %>% group_by(country_name) %>% fill(income_level, .direction = "up")


df_cities <- left_join(df_cities, df_countries, by = c("year", "country_name")) %>% 
  mutate(pop_share = population/population_country,
         year = as.numeric(year)) %>% 
  mutate(
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
    growthratecountry = (logpopcountry - lastlogpopcountry)/(year - lastyear),
    sharechange = (pop_share - lag(pop_share))/(year - lastyear),
    excessgrowth = growthrate - growthratecountry
  ) %>% fill(income_level, .direction = "up")


#### Search ####
largest_cities1970 <- (df_cities %>% group_by(country_name) %>% filter(year==1970) %>% 
                         filter(pop_share==max(pop_share), income_level %in% incomelevels[2:3]))$city

df_cities %>% filter(year < 1990, pop_share > 0.1, pop_share < 0.8, growthrate < 0.1, city %in% largest_cities1970) %>%
  ggplot() + geom_point(aes(x=logpop*pop_share*(1-pop_share), y =excessgrowth)) + 
  stat_smooth(aes(x=logpop*pop_share*(1-pop_share), y =excessgrowth), se = FALSE)

df_cities %>% filter(year < 1990, pop_share > 0.1, pop_share < 0.8, growthrate < 0.1, city %in% largest_cities1970) %>%
  ggplot() + geom_point(aes(x=lastlogpop, y =log((population - lastpop)/(year-lastyear)))) + 
  stat_smooth(aes(x=lastlogpop, y =log((population - lastpop)/(year-lastyear))), se = FALSE, method = "lm")



#### Nice Ones ####
largest_cities1970 <- (df_cities %>% group_by(country_name) %>% filter(year==1970) %>% 
                         filter(pop_share==max(pop_share), income_level %in% incomelevels[2:3]))$city

df_cities %>% filter(year < 2000, pop_share > 0.1, pop_share < 0.8, growthrate < 0.1, city %in% largest_cities1970) %>%
  ggplot() + geom_point(aes(x=lastlogpop, y =log((population - lastpop)/(year-lastyear)))) + 
  stat_smooth(aes(x=lastlogpop, y =log((population - lastpop)/(year-lastyear))), se = FALSE, method = "lm") + 
  geom_point(data=df_cities %>% filter(year < 2000, pop_share > 0.1, pop_share < 0.8, growthrate < 0.1, city =="Istanbul"), 
             aes(x=lastlogpop, y =log((population - lastpop)/(year-lastyear))), 
             color='red',
             size=3)


df_cities %>% filter(city %in% c("Istanbul", "Seoul", "Delhi", "Ciudad de México (Mexico City)")) %>%
  ggplot() + geom_line(aes(x=year, y =population, col = city))

df_cities %>% filter(city %in% c("Istanbul", "Seoul", "Manila", "São Paulo")) %>%
  ggplot() + geom_line(aes(x=year, y =pop_share, col = city))

df_cities %>% filter(city %in% c("Istanbul", "Seoul", "Tokyo")) %>%
  ggplot() + geom_point(aes(x=(population_country - population)/population, y =growthrate, col = city))


mdl <- lm("excessgrowth ~ lastlogpop + lastlogpopcountry", 
          data = df_cities %>% filter(year < 2001, pop_share > 0.1, pop_share < 0.7, excessgrowth < 0.1))
summary(mdl)

largest_cities1970 <- (df_cities %>% group_by(country_name) %>% filter(year==1970) %>% 
  filter(pop_share==max(pop_share)))$city

