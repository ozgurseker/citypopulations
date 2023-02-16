library(tidyverse)

#### Prep and Combine Data ####
df_global <- read_csv("UNdata_Export_20230215_190540160.csv") %>% filter(Sex == "Both Sexes") %>% select(-Sex)
df_turkey <- read_csv("turkey_cities.csv")
df_turkey <- df_turkey %>% mutate(
  total = str_remove_all(total, "[.]"),
  urban = str_remove_all(urban, "[.]"),
  rural = str_remove_all(rural, "[.]")
)
df <- df_global %>% select(`Country or Area`, City, Year, Value)
colnames(df) <- c("country", "city", "year", "population")
df_turkey$country <- "Turkey"
df_turkey <- df_turkey %>% select(country, city, year, total)
colnames(df_turkey) <- c("country", "city", "year", "population")
df <- rbind(df, df_turkey)
df$population <- as.numeric(df$population)
df$year <- as.numeric(df$year)

#### Prep Variables ####

df <- df %>% mutate(logpop = log(population)) %>%
  group_by(city) %>% arrange(year) %>%
  mutate(lastyear = lag(year),
         lastpop = lag(population)) %>%
  mutate(lastlogpop = log(lastpop),
         growthpop = (logpop - lastlogpop)/(year - lastyear))

df %>% filter(year<1992) %>% group_by(country) %>% count()
unique(df$country)

df %>% filter(str_detect(country, "Korea")) %>% group_by(year) %>% count()

df %>% filter(population > 5000000)

library(readr)
worldcities <- read_csv("~/Downloads/simplemaps_worldcities_basicv1.75/worldcities.csv")
View(worldcities)

# ulke populasyonu buyume hizlari ekle
# sehir populasyonlari buyume hizi ekleI

# istanbula benzer populasyona sahip sehirler belli yillarda onlari bi listele

# Sonra bu sehirlerin buyume hizi vs ulkelerinin populasyonlari
# Sonra bu sehirlerin buyume hizi vs ulkelerinin populasyonlarinin buyume hizi
# Sonra bu sehirlerin buyume hizi vs ulkelerinin populasyonlarinin buyume hizi