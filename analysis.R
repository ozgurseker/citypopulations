library(tidyverse)

df_global <- read_csv("UNdata_Export_20230215_190540160.csv") %>% filter(Sex == "Both Sexes") %>% select(-Sex)

df_country_pop <- df_global %>% group_by(Year, `Country or Area`) %>% summarise(population = sum(Value))

# ulke populasyonu buyume hizlari ekle
# sehir populasyonlari buyume hizi ekle

# istanbula benzer populasyona sahip sehirler belli yillarda onlari bi listele

# Sonra bu sehirlerin buyume hizi vs ulkelerinin populasyonlari
# Sonra bu sehirlerin buyume hizi vs ulkelerinin populasyonlarinin buyume hizi
# Sonra bu sehirlerin buyume hizi vs ulkelerinin populasyonlarinin buyume hizi