library(here)
library(dplyr)
library(purrr)
library(stringr)

source(here("R", "utils.R"))

# flags table made by flags.R
flags <- read.csv(here("data", "flags.csv"), header = TRUE, stringsAsFactors = FALSE)

flags <- flags %>% 
         mutate(country = str_replace_all(country, "_", " "),
                country = str_trim(country),
                country = str_replace_all(country, "Kingdom of the Netherlands", "Netherlands"),
                country = str_replace_all(country, "The Bahamas", "Bahamas"),
                country = str_replace_all(country, "Bolivia", "Bolivia (Plurinational state of)"),
                country = str_replace_all(country, "Bosnia and Herzegovina", "Bosnia Herzegovina"),
                country = str_replace_all(country, "Solomon Islands", "Solomon Isds"),
                country = str_replace_all(country, "Cape Verde", "Cabo Verde"),
                country = str_replace_all(country, "Central African Republic", "Central African Rep."),
                country = str_replace_all(country, "Republic of the Congo", "Congo"),
                country = str_replace_all(country, "Cook Islands", "Cook Isds"),
                country = str_replace_all(country, "Czech Republic", "Czechia"),
                country = str_replace_all(country, "Dominican Republic", "Dominican Rep."),
                country = str_replace_all(country, "Georgia (country)", "Georgia"),
                country = str_replace_all(country, "The Gambia", "Gambia"),
                country = str_replace_all(country, "Ivory Coast", "Côte d'Ivoire"),
                country = str_replace_all(country, "Republic of Ireland", "Ireland"),
                country = str_replace_all(country, "Laos", "Lao People's Dem. Rep."),
                country = str_replace_all(country, "Molvdova", "Rep. of Moldova"),
                country = str_replace_all(country, "Federated States of Micronesia", "FS Micronesia"),
                country = str_replace_all(country, "Republic of Macedonia", "TFYR of Macedonia"),
                country = str_replace_all(country, "Russia", "Russian Federation"),
                country = str_replace_all(country, "S%C3%A3o Tom%C3%A9 and Pr%C3%ADncipe", "Sao Tome and Principe"),
                country = str_replace_all(country, "Sudan", "Fmr Sudan"),
                country = str_replace_all(country, "Tanzania", "United Rep. of Tanzania"),
                country = str_replace_all(country, "United States", "USA"),
                country = str_replace_all(country, "Vietnam", "Viet Nam"),
                )

# CEPII country metadata
cepii <- read.csv(here("data", "geo_cepii.csv"), header = TRUE, stringsAsFactors = FALSE)

# there are mutiple entries for some countries so we will only pick the one that refers to the capital
cepii <- cepii %>% 
         filter(cap == 1)

comtrade <- read.csv(here("data", "comtrade-5.csv"), header = TRUE, stringsAsFactors = FALSE)
# api call to get total trade for all countries in 2010 - picked an old year to
# try and maximise the number of countries we get back
url <- "http://comtrade.un.org/api/get?max=500&type=C&freq=A&px=HS&ps=2010&r=all&p=0&rg=all&cc=TOTAL&fmt=csv"

#colClasses stops R from converting eg code 0106 to 106
coldatatypes <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "character", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

comtrade <- read.csv(url, header = TRUE, colClasses = coldatatypes, stringsAsFactors = FALSE) 

colnames(comtrade) <- dbSafeNames(colnames(comtrade))

comtrade <- comtrade %>% 
            group_by(reporter_code, reporter, reporter_iso) %>% 
            summarise(count = n()) %>% 
            select(reporter, reporter_code, reporter_iso)  
            

countries <- comtrade %>%
             left_join(cepii, by = c("reporter_iso" = "iso3")) %>% 
             select(reporter_code, reporter, reporter_iso, iso2, country, continent, lat, lon, city_en)

countries <- countries %>% 
             left_join(flags, by = c("reporter" = "country")) %>% 
             select(reporter_code, reporter, reporter_iso, iso2, country, continent, lat, lon, city_en, urlsvg, png)

write.csv(countries, here("data", "countries.csv"))



