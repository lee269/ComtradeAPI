library(wbstats)
library(here)
library(dplyr)

source(here("R", "utils.R"))

comtrade <- read.csv(here("data", "comtradesample.csv"), header = TRUE, stringsAsFactors = FALSE)

colnames(comtrade) <- dbSafeNames(colnames(comtrade))

comtrade <- comtrade %>% 
            filter(trade_flow == "Import") %>% 
            filter(reporter_iso != "")

c1 <- as.vector(comtrade$reporter_iso)

indicators <- data.frame(
                        ind_id = c("SP.POP.TOTL", "NY.GDP.PCAP.PP.KD", "NE.IMP.GNFS.ZS"),
                        ind_text = c("Population", "GDP per capita PPP", "Imports of goods and services (% GDP)")
                        )

pop_data <- wb(indicator = "SP.POP.TOTL", mrv = 10, freq = "Y", country = c1)

# https://stackoverflow.com/questions/30488389/using-dplyr-window-functions-to-calculate-percentiles

pop_growth <- pop_data %>% 
              arrange(iso3c, date) %>% 
              group_by(iso3c) %>% 
              mutate(growth = (value / lag(value) * 100) - 100) %>% 
              filter(!is.na(growth)) %>%
              summarise(avggrowth = mean(growth),
                        start = min(date),
                        end = max(date)) %>% 
              mutate(quartile = ntile(avggrowth, 4))

inds <- as.vector(indicators$ind_id)

wbinds <- wb(indicator = inds, mrv = 10, freq = "Y", country = c1)

inddata <- wbinds %>% 
           arrange(iso3c, indicatorID, date) %>% 
           group_by(iso3c, indicatorID) %>% 
           mutate(growth = (value / lag(value) * 100) - 100) %>% 
           filter(!is.na(growth)) %>% 
           summarise(avggrowth = mean(growth),
                      start = min(date),
                      end = max(date)) %>% 
           ungroup() %>%
           group_by(indicatorID) %>%
           mutate(quartile = ntile(avggrowth, 3))



