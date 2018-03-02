library(wbstats)
library(here)
library(dplyr)
library(tidyr)

source(here("R", "utils.R"))

# get some comtrade data to play with
comtrade <- read.csv(here("data", "comtradesample.csv"), header = TRUE, stringsAsFactors = FALSE)

colnames(comtrade) <- dbSafeNames(colnames(comtrade))

comtrade <- comtrade %>% 
            filter(trade_flow == "Import") %>% 
            filter(reporter_iso != "")

c1 <- as.vector(comtrade$reporter_iso)


# A df of indicators to extract
indicators <- data.frame(
                        ind_id = c("SP.POP.TOTL", "NY.GDP.PCAP.PP.KD", "NE.IMP.GNFS.ZS"),
                        ind_text = c("Population", "GDP per capita PPP", "Imports of goods and services (% GDP)")
                        )

# Now to work out some metrics from the WB indicators

# https://stackoverflow.com/questions/30488389/using-dplyr-window-functions-to-calculate-percentiles
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





growth <- inddata %>% 
           select(iso3c, indicatorID, avggrowth) %>% 
           spread(key = indicatorID, value = avggrowth)

quartiles <- inddata %>% 
              select(iso3c, indicatorID, quartile) %>% 
              spread(key = indicatorID, value = quartile)

dtoutput <- inddata %>% 
            group_by(iso3c, start, end) %>%
            summarise(yrmin = min(start), yrmax = max(end)) %>% 
            ungroup() %>% 
            select(iso3c, yrmin, yrmax) %>% 
            left_join(growth) %>% 
            left_join(quartiles, by = "iso3c")
 


