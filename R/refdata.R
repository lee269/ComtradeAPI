library(jsonlite)
library(here)


# Read in reference tables from Comtrade
# Reporter countries table - reporters
string <- "http://comtrade.un.org/data/cache/reporterAreas.json"
reporters <- fromJSON(txt = string)
reporters <- as.data.frame(sapply(reporters$results,rbind))
names(reporters) <- c("id", "name")
reporters$id <- as.character(reporters$id)
reporters$name <- as.character(reporters$name)


# Partner countries table - partners  
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
partners <- fromJSON(txt = string)
partners <- as.data.frame(sapply(partners$results,rbind))
names(partners) <- c("id", "name")
partners$id <- as.character(partners$id)
partners$name <- as.character(partners$name)

# Comcodes table - hscodes
string <- "http://comtrade.un.org/data/cache/classificationHS.json"
hscodes <- fromJSON(txt = string)
hscodes <- as.data.frame(sapply(hscodes$results, rbind))
names(hscodes) <- c("id", "text", "parent")
hscodes$id <- as.character(hscodes$id)
hscodes$text <- as.character(hscodes$text)
hscodes$parent <- as.character((hscodes$parent))

# Flows table
string <- "https://comtrade.un.org/data/cache/tradeRegimes.json"
flows <- fromJSON(txt = string)
flows <- as.data.frame(sapply(flows$results, rbind))
names(flows) <- c("id", "text")
flows$id <- as.character(flows$id)
flows$text <- as.character(flows$text)

write.csv(reporters, here("data", "reporters.csv"), row.names = FALSE)
write.csv(partners, here("data", "partners.csv"), row.names = FALSE)
write.csv(hscodes, here("data", "hscodes.csv"), row.names = FALSE)
write.csv(flows, here("data", "flows.csv"), row.names = FALSE)
