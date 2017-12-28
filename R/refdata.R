# Read in reference tables from Comtrade
# Reporter countries table - reporters

string <- "http://comtrade.un.org/data/cache/reporterAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))
names(reporters) <- c("id", "name")
reporters$id <- as.character(reporters$id)
reporters$name <- as.character(reporters$name)

if(dbExistsTable(comtrade, "reporters")) {
  dbRemoveTable(comtrade, "reporters")
  dbWriteTable(comtrade,'reporters', reporters, row.names = FALSE)
} else {
  dbWriteTable(comtrade,'reporters', reporters, row.names = FALSE)
}

# Partner countries table - partners  

string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
partners <- fromJSON(file=string)
partners <- as.data.frame(t(sapply(partners$results,rbind)))
names(partners) <- c("id", "name")
partners$id <- as.character(partners$id)
partners$name <- as.character(partners$name)



# Comcodes table - hscodes

string <- "http://comtrade.un.org/data/cache/classificationHS.json"
hscodes <- fromJSON(file = string)
hscodes <- as.data.frame(t(sapply(hscodes$results, rbind)))
names(hscodes) <- c("id", "text", "parent")
hscodes$id <- as.character(hscodes$id)
hscodes$text <- as.character(hscodes$text)
hscodes$parent <- as.character((hscodes$parent))


