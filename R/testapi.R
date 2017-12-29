library(here)
library(dplyr)

source(here("R", "get_comtrade.R"))
source(here("R", "utils.R"))

reporters <- read.csv(here("data", "reporters.csv"), stringsAsFactors = TRUE, header = TRUE)
partners <- read.csv(here("data", "partners.csv"), stringsAsFactors = TRUE, header = TRUE)
hscodes <- read.csv(here("data", "hscodes.csv"), stringsAsFactors = TRUE, header = TRUE)




year <- 2014
reporter <- "156" # China
partner <- "all"
flow <- "1" #imports
comcode <- "220830"


test <- get_comtrade(freq = "A",
                     ps = year,
                     r = reporter,
                     p = partner,
                     rg = flow,
                     cc = comcode,
                     fmt = "csv"
                     )

test <- test$data
colnames(test) <- dbSafeNames(colnames(test))

test1 <- test %>% 
        select(classification, 
               year, 
               trade_flow,
               reporter,
               partner,
               commodity_code,
               commodity,
               netweight_kg = netweight_kg_,
               trade_value_us = trade_value_us_)