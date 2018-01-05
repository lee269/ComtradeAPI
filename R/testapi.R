library(jsonlite)
library(here)
library(dplyr)
library(ggplot2)
library(networkD3)

source(here("R", "get_comtrade.R"))
source(here("R", "utils.R"))
source(here("R", "make_sankey.R"))


reporters <- read.csv(here("data", "reporters.csv"), stringsAsFactors = FALSE, header = TRUE)
partners <- read.csv(here("data", "partners.csv"), stringsAsFactors = FALSE, header = TRUE)
hscodes <- read.csv(here("data", "hscodes.csv"), stringsAsFactors = FALSE, header = TRUE)
regions <- read.csv(here("data", "UNSDregions.csv"), stringsAsFactors = FALSE, header = TRUE)

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

test <- test %>% 
  select(classification, 
         year, 
         trade_flow,
         reporter,
         reporter_iso,
         partner,
         partner_iso,
         commodity_code,
         commodity,
         netweight_kg = netweight_kg_,
         trade_value_us = trade_value_us_)



make_sankey(comtradedata = test)


test1 <- test %>% 
          select(reporter,
                 reporter_iso,
                 partner, 
                 partner_iso,
                 trade_value_us)

test2 <- test1 %>% 
          left_join(regions, by = c("reporter_iso" = "ISO.alpha3.Code")) %>% 
          select(reporter,
                 # reporter_iso,
                 partner, 
                 # partner_iso,
                 Region.Name,  
                 trade_value_us)

g <- graph.data.frame(test2)

plot(g)

v <- as_data_frame(g, what = "vertices")
e <- as_data_frame(g, what = "edges")


world <- map_data("world")
world <- world[world$region != "Antarctica",] # intercourse antarctica
world <- fortify(world)

test <- test %>% 
        mutate_if(is.factor, as.character)

gg <- ggplot()
gg <- gg + geom_map(data=world, map=world,
                    aes(x=long, y=lat, map_id=region),
                    color="white", fill="#7f7f7f", size=0.05, alpha=1/4)

gg <- gg + geom_map(data=test, map = world, 
                      aes(fill = trade_value_us_, map_id = partner), 
                      size=0.15, alpha=1/100)
