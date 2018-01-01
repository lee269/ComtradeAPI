library(here)
library(dplyr)
library(ggplot2)
library(networkD3)

source(here("R", "get_comtrade.R"))
source(here("R", "utils.R"))

reporters <- read.csv(here("data", "reporters.csv"), stringsAsFactors = FALSE, header = TRUE)
partners <- read.csv(here("data", "partners.csv"), stringsAsFactors = FALSE, header = TRUE)
hscodes <- read.csv(here("data", "hscodes.csv"), stringsAsFactors = FALSE, header = TRUE)

hscodes <- hscodes %>% 
          filter(nchar(id) == 4)


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

world <- test1 %>% 
          filter(partner == "World") %>% 
          select(trade_value_us) %>% 
          as.numeric()

mktshare <- test1 %>% 
            filter(partner != "World") %>% 
            mutate(mktshare = (trade_value_us/world) * 100) %>% 
            arrange(desc(mktshare))

cht <- ggplot(mktshare, aes(x = year, y = mktshare, fill = partner)) +
        geom_bar(stat = "identity") +
        scale_x_discrete(year)

cht


sankey <- test %>% 
          filter(partner_iso != "WLD") %>% 
          select(reporter_iso,
                 partner_iso,
                 trade_value_us = trade_value_us_) %>% 
          arrange(desc(trade_value_us)) %>% 
          mutate(source = 0, target =seq.int(nrow(sankey)), value = trade_value_us)

nodes <- as.vector(sankey$partner_iso)
nodes <- append("CHN", nodes)
nodes <- data.frame("name" = nodes)

links <- sankey %>% 
        select(source, target, value) 

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "target", Target = "source",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)