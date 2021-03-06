library(jsonlite)
library(here)
library(dplyr)
library(ggplot2)
library(networkD3)
library(data.tree)

source(here("R", "get_comtrade.R"))
source(here("R", "utils.R"))
source(here("R", "make_sankey.R"))


# reporters <- read.csv(here("data", "reporters.csv"), stringsAsFactors = FALSE, header = TRUE)
# partners <- read.csv(here("data", "partners.csv"), stringsAsFactors = FALSE, header = TRUE)
# hscodes <- read.csv(here("data", "hscodes.csv"), stringsAsFactors = FALSE, header = TRUE)
regions <- read.csv(here("data", "UNSDregions.csv"), stringsAsFactors = FALSE, header = TRUE)
colnames(regions) <- dbSafeNames(colnames(regions))



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

test1 <- test %>% 
  select(reporter,
         reporter_iso,
         partner, 
         partner_iso,
         trade_value_us)

test2 <- test1 %>% 
  left_join(regions, by = c("partner_iso" = "iso_alpha3_code")) %>% 
  select(reporter,
         # reporter_iso,
         partner, 
         # partner_iso,
         global_name,
         region_name,
         # Sub.region.Name,
         trade_value_us)


colnames(test2) <- dbSafeNames(colnames(test2))


trade <- test2 %>% 
          filter(partner != "World")

trade$pathString <- paste(trade$global_name, trade$region_name, trade$sub_region_name, trade$partner, sep = "/")

tradenet <- as.Node(trade)

print(tradenet, "trade_value_us")


tradenet$Do(function(node) node$trade_value_us <- Aggregate(node, attribute = "trade_value_us", aggFun = sum), traversal = "post-order")


print(tradenet,  "trade_value_us")


tradenetdf <- ToDataFrameNetwork(tradenet, "trade_value_us")

simpleNetwork(tradenetdf, fontSize = 12)

sources <- tradenetdf %>%
            distinct(from) %>% 
            rename(label = from)

destinations <- tradenetdf %>% 
            distinct(to) %>% 
            rename(label = to)

nodes <- full_join(sources, destinations, by = "label") 
nodes <- nodes %>% 
          mutate(id = 0:(nrow(nodes) - 1)) %>% 
          select(id, everything())


edges <- tradenetdf %>% 
          rename(source = from, destination = to) %>% 
          left_join(nodes, by = c("source" = "label")) %>% 
          rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id) %>% 
  select(from, to, trade_value_us)

nodes <- nodes %>% 
          mutate(label = gsub(label, pattern = "World", replacement = "China"))





sankeyNetwork(Links = edges, Nodes = nodes, Source = "from", Target = "to", Value = "trade_value_us", NodeID = "label", fontSize = 12)

make_region_sankey(comtradedata = test, regiondata = regions)



