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
  left_join(regions, by = c("partner_iso" = "ISO.alpha3.Code")) %>% 
  select(reporter,
         # reporter_iso,
         partner, 
         # partner_iso,
         Global.Name,
         Region.Name,
         Sub.region.Name,
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
treeNet

tnet <- graph_from_data_frame(tradenetdf)

plot(tnet)

v <- as_data_frame(tnet, what = "vertices")
e <- as_data_frame(tnet, what = "edges")


sankeyNetwork(Links = e, Nodes = v, Source = "from", Target = "to", Value = "trade_value_us", NodeID = "name")





