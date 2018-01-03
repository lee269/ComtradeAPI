library(jsonlite)
library(here)
library(dplyr)
library(ggplot2)
library(networkD3)
library(data.tree)
library(igraph)

source(here("R", "get_comtrade.R"))
source(here("R", "utils.R"))
source(here("R", "make_sankey.R"))


 reporters <- read.csv(here("data", "reporters.csv"), stringsAsFactors = FALSE, header = TRUE)
# partners <- read.csv(here("data", "partners.csv"), stringsAsFactors = FALSE, header = TRUE)
# hscodes <- read.csv(here("data", "hscodes.csv"), stringsAsFactors = FALSE, header = TRUE)
regions <- read.csv(here("data", "UNSDregions.csv"), stringsAsFactors = FALSE, header = TRUE)
colnames(regions) <- dbSafeNames(colnames(regions))


year <- 2014
reporter <- "826" # "156" # China
partner <- "all"
flow <- "all" #imports
comcode <- "07"


comtradedata <- get_comtrade(freq = "A",
                             ps = year,
                             r = reporter,
                             p = partner,
                             rg = flow,
                             cc = comcode,
                             fmt = "csv"
)

comtradedata <- comtradedata$data
colnames(comtradedata) <- dbSafeNames(colnames(comtradedata))
comtradedata <- comtradedata %>% 
  mutate_if(is.factor, as.character)

# merge these into one
temp <- comtradedata %>% 
  left_join(regions, by = c("partner_iso" = "iso_alpha3_code")) %>% 
  select(reporter,
         partner, 
         trade_flow,
         trade_value_us = trade_value_us_) %>% 
  filter(partner != "World")

imports <- temp %>% 
  filter(trade_flow == "Import") %>% 
  select(from = partner,
         to = reporter,
         trade_value_us,
         trade_flow)

exports <- temp %>% 
  filter(trade_flow == "Export") %>% 
  select(from = reporter,
         to = partner,
         trade_value_us,
         trade_flow)

tradenetdf <- imports %>%  
  bind_rows(exports)


sources <- tradenetdf %>%
  distinct(from, trade_flow) %>% 
  rename(label = from)

destinations <- tradenetdf %>% 
  distinct(to, trade_flow) %>% 
  rename(label = to)

nodes <- full_join(sources, destinations, by = c("label", "trade_flow")) 
nodes <- nodes %>% 
  mutate(id = 0:(nrow(nodes) - 1)) %>% 
  select(id, everything())


edges <- tradenetdf %>% 
  rename(source = from, destination = to) %>% 
  left_join(nodes, by = c("source" = "label", "trade_flow" = "trade_flow")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label", "trade_flow" = "trade_flow")) %>% 
  rename(to = id) %>% 
  select(from, to, trade_value_us, trade_flow)

  maincountry <- reporters %>% 
                  filter(id == reporter) %>% 
                  select(name) %>% 
                  as.character()

  xport <- nodes %>% 
        filter(label == maincountry, trade_flow == "Export") %>% 
        select(id) %>% 
        as.numeric()

  mport <- nodes %>% 
    filter(label == maincountry, trade_flow == "Import") %>% 
    select(id) %>% 
    as.numeric()
  
  edges <- edges %>% 
            mutate(from = replace(from, from==xport, mport))
  
  # nodes <- nodes %>% 
  #           filter(id != xport)
  
  
sankeyNetwork(Links = edges,
              Nodes = nodes,
              Source = "from",
              Target = "to",
              Value = "trade_value_us",
              NodeID = "label",
              fontSize = 12,
              LinkGroup = "trade_flow",
              NodeGroup = "trade_flow")

forceNetwork(Links = edges, Nodes = nodes, NodeID = "id", Group = "trade_flow")

g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)
plot(g)