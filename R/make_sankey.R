make_sankey <- function (comtradedata) {    
    
    
    # Get rid of total trade and order by size
    sankey <- comtradedata %>% 
      filter(partner_iso != "WLD") %>% 
      select(reporter,
             partner,
             trade_value_us) %>% 
      arrange(desc(trade_value_us))
    
    # make links data
    sankey <- sankey %>% 
      mutate(source = 0, target = seq.int(nrow(sankey)), value = trade_value_us)
    
    links <- sankey %>% 
      select(reporter = source, partner = target, value) 
    
    #  make nodes data. node 0 is the reporter, the rest are all the partners
    r <- as.character(comtradedata$reporter[1])
    nodes <- as.vector(sankey$partner)
    nodes <- append(r, nodes)
    nodes <- data.frame("name" = nodes)
    
    flow <- as.character(comtradedata$trade_flow[1])
    if (flow == "Import") {
      source = "partner"
      target = "reporter"
    }

    if (flow == "Export") {
      source = "reporter"
      target = "partner"
    }
        
    x <- sankeyNetwork(Links = links, Nodes = nodes,
                       Source = source, Target = target,
                       Value = "value", NodeID = "name",
                       fontSize= 12, nodeWidth = 30, units = "$", fontFamily = "Helvetica")
    
    x

}  


make_region_sankey <- function(comtradedata, regiondata) {
  require(data.tree)
  require(dplyr)
  require(networkD3)
  
  # uses the example here
  # http://www.sthda.com/english/articles/33-social-network-analysis/135-network-visualization-essentials-in-r/
  
  colnames(regiondata) <- dbSafeNames(colnames(regiondata))
  
  flow <- as.character(comtradedata$trade_flow[1])
  if (flow == "Import") {
    source = "to"
    target = "from"
  }
  
  if (flow == "Export") {
    source = "from"
    target = "to"
  }
  
  
  comtradedata <- comtradedata %>% 
    left_join(regiondata, by = c("partner_iso" = "iso_alpha3_code")) %>% 
    select(reporter,
           # reporter_iso,
           partner, 
           # partner_iso,
           global_name,
           region_name,
           # Sub.region.Name,
           trade_value_us)
  
  
  colnames(comtradedata) <- dbSafeNames(colnames(comtradedata))
  
  reportercountry <- as.character(comtradedata$reporter[1])
  
  trade <- comtradedata %>% 
    filter(partner != "World")
  
  trade$pathString <- paste(trade$global_name, trade$region_name, trade$sub_region_name, trade$partner, sep = "/")
  
  tradenet <- as.Node(trade)
  
  tradenet$Do(function(node) node$trade_value_us <- Aggregate(node, attribute = "trade_value_us", aggFun = sum), traversal = "post-order")
  
  tradenetdf <- ToDataFrameNetwork(tradenet, "trade_value_us")
  
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
    mutate(label = gsub(label, pattern = "World", replacement = reportercountry))
  
  
  x <- sankeyNetwork(Links = edges, Nodes = nodes, Source = source, Target = target, Value = "trade_value_us", NodeID = "label", fontSize = 12)
  
  x
  
}

