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