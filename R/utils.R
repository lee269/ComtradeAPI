# make names db safe: no '.' or other illegal characters,
# all lower case and unique
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}


# http://stackoverflow.com/questions/28159936/formatting-large-currency-or-dollar-values-to-millions-billions
money <-  function(x) {
  z <- abs(x)
  
  if (is.na(z)) {return(0)}
  
  b.index <-  z >= 1e9
  m.index <-  z >= 1e5 & z < 1e9
  
  if (x > 0) {
    output <-  paste("£", formatC(z, format = "d", big.mark = ","), sep = "")
    output[b.index] <-  paste("£", formatC(z[b.index] / 1e9, digits = 1, format = "f"), "bn", sep = "")
    output[m.index] <-  paste("£", formatC(x[m.index] / 1e6, digits = 1, format = "f"), "m", sep = "")
    return(output)
  } else {
    output <-  paste("-£", formatC(z, format = "d", big.mark = ","), sep = "")
    output[b.index] <-  paste("-£", formatC(z[b.index] / 1e9, digits = 1, format = "f"), "bn", sep = "")
    output[m.index] <-  paste("-£", formatC(z[m.index] / 1e6, digits = 1, format = "f"), "m", sep = "")
    return(output)
  }
}


bumpchart <- function(data, top_n = 5) {
  
  # http://data-slinky.com/2016/07/31/bump_charts.html
  summary <- data %>% 
    group_by(commodity_code, year) %>% 
    filter(mktsharevalrank <= top_n | partner_iso == "GBR") %>% 
    arrange(year, commodity_code, mktsharevalrank) %>% 
    select(year, commodity_code, commodity, partner, mktsharevalrank, mktshareval) 
  
  summaryfinalyear <- summary %>% 
    group_by(partner) %>% 
    filter(year == max(year)) %>% 
    ungroup()
  
  
  summaryfirstappearance <- summary %>%
    group_by(partner) %>% 
    filter(year == min(year)) %>% 
    ungroup()
  
  nyears <- data %>% 
            group_by(year) %>% 
            summarise(tot = n()) %>% 
            select(year) %>% 
            nrow()
         
  x <- ggplot(summary, aes(x = year, y = mktsharevalrank)) +
    geom_line(aes(colour = factor(partner)), size = 1.5) +
    geom_point(shape = 21, stroke = 2, size = 5, fill = "white", aes(colour = factor(partner))) +
    geom_label(data = summaryfinalyear, aes(x = year, y = mktsharevalrank, fill = factor(partner), label = partner), colour = "white") +
    geom_label(data = summaryfirstappearance, aes(x = year, y = mktsharevalrank, fill = factor(partner), label = partner), colour = "white") +
    scale_y_reverse(lim = c(top_n,1), breaks = scales::pretty_breaks(n = top_n)) +
    facet_wrap( ~ commodity) +
    ggtitle('Market share Ranking') +
    xlab(NULL) +
    ylab("Rank") +
    theme_minimal() +
    theme_bw() +
    # scale_colour_manual(values=pal1) +
    theme(panel.background = element_rect(fill = '#ffffff'),
          plot.title = element_text(size=14), legend.title=element_blank(),
          axis.text = element_text(size=11), axis.title=element_text(size=11), 
          panel.border = element_blank(), legend.position='none', 
          panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
          axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
  
   
  
}

arrowimg <- function(val, low, high) {
  if(val <= low) {
    return('<img src="www/downarrowgrey.png" height="25"></img>')
  }
  if (val >= high) {
    return('<img src="www/uparrowgrey.png" height="25"></img>')
  }
  return('<img src="www/levelarrowgrey.png" height="25"></img>')
}
