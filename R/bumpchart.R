library(here)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

source(here("R", "get_comtrade.R"))
source(here("R", "utils.R"))


apidata <- get_comtrade(freq = "A",
                        ps = "2013,2014,2015,2016",
                        r = 156,
                        p = "all",
                        rg = 1,
                        cc = "220830,0406", 
                        fmt = "csv"
                        )

apidata <- apidata$data
colnames(apidata) <- dbSafeNames(colnames(apidata))  
apidata <- apidata %>% 
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

test <- apidata %>% 
        filter(partner_iso != "WLD") %>% 
        group_by(year, commodity_code) %>% 
        mutate(mktshareval = (trade_value_us/sum(trade_value_us) * 100),
               mktsharevalrank = min_rank(-mktshareval),
               mktsharevol = (netweight_kg/sum(netweight_kg) * 100),
               mktsharevolrank = min_rank(-mktsharevol),
               worldval = sum(trade_value_us),
               worldvol = sum(netweight_kg)) 
 
z <- test %>% 
      group_by(year) %>% 
      summarise(tot = n()) %>% 
      select(year) %>% 
     nrow()

z1 <- bumpchart(test)
 
# http://data-slinky.com/2016/07/31/bump_charts.html
summary <- test %>% 
           group_by(commodity_code, year) %>% 
           filter(mktsharevalrank <=5 | partner_iso == "GBR") %>% 
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

x <- ggplot(summary, aes(x = year, y = mktsharevalrank)) +
  geom_line(aes(colour = partner), size = 1.5) +
  geom_point(shape = 21, stroke = 2, size=5, fill = "white", aes(colour=partner)) +
  geom_label(data = summaryfinalyear, aes(x = year, y = mktsharevalrank, label = partner)) +
  geom_label(data = summaryfirstappearance, aes(x = year, y = mktsharevalrank, label = partner)) +
  scale_y_reverse(lim=c(5,1), breaks = scales::pretty_breaks(n = 5)) +
  facet_wrap( ~ commodity)


pal1 = c("#c57c3c", "#e392c2", "#a5e7a8", "#bea3ea", "#d7e298", "#81a4e3", "#a6b16a", "#a7baf2", "#e4c587", "#5ab6e6",
         "#d6a16d", "#62d9f3", "#eb9189", "#3ec1c8", "#e1a6b6", "#7fe3c5", "#e5b4e2", "#8bba83", "#cd5136", "#84bb9c",
         "#e1ceeb", "#72b7b0", "#cd9e8c", "#93e7e2", "#ecc0b1", "#7bb1c6", "#d8e8c5", "#acbadd", "#b2b593", "#acd8eb")

x <- ggplot(summary, aes(x = year, y = mktsharevalrank)) +
          geom_line(aes(colour = factor(partner)), size = 1.5) +
          geom_point(shape = 21, stroke = 2, size=5, fill = "white", aes(colour= factor(partner))) +
          geom_label(data = summaryfinalyear, aes(x = year, y = mktsharevalrank, fill = factor(partner), label = partner), colour = "white") +
          geom_label(data = summaryfirstappearance, aes(x = year, y = mktsharevalrank, fill = factor(partner), label = partner), colour = "white") +
          scale_y_reverse(lim=c(5,1), breaks = scales::pretty_breaks(n = 5)) +
          facet_wrap( ~ commodity, ncol = 1) +
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


summary2 <- summary %>% 
            melt(measure.vars = c("mktsharevalrank", "mktshareval"))



test2 <- test %>% 
         group_by(year, commodity_code) %>% 
         top_n(5, mktshareval) %>% 
         select(year, reporter, partner, commodity, trade_value_us, netweight_kg, mktshareval, mktsharevalrank, mktsharevol, mktsharevolrank, worldval, worldvol) %>% 
         arrange(year, commodity, mktsharevalrank) %>% 
         melt(measure.vars = c("trade_value_us", "netweight_kg", "mktshareval", "mktsharevalrank", "mktsharevol", "mktsharevolrank", "worldval", "worldvol"))




test <- wbinds %>%
        group_by(date, indicatorID) %>% 
        mutate(indrank = min_rank(-value)) %>%
        ungroup() %>% 
        group_by(iso3c, indicatorID) %>% 
        filter(date == max(date))
