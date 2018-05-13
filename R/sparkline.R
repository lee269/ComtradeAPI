library(dplyr)
library(tidyr)
library(DT)
library(sparkline)

# https://leonawicz.github.io/HtmlWidgetExamples/ex_dt_sparkline.html

js <- "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"
colDefs1 <- list(list(targets = c(3), render = JS(js)))
colDefs2 <- list(list(targets = c(1:6), render = JS(js)))


bar_string <- "type: 'bar', barColor: 'orange', negBarColor: 'purple', highlightColor: 'black'"
cb_bar <- JS(paste0("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ", 
                    bar_string, " }); }"), collapse = "")


sl <- wbinds %>% 
      arrange(iso3c, ind_short_text, date) %>% 
      group_by(iso3c, ind_short_text) %>% 
      summarise(Val = paste(value, collapse = ","))

sldt <- datatable(sl, list(columnDefs = colDefs1, 
                           fnDrawCallback = cb_bar))

sldt$dependencies <- append(sldt$dependencies, htmlwidgets:::getDependency("sparkline"))
sldt