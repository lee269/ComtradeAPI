library(here)
library(rvest)
library(stringr)

url <- "https://en.wikipedia.org/wiki/Gallery_of_sovereign_state_flags"

links <- url %>%
  read_html %>% 
  html_nodes("td td a") %>% 
  html_attr("href")

inc <- seq(1, 618, by = 3)

flags1 <- links[inc]

flags1 <- data.frame(flags1) %>% 
          mutate(urlsvg = paste("https://commons.wikimedia.org", flags1, sep = "")) %>% 
          select(urlsvg)

inc <- seq(3, 618, by = 3)

ctry <- links[inc]

ctry <- data.frame(ctry) %>% 
        mutate(country = str_replace(string = ctry, pattern = "/wiki/", replacement = "")) %>% 
        select(country)
     
countryurls <- ctry %>% 
                bind_cols(flags1)

# need to purrr this
test <- "https://commons.wikimedia.org/wiki/File:Flag_of_Nigeria.svg"

x <- test %>%
      read_html %>% 
      html_nodes(".mw-thumbnail-link , .mw-filepage-resolutioninfo") %>% 
      html_attr("href")

png <- x[2]
   
