# Harvest some links to pictures of country flags, so we can use them in
# dashboards

library(here)
library(rvest)
library(stringr)
library(purrr)

# page with a list of countries nad flags
url <- "https://en.wikipedia.org/wiki/Gallery_of_sovereign_state_flags"

# returns triplets of links
links <- url %>%
  read_html %>% 
  html_nodes("td td a") %>% 
  html_attr("href")

# every third item is a link to an svg page, starting with the first
inc <- seq(1, 618, by = 3)

# turn the list into a dataframe of svg links
flagpages <- links[inc]

flagpages <- data.frame(flagpages) %>% 
          mutate(urlsvg = paste("https://commons.wikimedia.org", flagpages, sep = "")) %>% 
          select(urlsvg)

# the third item is a stub to a country page. We use this to get country names
inc <- seq(3, 618, by = 3)

countries <- links[inc]

countries <- data.frame(countries) %>% 
        mutate(country = str_replace(string = countries, pattern = "/wiki/", replacement = "")) %>% 
        select(country)

# no we have a dataframe of country names and svg urls
countryurls <- countries %>% 
                bind_cols(flagpages)


# But the svg links go to a page not a file, so we need to loop through them and
# pick out some links to specific png images
urls <- countryurls$urlsvg %>% 
        as.list()

html <- urls %>% map(read_html)

# There are multiple links to different sized png files so we get more than one
# for each country. The last group and summarise bit just takes the first url to
# a png file (usually the smallest one)
flagpng <- html %>% 
            map(html_nodes, ".mw-thumbnail-link , .mw-filepage-resolutioninfo") %>% 
            map_df(map_df, ~list(var = .x %>%  html_attr("href")), .id = "id") %>% 
            mutate(id = as.numeric(id)) %>% 
            rename(png = var) %>% 
            filter(!is.na(png)) %>%
            group_by(id) %>% 
            summarise(png = head(png, 1))

# The final dataframe. We will need to do some further text cleaning and find a
# way to get iso country codes so we can join it to other datasets
flags <- countryurls %>% bind_cols(flagpng)

write.csv(flags, here("data", "flags.csv"))

