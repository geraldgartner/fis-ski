library(rvest)
library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)

result = NULL

for (i in seq(1969, 2016)) {
  main.page1 <- read_html(paste("http://data.fis-ski.com/global-links/all-fis-results.html?place_search=&sector_search=AL&date_search=&gender_search=&category_search=WC&codex_search=&nation_search=&disciplinecode_search=&date_from=01&search=Search&limit=1000&seasoncode_search=", i, sep = ""))
  
  tmp = main.page1 %>% # feed `main.page` to the next step
    html_nodes(".btn-r") %>% # get the CSS nodes
    html_attr("href") %>%
    as.data.frame()
  
  if(is.null(result)) {
    result = tmp
  } else {
    result = rbind(result,tmp)
  }}

result <- na.omit(result)





sub.page <- read_html(fis)

