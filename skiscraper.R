library(rvest)
library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)

result = NULL

#set to desired years
for (i in seq(1967, 2016)) {
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
colnames(result)[1] <- "url"


## SUBPAGES

scrapeSubpages <- function(x) {
  tabletest <- read_html(as.character(x)) %>% 
    html_node("table.calendar") %>%
    html_table(header = TRUE, fill = TRUE)
  tabletest$url = x
  tabletest
    }


subresults <- mapply(scrapeSubpages, result$url)

subresults <- do.call(cbind.data.frame, subresults)
subresults <- subset(subresults, select=-"Downloads")
#df <- data.frame(matrix(unlist(subresults), nrow=69, byrow=T),stringsAsFactors=FALSE)

#refine scrape table attempt

refine <- read_html("http://data.fis-ski.com/dynamic/event-details.html?event_id=2687&cal_suchsector=AL") %>% 
  html_node("table.calendar") %>%
  html_table(header = TRUE, fill = TRUE)
refine

write.csv(result, file = "result.csv")
