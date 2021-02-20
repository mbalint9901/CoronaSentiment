library(tidyverse)
library(rvest)


page_base <- "https://www.theguardian.com/world/coronavirus-outbreak+uk/uk?page="

lapply(1:5, function(page_number){
  page <- paste0(page_base, page_number) %>% # URL to read
    read_html()
  data.frame(
    time = html_text(html_nodes(page, ".fc-date-headline"))[1],
    title = html_text(html_nodes(page, ".js-headline-text"))
  )
}) %>% 
  reduce(rbind) %>% 
  filter(!duplicated(title))