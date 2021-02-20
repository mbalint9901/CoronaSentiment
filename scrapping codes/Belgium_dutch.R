library(tidyverse)
library(rvest)
library(parallel)

# links.raw <-  
  read_html('https://www.vrt.be/vrtnws/en/search/?query=coronavirus') %>% 
    html_nodes('.vrt-teaser-v2__content') %>% 
    html_attr()
