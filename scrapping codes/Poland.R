library(tidyverse)
library(rvest)
library(parallel)

Poland_rawtext <- readxl::read_excel("scrapping RData/poland_content.xlsx") %>% 
  mutate(
    date = lubridate::ymd(date)
  )

Poland_rawtext[2, ] %>% pull() %>% 
  read_html() %>% 
  html_nodes("b font , b+ font font , .article__width font , .article-layout h2 font , br+ font font") %>% 
  html_text() %>% 
  paste(collapse = " ")
