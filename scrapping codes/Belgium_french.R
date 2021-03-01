# Sat Feb 20 20:31:57 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

URLs <- paste0('https://www.rtbf.be/info/mot-cle_coronavirus?page=', 1:137, "&keyword=1077522")

f.initial_df <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      URL = reduce(html_attr(html_nodes(page, '.rtbf-article-grid__title-item a, .rtbf-article-grid__container>.rtbf-article-grid__item>.clearfix'), 'href'), c)
    )},
    error = function(e) NULL)
}

f.text <- function(URL) {
  tryCatch({page <- read_html(URL) 
  data.frame(
    date =  page %>% # date
      html_nodes(".rtbf-article-main__author div") %>% 
      html_text(),
    
    title = page %>% # title
      html_nodes(".www-col-full-width") %>% 
      html_text() %>% 
      .[2],
    
    URL = URL,
    text = page %>% # text
      html_nodes(".rtbf-paragraph > p") %>% 
      html_text() %>%
      str_c(collapse = " ")
  )
  
  }
  ,
  error = function(e) NA)
}

cl <- makeCluster(7)
clusterExport(cl, list("URLs", "f.initial_df"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(purrr))

initial_df <- parLapply(cl = cl, X = URLs, fun = f.initial_df)
initial_df <- reduce(Filter(f = Negate(is.null), initial_df), rbind)

clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(stringr))
clusterExport(cl, list("f.text", "initial_df"), envir = environment())

articles <- parLapply(cl = cl, X = initial_df$URL, fun = f.text)
stopCluster(cl)

Belgium_french_rawtext <- articles %>% 
  reduce(rbind) %>% 
  filter(text != "") %>% 
  mutate(
    title = str_remove_all(title, "\n"),
    title = str_remove_all(title, "\t"),
    text = str_replace_all(text, "\n", " "), 
    text = str_replace_all(text, "\t", " "),
    text = str_replace_all(text, '"', " "),
    text = str_remove_all(text, '  '),
    date = str_remove_all(date, '  '),
    title = str_remove_all(title, '  ')
  )

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Belgium_french_rawtext"), file = "Belgium_french_rawtext.RData")

