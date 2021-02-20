# Fri Jan 29 02:08:41 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

URLs <- paste0('https://www.ruv.is/tag/covid-19/?page=', 1:2)



f.initial_df <- function(URL) {
  tryCatch({
  page <- read_html(URL)
  data.frame(
    date = reduce(html_text(html_nodes(page, '.pad0t-mobile')), c),
    title = reduce(html_text(html_nodes(page, '.big a')), c),
    URL = paste0("https://www.ruv.is", reduce(html_attr(html_nodes(page, '.big a'), 'href'), c))
  )},
  error = function(e) NULL)
}

f.text <- function(URL) {
  tryCatch(URL %>%
             read_html() %>%
             html_nodes("#mini-panel-body_and_author .even") %>%
             html_text %>%
             str_subset("Mynd:", T) %>% 
             str_replace_all("\n", " ") %>% 
             str_c(collapse = " ") %>% 
             str_remove_all("  ") %>% 
             {gsub(".*;\r", "", .)} %>% 
             {gsub("!function.*", "", .)}
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

Iceland_rawtext <- cbind(initial_df, reduce(articles, c)) %>% 
  set_names("date", "title", "URL", "text") %>% 
  filter(text != "") %>% 
  mutate(
    date = lubridate::dmy(gsub("-.*", "", date)),
    text = str_replace_all(text, "\n", " "), # clean text
    text = str_replace_all(text, "\t", " "),
    text = str_replace_all(text, '"', " "),
    text = str_replace_all(text, '  ', " ")
  )

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Iceland_rawtext"), file = "Iceland_rawtext.RData")

