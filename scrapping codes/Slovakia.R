# Fri Feb 19 19:53:48 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

URLs <- paste0('https://www.cas.sk/tema/koronavirus/', 1:820, "/")

f.initial_df <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = reduce(html_text(html_nodes(page, '.info-wrapper>.day')), c),
      title = reduce(html_text(html_nodes(page, '.article-text>a>.title')), c),
      URL = reduce(html_attr(html_nodes(page, '.article-text>a'), 'href'), c)
    )},
    error = function(e) NULL)
}

f.text <- function(URL) {
  tryCatch(URL %>%
             read_html() %>%
             html_nodes(".fulltext>p") %>%
             html_text %>%
             str_c(collapse = " ")
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

Slovakia_rawtext <- cbind(initial_df, reduce(articles, c)) %>% 
  set_names("date", "title", "URL", "text") %>% 
  filter(text != "") %>% 
  mutate(
    date = gsub(" .*", "", date),
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
save(list = c("Slovakia_rawtext"), file = "Slovakia_rawtext.RData")

