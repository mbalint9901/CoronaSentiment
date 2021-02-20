# Sat Feb 13 14:50:31 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

URLs <- paste0('https://politiken.dk/search/?q=covid&target=pol&page=', 1:186, "/") 

f.initial_df <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = reduce(html_text(html_nodes(page, '.time')), c),
      title = reduce(html_text(html_nodes(page, '.article-intro__title')), c),
      URL = paste0('https://politiken.dk/search/', 
                   reduce(html_attr(html_nodes(page, '.search-result__article>a'), 'href'), c))
    )},
    error = function(e) NULL)
}

f.text <- function(URL) {
  tryCatch(URL %>%
             read_html() %>%
             html_nodes(".body__p") %>%
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

Denmark_rawtext <- cbind(initial_df, reduce(articles, c)) %>% 
  set_names("date", "title", "URL", "text") %>% 
  filter(text != "") %>% 
  mutate(
    date = gsub(" kl.*", "", date),
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
save(list = c("Denmark_rawtext"), file = "Denmark_rawtext.RData")