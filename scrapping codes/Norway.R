

library(tidyverse)
library(rvest)
library(parallel)

URLs <- paste0('https://www.nrk.no/sok/?q=koronavirus&scope=nrkno&from=',
               seq(from = 0, to = 3320, by = 20))

f.initial_df <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = reduce(html_text(html_nodes(page, '.nrkno-search-hit__details')), c),
      title = reduce(html_text(html_nodes(page, '.nrkno-search-hit__title')), c),
      URL = reduce(html_attr(html_nodes(page, '.nrkno-search-hit__link'), 'href'), c)
    )},
    error = function(e) NULL)
}

f.text <- function(URL) {
  tryCatch(URL %>%
             read_html() %>%
             html_nodes(".article-body>p, .article-body>ul, .text-body>p") %>%
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

Norway_rawtext <- cbind(initial_df, reduce(articles, c)) %>% 
  set_names("date", "title", "URL", "text") %>% 
  filter(text != "") %>% 
  mutate(
    date = lubridate::dmy(gsub("–.*", "", date)),
    title = str_remove_all(title, "\n"),
    title = str_remove_all(title, "\t"),
    text = str_replace_all(text, "\n", " "), # clean text
    text = str_replace_all(text, "\t", " "),
    text = str_replace_all(text, '"', " "),
    text = str_replace_all(text, '  ', " ")
  )

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Norway_rawtext"), file = "Norway_rawtext.RData")