library(jsonlite)
library(tidyverse)
library(rvest)
library(parallel)


URLs <- paste0("https://www.lrt.lt/api/category/get?page=", 1:116, "&after=2021.01.31%2006%3A42&after_id=1329964&count=100&block_pre_tags=%3Cdiv%20class%3D%22col%22%3E%3Cdiv%20class%3D%22news%20news--xs%20news--xs-md%20news--sm-sm%20news--md-md%22%3E&slug=koronavirusas")

f.json <- function(URL) {
  tryCatch(paste0("https://www.lrt.lt", fromJSON(URL)[["articles"]][["url"]]) , error = function(e) c(NA))
}

f.gettext <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    df <- data.frame(
      date = page %>% html_nodes(".published__body span") %>% html_text() %>% paste(collapse = " "),
      title = page %>% html_nodes(".title-block__heading") %>% html_text() %>% paste(collapse = " "),
      URL = URL,
      text = page %>% 
        html_nodes('p') %>% 
        html_text() %>% 
        paste(collapse = " ")
    )
    if (df[1, 1] == "") {
      df[1, 1] = page %>% html_nodes(".info-block__category") %>% html_text() %>% paste(collapse = " ")
    } 
    df
    }
    ,
    error = function(e) NULL)
  
}

cl <- makeCluster(7)
clusterExport(cl, list("URLs", "f.json"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(jsonlite))
clusterEvalQ(cl, library(purrr))
clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(stringr))

URLsToArticle <- parLapply(cl = cl, X = URLs, fun = f.json)

URLsToArticle <- reduce(URLsToArticle, c)

URLsToArticle <- na.omit(URLsToArticle)


clusterExport(cl, list("f.gettext", "URLsToArticle"), envir = environment())

Lithuania_rawtext <- parLapply(cl = cl, X = URLsToArticle, fun = f.gettext)
stopCluster(cl)

Lithuania_rawtext <- reduce(Filter(f = Negate(is.null), Lithuania_rawtext), rbind)  %>% 
  mutate_all(function(x) {
    str_remove_all(x, "\r") %>% 
      str_remove_all("\n") %>% 
      str_remove_all("\t") %>% 
      str_remove_all("  ")
  } 
  ) %>% 
  mutate(
    date = gsub(" .*", "", date),
    date = lubridate::ymd(date)
    ) %>% 
  filter(!is.na(date))
  

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Lithuania_rawtext"), file = "Lithuania_rawtext.RData")
