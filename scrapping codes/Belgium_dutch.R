library(jsonlite)
library(tidyverse)
library(rvest)
library(parallel)


URLs <- paste0("https://search.vrt.be/advancedSearch?i=nws-en&q=coronavirus&from=", seq(from = 1, to = 1121, by = 10), "&tags=")

f.json <- function(URL) {
  tryCatch(fromJSON(URL)[["results"]][["externalUrl"]] , error = function(e) c(NA))
}

f.gettext <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = page %>% html_nodes(".dummy time") %>% html_text() %>% paste(collapse = " "),
      title = page %>% html_nodes(".vrt-title") %>% html_text() %>% paste(collapse = " "),
      URL = URL,
      text = page %>% 
        html_nodes('#main-content p') %>% 
        html_text() %>% 
        paste(collapse = " ")
    ) }
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

Belgium_dutch_rawtext <- parLapply(cl = cl, X = URLsToArticle, fun = f.gettext)
stopCluster(cl)

Belgium_dutch_rawtext <- reduce(Filter(f = Negate(is.null), Belgium_dutch_rawtext), rbind)

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Belgium_dutch_rawtext"), file = "Belgium_dutch_rawtext.RData")
