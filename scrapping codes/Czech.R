library(jsonlite)
library(tidyverse)
library(rvest)
library(parallel)


URLs <- paste0("https://ct24.ceskatelevize.cz/taxonomy_sites/load_more/sections/block_5/703363/",1:322,"/-1?taxonomy_sites_theme=ct24")

f.json <- function(URL) {
  tryCatch(paste0("https://ct24.ceskatelevize.cz", 
                  html_attr(html_nodes(read_html(fromJSON(URL)$data), "aside>a"), "href")),
           error = function(e) c(NA))
}

f.gettext <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = page %>% html_nodes('.article-meta-value') %>% html_text() %>% paste(collapse = " "),
      title = page %>% html_nodes(".view-main") %>% html_text() %>% paste(collapse = " "),
      URL = URL,
      text = page %>% 
        html_nodes('.textcontent p') %>% 
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

Czech_rawtext <- parLapply(cl = cl, X = URLsToArticle, fun = f.gettext)
stopCluster(cl)

Czech_rawtext <- reduce(Filter(f = Negate(is.null), Czech_rawtext), rbind) %>% 
  mutate_all(function(x) {
    str_remove_all(x, "\r") %>% 
      str_remove_all("\n") %>% 
      str_remove_all("\t") %>% 
      str_remove_all("  ")
  } 
  )


setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Czech_rawtext"), file = "Czech_rawtext.RData")


