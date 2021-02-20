# Sun Feb 14 00:51:56 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

URLs <- paste0('https://www.rtvslo.si/zdravje/novi-koronavirus/arhiv/?&page=', 0:129) 

f.initial_df <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      URL = reduce(paste0("https://www.rtvslo.si", html_attr(html_nodes(page, '.md-news>a'), 'href')), c)
    )},
    error = function(e) NULL)
}

f.text <- function(URL) {
  tryCatch({x <- URL %>%
             read_html()
  data.frame(
    URL = URL,
    date = html_text(html_nodes(x, ".publish-meta")),
    title = html_text(html_nodes(x, "h1")),
    text = paste0(html_text(html_nodes(x, "#main-container .article p")), collapse = " ")
  )
  },
  error = function(e) NULL)
}

cl <- makeCluster(7)
clusterExport(cl, list("URLs", "f.initial_df"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(purrr))

initial_df <- parLapply(cl = cl, X = URLs, fun = f.initial_df)
initial_df <- reduce(Filter(f = Negate(is.null), initial_df), rbind)

clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(stringr))
clusterEvalQ(cl, library(purrr))
clusterExport(cl, list("f.text", "initial_df"), envir = environment())

articles <- parLapply(cl = cl, X = initial_df$URL, fun = f.text)
stopCluster(cl)

Slovenia_rawtext <- merge(initial_df, reduce(Filter(f = Negate(is.null), articles), rbind)) %>% 
  mutate(
    date = gsub(" ob .*", "", date),
    text = gsub(".*Rezultati iskanja", "", text),
    text = str_remove_all(text, '\"'),
    text = str_remove_all(text, '  ')
  )


setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Slovenia_rawtext"), file = "Slovenia_rawtext.RData")

