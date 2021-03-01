library(jsonlite)
library(tidyverse)
library(rvest)
library(parallel)



URLs <- paste0("https://hirado.hu/wp-content/plugins/hirado.hu.widgets/widgets/newSearch/ajax_loadmore.php?s=koronav%C3%ADrus&media_type=article&page_number=", 1:543, "&gallery_slug=galeriak&video_slug=videok&video_category=431&elasticPage=-1&elasticPostPage=-1&allVideos=false&galleriesNum=0&videosNum=0") %>%  
  .[1:10]

f.json <- function(URL) {
  tryCatch(fromJSON(URL)$link, error = function(e) c(NA))
}

f.gettext <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = page %>% html_nodes("#bodywrapper > .catBanner .artTime") %>% html_text() %>% paste(collapse = " "),
      title = page %>% html_nodes("#bodywrapper > .catBanner h1") %>% html_text() %>% paste(collapse = " "),
      URL = URL,
      text = page %>% 
        html_nodes('#bodywrapper > .gridFix p') %>% 
        html_text() %>% 
        paste(collapse = " ")
    ) }
    ,
    error = function(e) NULL)
}

URLsToArticle <- vector()

for (i in seq_along(URLs)) {
 URLsToArticle <- c(URLsToArticle, f.json(URLs[i]))
 print(i)
}

URLsToArticle <- na.omit(URLsToArticle) 
print(URLsToArticle)

Hungary_rawtext <- f.gettext(URL[1])
for (i in 2:length(URLsToArticle)) {
  Hungary_rawtext <- rbind(f.gettext(URL[i]))
  if (i %/% 10 == 0) { # check
    print(tail(Hungary_rawtext)); print(tibble(Hungary_rawtext))
  }
}

# cl <- makeCluster(7)
# 
# clusterEvalQ(cl, library(rvest))
# clusterEvalQ(cl, library(jsonlite))
# clusterEvalQ(cl, library(purrr))
# clusterEvalQ(cl, library(magrittr))
# clusterEvalQ(cl, library(stringr))
# 
# 
# clusterExport(cl, list("f.gettext", "URLsToArticle"), envir = environment())

# Hungary_rawtext <- parLapply(cl = cl, X = URLsToArticle, fun = f.gettext)
# stopCluster(cl)

Hungary_rawtext <- reduce(Filter(f = Negate(is.null), Hungary_rawtext), rbind) %>% 
  mutate_all(function(x) {
    str_remove_all(x, "\r") %>% 
    str_remove_all("\n") %>% 
    str_remove_all("\t") %>% 
    str_remove_all("  ")
  } 
  )

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Hungary_rawtext"), file = "Hungary_rawtext.RData")