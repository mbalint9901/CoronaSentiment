# Sat Feb 13 14:50:31 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

start_date <- seq.Date(from = as.Date(43845, origin = "1899-12-30"), to = as.Date(44238, origin = "1899-12-30"), by = 2)

URLs <- vector()

for (i in seq_along(start_date)) {

available_page <- paste0(
  "https://www.rtve.es/buscador?q=coronavirus&desde=",
  lubridate::day(start_date[i]),
  "%2F",
  lubridate::month(start_date[i]),
  "%2F",
  lubridate::year(start_date[i]),
  "&hasta=",
  lubridate::day(start_date[i] + 1),
  "%2F",
  lubridate::month(start_date[i] + 1),
  "%2F",
  lubridate::year(start_date[i] + 1),
  "&site=noticias&start=",
  1, "&sort=timestamp"
) %>% read_html() %>% 
  html_nodes(".goend a") %>% 
  html_attr("href") %>% 
  {gsub(".*start=", "", .)} %>% 
  {gsub("&sort.*", "", .)} %>% 
  as.numeric()

if (length(available_page) == 0) available_page <- 1

URLs <- c(URLs, paste0(
  "https://www.rtve.es/buscador?q=coronavirus&desde=",
  lubridate::day(start_date[i]),
  "%2F",
  lubridate::month(start_date[i]),
  "%2F",
  lubridate::year(start_date[i]),
  "&hasta=",
  lubridate::day(start_date[i] + 1),
  "%2F",
  lubridate::month(start_date[i] + 1),
  "%2F",
  lubridate::year(start_date[i] + 1),
  "&site=noticias&start=",
  1:available_page, "&sort=timestamp"
)
)
}

f.initial_df <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = reduce(html_text(html_nodes(page, '.datpub')), c),
      title = reduce(html_text(html_nodes(page, '.maintitle')), c),
      URL = reduce(html_attr(html_nodes(page, '.txtBox>h2>a'), 'href'), c)
    )},
    error = function(e) NULL)
}

f.text <- function(URL) {
  tryCatch(URL %>%
             read_html() %>%
             html_nodes(".artBody p") %>%
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

initial_df <- filter(initial_df, !str_detect(URL, "/videos/")) %>% 
  filter(!str_detect(URL, "/audios/"))

clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(stringr))
clusterExport(cl, list("f.text", "initial_df"), envir = environment())

articles <- parLapply(cl = cl, X = initial_df$URL, fun = f.text)
stopCluster(cl)

Spain_rawtext <- cbind(initial_df, reduce(articles, c)) %>% 
  set_names("date", "title", "URL", "text") %>% 
  filter(text != "") %>% 
  mutate(
    # date = lubridate::dmy(date),
    title = str_remove_all(title, "\n"),
    title = str_remove_all(title, "\t"),
    text = str_replace_all(text, "\n", " "), # clean text
    text = str_replace_all(text, "\t", " "),
    text = str_replace_all(text, '"', " "),
    text = str_replace_all(text, '  ', " ")
  )

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Spain_rawtext"), file = "Spain_rawtext.RData")

