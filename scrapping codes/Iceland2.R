# Fri Jan 29 02:08:41 2021 ------------------------------

library(tidyverse)
library(rvest)

URLs <- paste0('https://www.ruv.is/tag/covid-19/?page=', 1:266)

f.URL <- function(page) {
    page %>%
      html_nodes('.big a') %>%
      html_attr("href") %>%
      na.omit %>%
    {paste0("https://www.ruv.is", .)} %>%
  reduce(c)
}

f.date <- function(page) {
    page %>%
      html_nodes('.pad0t-mobile') %>%
      html_text()  %>%
  {lubridate::dmy(str_sub(., 2, 11))} %>%
  reduce(c)
}

f.title <- function(page) {
    page %>%
      html_nodes('.big a') %>%
      html_text() %>%
  reduce(c)
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

f.rawtext <- function(raw.page) {
  tryCatch(
  {page <- read_html(raw.page)
  URL <- f.URL(page)
  tibble(date = f.date(page), title = f.title(page), URL = URL, text = sapply(URL, f.text))},
  error = function(e) {print(paste("Error with", raw.page)); NULL})
}

f.new_rawtext <- function(URLs) {
  lapply(URLs, f.rawtext) %>%
  Filter(f = Negate(is.null)) %>%
  reduce(rbind) %>%
  filter(text != "")
}

# test ----------------------------------------------------------
read_html(URLs[1]) %>% 
  f.date

read_html(URLs[1]) %>% 
  f.title()

read_html(URLs[1]) %>% 
  f.URL()

read_html(URLs[1]) %>% 
  f.URL %>% 
  .[[6]] %>% 
  f.text()

f.new_rawtext(URLs[1:2])

# start scrapping -----------------------------------------------

i <- 1
cat(paste0((i-1)*10, "% process"))

f.get_text <- function() {
  quantile(seq_along(URLs), probs = 0:10/10) %>% 
  floor() %>% 
  .[i:(i+1)] %>% 
  {i <<- i + 1; URLs[.[1]:.[2]]} %>% 
  f.new_rawtext
}

text_df <- f.get_text()
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))


# saving --------------------------------------------------------
Iceland_rawtext <- text_df %>% 
  filter(!duplicated(text)) # remove duplicates

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Iceland_rawtext"), file = "Iceland_rawtext.RData")
