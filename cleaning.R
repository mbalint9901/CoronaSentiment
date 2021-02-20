library(tidyverse)

setwd("C:/rprojects/CoronaSentiment/translated excel")

# greece --------------------------------------------------------

greece <- readxl::read_excel("greece.xlsx")

for (i in 1:nrow(greece)) {
  if (!is.na(greece[i, 1])) {
    title <- greece[i, 1]
    date <- greece[i, 2]
    URL <- greece[i, 3]
    text <- greece[i, 4]
    n_missing <- 0
    
  } else{
    greece[i, 1] <- title
    greece[i, 2] <- date
    greece[i, 3] <- URL
    greece[i, 4] <- str_c(text, greece[i, 4], collapse = " ")
    text <- greece[i, 4]
  }
}

greece <- greece %>% 
  mutate(
    r = 1:nrow(greece)
  ) %>% 
  arrange(desc(r)) %>% 
  filter(!duplicated(URL) & !is.na(text) & str_length(text) != 0) %>% 
  mutate(
    date2 = lubridate::mdy(date),
    text = str_remove_all(text, '\"'),
    text = gsub("Photo: ", "", text),
    text = str_remove_all(text, 'Share the article:'),
    text = gsub("Reportage-text-photo: ", "", text),
    date2 = ifelse(is.na(date2), as.character(lubridate::dmy(date)), as.character(date2)),
    date = ifelse(is.na(date2), as.character(as.Date(as.numeric(date), origin = "1899-12-30")), as.character(date2)),
    date = lubridate::ymd(date)
  ) %>% 
  select(-r, -date2)


# norway --------------------------------------------------------

norway <- readxl::read_excel("norway.xlsx") %>%
  mutate(
    text = str_remove_all(text, '\"'),
    date = lubridate::mdy(date)
         )


# bulgaria ------------------------------------------------------

bulgaria <- readxl::read_excel("bulgaria.xlsx") %>% 
  mutate(date = lubridate::mdy(date))


# finland -------------------------------------------------------

finland <- read_excel("finland.xlsx")

for (i in 1:nrow(finland)) {
  if (!is.na(finland[i, 1])) {
    title <- finland[i, 1]
    date <- finland[i, 2]
    URL <- finland[i, 3]
    text <- finland[i, 4]
    n_missing <- 0
    
  } else{
    finland[i, 1] <- title
    finland[i, 2] <- date
    finland[i, 3] <- URL
    finland[i, 4] <- str_c(text, finland[i, 4], collapse = " ")
    text <- finland[i, 4]
  }
}

finland <- finland %>% 
  mutate(
    r = 1:nrow(finland)
  ) %>% 
  arrange(desc(r)) %>% 
  filter(!duplicated(URL) & !is.na(text) & str_length(text) != 0) %>% 
  mutate(
    date = lubridate::mdy(date)
  ) %>% 
  select(-r) %>% 
  filter(text != "AS")


# france --------------------------------------------------------

france <- read_excel("france.xlsx") %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  filter(!is.na(date))


# iceland -------------------------------------------------------

iceland <- read_excel("iceland.xlsx") %>% 
  mutate(date = lubridate::mdy(date))


# italy ---------------------------------------------------------

italy <- read_excel("italy.xlsx") %>% 
  mutate(date = lubridate::mdy(date))


# malta ---------------------------------------------------------

malta <- read_excel("malta.xlsx") %>% 
  mutate(date = lubridate::mdy(date))

# merge ---------------------------------------------------------
setwd("C:/rprojects/CoronaSentiment")

dat <- greece %>% mutate(country = "greece") %>% 
  rbind(mutate(norway, country = "norway")) %>% 
  rbind(mutate(bulgaria, country = "bulgaria")) %>% 
  rbind(mutate(finland, country = "finland")) %>% 
  rbind(mutate(france, country = "france")) %>% 
  rbind(mutate(iceland, country = "iceland")) %>% 
  rbind(mutate(italy, country = "italy")) %>% 
  rbind(mutate(malta, country = "malta"))

setwd("C:/rprojects/CoronaSentiment")
save(list = c("dat"), file = "dat.RData")
