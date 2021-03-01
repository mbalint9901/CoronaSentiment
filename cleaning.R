library(tidyverse)
library(readxl)
library(lubridate)

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


# czech -----------------------------------------------------------------------------
czech <- read_excel("czech.xlsx") %>% mutate(
  date2 = dmy(date), date = ifelse(is.na(date2), 
                                   as.character(as.Date(as.numeric(date), origin = "1899-12-30")), 
                                   as.character(date2)),
  date = ymd(date)
) %>% select(-date2)


# portugal --------------------------------------------------------------------------

portugal <- read_excel("portugal.xlsx") %>% 
  mutate(
    date2 = dmy(date),
    date = ifelse(is.na(date2), 
                  as.character(mdy(date)), 
                  as.character(date2)),
    date = ymd(date)
  ) %>% select(-date2)


# lithuania -------------------------------------------------------------------------

lithuania <- read_excel("lithuania.xlsx") %>% 
  mutate(
    date = mdy(date)
  )


# sweden ----------------------------------------------------------------------------

sweden <- read_excel("sweden.xlsx") # TODO date


# spain -----------------------------------------------------------------------------

spain <- read_excel("spain.xlsx") %>% 
  filter(!is.na(text)) %>% 
  mutate(
    date2 = ifelse(str_detect(date, "/"), as.character(mdy(date)), as.character(dmy(date))),
    date2 = ymd(date2)
  ) # TODO differenct time format


# belgium_french --------------------------------------------------------------------

belgium_french <- read_excel("belgium_french.xlsx") %>% 
  mutate_at(-1, function(x) zoo::na.locf(x)) %>% 
  filter(str_detect(date, '202')) %>% 
  mutate(
    date = gsub(".*day, ", "", date),
    date = gsub(".*day ", "", date),
    date = mdy(date)
  )

# slovakia --------------------------------------------------------------------------

slovakia <- read_excel("slovakia.xlsx")
slovakia %>% mutate(
  date2 = mdy(date), 
  date3 = ifelse(is.na(date2),
                 as.character(as.Date(as.numeric(date), origin = "1899-12-30")),
                 as.character(date2)),
  date3 = ymd(date3)
) # TODO clean


# romania ---------------------------------------------------------------------------

romania <- read_excel("romania.xlsx") %>% 
  mutate_at(-1, function(x) zoo::na.locf(x)) %>% 
  filter(date != "_x000D_") %>% 
  mutate(
    date2 = dmy(date),
    date3 = ifelse(is.na(date2), 
                   as.character(mdy(date)), 
                   as.character(date2)),
    date3 = ifelse(is.na(date3), 
                   as.character(as.Date(as.numeric(date), origin = "1899-12-30")),
                   as.character(date3)),
    date = ymd(date3)
  ) %>% select(-date2, -date3) 


# denmark ---------------------------------------------------------------------------

denmark <- read_excel("denmark.xlsx") %>% 
  mutate(
    date = mdy(date)
  )


# croatia ---------------------------------------------------------------------------

croatia <- read_excel("croatia.xlsx") %>% 
  mutate(
    date2 = mdy(date),
    date = ifelse(is.na(date2), 
                  as.character(as.Date(as.numeric(date), origin = "1899-12-30")),
                  as.character(date2)),
    date = ymd(date)
  ) %>% select(-date2)


# austria ---------------------------------------------------------------------------

austria <- read_excel("austria.xlsx") %>% 
  mutate(
    date = as.Date(as.numeric(date), origin = "1899-12-30")
  ) %>% rename(URL = Url)


# slovenia --------------------------------------------------------------------------

slovenia <- read_excel("slovenia.xlsx")  %>% 
  mutate(
    date2 = mdy(date),
    date = ifelse(is.na(date2), 
                  as.character(as.Date(as.numeric(date), origin = "1899-12-30")),
                  as.character(date2)),
    date = ymd(date)
  ) %>% select(date, title, URL, text)


# switzerland -----------------------------------------------------------------------

load("C:/rprojects/CoronaSentiment/scrapping RData/Switzerland_rawtext.RData")


# hungary ---------------------------------------------------------------------------

hungary <- read_excel("hungary.xlsx") %>% 
  select(date, title, URL = links, text) %>% 
  mutate_at(-1, function(x) zoo::na.locf(x)) %>% 
  filter(!str_detect(date, '_x000') & !str_detect(date, ':') & date != '0') %>% 
  filter(!str_detect(text, 'mtva_player')) %>% # TODO consider a better solution
  mutate(
    text = str_remove_all(text, "_x000D_"),
    date = dmy(date)
    )

  

# merge ---------------------------------------------------------
setwd("C:/rprojects/CoronaSentiment")

dat <- greece %>% mutate(country = "EL") %>% 
  rbind(mutate(norway, country = "NO")) %>% 
  rbind(mutate(bulgaria, country = "BG")) %>% 
  rbind(mutate(finland, country = "DI")) %>% 
  rbind(mutate(france, country = "FR")) %>% 
  rbind(mutate(iceland, country = "IS")) %>% 
  rbind(mutate(italy, country = "IT")) %>% 
  rbind(mutate(malta, country = "MT")) %>% 
  # rbind(mutate(malta, country = "CZ")) %>% 
  rbind(mutate(lithuania, country = "LT")) %>% 
  # rbind(mutate(malta, country = "SE")) %>% 
  # rbind(mutate(spain, country = "ES")) %>% 
  rbind(mutate(malta, country = "BE_french")) %>% 
  # rbind(mutate(slovakia, country = "SK")) %>% 
  rbind(mutate(romania, country = "RO")) %>% 
  rbind(mutate(denmark, country = "DK")) %>% 
  rbind(mutate(croatia, country = "HR")) %>% 
  rbind(mutate(austria, country = "AT")) %>% 
  rbind(mutate(slovenia, country = "SI")) %>% 
  rbind(mutate(Switzerland_rawtext, country = "CH")) %>% 
  rbind(mutate(hungary, country = "HU")) %>% 
  mutate(
    text = str_replace_all(text, '\"', " "),
    title = str_replace_all(title, '\"', " ") # TODO more special characters
  ) %>% 
  filter(
    !is.na(text) & text != "" & str_length(text) > 20
  ) %>% 
  filter(date < lubridate::ymd("2021-02-01") &
           date > lubridate::ymd("2019-12-31")
           )

# TODO imput dates

setwd("C:/rprojects/CoronaSentiment")
save(list = c("dat"), file = "dat.RData")
