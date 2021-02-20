
#AFINN szótárhoz
library(tidytext)
library(textdata)
#Szózsák-modell
library(tm)
library(SnowballC)
library(wordcloud)

library(tools)
library(rvest)
Sys.setlocale("LC_TIME", "C")

nr_of_pages=534
page_base <- "https://www.theguardian.com/world/coronavirus-outbreak+uk/uk?page="

lapply(1:534, function(page_number){
  print(page_number)
  paste0(page_base, page_number) %>% # URL to read
    read_html()
})

for (i in 1:nr_of_pages) {
  URL <- paste("https://www.theguardian.com/world/coronavirus-outbreak+uk/uk?page=", i, sep = "")
  page <- read_html(URL)
  title <- html_text(html_nodes(page, ".js-headline-text"))
  #Van olyan hogy egy lapon két dátum is szerepel, heti bontás miatt ez nem probléma
  time <- html_text(html_nodes(page, ".fc-date-headline"))[1]
  if (i==1){
    data_raw <- data.frame(time, title) 
  }
  else{
    data_raw <- rbind(data_raw, data.frame(time, title))  
  }
}

data_cleansed <- data_raw %>% filter(!duplicated(title)) %>% {mutate(., id = seq(nrow(.)))} %>% 
  mutate(
    date = as.Date(time, format = "%d %B %Y"),
    month = format(date, "%Y-%m")
  ) %>% na.omit()

corpus_raw <- Corpus(VectorSource(as.character(data_cleansed$title)))

corpus_filtered <- corpus_raw %>% tm_map(content_transformer(tolower)) %>% 
  tm_map(stripWhitespace) %>%  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, ucp=TRUE) %>%
  tm_map(removeWords, c(stopwords("english"), "also", "one", "will"))
termdocument <- removeSparseTerms(TermDocumentMatrix(corpus_filtered), 0.999)
documentterm <- removeSparseTerms(DocumentTermMatrix(corpus_filtered), 0.999)
words_frequency_all <- data.frame(Words=row.names(as.matrix(termdocument)),
                                  Freq=rowSums(as.matrix(termdocument), na.rm = TRUE))

freqterms <- findFreqTerms(documentterm, lowfreq = 100)
ggwordcloud(words_frequency_all$Words, words_frequency_all$Freq, max.words = 100)
