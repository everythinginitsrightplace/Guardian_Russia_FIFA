library(GuardianR)
library(dplyr)
library(tidytext)
library(tidyr)
articles <- get_guardian(keywords = "russia",
                         section = "football",
                         from.date = "2018-06-14",
                         to.date = "2018-07-10",
                         api.key = "XXXXXXXXXXXX") ### For football 




articles_1 <- get_guardian(keywords = "kremlin",
                         section = "world",
                         from.date = "2017-06-14",
                         to.date = "2018-07-15",
                           api.key = "XXXXXXXXXXXXX") ### For politics

##################
##################
##################
# For two datasets
total_articles <- rbind(articles, articles_1)

dim(total_articles)

colnames(total_articles)
f <- as.numeric(as.character(total_articles$wordcount))
sum(f)
#laziest subset for only two variables

want.var <- c("webPublicationDate", "body")
want <- which(colnames(total_articles) %in% want.var)
total_articles <- total_articles[, want]
total_articles$webPublicationDate <- as.Date.factor(total_articles$webPublicationDate)
dplyr::glimpse(total_articles$body[1])

total_articles$body <- iconv(total_articles$body, "", "ASCII", "byte")
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

total_articles$body <- cleanFun(total_articles$body)
dplyr::glimpse(total_articles$body[1])



#You can also use negative index for subsetting
articles.before <- total_articles[total_articles$webPublicationDate < "2018-03-12", ]
articles.after <- total_articles[total_articles$webPublicationDate >= "2018-03-12", ]

full.text.before <- articles.before[, 2]
full.text.before <- as.data.frame(full.text.before)

full.text.after <- articles.after[, 2]
full.text.after <- as.data.frame(full.text.after)

bigrams.before <- full.text.before %>%
  unnest_tokens(bigram,
                full.text.before,
                token = "ngrams",
                n = 2)
nrow(bigrams.before)

head(bigrams.before)
bigrams.separated.before <- bigrams.before %>%
  separate(bigram, c("word1", "word2"), sep = " ")

head(bigrams.separated.before)



Russia.Kremlin.words.before <- bigrams.separated.before %>%
  filter(word1 %in% c("russia", "kremlin"))

#Fix the missing t's after apostrophe
fix.apos <- c("hasn", "hadn", "doesn", "didn", "isn", "wasn", "couldn", "wouldn")
Russia.Kremlin.words.before <- Russia.Kremlin.words.before %>%
  mutate(word2 = ifelse(word2 %in% fix.apos, paste0(word2, "t"), word2))

#10 random samples; the numbers are row numbers not counts
set.seed(1895)
dplyr::sample_n(Russia.Kremlin.words.before, 10)


Russia.Kremlin.words.before <- Russia.Kremlin.words.before %>%
  count(word1, word2) %>%
  spread(word1, n, fill = 0) %>%
  mutate(total = russia + kremlin,
         he = (russia + 1) / sum(russia + 1),
         she = (kremlin + 1) / sum(kremlin + 1),
         log.ratio = log2(kremlin / russia),
         abs.ratio = abs(log.ratio)) %>%
  arrange(desc(log.ratio))
head(Russia.Kremlin.words.before)




library(devtools)
#devtools::install_github("juliasilge/silgelib", force = TRUE)
#Required Fonts
#https://fonts.google.com/specimen/Roboto+Condensed
#https://fonts.google.com/specimen/Roboto
library(ggplot2)
library(ggrepel)
#install.packages("scales")
library(scales)
library(silgelib) 
theme_set(theme_roboto())



Russia.Kremlin.words.before %>%
  filter(!word2 %in% c("of",
                       "actually", "allegedly", "have"),
         total >= 4) %>%
  group_by(direction = ifelse(log.ratio > 0, 'Больше "России"', "Кремля")) %>%
  top_n(30, abs.ratio) %>%
  ungroup() %>%
  mutate(word2 = reorder(word2, log.ratio)) %>%
  ggplot(aes(word2, log.ratio, fill = direction)) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = 'Относительная встречаемость слова "Россия" по сравнению со словом "Кремль"',
       fill = "",
       title = "Россия в издании The Guardian",
       subtitle = "Наиболее встречающиеся слова в связке с интересующими нас выражениями") +
  scale_y_continuous(labels = c("8X", "6X", "4X", "2X", "Same", "2X", "4X", "6X", "8X"),
                     breaks = seq(-4, 4)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  expand_limits(y = c(-4, 4))






new.variable <- as.vector(total_articles$body) 
cat(new.variable,file="Windows_Guardian.txt", encoding = "utf-8")



########
library(readr)
library(corpora)
library(janitor)
library("tidytext")
library("dplyr")
library("rcorpora")
library("lubridate")
library("ggplot2")
library("viridis")
library(quanteda)
library(tm)
library(biganalytics)
library(bigmemory)
library(bigmemory.sri)

text <- readLines(file.choose(), encoding = "UTF-8")
docs <- Corpus(VectorSource(text))
options(max.print=999999)
#inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "\")
docs <- tm_map(docs, toSpace, "—")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "–")

docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove russian common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#stopwords("english")
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("getty", "images", "image", "via", "email", "height", "width", "classgu", "afp", "pm", "bst", "min", "max", "updated", "in", "the","of", "will", "be", "so", "on", "had", "been", "has", "did", "not", "it", "was", "is", "he", "said", "would", "i", "out", "of", "you", "can", "there", "sign up", "class =", "from", "from the", "for", "for the", "i donat", "as well", "such as", "get our", "he has", "to get", "far post", "has", "get", "as", "a", "as a", "height =", "=", "is", "not", "she", "said", "well", "to", "lot", "of", "who", "has")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)


# Облаков слов
library(wordcloud)

collocations <- textstat_collocations(docs$content, size = 2:3)
wordcloud(words = collocations$collocation, freq = collocations$count,
          scale=c(7,.7), min.freq = 100, max.words=Inf, 
          random.order=FALSE, rot.per=0.1, ordered.colors=FALSE, 
          random.color=TRUE, colors=brewer.pal(8, "Dark2"))
write.csv(collocations, file = "correct_collocations.csv")












###################################################
###################################################
###################################################
###################################################
###################################################
# Only for football dataset

dim(articles)

colnames(articles)
f <- as.numeric(as.character(articles$wordcount))
sum(f)
#laziest subset for only two variables

want.var <- c("webPublicationDate", "body")
want <- which(colnames(articles) %in% want.var)
articles <- articles[, want]
articles$webPublicationDate <- as.Date.factor(articles$webPublicationDate)
dplyr::glimpse(articles$body[1])

articles$body <- iconv(articles$body, "", "ASCII", "byte")
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

articles$body <- cleanFun(articles$body)
dplyr::glimpse(articles$body[1])



#You can also use negative index for subsetting
articles.before <- articles[articles$webPublicationDate < "2018-07-03", ]
articles.after <- articles[articles$webPublicationDate >= "2018-07-03", ]

full.text.before <- articles.before[, 2]
full.text.before <- as.data.frame(full.text.before)

full.text.after <- articles.after[, 2]
full.text.after <- as.data.frame(full.text.after)

bigrams.before <- full.text.before %>%
  unnest_tokens(bigram,
                full.text.before,
                token = "ngrams",
                n = 2)
nrow(bigrams.before)

head(bigrams.before)
bigrams.separated.before <- bigrams.before %>%
  separate(bigram, c("word1", "word2"), sep = " ")

head(bigrams.separated.before)



Russia.Kremlin.words.before <- bigrams.separated.before %>%
  filter(word1 %in% c("russia", "fifa"))

#Fix the missing t's after apostrophe
fix.apos <- c("hasn", "hadn", "doesn", "didn", "isn", "wasn", "couldn", "wouldn")
Russia.Kremlin.words.before <- Russia.Kremlin.words.before %>%
  mutate(word2 = ifelse(word2 %in% fix.apos, paste0(word2, "t"), word2))

#10 random samples; the numbers are row numbers not counts
set.seed(1895)
dplyr::sample_n(Russia.Kremlin.words.before, 10)


Russia.Kremlin.words.before <- Russia.Kremlin.words.before %>%
  count(word1, word2) %>%
  spread(word1, n, fill = 0) %>%
  mutate(total = russia + fifa,
         he = (russia + 1) / sum(russia + 1),
         she = (fifa + 1) / sum(fifa + 1),
         log.ratio = log2(fifa / russia),
         abs.ratio = abs(log.ratio)) %>%
  arrange(desc(log.ratio))
head(Russia.Kremlin.words.before)




library(devtools)
#devtools::install_github("juliasilge/silgelib", force = TRUE)
#Required Fonts
#https://fonts.google.com/specimen/Roboto+Condensed
#https://fonts.google.com/specimen/Roboto
library(ggplot2)
library(ggrepel)
#install.packages("scales")
library(scales)
library(silgelib) 
theme_set(theme_roboto())
?theme_set


Russia.Kremlin.words.before %>%
  filter(!word2 %in% c("of",
                       "actually", "allegedly", "have"),
         total >= 25) %>%
  group_by(direction = ifelse(log.ratio > 0, 'Больше встречаются со словом "Россия"', "Больше встречаются со словом 'ФИФА'")) %>%
  top_n(15, abs.ratio) %>%
  ungroup() %>%
  mutate(word2 = reorder(word2, log.ratio)) %>%
  ggplot(aes(word2, log.ratio, fill = direction)) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = 'Слова, с которыми "Россия" и "ФИФА" встречаются чаще всего',
       fill = "",
       title = "Россия и ФИФА в издании The Guardian",
       subtitle = "Наиболее встречающиеся слова в связке с интересующими нас выражениями") +
  scale_y_continuous(labels = c("8X", "6X", "4X", "2X", "Same", "2X", "4X", "6X", "8X"),
                     breaks = seq(-4, 4)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  expand_limits(y = c(-4, 4))+
  hrbrthemes::theme_modern_rc()







new.variable <- as.vector(articles$body) 
cat(new.variable,file="Windows_Guardian_just_Russia_football.txt", encoding = "utf-8")



########
library(readr)
library(corpora)
library(janitor)
library("tidytext")
library("dplyr")
library("rcorpora")
library("lubridate")
library("ggplot2")
library("viridis")
library(quanteda)
library(tm)
library(biganalytics)
library(bigmemory)
library(bigmemory.sri)

text <- readLines(file.choose(), encoding = "UTF-8")
docs <- Corpus(VectorSource(text))
options(max.print=999999)
#inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "\")
docs <- tm_map(docs, toSpace, "—")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "–")

docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove russian common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#stopwords("english")
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("getty", "images", "image", "via", "email", "height", "width", "classgu", "afp", "pm", "bst", "min", "max", "updated", "in", "the","of", "will", "be", "so", "on", "had", "been", "has", "did", "not", "it", "was", "is", "he", "said", "would", "i", "out", "of", "you", "can", "there", "sign up", "class =", "from", "from the", "for", "for the", "i donat", "as well", "such as", "get our", "he has", "to get", "far post", "has", "get", "as", "a", "as a", "height =", "=", "is", "not", "she", "said", "well", "to", "lot", "of", "who", "has")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)


# Облаков слов
library(wordcloud)

collocations <- textstat_collocations(docs$content, size = 2:3)
wordcloud(words = collocations$collocation, freq = collocations$count,
          scale=c(7,.7), min.freq = 100, max.words=Inf, 
          random.order=FALSE, rot.per=0.1, ordered.colors=FALSE, 
          random.color=TRUE, colors=brewer.pal(8, "Dark2"))
write.csv(collocations, file = "correct_collocations_only_football.csv")








###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
# For political dataset
dim(articles_1)

colnames(articles_1)
f <- as.numeric(as.character(articles_1$wordcount))
sum(f)
#laziest subset for only two variables

want.var <- c("webPublicationDate", "body")
want <- which(colnames(articles_1) %in% want.var)
articles_1 <- articles_1[, want]
articles_1$webPublicationDate <- as.Date.factor(articles_1$webPublicationDate)
dplyr::glimpse(articles_1$body[1])

articles_1$body <- iconv(articles_1$body, "", "ASCII", "byte")
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

articles_1$body <- cleanFun(articles_1$body)
dplyr::glimpse(articles_1$body[1])



#You can also use negative index for subsetting
articles.before <- articles_1[articles_1$webPublicationDate < "2018-03-12", ]
articles.after <- articles_1[articles_1$webPublicationDate >= "2018-03-12", ]

full.text.before <- articles.before[, 2]
full.text.before <- as.data.frame(full.text.before)

full.text.after <- articles.after[, 2]
full.text.after <- as.data.frame(full.text.after)

bigrams.before <- full.text.before %>%
  unnest_tokens(bigram,
                full.text.before,
                token = "ngrams",
                n = 2)
nrow(bigrams.before)

head(bigrams.before)
bigrams.separated.before <- bigrams.before %>%
  separate(bigram, c("word1", "word2"), sep = " ")

head(bigrams.separated.before)



Russia.Kremlin.words.before <- bigrams.separated.before %>%
  filter(word1 %in% c("russia", "kremlin"))

#Fix the missing t's after apostrophe
fix.apos <- c("hasn", "hadn", "doesn", "didn", "isn", "wasn", "couldn", "wouldn")
Russia.Kremlin.words.before <- Russia.Kremlin.words.before %>%
  mutate(word2 = ifelse(word2 %in% fix.apos, paste0(word2, "t"), word2))

#10 random samples; the numbers are row numbers not counts
set.seed(1895)
dplyr::sample_n(Russia.Kremlin.words.before, 10)


Russia.Kremlin.words.before <- Russia.Kremlin.words.before %>%
  count(word1, word2) %>%
  spread(word1, n, fill = 0) %>%
  mutate(total = russia + kremlin,
         he = (russia + 1) / sum(russia + 1),
         she = (kremlin + 1) / sum(kremlin + 1),
         log.ratio = log2(kremlin / russia),
         abs.ratio = abs(log.ratio)) %>%
  arrange(desc(log.ratio))
head(Russia.Kremlin.words.before)




library(devtools)
#devtools::install_github("juliasilge/silgelib", force = TRUE)
#Required Fonts
#https://fonts.google.com/specimen/Roboto+Condensed
#https://fonts.google.com/specimen/Roboto
library(ggplot2)
library(ggrepel)
#install.packages("scales")
library(scales)
library(silgelib) 
theme_set(theme_roboto())



Russia.Kremlin.words.before %>%
  filter(!word2 %in% c("of",
                       "actually", "allegedly", "have"),
         total >= 4) %>%
  group_by(direction = ifelse(log.ratio > 0, 'Больше встречаются со словом "Россия"', "Больше встречаются со словом 'Кремль'")) %>%
  top_n(30, abs.ratio) %>%
  ungroup() %>%
  mutate(word2 = reorder(word2, log.ratio)) %>%
  ggplot(aes(word2, log.ratio, fill = direction)) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = 'Слова, с которыми "Россия" и "Кремль" встречаются чаще всего',
       fill = "",
       title = "Политическая Россия в издании The Guardian",
       subtitle = "Наиболее встречающиеся слова в связке с интересующими нас словами") +
  scale_y_continuous(labels = c("8X", "6X", "4X", "2X", "Same", "2X", "4X", "6X", "8X"),
                     breaks = seq(-4, 4)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  expand_limits(y = c(-4, 4))+
  hrbrthemes::theme_modern_rc()





new.variable <- as.vector(articles_1$body) 
cat(new.variable,file="Windows_Guardian_Politics.txt", encoding = "utf-8")



########
library(readr)
library(corpora)
library(janitor)
library("tidytext")
library("dplyr")
library("rcorpora")
library("lubridate")
library("ggplot2")
library("viridis")
library(quanteda)
library(tm)
library(biganalytics)
library(bigmemory)
library(bigmemory.sri)

text <- readLines(file.choose(), encoding = "UTF-8")
docs <- Corpus(VectorSource(text))
options(max.print=999999)
#inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "\")
docs <- tm_map(docs, toSpace, "—")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "–")

docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove russian common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#stopwords("english")
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("getty", "images", "image", "via", "email", "height", "width", "classgu", "afp", "pm", "bst", "min", "max", "updated", "in", "the","of", "will", "be", "so", "on", "had", "been", "has", "did", "not", "it", "was", "is", "he", "said", "would", "i", "out", "of", "you", "can", "there", "sign up", "class =", "from", "from the", "for", "for the", "i donat", "as well", "such as", "get our", "he has", "to get", "far post", "has", "get", "as", "a", "as a", "height =", "=", "is", "not", "she", "said", "well", "to", "lot", "of", "who", "has")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)


# Облаков слов
library(wordcloud)

collocations <- textstat_collocations(docs$content, size = 2:3)
wordcloud(words = collocations$collocation, freq = collocations$count,
          scale=c(8,.2), min.freq = 50, max.words=Inf, 
          random.order=FALSE, rot.per=0.1, ordered.colors=FALSE, 
          random.color=TRUE, colors=brewer.pal(8, "Dark2"))
write.csv(collocations, file = "correct_collocations_politics.csv")

