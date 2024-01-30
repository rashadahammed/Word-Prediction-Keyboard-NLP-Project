# model creation
library(tidyverse)
library(data.table)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(reader)


# download
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download_zip <- "download.zip"

if(!file.exists(download_zip)){
  download.file(url, destfile = download_zip)
}

if(!dir.exists("final")){
  unzip(download_zip)
}

list <- unzip(download_zip, list = T)

# corpus 9.9k
path_twitter <- list[11,1]
path_news <- list[12,1]
path_blogs <- list[13,1]

con <- file(path_twitter, "r")
tw1 <- readLines(con, 10000)
close(con)

con <- file(path_news, "r")
ne1 <- readLines(con, 10000)
close(con)

con <- file(path_blogs, "r")
bl1 <- readLines(con, 10000)
close(con)

corpus_99 <- corpus(c(tw1, ne1, bl1))

# tokens 9.9k
toks99 <- tokens(corpus_99, remove_punct = T, remove_numbers = T, remove_symbols = T,
                 remove_url = T)
toks99_stem <- tokens_wordstem(toks99, language = "en")

# n-grams
gram2 <- tokens_ngrams(toks99_stem, n = 2)
gram3 <- tokens_ngrams(toks99_stem, n = 3)
gram4 <- tokens_ngrams(toks99_stem, n = 4)

# dfm
dfm1 <- dfm(toks99_stem)
dfm2 <- dfm(gram2)
dfm3 <- dfm(gram3)
dfm4 <- dfm(gram4)

dfm1 <- dfm_trim(dfm1, min_termfreq = 2, max_docfreq = 0.2,docfreq_type = "prop")
dfm2 <- dfm_trim(dfm2, min_termfreq = 2, max_docfreq = 0.2, docfreq_type = "prop")
dfm3 <- dfm_trim(dfm3, max_docfreq = 0.2, docfreq_type = "prop")
dfm4 <- dfm_trim(dfm4, max_docfreq = 0.2, docfreq_type = "prop")

# words
sums1 <- colSums(dfm1)
sums2 <- colSums(dfm2)
sums3 <- colSums(dfm3)
sums4 <- colSums(dfm4)

words1 <- data.table(word_1 = names(sums1), count = sums1)

words2 <- data.table(
  word_1 = sapply(strsplit(names(sums2), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums2), "_", fixed = TRUE), '[[', 2),
  count = sums2)

words3 <- data.table(
  word_1 = sapply(strsplit(names(sums3), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums3), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums3), "_", fixed = TRUE), '[[', 3),
  count = sums3)

words4 <- data.table(
  word_1 = sapply(strsplit(names(sums4), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums4), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums4), "_", fixed = TRUE), '[[', 3),
  word_4 = sapply(strsplit(names(sums4), "_", fixed = TRUE), '[[', 4),
  count = sums4)




setkey(words1, word_1)
setkey(words2, word_1, word_2)
setkey(words3, word_1, word_2, word_3)
setkey(words4, word_1, word_2, word_3, word_4)

saveRDS(words1,"words1.rds")
saveRDS(words2,"words2.rds")
saveRDS(words3,"words3.rds")
saveRDS(words4,"words4.rds")

# training words
words1 <- readRDS("words1.rds")
words2 <- readRDS("words2.rds")
words3 <- readRDS("words3.rds")
words4 <- readRDS("words4.rds")

# word prediction function
predict_word <- function(raw_input){
inp <- tokens(raw_input, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
                      remove_url = TRUE)
inp <- tokens_wordstem(inp, language = "en")
inp_words <- as.character(inp[[1]])
inp_l <- length(inp_words)

if (inp_l >= 3) {
  w_3 <- inp_words[inp_l - 2]
  w_2 <- inp_words[inp_l - 1]
  w_1 <- inp_words[inp_l]
  words4_f <- words4 |> filter(word_1 == w_3) |> filter(word_2 == w_2) |> filter(word_3 == w_1)  |> arrange(-count)
  words3_f <- words3 |> filter(word_1 == w_2) |> filter(word_2 == w_1) |> arrange(-count)
  words2_f <- words2 |> filter(word_1 == w_1) |> arrange(-count)
  words1_f <- words1 |> arrange(-count)
  
  if (nrow(words4_f)>0) {
          y <- words4_f[1,4]
          print(y)
          return(y)
  } else if (nrow(words4_f)==0 && nrow(words3_f)>0){
          y <- words3_f[1,3]
          print(y) 
          return(y)
  } else if (nrow(words3_f)==0 && nrow(words2_f)>0){
          y <- words2_f[1,2]
          print(y)
          return(y)
  } else if (nrow(words2_f)==0){
          y <- words1_f[1,1]
          print(y)
          return(y)
  }
} 

if (inp_l == 2) {
        w_2 <- inp_words[inp_l - 1]
        w_1 <- inp_words[inp_l]
        words3_f <- words3 |> filter(word_1 == w_2) |> filter(word_2 == w_1) |> arrange(-count)
        words2_f <- words2 |> filter(word_1 == w_1) |> arrange(-count)
        words1_f <- words1 |> arrange(-count)
        if (nrow(words3_f)>0){
                y <- words3_f[1,3]
                print(y) 
                return(y)
        } else if (nrow(words3_f)==0 && nrow(words2_f)>0){
                y <- words2_f[1,2]
                print(y)
                return(y)
        } else if (nrow(words2_f)==0){
                y <- words1_f[1,1]
                print(y)
                return(y)
        }
} 

if (inp_l == 1) {
        w_1 <- inp_words[inp_l]
        words2_f <- words2 |> filter(word_1 == w_1) |> arrange(-count)
        words1_f <- words1 |> arrange(-count)
        if (nrow(words2_f)>0){
                y <- words2_f[1,2]
                print(y)
                return(y)
        } else if (nrow(words2_f)==0) {
                y <- words1_f[1,1]
                print(y)
                return(y)
        }
}

if (inp_l == 0){
        words1_f <- words1 |> arrange(-count)
        y <- words1_f[1,1]
        print(y)
        return(y)
}
}




################################################################################
# model testing
library("reader")

path_twitter_test <- list[11,1]
path_news_test <- list[12,1]
path_blogs_test <- list[13,1]

tw1_test <- n.readLines(path_twitter_test, header = FALSE, n = 3300, skip = 10000)
ne1_test <- n.readLines(path_news_test, header = FALSE, n = 3300, skip = 10000)
bl1_test <- n.readLines(path_blogs_test, header = FALSE, n = 3300, skip = 10000)

# testing data into a single corpus
corpus_test <- corpus(c(tw1_test, ne1_test, bl1_test))

# tokenization of testing data
toks_test <- tokens(corpus_test, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
                    remove_url = TRUE)
toks_test_stem <- tokens_wordstem(toks_test, language = "en")

# n-grams for testing data
gram2_test <- tokens_ngrams(toks_test_stem, n = 2)
gram3_test <- tokens_ngrams(toks_test_stem, n = 3)
gram4_test <- tokens_ngrams(toks_test_stem, n = 4)

# dfm
dfm2_test <- dfm(gram2_test)
dfm3_test <- dfm(gram3_test)
dfm4_test <- dfm(gram4_test)

# words
sums2_test <- colSums(dfm2_test)
sums3_test <- colSums(dfm3_test)
sums4_test <- colSums(dfm4_test)

words2_test <- data.table(
        word_1 = sapply(strsplit(names(sums2_test), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums2_test), "_", fixed = TRUE), '[[', 2),
        count = sums2_test)

words3_test <- data.table(
        word_1 = sapply(strsplit(names(sums3_test), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums3_test), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sums3_test), "_", fixed = TRUE), '[[', 3),
        count = sums3_test)

words4_test <- data.table(
        word_1 = sapply(strsplit(names(sums4_test), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums4_test), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sums4_test), "_", fixed = TRUE), '[[', 3),
        word_4 = sapply(strsplit(names(sums4_test), "_", fixed = TRUE), '[[', 4),
        count = sums4_test)

saveRDS(words2_test,"words2_t.rds")
saveRDS(words3_test,"words3_t.rds")
saveRDS(words4_test,"words4_t.rds")

# testing words
words2_t <- readRDS("words2_t.rds")
words3_t <- readRDS("words3_t.rds")
words4_t <- readRDS("words4_t.rds")

# testing accuracy using 2-grams

for (j in 1:10) {

correct_predictions <- 0
total_predictions <- 1000

for (i in 1:total_predictions) {
        
        line_sel <- sample(nrow(words2_t),1)
        input_word <- as.character(words2_t[line_sel, 1])
        
        predicted_word <- predict_word(input_word)
        
        actual_word <- words2_t[line_sel, "word_2"]
        
        if (predicted_word == actual_word) {
                correct_predictions <- correct_predictions + 1
        }
}

# accuracy 2g
accuracy_2g[j] <- correct_predictions / total_predictions
cat("Accuracy_2g:", accuracy_2g, "\n")

}

summary(accuracy_2g)


# testing accuracy using 3-grams

for (j in 1:10) {

correct_predictions <- 0
total_predictions <- 1000

for (i in 1:total_predictions) {
        
        line_sel <- sample(nrow(words3_t),1)
        input_word <- as.character(paste(words3_t[line_sel, 1],words3_t[line_sel, 2], sep= " " ))
        
        predicted_word <- predict_word(input_word)
        
        actual_word <- words3_t[line_sel, "word_3"]
        
        if (predicted_word == actual_word) {
                correct_predictions <- correct_predictions + 1
        }
}

# accuracy 3g
accuracy_3g[j] <- correct_predictions / total_predictions
cat("Accuracy_3g:", accuracy_3g, "\n")

}

summary(accuracy_3g)


# testing accuracy using 4-grams

for (j in 1:10) {

correct_predictions <- 0
total_predictions <- 1000

for (i in 1:total_predictions) {
  
  line_sel <- sample(nrow(words4_t),1)
  input_word <- as.character(paste(words4_t[line_sel, 1],words4_t[line_sel, 2], words4_t[line_sel, 3], sep= " " ))
  
  predicted_word <- predict_word(input_word)
  
  actual_word <- words4_t[line_sel, "word_4"]
  
  if (predicted_word == actual_word) {
    correct_predictions <- correct_predictions + 1
  }
}

# accuracy 4g
accuracy_4g[j] <- correct_predictions / total_predictions
cat("Accuracy_4g:", accuracy_4g, "\n")

}

summary(accuracy_4g)

boxplot(accuracy_2g, accuracy_3g, accuracy_4g,
                ylab = "Accuracy")

boxplot(list(accuracy_2g, accuracy_3g, accuracy_4g),
        names = c("single word", "two words", "three words"),
        xlab = "Input length",
        ylab = "Accuracy")


# hope it all works well




