# server.R

library(shiny)
library(shinydashboard)
library(dplyr)
library(quanteda)

# Load prediction model files
words1 <- readRDS("words1.rds")
words2 <- readRDS("words2.rds")
words3 <- readRDS("words3.rds")
words4 <- readRDS("words4.rds")

# Word prediction function
predict_word <- function(raw_input) {
        inp <- tokens(raw_input, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_url = TRUE)
        inp <- tokens_wordstem(inp, language = "en")
        inp_words <- as.character(inp[[1]])
        inp_l <- length(inp_words)
        
        if (inp_l >= 3) {
                w_3 <- inp_words[inp_l - 2]
                w_2 <- inp_words[inp_l - 1]
                w_1 <- inp_words[inp_l]
                words4_f <- words4 %>%
                        filter(word_1 == w_3) %>%
                        filter(word_2 == w_2) %>%
                        filter(word_3 == w_1) %>%
                        arrange(-count)
                words3_f <- words3 %>%
                        filter(word_1 == w_2) %>%
                        filter(word_2 == w_1) %>%
                        arrange(-count)
                words2_f <- words2 %>%
                        filter(word_1 == w_1) %>%
                        arrange(-count)
                words1_f <- words1 %>%
                        arrange(-count)
                
                if (nrow(words4_f) > 0) {
                        return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words4_f[1, 4], "</span>")))
                } else if (nrow(words4_f) == 0 && nrow(words3_f) > 0) {
                        return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words3_f[1, 3], "</span>")))
                } else if (nrow(words3_f) == 0 && nrow(words2_f) > 0) {
                        return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words2_f[1, 2], "</span>")))
                } else if (nrow(words2_f) == 0) {
                        return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words1_f[1, 1], "</span>")))
                }
        }
        
        if (inp_l == 2) {
                w_2 <- inp_words[inp_l - 1]
                w_1 <- inp_words[inp_l]
                words3_f <- words3 %>%
                        filter(word_1 == w_2) %>%
                        filter(word_2 == w_1) %>%
                        arrange(-count)
                words2_f <- words2 %>%
                        filter(word_1 == w_1) %>%
                        arrange(-count)
                words1_f <- words1 %>%
                        arrange(-count)
                if (nrow(words3_f) > 0) {
                        return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words3_f[1, 3], "</span>")))
                } else if (nrow(words3_f) == 0 && nrow(words2_f) > 0) {
                        return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words2_f[1, 2], "</span>")))
                } else if (nrow(words2_f) == 0) {
                        return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words1_f[1, 1], "</span>")))
                }
        }
        
        if (inp_l == 1) {
                w_1 <- inp_words[inp_l]
                words2_f <- words2 %>%
                        filter(word_1 == w_1) %>%
                        arrange(-count)
                words1_f <- words1 %>%
                        arrange(-count)
                if (nrow(words2_f) > 0) {
                        return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words2_f[1, 2], "</span>")))
                } else if (nrow(words2_f) == 0) {
                        return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words1_f[1, 1], "</span>")))
                }
        }
        
        if (inp_l == 0) {
                words1_f <- words1 %>%
                        arrange(-count)
                return(HTML(paste0("<span style='font-size: 50px; color: red;'>", words1_f[1, 1], "</span>")))
        }
}

# Shiny server function
shinyServer(function(input, output, session) {
        observeEvent(input$predict_button, {
                prediction_result <- predict_word(input$user_input)
                output$prediction_output <- renderUI({
                        HTML(paste0("<p>", prediction_result, "</p>"))
                })
        })
        
        observeEvent(input$clear_button, {
                updateTextInput(session, "user_input", value = "")
                output$prediction_output <- renderUI(NULL)
        })
})
