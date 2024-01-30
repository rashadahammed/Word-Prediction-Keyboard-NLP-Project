# ui.R

library(shiny)
library(shinydashboard)

header <- dashboardHeader()

sidebar <- dashboardSidebar(
        sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("How to Use", tabName = "how_to_use", icon = icon("info")),
                menuItem("About Text Prediction", tabName = "text_prediction", icon = icon("book"))
        )
)

body <- dashboardBody(
        tabItems(
                # Home tab
                tabItem(
                        tabName = "home",
                        fluidPage(
                                titlePanel("Text Prediction App"),
                                textInput("user_input", label = "Enter your text:"),
                                actionButton("predict_button", "Predict"),
                                actionButton("clear_button", "Clear Input"),
                                htmlOutput("prediction_output")
                        )
                ),
                
                # How to Use tab
                tabItem(
                        tabName = "how_to_use",
                        fluidPage(
                                titlePanel("How to Use"),
                                # Add your instructions or content here
                                tags$div(
                                        h3("Instructions:"),
                                        p("1. Enter your text in the input field."),
                                        p("2. Click the 'Predict' button to get a word prediction."),
                                        p("3. Use the 'Clear Input' button to reset the input field."),
                                        p("Feel free to explore and have fun!")
                                )
                        )
                ),
                
                # About text prediction tab
                tabItem(
                        tabName = "text_prediction",
                        fluidPage(
                                titlePanel("About Text Prediction"),
                                # Add your information or content here
                                tags$div(
                                        h3("About Text Prediction"),
                                        p("Text prediction is a natural language processing (NLP) task that involves forecasting the next word or sequence of words in a given context."),
                                        p("This app employs an n-gram-based model to make predictions. Here's a brief overview of key concepts:"),
                                        tags$ul(
                                                tags$li("Corpus: The model is trained on a corpus, which is a collection of text documents. In this case, the corpus is used to capture the language patterns and frequencies."),
                                                tags$li("Tokenization: The input text is tokenized, meaning it is broken down into individual words or tokens. This step is essential for analyzing the sequential structure of the text."),
                                                tags$li("N-gram Models: N-grams are contiguous sequences of n items (typically words) from the text. The model uses the frequency of n-grams to predict the next word based on the context of the previous words."),
                                                tags$li("Stemming: Word stemming is applied to reduce words to their root form, ensuring that similar words are treated alike during prediction.")
                                        ),
                                        p("The prediction model analyzes the input context, considering the previous words, and predicts the most likely word to follow. It's designed to showcase text prediction capabilities using Shiny."),
                                        p("Feel free to experiment and see how the predictions change with different inputs.")
                                )
                        )
                )
        )
)

dashboardPage(header, sidebar, body)
