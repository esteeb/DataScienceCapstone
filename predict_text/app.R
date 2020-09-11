#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(stringr)

# Load model data 
bi <- readRDS("./data/modelBigrams.rds")
tri <- readRDS("./data/modelTrigrams.rds")
quad <- readRDS("./data/modelQuadgrams.rds")


replace_regex <- "[^[:alpha:][:space:]]*"

# Bigram matching function
bigram_match <- function(input_split){
  len <- length(input_split)
  out <- bi%>%
    filter(word1 == input_split[len])%>%
    select(word2)%>%
    slice_head()%>%
    as.character()
  ifelse(out == "character(0)", "???", 
         return(out[1]))
}

trigram_match <- function(input_split) {
  len <- length(input_split)
  out <- tri%>%
    filter(word1 == input_split[len-1],
           word2 == input_split[len])%>%
    select(word3)%>%
    slice_head()%>%
    as.character()
  ifelse(out == "character(0)",
         bigram_match(input_split),
         return(out[1]))
}


quadgram_match <- function(input_split){
  len <- length(input_split)
  out <- quad%>%
    filter(word1 == input_split[len-2],
           word2 == input_split[len-1],
           word3 == input_split[len])%>%
    select(word4)%>%
    slice_head()%>%
    as.character()
  ifelse(out == "character(0)", 
         trigram_match(input_split),
         return(out[1]))
}

word_predict <- function(input){
  
  input <- as.data.frame(input)
  colnames(input) <- c("text")
  input <- input%>%
    mutate(text = str_replace_all(text, replace_regex, ""))
  
  # Get word count
  input_count <- str_count(input, boundary("word"))
  
  # Split input into individual words
  input_split <- tolower(unlist(str_split(input, boundary("word"))))
  
  # Use matching functions on split input
  predicted <- ifelse(input_count == 0, "Type in a word or phrase!",
                      ifelse(input_count >= 3, quadgram_match(input_split),
                             ifelse(input_count == 2, trigram_match(input_split),
                                    # Handle special case to predict "the" as the word after "of"
                                    ifelse(input_split[length(input_split)] == "of", "the",bigram_match(input_split)))))
  
  # Output
  return(predicted)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Word Prediction Model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Instructions"),
            h4("Enter a word or phrase in the text box on the right"),
            h4("The predicted word will be printed below."),
            h4("A series of question marks indicate no prediction"),
            a("Source Code", href = "https://github.com/esteeb")
            
        )
        ,

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("predict",
                         textInput("user_input", h3("Your Input:"), 
                                   value = ""),
                         h3("Predicted Next Word:"),
                         h4(em(span(textOutput("ngram_output"), style="color:blue"))))
        )
    )
)
)

# Define server logic for the prediction model
server <- function(input, output) {

    output$ngram_output <- renderText({
      word_predict(input$user_input)  
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
