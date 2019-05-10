library(tidyverse)
library(shiny)
library(pryr)

# Load functions
source("functions.R")

# Load inputdata
load("./data/inputData.RData", envir=.GlobalEnv)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Translate text to DNA"),
   
   # Sidebar with a text input and option to translate to text or DNA.
   sidebarLayout(
      sidebarPanel(
        width = 5,
        
        # Text input area
        textAreaInput(inputId = "textIn", label = "Input Text", width = "100%", placeholder = "Enter your text"),
        
        # Translate to button
        radioButtons(inputId = "choice", label = "Translate into", choices = c("DNA" = "dna", "Plain text" = "text")),
        
        # Submit button
        actionButton(inputId = "button", label = "Translate")
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        width = 7,
        verbatimTextOutput(outputId = "out")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  inputText <- eventReactive(input$button, {
    
    validate(
      # Make sure that text is not empty
      need(input$textIn != "", "Please provide an input"),
      # Make sure that input is no longer than 500 characters for text to DNA and 1000 from DNA to text
      if_else(input$choice == "dna",
              need(nchar(input$textIn) <= 500, "Please do not enter more than 500 characters"),
              need(nchar(input$textIn) <= 2500, "Please do not enter more than 1000 DNA letters")),
      # If going from DNA to text, validate whether only DNA is enterd
      if(input$choice == "text"){
        # Trim whitespaces in DNA input and
        # Check if there are other characters than ACTG
        need(str_detect( input$textIn %>% str_remove_all("[ \n]"), "^[ATCG]*$"), "Please enter DNA only")}
    )
    
    as.character(input$textIn)
    
  })
  
  # Main output
  output$out <- renderText({

    if (isolate(input$choice == "dna")) {
    # From text to DNA
      S0_to_S1(inputText()) %>%
      S1_to_S5()
      
    } else {
  
    # From DNA to text
      inputText() %>% 
        str_remove_all("[ \n]") %>% 
        S5_to_S1() %>%
        S1_to_S0()
    }
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

