
library(shiny)
# install.packages("wordcloud2")
library(wordcloud2)
# install.packages("tm")
library(tm)
# install.packages("colourpicker")
library(colourpicker)
library(usydColours)

ui <- fluidPage(
  h1("Wordclouder"),
  # Create a container for tab panels
  tabsetPanel(
    # Create a "Word cloud" tab
    tabPanel(
      title = "Word cloud",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "source",
            label = "Word source",
            choices = c(
              "Use your own words" = "own",
              "Upload a file" = "file"
            )
          ),
          hr(),
          # Add the selector for the language of the text
          selectInput(
            inputId = "language",
            label = "Remove stopwords in",
            choices = c("Danish", "Dutch", "English", "Finnish", "French", "German", "Hungarian", "Italian", "Norwegian", "Portuguese", "Russian", "Spanish", "Swedish"),
            multiple = FALSE,
            selected = "English"
          ),
          conditionalPanel(
            condition = "input.source == 'own'",
            textAreaInput("text", "Enter text", rows = 7)
          ),
          # Wrap the file input in a conditional panel
          conditionalPanel(
            # The condition should be that the user selects
            # "file" from the radio buttons
            condition = "input.source == 'file'",
            fileInput("file", "Select a file")
          ),
          hr(),
          checkboxInput("remove_words", "Remove specific words?", FALSE),
          conditionalPanel(
            condition = "input.remove_words == 1",
            textAreaInput("words_to_remove", "Words to remove (comma-seperated)")
          ),
          hr(),
          numericInput("num", "Maximum number of words",
                       value = 25, min = 5
          ),
          hr(),
          # WIP: Select Word cloud colour palette. Default is black
          # selectInput(
          #   inputId = "pal",
          #   label = "Select word colour(s)",
          #   choices = c("All Black","Flametree","Harbour","Jacaranda"),
          #   multiple = FALSE,
          #   selected = "All Black"
          # ),
          hr(),
          colourInput("word_col", "Word color", value = "#000000"),
          colourInput("back_col", "Background color", value = "white"),
          hr(),
          HTML('<p>Report a <a href="https://github.com/hlydecker/wordclouder/issues">bug</a> or view the <a href="https://github.com/hlydecker/wordclouder">source code</a>.</p>')
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          # br(),
          # br(),
          br(),
          br()
        )
      )
    ),
    # Create an "About this app" tab
    tabPanel(
      title = "About this app",
      br(),
      "Instructions on how to use this Shiny app:",
      br(),
      br(),
      HTML("<ul><li>When uploading a file, make sure to upload a .csv or .txt file</li>
       <li>If it is a .csv file, there should be only one column containing all words or sentences (see below for example files)</li>
       <li>Numbers and punctuations will be automatically removed, as well as stop words in the language of your choice (via the dropdown selector)</li></ul>"),
      br(),
      br(),
      br(),
      HTML('<p>Report a <a href="https://github.com/hlydecker/wordclouder/issues">bug</a> or view the <a href="https://github.com/hlydecker/wordclouder">source code</a>. Based off code developed by <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>'),
      br(),
      br()
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })

  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })

  # WIP: word colur selection
  selected_pal <- reactive({
    if (input$pal == "All Black"){
      "Black"
    } else if (input$pal == "Flametree"){
      usyd_palette("flametree")
    }
  })

  create_wordcloud <- function(data, num_words = 25, background = "white", word_color = "#000000") {

    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      if (input$remove_words) {
        custom_stopwords <- unlist(strsplit(input$words_to_remove, ",\\s*"))
        corpus <- tm_map(corpus, removeWords, custom_stopwords)
      }
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    }

    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }

    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }

    wordcloud2(data,
               backgroundColor = background,
               minRotation = 0,
               maxRotation = 0,
               rotateRatio = 0,
               color = word_color
    )
  }
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$back_col,
                     word_color = input$word_col
    )
  })
}

shinyApp(ui = ui, server = server)
