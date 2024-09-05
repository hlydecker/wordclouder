library(shiny)
library(wordcloud2)
library(tm)
library(colourpicker)
library(usydColours)
library(dplyr)
library(ggplot2)
library(tidytext)
library(scales)
library(RColorBrewer)

ui <- fluidPage(
  h1("Wordclouder"),
  tabsetPanel(
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
          conditionalPanel(
            condition = "input.source == 'file'",
            fileInput("file", "Select a file")
          ),
          hr(),
          checkboxInput("remove_words", "Remove specific words?", FALSE),
          conditionalPanel(
            condition = "input.remove_words == 1",
            textAreaInput("words_to_remove", "Words to remove (comma-separated)")
          ),
          hr(),
          numericInput("num", "Maximum number of words",
                       value = 25, min = 5
          ),
          hr(),
          checkboxInput("use_color_spectrum", "Use color spectrum based on word frequency", FALSE),
          conditionalPanel(
            condition = "input.use_color_spectrum == true",
            colourInput("low_freq_color", "Color for low frequency words", value = "#FF0000"),
            colourInput("high_freq_color", "Color for high frequency words", value = "#0000FF")
          ),
          conditionalPanel(
            condition = "input.use_color_spectrum == false",
            colourInput("word_col", "Word color", value = "#000000")
          ),
          colourInput("back_col", "Background color", value = "white"),
          hr(),
          HTML('<p>Request a feature or report a bug on <a href="https://github.com/hlydecker/wordclouder/issues">GitHub</a> or view the <a href="https://github.com/hlydecker/wordclouder">source code</a></p>')
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          br(),
          conditionalPanel(
            condition = "input.use_color_spectrum == true",
            plotOutput("color_legend", height = "100px")
          ),
          br()
        )
      )
    ),
    tabPanel(
      title = "Word Statistics",
      sidebarLayout(
        sidebarPanel(
          numericInput("top_n", "Number of top words to display", value = 10, min = 5, max = 50)
        ),
        mainPanel(
          plotOutput("freq_plot"),
          br(),
          plotOutput("sentiment_plot"),
          br(),
          plotOutput("sentiment_pie", height = "400px")
        )
      )
    ),
    tabPanel(
      title = "Sentiment Word Clouds",
      sidebarLayout(
        sidebarPanel(
          numericInput("sentiment_num", "Maximum number of words per cloud",
                       value = 25, min = 5
          ),
          colourInput("positive_col", "Positive words color", value = "#00FF00"),
          colourInput("negative_col", "Negative words color", value = "#FF0000")
        ),
        mainPanel(
          fluidRow(
            column(6,
                   h3("Positive Sentiment", align = "center"),
                   wordcloud2Output("positive_cloud")
            ),
            column(6,
                   h3("Negative Sentiment", align = "center"),
                   wordcloud2Output("negative_cloud")
            )
          )
        )
      )
    ),
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
      HTML('<p>Request a feature or report a bug on <a href="https://github.com/hlydecker/wordclouder/issues">GitHub</a> or view the <a href="https://github.com/hlydecker/wordclouder">source code</a>. Based off code developed by <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>'),
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

  process_text <- reactive({
    data <- data_source()
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
    data.frame(word = names(data), freq = as.numeric(data))
  })

  create_wordcloud <- function(data, num_words = 25, background = "white", word_color = "#000000", use_spectrum = FALSE, low_color = "#FF0000", high_color = "#0000FF") {
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }

    if (use_spectrum) {
      color_palette <- colorRampPalette(c(low_color, high_color))(100)
      colors <- color_palette[findInterval(data$freq, seq(min(data$freq), max(data$freq), length.out = 100))]
      wordcloud2(data,
                 backgroundColor = background,
                 minRotation = 0,
                 maxRotation = 0,
                 rotateRatio = 0,
                 color = colors
      )
    } else {
      wordcloud2(data,
                 backgroundColor = background,
                 minRotation = 0,
                 maxRotation = 0,
                 rotateRatio = 0,
                 color = word_color
      )
    }
  }

  output$cloud <- renderWordcloud2({
    data <- process_text()
    create_wordcloud(data,
                     num_words = input$num,
                     background = input$back_col,
                     word_color = if(input$use_color_spectrum) NULL else input$word_col,
                     use_spectrum = input$use_color_spectrum,
                     low_color = input$low_freq_color,
                     high_color = input$high_freq_color
    )
  })

  output$color_legend <- renderPlot({
    if (input$use_color_spectrum) {
      color_palette <- colorRampPalette(c(input$low_freq_color, input$high_freq_color))(100)
      par(mar = c(2, 0, 2, 0))  # Adjust margins
      image(1:100, 1, as.matrix(1:100), col = color_palette,
            xlab = "", ylab = "", xaxt = "n", yaxt = "n",
            main = "Word Frequency Color Legend")
      axis(1, at = c(1, 50, 100), labels = c("Low", "Medium", "High"))
    }
  })

  output$freq_plot <- renderPlot({
    data <- process_text()
    top_words <- head(data, n = input$top_n)
    ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
      geom_col(fill = "#4E79A7") +
      coord_flip() +
      labs(x = "Word", y = "Frequency", title = "Top Words Frequency") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })

  sentiment_data <- reactive({
    data <- process_text()
    data %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(sentiment, word, wt = freq, sort = TRUE)
  })

  output$sentiment_plot <- renderPlot({
    sentiment <- sentiment_data() %>%
      group_by(sentiment) %>%
      top_n(input$top_n) %>%
      ungroup() %>%
      mutate(word = reorder(word, n))

    ggplot(sentiment, aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(title = "Top Words by Sentiment",
           y = "Frequency",
           x = NULL) +
      coord_flip() +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_manual(values = c("positive" = "#4DAF4A", "negative" = "#E41A1C")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })

  output$sentiment_pie <- renderPlot({
    sentiment_summary <- sentiment_data() %>%
      group_by(sentiment) %>%
      summarise(total = sum(n)) %>%
      mutate(percentage = total / sum(total) * 100)

    ggplot(sentiment_summary, aes(x = "", y = percentage, fill = sentiment)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("positive" = "#4DAF4A", "negative" = "#E41A1C")) +
      labs(title = "Sentiment Distribution",
           fill = "Sentiment",
           x = NULL,
           y = NULL) +
      geom_text(aes(label = sprintf("%.1f%%", percentage)),
                position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
  })

  sentiment_clouds <- reactive({
    data <- sentiment_data()

    positive_words <- data %>%
      filter(sentiment == "positive") %>%
      arrange(desc(n)) %>%
      head(input$sentiment_num)

    negative_words <- data %>%
      filter(sentiment == "negative") %>%
      arrange(desc(n)) %>%
      head(input$sentiment_num)

    list(positive = positive_words, negative = negative_words)
  })

  output$positive_cloud <- renderWordcloud2({
    clouds <- sentiment_clouds()
    if(nrow(clouds$positive) > 0) {
      create_wordcloud(clouds$positive, num_words = input$sentiment_num, word_color = input$positive_col)
    }
  })

  output$negative_cloud <- renderWordcloud2({
    clouds <- sentiment_clouds()
    if(nrow(clouds$negative) > 0) {
      create_wordcloud(clouds$negative, num_words = input$sentiment_num, word_color = input$negative_col)
    }
  })
}

shinyApp(ui = ui, server = server)
