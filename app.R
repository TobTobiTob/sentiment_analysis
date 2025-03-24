# app.R

library(shiny)
library(sentimentr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sentiment Analysis of Review Comments"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      textInput("column", "Column Name for Reviews", "review"),
      helpText("Make sure your CSV file contains a text column with review comments.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("sentPlot")
        ),
        tabPanel("Sentiment Scores",
                 tableOutput("sentTable")
        ),
        tabPanel("Data Preview",
                 tableOutput("headTable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to read the CSV file
  reviewsData <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    validate(
      need(input$column %in% names(df), "The specified column does not exist in the uploaded CSV.")
    )
    df
  })
  
  # Reactive expression to compute sentiment using the sentimentr package
  sentimentResults <- reactive({
    df <- reviewsData()
    reviews <- as.character(df[[input$column]])
    sentiment_by(reviews)
  })
  
  # Show a preview of the uploaded data (first 10 rows)
  output$headTable <- renderTable({
    head(reviewsData(), 10)
  })
  
  # Render a plot of the average sentiment per review
  output$sentPlot <- renderPlot({
    scores <- sentimentResults()
    ggplot(scores, aes(x = element_id, y = ave_sentiment)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Average Sentiment per Review",
           x = "Review Number", y = "Average Sentiment") +
      theme_minimal()
  })
  
  # Render a table with sentiment scores and a sentiment category
  output$sentTable <- renderTable({
    scores <- sentimentResults()
    # Categorize each review as Positive or Negative
    scores$Sentiment_Category <- ifelse(scores$ave_sentiment > 0, "Positive", "Negative")
    scores
  })
}

shinyApp(ui, server)
