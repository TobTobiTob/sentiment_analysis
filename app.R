# app.R

# ------------------- #
#      LIBRARIES      #
# ------------------- #
library(shiny)
library(shinydashboard)
library(tidyverse)
library(syuzhet)
library(wordcloud)
library(DT)
library(tm)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)

# Also needed for old code:
library(sentimentr)
library(ggplot2)

# ------------------- #
#   HELPER FUNCTIONS  #
# ------------------- #

# Classify text into broad categories (for "Business Insights")
get_category <- function(text) {
  txt <- tolower(text)
  if (grepl("crew|staff|attendant|hospitality", txt)) {
    return("Crew Service")
  } else if (grepl("delay|late|timing|on time|early", txt)) {
    return("Flight Timing")
  } else if (grepl("food|meal|snack|beverage", txt)) {
    return("Food")
  } else if (grepl("seat|legroom|recline|comfort", txt)) {
    return("Seating")
  } else if (grepl("baggage|luggage|suitcase|lost bag", txt)) {
    return("Baggage")
  } else if (grepl("booking|reservation|check-in|boarding", txt)) {
    return("Booking/Check-in")
  } else if (grepl("entertainment|movie|screen|tv|wifi", txt)) {
    return("In-flight Entertainment")
  } else if (grepl("price|fare|cheap|expensive", txt)) {
    return("Pricing/Fares")
  } else if (grepl("support|service|help|call center|rude", txt)) {
    return("Customer Support")
  } else {
    return("Other")
  }
}

# ------------------- #
#         UI          #
# ------------------- #
ui <- dashboardPage(
  dashboardHeader(title = "Airline Tweet Sentiment Analyzer"),
  
  dashboardSidebar(
    fileInput("file", "Upload CSV File", accept = ".csv"),
    actionButton("analyze", "Analyze Sentiment"),
    br(),
    p("Your CSV must have columns named 'doc' and 'text'")
  ),
  
  dashboardBody(
    # We use a tabBox so each feature is in its own tab
    tabBox(
      id = "tabs", width = 12,
      
      # 1) Data Preview (old code approach)
      tabPanel("Data Preview",
               fluidRow(
                 box(
                   title = "Data Preview",
                   width = 12, status = "primary", solidHeader = TRUE,
                   DTOutput("preview_table")
                 )
               )
      ),
      
      # 2) Plot (old code: sentimentr bar plot)
      tabPanel("Plot (sentimentr)",
               fluidRow(
                 box(
                   title = "Average Sentiment per Document (sentimentr)",
                   width = 12, status = "info", solidHeader = TRUE,
                   plotOutput("plot_sentimentr")
                 )
               )
      ),
      
      # 3) Sentiment Scores (old code: sentimentr table)
      tabPanel("Sentiment Scores (sentimentr)",
               fluidRow(
                 box(
                   title = "Sentimentr Scores Table",
                   width = 12, status = "warning", solidHeader = TRUE,
                   DTOutput("sentimentr_scores_table")
                 )
               )
      ),
      
      # 4) Sentiment Table (new code: syuzhet)
      tabPanel("Sentiment Table (syuzhet)",
               fluidRow(
                 box(
                   title = HTML("Sentiment Table <i class='fa fa-info-circle' title='Table of sentiment scores for each tweet.'></i>"),
                   width = 12, status = "primary", solidHeader = TRUE,
                   DTOutput("sentiment_table")
                 )
               )
      ),
      
      # 5) Word Cloud (new code)
      tabPanel("Word Cloud",
               fluidRow(
                 box(
                   title = HTML("Word Cloud <i class='fa fa-info-circle' title='Top words from all tweets shown as a cloud.'></i>"),
                   width = 12, status = "warning", solidHeader = TRUE,
                   plotOutput("wordcloud")
                 )
               )
      ),
      
      # 6) Average Sentiment (new code: syuzhet)
      tabPanel("Average Sentiment (syuzhet)",
               fluidRow(
                 box(
                   title = HTML("Average Sentiment <i class='fa fa-info-circle' title='Average sentiment scores across all tweets.'></i>"),
                   width = 12, status = "success", solidHeader = TRUE,
                   plotOutput("sentiment_bar")
                 )
               )
      ),
      
      # 7) Sentiment by Category (new code)
      tabPanel("Sentiment by Category",
               fluidRow(
                 box(
                   title = HTML("Sentiment by Category <i class='fa fa-info-circle' title='Shows how different service categories are perceived.'></i>"),
                   width = 12, status = "info", solidHeader = TRUE,
                   DTOutput("category_sentiment")
                 )
               )
      ),
      
      # 8) Co-occurrence Graph (new code)
      tabPanel("Co-occurrence Graph",
               fluidRow(
                 box(
                   title = HTML("Co-occurrence Graph <i class='fa fa-info-circle' title='Shows which words often appear together in tweets.'></i>"),
                   width = 12, status = "danger", solidHeader = TRUE,
                   plotOutput("cooc_graph", height = "500px")
                 )
               )
      ),
      
      # 9) Business Insights (new code)
      tabPanel("Business Insights",
               fluidRow(
                 box(
                   title = HTML("Business Insights <i class='fa fa-info-circle' title='Summarized insights based on sentiment analysis.'></i>"),
                   width = 12, status = "success", solidHeader = TRUE,
                   verbatimTextOutput("insights")
                 )
               )
      )
    )
  )
)

# ------------------- #
#       SERVER        #
# ------------------- #
server <- function(input, output, session) {
  
  # -------------------------------------------------------------------
  #  A) READ DATA (common to both old and new code)
  # -------------------------------------------------------------------
  df <- reactive({
    req(input$file)
    # Using read_csv (tidyverse) for consistency with the new code
    read_csv(input$file$datapath)
  })
  
  # -------------------------------------------------------------------
  #  B) OLD CODE: sentimentr-based logic
  # -------------------------------------------------------------------
  
  # 1) sentimentr results
  sentimentr_results <- reactive({
    req(df())
    # We'll analyze the same 'text' column
    text_col <- df()$text
    # Calculate sentiment by document
    sentiment_by(text_col)
  })
  
  # 2) Data Preview
  output$preview_table <- renderDT({
    req(df())
    datatable(df(), options = list(scrollX = TRUE))
  })
  
  # 3) Plot with sentimentr
  output$plot_sentimentr <- renderPlot({
    scores <- sentimentr_results()
    ggplot(scores, aes(x = element_id, y = ave_sentiment)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(
        title = "Average Sentiment per Document (sentimentr)",
        x = "Document ID", y = "Average Sentiment"
      ) +
      theme_minimal()
  })
  
  # 4) Sentimentr Scores table
  output$sentimentr_scores_table <- renderDT({
    scores <- sentimentr_results()
    # Add a quick category (Positive/Negative) if needed
    scores$Sentiment_Category <- ifelse(scores$ave_sentiment > 0, "Positive", 
                                        ifelse(scores$ave_sentiment < 0, "Negative", "Neutral"))
    datatable(scores, options = list(scrollX = TRUE))
  })
  
  # -------------------------------------------------------------------
  #  C) NEW CODE: syuzhet-based logic (triggered by "Analyze Sentiment")
  # -------------------------------------------------------------------
  
  # Clean text for word cloud, co-occurrence, etc.
  clean_text <- reactive({
    req(df())
    text_vec <- tolower(df()$text)
    text_vec <- removePunctuation(text_vec)
    text_vec <- removeNumbers(text_vec)
    text_vec <- removeWords(text_vec, stopwords("en"))
    text_vec
  })
  
  # syuzhet-based sentiment scores, computed on button click
  sentiment_scores <- eventReactive(input$analyze, {
    req(df())
    clean_vec <- clean_text()
    # NRC sentiment from syuzhet
    sentiments <- get_nrc_sentiment(clean_vec)
    # Add neutral column (rowSums of the first 8 emotions == 0 => neutral = 1)
    sentiments$neutral <- ifelse(rowSums(sentiments[, 1:8]) == 0, 1, 0)
    # Bring in doc, text, and categories
    sentiments$doc <- df()$doc
    sentiments$text <- df()$text
    sentiments$category <- sapply(df()$text, get_category)
    sentiments
  })
  
  # 1) Sentiment Table
  output$sentiment_table <- renderDT({
    req(sentiment_scores())
    datatable(
      sentiment_scores()[, c("doc", "text", "category", "positive", "negative", "neutral")],
      options = list(scrollX = TRUE)
    )
  })
  
  # 2) Word Cloud
  output$wordcloud <- renderPlot({
    req(clean_text())
    words <- clean_text()
    corpus <- Corpus(VectorSource(words))
    dtm <- TermDocumentMatrix(corpus)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    wordcloud(words = d$word, freq = d$freq, min.freq = 2, colors = brewer.pal(8, "Dark2"))
  })
  
  # 3) Average Sentiment (barplot of mean positive/negative/neutral)
  output$sentiment_bar <- renderPlot({
    req(sentiment_scores())
    sentiments <- sentiment_scores()
    sentiment_avg <- sentiments %>%
      select(positive, negative, neutral) %>%
      summarise_all(mean)
    barplot(
      as.numeric(sentiment_avg),
      names.arg = colnames(sentiment_avg),
      col = c("forestgreen", "firebrick", "gray"),
      las = 2,
      main = "Average Sentiment Scores (syuzhet)"
    )
  })
  
  # 4) Sentiment by Category
  output$category_sentiment <- renderDT({
    req(sentiment_scores())
    sentiments <- sentiment_scores()
    sentiments_long <- sentiments %>%
      select(category, positive, negative, neutral) %>%
      pivot_longer(cols = c(positive, negative, neutral),
                   names_to = "sentiment", values_to = "value") %>%
      group_by(category, sentiment) %>%
      summarise(count = sum(value), .groups = "drop") %>%
      arrange(desc(count))
    datatable(sentiments_long, options = list(scrollX = TRUE))
  })
  
  # 5) Co-occurrence Graph
  output$cooc_graph <- renderPlot({
    req(df())
    tidy_df <- df() %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% stop_words$word,
             str_detect(word, "[a-z]"))
    
    # Pairwise counts for co-occurrence
    word_pairs <- tidy_df %>%
      pairwise_count(word, doc, sort = TRUE, upper = FALSE) %>%
      filter(n >= 10)
    
    if (nrow(word_pairs) == 0) {
      plot.new()
      title("No frequent co-occurrences found (n < 10).")
      return(NULL)
    }
    
    graph <- graph_from_data_frame(word_pairs)
    set.seed(123)
    ggraph(graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 4) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()
  })
  
  # 6) Business Insights
  output$insights <- renderPrint({
    req(sentiment_scores())
    sentiments <- sentiment_scores()
    
    # Most negative category
    most_negative_category <- sentiments %>%
      group_by(category) %>%
      summarise(neg_total = sum(negative), .groups = "drop") %>%
      arrange(desc(neg_total)) %>%
      slice(1) %>%
      pull(category)
    
    # Most positive category
    most_positive_category <- sentiments %>%
      group_by(category) %>%
      summarise(pos_total = sum(positive), .groups = "drop") %>%
      arrange(desc(pos_total)) %>%
      slice(1) %>%
      pull(category)
    
    cat("📌 Business Insights:\n")
    cat("- Most negatively perceived category: ", most_negative_category, "\n")
    cat("- Most positively perceived category: ", most_positive_category, "\n")
    cat("- Consider improving services in the '", most_negative_category, "' category to improve overall sentiment.\n")
  })
}

# ------------------- #
#      RUN APP        #
# ------------------- #
shinyApp(ui = ui, server = server)
