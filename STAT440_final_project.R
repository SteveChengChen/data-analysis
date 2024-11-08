library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(DT)
library(purrr)

# Global setup
api_key <- "84630d4b"  # Replace with your actual IMDb API key

# Define UI
ui <- fluidPage(
  titlePanel("Movie Database Search with IMDb"),
  tabsetPanel(
    tabPanel("Search Movies",
             sidebarLayout(
               sidebarPanel(
                 textInput("searchTerm", "Search Movie:", value = "Spider-Man"),
                 actionButton("searchButton", "Search")
               ),
               mainPanel(
                 DTOutput("results"),
                 uiOutput("summaryStatistic"),
                 plotOutput("ratingPlot")
               )
             )
    )
)
)

# Define server logic
server <- function(input, output) {
  movieData <- eventReactive(input$searchButton, {
    searchTermEncoded <- URLencode(input$searchTerm, reserved = TRUE)
    url <- paste0("http://www.omdbapi.com/?s=", searchTermEncoded, "&type=movie&apikey=", api_key)
    response <- GET(url)
    content <- content(response, as = "text")
    parsed_content <- fromJSON(content)
    
    if ("Search" %in% names(parsed_content)) {
      basic_movies_info <- data.frame(
        Title = parsed_content$Search$Title,
        Year = parsed_content$Search$Year,
        imdbID = parsed_content$Search$imdbID,
        Type = parsed_content$Search$Type,
        stringsAsFactors = FALSE
      )
      
      imdb_scores <- map_chr(basic_movies_info$imdbID, function(imdb_id) {
        detail_url <- paste0("http://www.omdbapi.com/?i=", imdb_id, "&apikey=", api_key)
        detail_response <- GET(detail_url)
        detail_content <- content(detail_response, as = "text")
        detail_parsed <- fromJSON(detail_content)
        return(detail_parsed$imdbRating %||% NA)
      })
      
      basic_movies_info$imdbRating <- as.numeric(imdb_scores)
      
      # Sort the data frame by IMDb rating in descending order
      sorted_movies_info <- basic_movies_info[order(-as.numeric(basic_movies_info$Year)), ]
      
      
      return(sorted_movies_info)
    } else {
      return(data.frame())
    }
  })
  
  output$results <- renderDT({
    datatable(movieData(), 
              options = list(
                lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All')), 
                pageLength = 5,  # Adjust the number of rows per page as needed
                autoWidth = TRUE,
                order = list(list(2, 'desc'))  # Default sorting: 1st index is column index (0-based), 'desc' for descending
              ),
              rownames = FALSE)  # Disable row names
  }, server = FALSE)  # server = FALSE for processing in the client-side
  
  output$ratingPlot <- renderPlot({
    data <- movieData()
    if (nrow(data) > 0) {
      # Create a new column combining Title and Year
      data$TitleWithYear <- paste(data$Title, "(", data$Year, ")", sep = "")
      
      ggplot(data, aes(x = TitleWithYear, y = imdbRating, fill = imdbRating)) +
        geom_col() +
        scale_fill_gradient2(low = "darkred", mid = "grey", high = "darkgreen", midpoint = 5) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Movie (Year)", y = "IMDb Rating", title = "IMDb Ratings of Movies")
    }
  })
  
  output$summaryStatistic <- renderUI({
    data <- movieData()
    if (nrow(data) > 0) {
      average_rating <- mean(data$imdbRating, na.rm = TRUE)
      
      rating_color <- ifelse(average_rating >= 7, "green", 
                             ifelse(average_rating >= 5, "orange", "red"))
      HTML(paste0("<span style='color:", rating_color, "; font-weight: bold; '>Overall Average Rating: ", 
                  sprintf("%.2f", average_rating), "</span>"))
    } else {
      return(NULL)
    }
  })
  
 
}

# Run the application
shinyApp(ui = ui, server = server)
