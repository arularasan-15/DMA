library(shiny)
library(dplyr)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)

# Sample movie data
movies <- data.frame(
  title = c("sita ramam", "kakki sattai", "viduthalai", "pichaikaran", 
            "vikram", "bakiyalakshmi","paasamalar","Amaran","sivakasi","Muthuku Muthaga"),
  genre = c("Romance", "Action", "Action", "Sentiment", 
            "Action", "Sentiment","Sentiment","Romance","Sentiment","Sentiment"),
  rating = c(8,9,6,7,5,4,3,2,1,0),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #f9d423, #ff4e50);
        color: #333333;
      }
      .sidebar {
        background: rgba(255, 255, 255, 0.85);
        border-radius: 15px;
        padding: 20px;
        box-shadow: 2px 2px 12px rgba(0,0,0,0.2);
      }
      .main-panel {
        background: rgba(255, 255, 255, 0.9);
        border-radius: 15px;
        padding: 20px;
        box-shadow: 2px 2px 12px rgba(0,0,0,0.3);
      }
      #rotating_text {
        font-size: 28px;
        font-weight: bold;
        color: #fff;
        text-shadow: 2px 2px 5px #000;
        animation: colorChange 4s infinite;
      }
      @keyframes colorChange {
        0% { color: #ff0000; }
        25% { color: #00ff00; }
        50% { color: #0000ff; }
        75% { color: #ff00ff; }
        100% { color: #ff0000; }
      }
    "))
  ),
  titlePanel(tags$h1("🎬 Movie Recommendation System", 
                     style = "color:white; text-shadow:2px 2px 5px #000;")),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("genre", "Select Genre:",
                  choices = c("All", unique(movies$genre)), selected = "All"),
      selectInput("min_rating", "Minimum Rating:",
                  choices = as.character(1:10)),
      selectizeInput("fav_movie", "Your Favorite Movie (optional):",
                     choices = c("", movies$title),
                     selected = "",
                     options = list(placeholder = 'Type or select a movie'))
    ),
    mainPanel(
      class = "main-panel",
      h3("Recommended Movies (Rotating)", style = "color:#333;"),
      withSpinner(uiOutput("rotating_recommendation"), type = 6, color = "#FF5733")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive recommendations (auto-update when inputs change)
  recommended_titles <- reactive({
    min_rating_num <- as.numeric(input$min_rating)
    
    filtered <- movies %>%
      filter((genre == input$genre | input$genre == "All") & rating >= min_rating_num)
    
    if (input$fav_movie != "" && input$fav_movie %in% movies$title) {
      fav_genre <- movies$genre[movies$title == input$fav_movie]
      filtered <- filtered %>% filter(genre == fav_genre)
    }
    
    recs <- filtered %>%
      arrange(desc(rating)) %>%
      head(5) %>%
      pull(title)
    
    if (length(recs) == 0) {
      return("No movies found matching criteria.")
    } else {
      return(recs)
    }
  })
  
  # Rotating display logic
  output$rotating_recommendation <- renderUI({
    recs <- recommended_titles()
    
    if (length(recs) == 1 && recs == "No movies found matching criteria.") {
      tags$div(style = "font-size:20px; font-weight:bold; color: red;", recs)
    } else {
      tags$div(
        id = "rotating_text",
        style = "font-size:24px; font-weight:bold;",
        recs[1]
      )
    }
  })
  
  # JavaScript to rotate text every 2 seconds
  observe({
    recs <- recommended_titles()
    if (length(recs) > 1) {
      session$sendCustomMessage(type = 'startRotation', message = recs)
    }
  })
}

jsCode <- "
Shiny.addCustomMessageHandler('startRotation', function(movies) {
  var index = 0;
  var element = document.getElementById('rotating_text');
  if (!element) return;
  clearInterval(window.rotationInterval);
  window.rotationInterval = setInterval(function() {
    index = (index + 1) % movies.length;
    element.innerHTML = movies[index];
  }, 2000);
});
"

shinyApp(ui, server, onStart = function() {
  shinyjs::extendShinyjs(text = jsCode, functions = c())
})
