# Load Libraries
library(shiny)
library(dplyr)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)
library(plotly)
library(pROC)

# 🎬 Tamil Movie Dataset
movies <- data.frame(
  title = c("Sita Ramam", "Kakki Sattai", "Viduthalai", "Pichaikaran",
            "Vikram", "Bakiyalakshmi", "Paasamalar", "Amaran",
            "Sivakasi", "Muthuku Muthaga", "Master", "Kaithi", 
            "Asuran", "Thunivu", "Varisu", "Ponniyin Selvan 1",
            "Ponniyin Selvan 2", "Maamannan", "Soorarai Pottru", "Karnan",
            "Leo", "Jailer", "Vettaiyaadu Vilaiyaadu", "Aayirathil Oruvan",
            "Theri", "96", "Love Today", "Jigarthanda", "Pariyerum Perumal", "Super Deluxe"),
  genre = c("Romance","Action","Action","Sentiment","Action","Sentiment","Sentiment","Romance",
            "Sentiment","Sentiment","Action","Action","Drama","Action","Drama",
            "Historical","Historical","Drama","Drama","Action",
            "Action","Action","Thriller","Adventure","Action","Romance","Drama","Crime","Drama","Comedy"),
  rating = c(8,9,6,7,9,5,8,6,5,7,9,8,9,8,7,9,8,7,10,9,8,9,8,7,8,9,8,8,9,9),
  stringsAsFactors = FALSE
)

# 🌈 UI
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(-45deg, #ff6ec7, #ffcc00, #00c9ff, #92fe9d);
        background-size: 400% 400%;
        animation: gradientBG 15s ease infinite;
        color: #333;
        font-family: 'Poppins', sans-serif;
      }
      @keyframes gradientBG {
        0% { background-position: 0% 50%; }
        50% { background-position: 100% 50%; }
        100% { background-position: 0% 50%; }
      }
      h1 {
        font-weight: 800;
        color: white;
        text-align: center;
        text-shadow: 2px 2px 8px #000;
      }
      .sidebar {
        background: rgba(255,255,255,0.85);
        border-radius: 20px;
        padding: 25px;
        box-shadow: 0 0 25px rgba(255,255,255,0.5);
      }
      .main-panel {
        background: rgba(255,255,255,0.9);
        border-radius: 20px;
        padding: 30px;
        box-shadow: 0 0 25px rgba(0,0,0,0.3);
      }
      #rotating_text {
        font-size: 30px;
        font-weight: bold;
        color: #ff3366;
        text-align: center;
        text-shadow: 0 0 10px #fff;
        animation: colorShift 3s infinite alternate;
      }
      @keyframes colorShift {
        0% { color: #ff3366; }
        25% { color: #ff9900; }
        50% { color: #33cc33; }
        75% { color: #3399ff; }
        100% { color: #cc33ff; }
      }
      .movie-card {
        background: linear-gradient(135deg, rgba(255,255,255,0.9), rgba(255,255,255,0.7));
        border-radius: 15px;
        padding: 15px;
        margin: 10px;
        text-align: center;
        box-shadow: 0 0 20px rgba(0,0,0,0.2);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
      }
      .movie-card:hover {
        transform: scale(1.07);
        box-shadow: 0 0 30px rgba(255,105,180,0.7);
      }
    "))
  ),
  
  titlePanel(tags$h1("🎬 Tamil Movie Recommendation System")),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("genre", "🎭 Select Genre:",
                  choices = c("All", unique(movies$genre)), selected = "All"),
      selectInput("min_rating", "⭐ Minimum Rating:",
                  choices = as.character(1:10), selected = "5"),
      selectizeInput("fav_movie", "🎞️ Your Favorite Movie (optional):",
                     choices = c("", movies$title),
                     selected = "",
                     options = list(placeholder = 'Type or select a movie'))
    ),
    
    mainPanel(
      class = "main-panel",
      h3("🌟 Recommended Movies", style = "text-align:center; color:#ff6600;"),
      withSpinner(uiOutput("rotating_recommendation"), type = 6, color = "#ff33cc"),
      uiOutput("movie_cards"),
      br(),
      h3("📊 Ratings by Genre", style = "text-align:center; color:#ff6600;"),
      withSpinner(plotlyOutput("rating_plot"), type = 6, color = "#33ccff"),
      br(),
      h3("🥧 Genre Distribution (Pie Chart)", style = "text-align:center; color:#ff6600;"),
      withSpinner(plotlyOutput("genre_pie"), type = 6, color = "#ff9900"),
      br(),
      h3("📈 ROC Curve (Demo)", style = "text-align:center; color:#ff6600;"),
      withSpinner(plotlyOutput("roc_curve"), type = 6, color = "#33cc33")
    )
  )
)

# ⚙️ Server Logic
server <- function(input, output, session) {
  
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
      head(6)
    
    if (nrow(recs) == 0) {
      return(data.frame(title = "No movies found matching criteria.", genre = "", rating = NA))
    } else {
      return(recs)
    }
  })
  
  # Rotating text
  output$rotating_recommendation <- renderUI({
    recs <- recommended_titles()
    if (nrow(recs) == 1 && recs$title[1] == "No movies found matching criteria.") {
      tags$div(style = "font-size:20px; font-weight:bold; color:red;", recs$title[1])
    } else {
      tags$div(id = "rotating_text", recs$title[1])
    }
  })
  
  # Display cards
  output$movie_cards <- renderUI({
    recs <- recommended_titles()
    if (nrow(recs) > 0 && recs$title[1] != "No movies found matching criteria.") {
      tagList(
        lapply(1:nrow(recs), function(i) {
          tags$div(class = "movie-card",
                   tags$h4(style="color:#ff3366;", recs$title[i]),
                   tags$p(paste("🎭 Genre:", recs$genre[i])),
                   tags$p(paste("⭐ Rating:", recs$rating[i])))
        })
      )
    }
  })
  
  # Rotation
  observe({
    recs <- recommended_titles()$title
    if (length(recs) > 1) {
      session$sendCustomMessage(type = 'startRotation', message = recs)
    }
  })
  
  # Bar Plot (Ratings by Genre)
  output$rating_plot <- renderPlotly({
    g <- movies %>%
      group_by(genre) %>%
      summarise(avg_rating = mean(rating)) %>%
      ggplot(aes(x = reorder(genre, avg_rating), y = avg_rating, fill = genre)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      geom_text(aes(label = round(avg_rating, 1)), hjust = -0.3, color = "black", size = 5) +
      theme_minimal(base_size = 15) +
      labs(x = "", y = "Average Rating") +
      ylim(0, 10)
    ggplotly(g)
  })
  
  # Pie Chart (Genre Distribution)
  output$genre_pie <- renderPlotly({
    pie_data <- movies %>%
      count(genre)
    plot_ly(pie_data, labels = ~genre, values = ~n, type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial') %>%
      layout(showlegend = TRUE)
  })
  
  # ROC Curve (Demo)
  output$roc_curve <- renderPlotly({
    # Simulate true labels & predicted scores
    set.seed(123)
    true_labels <- ifelse(movies$rating > 7, 1, 0)
    predicted_scores <- (movies$rating + rnorm(length(true_labels), 0, 1)) / 10
    predicted_scores <- pmin(pmax(predicted_scores, 0), 1)
    
    roc_obj <- roc(true_labels, predicted_scores)
    df <- data.frame(
      FPR = 1 - roc_obj$specificities,
      TPR = roc_obj$sensitivities
    )
    
    plot_ly(df, x = ~FPR, y = ~TPR, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#ff3366', width = 3)) %>%
      layout(title = "ROC Curve (Demo)",
             xaxis = list(title = "False Positive Rate"),
             yaxis = list(title = "True Positive Rate"),
             shapes = list(
               list(type = "line", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
                    line = list(dash = 'dot', color = 'gray'))
             ))
  })
}

# JS Rotation Code
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

# Launch App
shinyApp(ui, server, onStart = function() {
  shinyjs::extendShinyjs(text = jsCode, functions = c())
})
