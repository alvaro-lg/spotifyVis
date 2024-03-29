library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(ggvis)
library(stringr)
source("helper.R")

# Auxiliary function
axis_vars <- c(
  "In Spotify playlist" = "in_spotify_playlists",
  "In Apple playlist" = "in_apple_playlists",
  "In Deezer playlist" = "in_deezer_playlists",
  "Charts" = "in_spotify_charts",
  "Streams" = "streams",
  "Danceability" = "danceability_.",
  "Valence" = "valence_.",
  "Energy" = "energy_.",
  "Acousticness" = "acousticness_.",
  "Instrumentalness" = "instrumentalness_.",
  "Liveness" = "liveness_.",
  "Speechiness" = "speechiness_."
)

TOP_X_OPTIONS <- c("TOP10"=10, "TOP20"=20, "TOP50"=50, "TOP100"=100, "TOP250"=250,"TOP500"=500, "ALL"=1000)
MEDIUM_OPTIONS <- c("streams", "in_spotify_charts", "in_spotify_playlists", "in_apple_charts", "in_apple_playlists", "in_deezer_charts", "in_deezer_playlists", "in_shazam_charts")
ATTR_OPTIONS <- c("danceability"="danceability_.", "valence"="valence_.", "energy"="energy_.", "acousticness"="acousticness_.", "instrumentalness"="instrumentalness_.", "liveness"="liveness_.", "speechiness"="speechiness_." )
TIME_OPTIONS <- c("season"="season", "year"="released_year", "month"="released_month")
SEASONS <- c("Spring", "Summer", "Autumn", "Winter")
MONTHS <- c("Jan"=1, "feb"=2, "Mar"=3, "Apr"=4, "May"=5, "Jun"=6, "Jul"=7, "Aug"=8, "Sep"=9, "Oct"=10, "Nov"=11, "Dec"=12)


# Define UI ----
ui <- fluidPage(
  titlePanel("spotifyVis"),
  tabsetPanel(
    tabPanel("Release Date VS Other Parameters", fluid = TRUE,
       sidebarLayout(
         sidebarPanel(
           h3("Release Date VS Other Parameters"),
           helpText("Compare how release date affects songs' parameters"),
           selectInput("medium", "Filter based on:", MEDIUM_OPTIONS),
           selectInput("top_x", "Select:", TOP_X_OPTIONS),
           selectInput("attr", "Parameter:", ATTR_OPTIONS),
           selectInput("time_option", "Group by:", TIME_OPTIONS),
         ),
         mainPanel(plotOutput("releaseDatePlot"))
       )
    ),
    tabPanel("Outlier Exploration", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          h3("Explore Outliers."),
          helpText("Filter the dataset, pick a song and compare its characteristics to the rest of them."),
          sliderInput("num_artists", "Maximum number of artists", min = 1, 
                      max = 8, value = 8),
          sliderInput("release_year", "Year of release", min = 1930, max = 2023, 
                      value = c(1930,2023)),
          textInput("search_artist", "Search by artist (e.g. \"The Weeknd\")"),
          textInput("search_song", "Search song (e.g. \"Circles\")"),
          selectInput("xvar", "X-axis variable", axis_vars, 
                      selected = "in_spotify_playlists"),
          selectInput("yvar", "Y-axis variable", axis_vars, 
                      selected = "streams"),
          checkboxInput("add_regression", "Add Regression Line", value = FALSE)
        ),
        mainPanel(
          ggvisOutput("outlierPlot"),
        )
      )
    ),
    tabPanel("Ocurrences on Playlists", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          h3("Song Ocurrences on Playlists"),
          helpText("Compare the ocurrences on playlists by platform and relase date 
                  periods"),
          checkboxGroupInput("checkGroup",
                            label = "Select platforms to compare",
                            choices = list("Spotify" = 1, 
                                           "Apple Music" = 2, 
                                           "Deezer" = 3),
                            selected = c(1, 2)
          ),
          dateRangeInput(
             "dateRange", label = "Select Period", start = "2018-01-01", 
             end = "2023-01-01"
          ),
          selectInput(
             "timeGroup", label = "Group by", choices = c("Month", "Year"), 
             selected = "Month"
          ),
          h4("Other settings"),
          checkboxInput(
            "logScale", label = "Logarithmic Scale (y-axis)", value = TRUE
          ),
          checkboxInput(
            "songMean", label = "Compute Mean", value = FALSE
          )
        ),
        mainPanel(
          plotOutput("playlistsPlot")
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  observe({
    # Validation to ensure at least one checkbox is selected
    if(length(input$checkGroup) < 1) {
      updateCheckboxGroupInput(session, "checkGroup", selected=1)
    }
  })
  
  spotify_data <- reactive({
    # Reading data
    file_path <- "data/spotify-2023.csv"
    data <- read.csv(file_path, encoding='ISO-8859-1') %>% 
      mutate(
        ID = 1:n(),
        fill = "black",
        size = 10,
        shape = "circle"
      )
    
    # Light data pre-processing
    data$combined_date <- as.Date(paste(data$released_year, data$released_month, 
                                        data$released_day, sep = "-"), 
                                  format = "%Y-%m-%d")
    # Casting number of streams
    data$streams <- as.numeric(as.character(data$streams))
    data[is.na(data$streams), "streams"] <- 0
    # Casting number of playlist in deezer
    data$in_deezer_playlists <- as.numeric(as.character(data$in_deezer_playlists))
    data[is.na(data$in_deezer_playlists), "in_deezer_playlists"] <- 0
    return(na.omit(data))
  })
  
  songs_data <- reactive({
    
    # Argument reading
    num_artists <- input$num_artists
    artist <- input$search_artist
    min_song_year <- input$release_year[1]
    max_song_year <- input$release_year[2]
    looked_song <- input$search_song
    
    songs_filter <- spotify_data() %>%
      filter(
        artist_count <= num_artists &
        released_year <= max_song_year &
        released_year >= min_song_year
      )
    
    # Optional: filter by singer member
    if (!is.null(input$search_artist) && input$search_artist != "") {
      songs_filter <- songs_filter %>% 
        filter(str_detect(str_to_lower(artist.s._name), str_to_lower(artist)))
    }
    
    # optional: change searched song dot visuals
    if (!is.null(looked_song) && looked_song != "" && nrow(songs_filter)) {
      songs_filter <- songs_filter %>%
        mutate(
          fill = ifelse(str_detect(str_to_lower(track_name), str_to_lower(looked_song)), "red", "black"),
          size = ifelse(str_detect(str_to_lower(track_name), str_to_lower(looked_song)), 50, 10),
          shape = ifelse(str_detect(str_to_lower(track_name), str_to_lower(looked_song)), "diamond", "circle")
        )
    }
    return(songs_filter)
  })
  
  songs_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$ID)) return(NULL)
    
    all_sngs <- isolate(songs_data())
    song <- all_sngs[all_sngs$ID == x$ID, ]
    
    paste0("<b>", song$track_name, "</b><br>",
           song$artist.s._name, "<br>",
           song$released_year, "<br>"
    )
  }
  
  vis <- reactive({
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    plot <- songs_data() %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(
        size := 10,
        size.hover := 50, 
        fill.hover := "orange", 
        fillOpacity := 1, 
        key := ~ID,
        fill := ~fill,
        size := ~size,
        shape := ~shape
      ) %>%
      add_tooltip(songs_tooltip, on = "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
    
    # Conditionally add the regression line based on the checkbox value
    if (input$add_regression && input$xvar != input$yvar) {
      plot <- plot %>% layer_smooths()
    }
    
    
    plot
  })
  
  vis %>% bind_shiny("outlierPlot")
  
  playlists_data <- reactive({
    # Getting data in the desired range
    filtered <- spotify_data() %>%
      filter(combined_date >= input$dateRange[1] & 
               combined_date <= input$dateRange[2])
    
    # Grouping by the desired value
    grouping_var <- switch(input$timeGroup,
                           "Month" = as.yearmon(filtered$combined_date),
                           "Year" = as.integer(filtered$released_year))
    filtered <- cbind(filtered, grouping_var)
    
    # Map checkboxes names to corresponding column names
    names <- c("in_spotify_playlists", 
               "in_apple_playlists", 
               "in_deezer_playlists")
    selected_platforms <- names[as.numeric(input$checkGroup)]
    selected_columns <- c("grouping_var", selected_platforms)
    
    # Subsetting the data for selected columns (platforms)
    data_subset <- filtered[, selected_columns]
    
    # Computing mean conditionally
    if (input$songMean) {
      data <- data_subset %>%
        group_by(grouping_var) %>%
        summarise(across(all_of(selected_platforms), mean, na.rm = TRUE))
    } else {
      data <- data_subset %>%
        group_by(grouping_var) %>%
        summarise(across(all_of(selected_platforms), sum, na.rm = TRUE))
    }
    
    return(data)
  })
  
  output$playlistsPlot <- renderPlot({

    data <- playlists_data()
    
    # Reshaping the data for multiple bars
    data_long <- tidyr::pivot_longer(data, cols = -grouping_var, 
                                     names_to = "Platform", values_to = "Count")
    
    plot <- ggplot(data_long, aes(x = grouping_var, y = Count, fill = Platform)) +
      geom_area(position = "stack") +
      labs(x = "Release Date", y = "Occurrences in playlists") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
            legend.position = "bottom", legend.justification = "left") + 
      scale_fill_manual(
        values = c("in_spotify_playlists" = "forestgreen", 
                   "in_apple_playlists" = "firebrick", 
                   "in_deezer_playlists" = "goldenrod"),
        labels = c("in_spotify_playlists" = "Spotify Playlists", 
                   "in_apple_playlists" = "Apple Music Playlists", 
                   "in_deezer_playlists" = "Deezer Playlists")
      )
    
    # For switching on the log-scale on the y-axis
    if(input$logScale){
      plot + scale_y_continuous(trans = "log10")
    } else {
      plot
    }
  })
  
  release_date_data = reactive({
    data <- spotify_data()
    data$season <- apply(data, 1, date_to_season_row)
    data <- head(data[
                  order(data[,input$medium], 
                  decreasing = if (input$medium == "streams") F else T),
                ], 
                n = as.numeric(input$top_x))
    return(data)
  })
  
  output$releaseDatePlot <- renderPlot({
    data_parsed <- release_date_data()
    boxplot(data_parsed[[input$attr]]~data_parsed[[input$time_option]],
            data=data_parsed,
            xlab=if (input$time_option == "released_month") "Month Number" 
            else if (input$time_option == "released_year") "year" else "season",
            ylab="%",
            col="orange",
            border="brown")
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)
