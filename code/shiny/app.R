library(shiny)
library(here)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Serie A Club Performance, 2023-2024"),
  #select club of interest
  selectInput(inputId = "club", label = "Select club", choices = NULL),
  #display performance table
  tableOutput("static"),
  #display line plot of points over time
  plotOutput("linePlot")
)

server <- function(input, output, session) {
  #load aggregate data
  root <- here()
  load(paste0(root, "/data/processed/data_aggregated_matches_23_24"))
  load(paste0(root, "/data/processed/data_matchday_23_24"))
  
  #once data is loaded, update selectInput choices
  observe({
    updateSelectInput(session, "club", choices = unique(data_aggregate$Club))
  })
  
  #display select variables from aggregate table based on selected club
  output$static <- renderTable({
    season_performance <- data_aggregate %>%
      filter(Club == input$club) %>%
      select(WinPct:GD)
    season_performance #return the df to be displayed
  })
  
  output$linePlot <- renderPlot({
    club_data <- data_matchday %>%
      #filter matchday data for the selected club
      filter(club == input$club) %>%
      #create lineplot
      ggplot(aes(x = matchday, y = total_points, group = 1)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(color = "red", size = 3) +
      labs(
        title = paste("Total Points After Each Matchday:", input$club),
        x = "Matchday",
        y = "Total Points"
      ) +
      theme_minimal()
    club_data #return the plot to be displayed
  })
}

shinyApp(ui, server)