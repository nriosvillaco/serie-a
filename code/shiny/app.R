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
    matches_ranked <- data_matchday %>%
      group_by(matchday) %>%
      mutate(
        rank = dense_rank(-total_points), #rank each team in relation to others within each matchday
        highlight = ifelse(club == input$club, "Selected", "Other") #mark the selected team
        )
      
  #create lineplot
  ggplot(matches_ranked, aes(x = matchday, y = rank, color = highlight)) +
    geom_line(alpha = 0.3) + #other non-selected teams have a faded color
    geom_line(data = subset(matches_ranked, highlight == "Selected"), aes(x = matchday, y = rank), size = 1.2, color = "blue") +  #bold line for selected team
    labs(
      title = paste("Rank of Teams Over Time:", input$club),
      x = "Matchday",
      y = "Rank by Total Points",
      color = "Team"
      ) +
    scale_y_reverse() + #invert rank axis
    theme_minimal()
  })
}

shinyApp(ui, server)