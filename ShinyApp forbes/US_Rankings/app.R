#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(packcircles)
library(ggplot2)
library(ggforce)
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(plotly)
library(ggplot2)

first_ten_rank_holders_fortune=read_excel('top_ten_rank_holders_fortune.xlsx', na = c(''))
first_ten_rank_holders_inc=read_excel('top_ten_rank_holders_inc.xlsx', na = c(''))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("US Rankings (Fortune 500 and INC 5000)"),
  
  # Top row with user inputs
  fluidRow(
    column(
      width = 6,
      offset = 3,
      align = "center",
      selectInput("year", "Select a year:", choices = c(2019, 2020, 2021))
    )
  ),
  
  # Second row with rankplot and parameterplot
  fluidRow(
    column(
      width = 6,
      height = "500px",
      plotlyOutput("rankplotfortune")
    ),
    column(
      width = 6,
      height = "500px",
      plotlyOutput("rankplotinc")
    )
  ),
  
  # Third row with industryplot centered
  fluidRow(
    column(
      width = 5,
      style = "height:500px; margin-top:75px;",
      align = "center",
      plotlyOutput("industrypieplotfortune")
    ),
    column(
      width = 5,
      style = "height:500px; margin-top:75px;",
      align = "center",
      plotlyOutput("industrypieplotinc")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$rankplotfortune <- renderPlotly({
    # Filter the data for the selected year
    filtered_data <- filter(first_ten_rank_holders_fortune, Year == input$year) 
    filtered_data <- filtered_data %>% arrange(Rank)
    filtered_data$Name <- reorder(filtered_data$Name, filtered_data$Rank)
    
    # colors =c("#02583E","#117F5A", 
    #            "#20A877", "#34C594", 
    #            "#4ED9AC", "#6BE0C0",
    #            "#8BE0C0", "#98E6D4",
    #            "#BAE6D4", "#DAE6D4")
    plot <- plot_ly(filtered_data, x = ~Rank, y = ~Name, 
                    type = "bar", 
                    orientation = "h",
                    color = ~Name, width = 500, height = 400,
                    text = ~paste0(Rank), hovertemplate = "%{x}:%{y}",
                    marker=list(color ="#38AEBE")) %>%
      layout(title = list(text = paste0("<b>Top 10 Fortune 500 Companies for Year ", input$year ,"</b>"), 
                          font = list(size = 15, color ="#33A7F0")),
             yaxis = list(title = "<b>Company Name</b>",
                          titlefont = list(size = 12, 
                                           family = "Arial", 
                                           color = "black", 
                                           bold = TRUE)),
             xaxis = list(title = "Rank",
                          titlefont = list(size = 12, 
                                           family = "Arial", 
                                           color = "black", 
                                           bold = TRUE),
                          range = c(0, 10), tickvals = c(0,2,4,6,8,10)),
             showlegend=FALSE)
    
    # Return the plot
    plot
  })
  
  output$rankplotinc <- renderPlotly({
    # Filter the data for the selected year
    filtered_data <- filter(first_ten_rank_holders_inc, Year == input$year) 
    filtered_data <- filtered_data %>% arrange(Rank)
    filtered_data$Name <- reorder(filtered_data$Name, filtered_data$Rank)
    
    plot <- plot_ly(filtered_data, x = ~Rank, y = ~Name, 
                    type = "bar", 
                    orientation = "h",
                    color = ~Name, width = 500, height = 400,
                    text = ~paste0(Rank), hovertemplate = "%{x}:%{y}",
                    marker=list(color ="#38AEBE")) %>%
      layout(title = list(text = paste0("<b>Top 10 Inc. 5000 Companies for Year ", input$year ,"</b>"), 
                          font = list(size = 15, color ="#33A7F0")),
             yaxis = list(title = "<b>Company Name</b>",
                          titlefont = list(size = 12, 
                                           family = "Arial", 
                                           color = "black", 
                                           bold = TRUE)),
             xaxis = list(title = "Rank",
                          titlefont = list(size = 12, 
                                           family = "Arial", 
                                           color = "black", 
                                           bold = TRUE),
                          range = c(0, 10), tickvals = c(0,2,4,6,8,10)),
             showlegend=FALSE)
    
    # Return the plot
    plot
  })
  
  
  output$industrypieplotfortune <- renderPlotly({
    
    # Filter the data for the selected year
    filtered_data <- filter(first_ten_rank_holders_fortune, Year == input$year)
    
    colors <- c("#01583E","#058962", "#0EB9B1", "#037D8B", "#60CFDC", "#9EEDF7"
    )
    
    # Create the plot
    plot <- plot_ly(
      filtered_data,
      labels = unique(filtered_data$Industry),
      values = table(filtered_data$Industry),
      type = "pie",
      hole =0.4,
      textinfo = "label+percent",
      hovertemplate = "%{label}<br>Frequency: %{value}<br>Percentage: %{percent}",
      marker = list(colors = colors)
    ) %>%
      layout(
        title = list(text = paste0("<b>Industry sectors for Top 10 Rankings for Year </b>"),
                     font = list(size = 15, color ="#33A7F0")),
        showlegend = FALSE,
        margin = list(t = 50) # adjust margins here
      )
    
    plot
  })
  
  output$industrypieplotinc <- renderPlotly({
    
    # Filter the data for the selected year
    filtered_data <- filter(first_ten_rank_holders_inc, Year == input$year)
    
    colors <- c("#01583E","#058962", "#0EB9B1", "#037D8B", "#60CFDC", "#9EEDF7" )
    
    # Create the plot
    plot <- plot_ly(
      filtered_data,
      labels = unique(filtered_data$Industry),
      values = table(filtered_data$Industry),
      type = "pie",
      hole =0.4,
      textinfo = "label+percent",
      hovertemplate = "%{label}<br>Frequency: %{value}<br>Percentage: %{percent}",
      marker = list(colors = colors)
    ) %>%
      layout(
        title = list(text = paste0("<b>Industry sectors for Top 10 Rankings for Year</b> "),
                     font = list(size = 15, color ="#33A7F0")),
        showlegend = FALSE,
        margin = list(t = 50) # adjust margins here
      )
    
    plot
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


