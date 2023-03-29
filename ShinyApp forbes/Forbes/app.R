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


dfall=read_excel('all.xlsx', na = c(''))
first_ten_rank_holders=read_excel('first_10_ranks_allyears_forbes.xlsx', na = c(''))
first_ten_rank_holders <- first_ten_rank_holders%>% mutate(Sales = as.numeric(gsub("\\$|\\s|B|,", "", Sales)))
first_ten_rank_holders <- first_ten_rank_holders %>%mutate(Profits = as.numeric(gsub("\\$|\\s|B|,", "", Profits)))
first_ten_rank_holders <- first_ten_rank_holders %>%mutate(Assets = as.numeric(gsub("\\$|\\s|B|,", "", Assets)))
first_ten_rank_holders <- first_ten_rank_holders %>%mutate(Market_Value = as.numeric(gsub("\\$|\\s|B|,", "", Market_Value)))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Forbes 2000 Rankings (Global)"),
  
  # Top row with user inputs
  fluidRow(
    column(
      width = 6,
      selectInput("year", "Select a year:", choices = c(2019, 2020, 2021))
    ),
    column(
      width = 6,
      selectInput(
        inputId = "parameters",
        label = "Select Parameter",
        choices = c("Profits", "Sales", "Assets", "Market_Value"),
        selected = "Profits"
      )
    )
  ),
  
  # Second row with rankplot and parameterplot
  fluidRow(
    column(
      width = 6,
      style = "width : 500px; height:400px;",
      plotlyOutput("rankplot")
    ),
    column(
      width = 6,
      style = "width : 500px; height:400px;",
      plotlyOutput("parameterplot")
    )
  ),
  
  # Third row with industryplot centered
  fluidRow(
    column(
      width = 8,
      style = "height:500px; margin-top:75px;",
      align = "center",
      plotlyOutput("industrypieplot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$rankplot <- renderPlotly({
    # Filter the data for the selected year
    filtered_data <- filter(first_ten_rank_holders, Year == input$year) 
    filtered_data <- filtered_data %>% arrange(Rank)
    filtered_data$Name <- reorder(filtered_data$Name, filtered_data$Rank)
    
    plot <- plot_ly(filtered_data, x = ~Name, y = ~Rank, 
                    type = "bar", width = 500, height = 400,
                    text = ~paste0(Rank), hovertemplate = "%{x}:%{y}",
                    marker=list(color ="#38AEBE")) %>%
      layout(title = list(text = paste0("<b>Top 10 Forbes 2000 Companies for Year ", input$year, "</b>"), 
                          font = list(size = 15, color ="#33A7F0")),
             xaxis = list(title = "<b>Company Name</b>", 
                          titlefont = list(size = 12, 
                                           family = "Arial", 
                                           color = "black", 
                                           bold = TRUE)),
             yaxis = list(title = "<b>Rank</b>", 
                          titlefont = list(size = 12, 
                                           family = "Arial", 
                                           color = "black", 
                                           bold = TRUE), 
                          range = c(0, 10), 
                          tickvals = c(0, 2, 4, 6, 8, 10)),
             margin = list(t = 75)
      )
    
    # Return the plot
    plot
  })
  
  output$industrypieplot <- renderPlotly({
    
    # Filter the data for the selected year
    filtered_data <- filter(first_ten_rank_holders, Year == input$year)
    
    colors <- c("#01583E","#058962", "#0EB9B1", "#037D8B", "#60CFDC", "#9EEDF7" )
    
    # Create the plot
    plot <- plot_ly(
      filtered_data,
      labels = unique(filtered_data$Industry),
      values = table(filtered_data$Industry),
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      hovertemplate = "%{label}<br>Frequency: %{value}<br>Percentage: %{percent}",
      marker = list(colors = colors)
    ) %>%
      
      layout(
        title = list(text= paste0("<b>Industries sectors contributing to Top 10 Rankings for Year ", input$year,"</b>"),
                     font = list(size = 15,color ="#33A7F0")),
        showlegend = FALSE,
        margin = list(l = 20, r = 20, t = 100, b = 20) # adjust margins here
      )
    
    plot
    
  })
  
  output$parameterplot <- renderPlotly({
    # Filter the data for the selected year
    filtered_data <- filter(first_ten_rank_holders, Year == input$year)
    filtered_data <- filtered_data %>% arrange(Rank)
    filtered_data$Name <- reorder(filtered_data$Name, filtered_data$Rank)
    
    # Create the plot
    plot <- plot_ly(filtered_data, x = ~Name, 
                    y = switch(input$parameters,
                               "Profits" = ~Profits,
                               "Sales" = ~Sales,
                               "Assets" = ~Assets,
                               "Market_Value" = ~`Market_Value`),
                    type = "bar", width = 500, height = 400,
                    text = ~paste0(input$parameters),
                    hovertemplate = "%{x}:%{y}",
                    marker = list(color = '#04865f')) %>%
      layout(title = list(text= paste0("<b>",input$parameters, " eavluation of Top 10 Companies for Year ", input$year,"</b>"),
                          font = list(size = 15, color ="#33A7F0")),
             xaxis = list(title = "<b>Company Name</b>", 
                          titlefont = list(size = 12, 
                                           family = "Arial", 
                                           color = "black", 
                                           bold = TRUE)),
             yaxis = list(title = paste0("<b>",input$parameters,"</b>"),
                          titlefont = list(size = 12, 
                                           family = "Arial", 
                                           color = "black", 
                                           bold = TRUE)),
             margin = list(t = 75))

    # Return the plot
    plot
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


