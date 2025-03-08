library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# UI
ui <- fluidPage(
  titlePanel("Overseas Pakistani Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c(".csv")
      ),
      br(),
      actionButton("analyzeBtn", "Analyze Data")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabset",  # Set an ID for tabsetPanel
        tabPanel("Data", dataTableOutput("data_table")),
        
        tabPanel("Histograms",
                 plotOutput("hist_remittance"),
                 plotOutput("hist_population_country"),
                 plotOutput("hist_population_continent"),
                 plotOutput("hist_year")
        ),
        
        tabPanel("Pie Charts",
                 plotOutput("pie_remittance"),
                 plotOutput("pie_population_country"),
                 plotOutput("pie_population_continent"),
                 plotOutput("pie_year")
        ),
        
        tabPanel("Bar Plots",
                 plotOutput("bar_remittance"),
                 plotOutput("bar_population_country"),
                 plotOutput("bar_population_continent"),
                 plotOutput("bar_year")
        ),
        
        tabPanel("Summary Statistics",
                 verbatimTextOutput("summary_stats")
        ),
        
        tabPanel("Regression Model and Distribution",
                 plotOutput("regression_plot"),
                 plotOutput("distribution_plot")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$data_table <- renderDataTable({
    dataset()
  })
  
  output$hist_remittance <- renderPlot({
    ggplot(dataset(), aes(x = Remittance....billion.)) +
      geom_histogram(fill = "lightblue", bins = 20) +
      labs(x = "Remittance (billion)", y = "Frequency", title = "Histogram of Remittance")
  })
  
  output$hist_population_country <- renderPlot({
    ggplot(dataset(), aes(x = Overseas.Pakistani.population..By.Country.)) +
      geom_histogram(fill = "lightblue", bins = 20) +
      labs(x = "Overseas Pakistani Population (by Country)", y = "Frequency", title = "Histogram of Overseas Pakistani Population (by Country)")
  })
  
  output$hist_population_continent <- renderPlot({
    ggplot(dataset(), aes(x = Population..by.Continent.)) +
      geom_histogram(fill = "lightblue", bins = 20) +
      labs(x = "Population (by Continent)", y = "Frequency", title = "Histogram of Population (by Continent)")
  })
  
  output$hist_year <- renderPlot({
    ggplot(dataset(), aes(x = Year)) +
      geom_histogram(fill = "lightblue", bins = 20) +
      labs(x= "Year", y = "Frequency", title = "Histogram of Year")
  })
  
  output$pie_remittance <- renderPlot({
    ggplot(dataset(), aes(x = "", y = Remittance....billion.)) +
      geom_bar(stat = "identity", fill = rainbow(nrow(dataset()))) +
      coord_polar(theta = "y") +
      labs(title = "Pie Chart of Remittance")
  })
  
  output$pie_population_country <- renderPlot({
    ggplot(dataset(), aes(x = "", y = Overseas.Pakistani.population..By.Country.)) +
      geom_bar(stat = "identity",fill = rainbow(nrow(dataset()))) +
      coord_polar(theta ="y") +
      labs(title = "Pie Chart of Overseas Pakistani Population (by Country)")
  })
  
  output$pie_population_continent <- renderPlot({
    ggplot(dataset(), aes(x = "", y = Population..by.Continent.)) +
      geom_bar(stat = "identity", fill = rainbow(nrow(dataset()))) +
      coord_polar(theta = "y") +
      labs(title = "Pie Chart of Population (by Continent)")
  })
  
  output$pie_year <- renderPlot({
    ggplot(dataset(), aes(x = "", y = Year)) +
      geom_bar(stat = "identity", fill = rainbow(nrow(dataset()))) +
      coord_polar(theta = "y") +
      labs(title = "Pie Chart of Year")
  })
  
  output$bar_remittance <- renderPlot({
    ggplot(dataset(), aes(x = "", y = Remittance....billion.)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Bar Plot of Remittance")
  })
  
  output$bar_population_country <- renderPlot({
    ggplot(dataset(), aes(x = "", y = Overseas.Pakistani.population..By.Country.)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Bar Plot of Overseas Pakistani Population (by Country)")
  })
  
  output$bar_population_continent <- renderPlot({
    ggplot(dataset(), aes(x = "", y = Population..by.Continent.)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Bar Plot of Population (by Continent)")
  })
  
  output$bar_year <- renderPlot({
    ggplot(dataset(), aes(x = "", y = Year)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Bar Plot of Year")
  })
  
  output$summary_stats <- renderPrint({
    stats <- summary(dataset())
    stats$stats <- NULL # Remove the "stats" column from the summary table
    stats
  })
  
  output$regression_plot <- renderPlot({
    # Perform regression analysis and create a plot
    # Replace the code below with your own regression analysis code
    lm_model <- lm(Remittance....billion. ~ Year, data = dataset())
    plot(dataset()$Year, dataset()$Remittance....billion.,
         xlab = "Year", ylab = "Remittance (billion)",
         main = "Regression Analysis: Remittance over Time")
    abline(lm_model, col = "red")
  })
  
  output$distribution_plot <- renderPlot({
    # Create a distribution plot
    # Replace the code below with your own distribution plot code
    ggplot(dataset(), aes(x = Remittance....billion.)) +
      geom_density(fill = "lightblue") +
      labs(x = "Remittance (billion)", y = "Density",
           title = "Distribution of Remittance")
  })
}