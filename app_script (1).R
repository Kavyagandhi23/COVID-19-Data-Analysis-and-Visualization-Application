# Load required packages
library(shiny)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(readr)

ui <- dashboardPage(
  
  # Dashboard header
  dashboardHeader(title = "COVID-19 Data Analysis and Visualization"),
  
  # Sidebar layout
  dashboardSidebar(
    width = 400,
    tags$style(type = "text/css",
               ".sidebar {
                 background-color: #322F2F;
               }"),
    
    tags$style(type = "text/css",
               ".skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
                 background-color: #322F2F;
               }"),
    
    
    # Data loading options
    fileInput("file", "Choose CSV File", accept = ".csv"),
    br(),
    actionButton("load", "Load Data"),
    
    # Data cleaning options
    br(),
    actionButton("clean", "Clean Data"),
    
    # Data exploration options
    br(),
    actionButton("summary", "View Summary Statistics"),
    
    # Data visualization options
    br(),
    selectInput("type", "Choose Plot Type", choices = c("Time Series", "Bar Chart")),
    br(),
    uiOutput("xvar_select"),
    br(),
    uiOutput("yvar_select"),
    br(),
    actionButton("plot", "Create Plot")
    
  ),
  
  # Main panel
  dashboardBody(
    
    tags$style(type = "text/css",
               ".control-label, .selectize-input { font-size: 16px; }",
               ".control-label, .selectize-control.multi .selectize-input div { font-size: 16px; line-height: normal; }"), # Add this CSS code
    
    # Data table
    tabItem(tabName = "table",
            dataTableOutput("table")),
    
    # Summary statistics
    tabItem(tabName = "summary",
            verbatimTextOutput("summary")),
    
    # Data visualization
    tabItem(tabName = "plot",
            plotOutput("plot"))
    
  )
  
)


# Define server function
server <- function(input, output) {
  
  # Load data from CSV file or URL
  data <- eventReactive(input$load, {
    if (!is.null(input$file)) {
      read.csv(input$file$datapath, header = TRUE)
    }
  })
  
  # Clean data
  cleaned_data <- eventReactive(input$clean, {
    if (!is.null(data())) {
      na.omit(data())
    }
  })
  
  # Create X variable select input based on loaded data
  output$xvar_select <- renderUI({
    if (!is.null(data())) {
      selectInput("xvar", "Choose X Variable", choices = colnames(data()))
    }
  })
  # Create Y variable select input based on loaded data
  output$yvar_select <- renderUI({
    if (!is.null(data())) {
      selectInput("yvar", "Choose Y Variable", choices = colnames(data()))
    }
  })
  
  # Create plot based on plot type selection
  output$plot <- renderPlot({
    if (input$plot > 0) {
      plot_data <- cleaned_data()
      xvar <- input$xvar
      yvar <- input$yvar
      if (input$type == "Time Series") {
        ggplot(plot_data, aes_string(x = xvar, y = yvar, group = 1)) +
          geom_line() +
          labs(x = xvar, y = yvar, title = "Time Series Plot")
      } else if (input$type == "Bar Chart") {
        ggplot(plot_data, aes_string(x = xvar, y = yvar, group = 1)) +
          geom_bar(stat = "identity") +
          labs(x = xvar, y = yvar, title = "Bar Chart")
      }
    }
  })
  
  # Create summary statistics table
  output$summary <- renderPrint({
    if (input$summary > 0) {
      summary(cleaned_data())
    }
  })
  
  # Create data table
  output$table <- renderDataTable({
    if (input$load > 0) {
      data()
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server=server)