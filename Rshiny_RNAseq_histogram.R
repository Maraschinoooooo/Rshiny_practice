# Load necessary libraries
library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("RNA-Seq Data Histograms"),
  sidebarLayout(
    sidebarPanel(
      fileInput('dataFile', 'Upload Counts Table (CSV format)'),
      selectInput('sampleSelect', 'Select Sample', choices = NULL),
      sliderInput('binWidth', 'Bin Width', min = 1, max = 100, value = 10),
      tags$hr(),
      tags$p("Select a sample to view its expression histogram.")
    ),
    mainPanel(
      fluidRow(
        column(12,
               plotOutput('histPlot')
        )
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$dataFile)
    read.csv(input$dataFile$datapath, row.names = 1)
  })
  
  # Update the sample selection input based on the columns of the uploaded file
  observe({
    df <- data()
    updateSelectInput(session, 'sampleSelect', choices = colnames(df))
  })
  
  # Generate and render the histogram
  output$histPlot <- renderPlot({
    req(data())
    selectedSample <- input$sampleSelect
    ggplot(data(), aes_string(x = selectedSample)) +
      geom_histogram(binwidth = input$binWidth, fill = "#00BFC4", color = "black") +
      theme_minimal() +
      labs(title = paste("Histogram of", selectedSample), x = "Counts", y = "Frequency")
  })
}

# Run the App
shinyApp(ui = ui, server = server)
