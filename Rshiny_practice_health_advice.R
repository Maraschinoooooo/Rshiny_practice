# Install and load necessary libraries
library(shiny)
library(shinydashboard)
library(shinyjs)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Health Advice App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Health Advice", tabName = "health_advice", icon = icon("heartbeat"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "health_advice",
        fluidPage(
          box(
            title = "Enter Your Information",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            background = "aqua",
            numericInput("age", "Enter your age:", value = 30, min = 1, max = 150),
            numericInput("bmi", "Enter your BMI:", value = 25, min = 10, max = 50),
            actionButton("submit", "Get Health Advice", class = "btn-primary")
          ),
          box(
            title = "Health Advice",
            width = 6,
            solidHeader = TRUE,
            status = "success",
            background = "lightgreen",
            uiOutput("adviceText")
          )
        )
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  observe({
    shinyjs::useShinyjs()
    shinyjs::extendShinyjs(text = "shinyjs.reset = function() { document.getElementById('age').value = ''; document.getElementById('bmi').value = ''; }")
  })
  
  observeEvent(input$submit, {
    # Health advice logic 
    age_advice <- if (input$age < 18) "You're still young. Focus on a balanced diet and exercise."
    else if (input$age < 65) "Maintain a healthy lifestyle with regular exercise and balanced nutrition."
    else "Make sure to prioritize your health with regular check-ups and a healthy lifestyle."
    
    bmi_advice <- if (input$bmi < 18.5) "Consider gaining some weight in a healthy way."
    else if (input$bmi < 25) "Your BMI is in a healthy range. Keep it up!"
    else if (input$bmi < 30) "Consider focusing on a balanced diet and regular exercise to maintain a healthy weight."
    else "It's important to address your weight. Consult with a healthcare professional for personalized advice."
    
    # Combine the advice
    advice <- paste(age_advice, bmi_advice, sep = "<br><br>")
    
    # Display the advice
    output$adviceText <- renderUI({
      HTML(paste("<strong>Health Advice:</strong><br>", advice))
    })
    
    # Reset the input values
    shinyjs::reset()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
