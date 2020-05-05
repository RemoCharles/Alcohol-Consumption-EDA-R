# shinyapp template
library(shiny)
library(ggplot2)
library(dplyr)
df <- read.csv("student_merged.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("Student Alcohol Consumption"),
  
  sidebarLayout(
    sidebarPanel(
      
      titlePanel("Exploratory Data Analysis"),
      
      sliderInput("ageInput", "Age", min = 15, max = 22,
                             value = c(18, 20)),
      uiOutput("subjectOutput"),
      uiOutput("genderOutput")
    ),
  
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
      
    )
  )
)




server <- function(input, output) {
  output$genderOutput <- renderUI({
    radioButtons("genderInput", "Gender",
                sort(unique(df$sex)),
                selected = "F")
  })
  
  output$subjectOutput <- renderUI({
    radioButtons("subjectInput", "Subject",
                 sort(unique(df$school)),
                 selected = "MS")
  })  
  
  filtered <- reactive({
    
    if (is.null(input$subjectInput)) {
      return(NULL)
    }   
    
    df %>%
      filter(age >= input$ageInput[1],
             age <= input$ageInput[2],
             school == input$subjectInput,
             sex == input$genderInput
      )
  })
  
  output$coolplot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(failures)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
  
}

shinyApp(ui = ui, server = server)
