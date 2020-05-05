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
      radioButtons("subjectInput", "School subject",
                   choices = c("MS", "GP", "Both"),
                   selected = "MS"),
      radioButtons("genderInput", "Country",
                  choices = c("M", "F", "Both"),
                  selected = "F"),
    ),
  
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
      
    )
  )
)




server <- function(input, output) {
  output$coolplot <- renderPlot({
    filtered <-
      df %>%
      filter(age >= input$ageInput[1],
             age <= input$ageInput[2],
             school == input$subjectInput,
             sex == input$genderInput
      )
    ggplot(filtered, aes(failures)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered <-
      df %>%
      filter(age >= input$ageInput[1],
             age <= input$ageInput[2],
             school == input$subjectInput,
             sex == input$genderInput
      )
    filtered
  })
  
}

shinyApp(ui = ui, server = server)
