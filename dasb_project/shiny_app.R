# shinyapp template
library(DT)
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
df <- read.csv("student_merged.csv", stringsAsFactors = FALSE)


ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Student Alcohol Consumption"),
    navbarPage("Let's get started",
               
               tabPanel("Project proposal",
                        uiOutput("pdfview")
               ),
             
               tabPanel("Information about the Data", "Tbd"
                        ),
               
               tabPanel("Exploratory Data Analysis",
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Exploratory Data Analysis"),
                            sliderInput("ageInput", "Age", min = 15, max = 22,
                             value = c(18, 20)),
                            uiOutput("subjectOutput"),
                            uiOutput("genderOutput")
                            ),
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot", plotOutput("coolplot")),
                              tabPanel("Table", dataTableOutput("results"))
                              )
                            )
                          )
                        ),
               tabPanel("Prediciton"
                        )
               ),
  )



server <- function(input, output) {
  
  #PDF output of the project proposal. Shiny app has to be opened in Browser and NOT in Rstudio!!!!
  output$pdfview <- renderUI({
    tags$iframe(style="height:1500px; width:100%", src="Project_proposal.pdf")
  })
  
  
  output$genderOutput <- renderUI({
    checkboxGroupInput("genderInput", "Gender",
                sort(unique(df$sex)),
                selected = "F")
  })
  
  output$subjectOutput <- renderUI({
    checkboxGroupInput("subjectInput", "Subject",
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
             school %in% input$subjectInput,
             sex %in% input$genderInput
      )
  })
  
  output$coolplot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(failures)) +
      geom_histogram()
  })
  
  output$results <- renderDataTable({
    filtered()
  })
  
}

shinyApp(ui = ui, server = server)
