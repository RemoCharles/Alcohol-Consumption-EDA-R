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
                             value = c(15, 22)),
                            checkboxGroupInput("subjectInput", "Genders",
                                               choiceNames = c("Mathematics", "Geography"),
                                               choiceValues = c("MS", "GP"),
                                               selected = c("MS", "GP")
                            ),
                            uiOutput("genderOutput"),
                            checkboxGroupInput("pStatusInput", "Parent status",
                                               choiceNames = c("living together", "living apart"),
                                               choiceValues = c("T", "A"),
                                               selected = c("T", "A")
                            ),
                            checkboxGroupInput("dAlcInput", "Workday alcohol consumption",
                                               choiceNames = c("very low", "low", "average", "high", "very high"),
                                               choiceValues = c(1,2,3,4,5),
                                               selected = c(1,2,3,4,5)
                                               ),
                            checkboxGroupInput("wAlcInput", "Weekend alcohol consumption",
                                               choiceNames = c("very low", "low", "average", "high", "very high"),
                                               choiceValues = c(1,2,3,4,5),
                                               selected = c(1,2,3,4,5)
                            )
                            ),
                
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Failuers", plotOutput("failurePlot")),
                              tabPanel("Grades",
                                       plotOutput("Grade1Plot"),
                                       plotOutput("Grade2Plot"),
                                       plotOutput("Grade3Plot")
                                       ),
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
                selected = c("F", "M"))
  })
  
  filtered <- reactive({
    
    if (is.null(input$subjectInput)) {
      return(NULL)
    }   
    
    df %>%
      filter(age >= input$ageInput[1],
             age <= input$ageInput[2],
             school %in% input$subjectInput,
             Pstatus %in% input$pStatusInput,
             sex %in% input$genderInput,
             Dalc %in% input$dAlcInput,
             Walc %in% input$wAlcInput
      )
  })
  
  output$failurePlot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(failures)) +
      geom_histogram()
  })
  
  output$Grade1Plot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G3)) +
      geom_histogram()
  })
  
  output$Grade2Plot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G2)) +
      geom_histogram()
  })
  
  output$Grade3Plot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G1)) +
      geom_histogram()
  })
  
  
  output$results <- renderDataTable({
    filtered()
  })
  
}

shinyApp(ui = ui, server = server)

#Loading the whole main_app.R script
#Need to find out how to load specific funtions
#source("main_app.R")
