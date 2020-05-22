# shinyapp template
library(DT)
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
df <- read.csv("student_merged.csv", stringsAsFactors = FALSE)


ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Student Alcohol Consumption"),
    navbarPage("DASB Project - Group 3",
               
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
                              tabPanel("Grades",
                                       plotOutput("Grade3Plot"),
                                       plotOutput("Grade2Plot"),
                                       plotOutput("Grade1Plot")
                                       
                                       ),
                              tabPanel("Failuers", plotOutput("failurePlot")),
                              tabPanel("Correlations", plotOutput("correlationPlot")),
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
    
    ggplot(filtered()) +
      geom_bar(aes(x=failures, y = ..prop.., stat="count", fill=school, group=school), position="dodge") +
      geom_text(aes(x=failures, label = scales::percent(..prop..),
                     y= ..prop.., group=school ), stat= "count", position = position_dodge(width = 1), vjust = -.5) +
      ggtitle("Percentage of Students who failed at exams with regards to school subject and gender") +
      labs(y="Percent", x="Number of failed Exams") +
      facet_grid(~ sex)+
      scale_y_continuous(labels=scales::percent)
  })
  

  output$Grade3Plot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G3, color=school, fill=school)) +
      geom_bar(aes(y = ..prop.., stat="count"), position="dodge") +
      ggtitle("Grades of Students with regards to gender subjects") +
      scale_y_continuous(labels=scales::percent) +
      labs(y="Percent", x="Final grade") +
      facet_grid(~ sex)
  })
  
  output$Grade2Plot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G2, color=school, fill=school)) +
      geom_bar(aes(y = ..prop.., stat="count"), position="dodge") +

      scale_y_continuous(labels=scales::percent) +
      labs(y="Percent", x="Second period grade") +
      facet_grid(~ sex)
  })
  
  output$Grade1Plot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G1, color=school, fill=school)) +
      geom_bar(aes(y = ..prop.., stat="count"), position="dodge") +
      scale_y_continuous(labels=scales::percent) +
      labs(y="Percent", x="First period grade") +
      facet_grid(~ sex)
  })
  
  output$correlationPlot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G1, color=sex, fill=sex)) +
      geom_histogram(position='dodge', stat='count')
  })
  
  output$results <- renderDataTable({
    filtered()
  })
  
}

shinyApp(ui = ui, server = server)

#Loading the whole main_app.R script
#Need to find out how to load specific funtions
#source("main_app.R")
