# shinyapp template
library(DT)
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
df <- read.csv("student_merged.csv", stringsAsFactors = FALSE)

#Code for Data Preapration (for correlation or Prediciton Model)
df$Dalc <- as.factor(df$Dalc)
df$Walc <- as.factor(df$Walc)
df$failures <- as.factor(df$failures)


#Code for Shiny UI

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
                              tabPanel("Grades", h2("Grades of Students with regards to gender and subject"),
                                       plotOutput("Grade3Plot"),
                                       plotOutput("Grade2Plot"),
                                       plotOutput("Grade1Plot")
                                       
                                       ),
                              tabPanel("Failures", h2("Percentage of Students who failed at exams with regards to school subject and gender"),
                                       plotOutput("failurePlot")),
                              tabPanel("Alcohol Consumption", h2("Impact of Alcohol on the final grade of Students"),
                                       plotOutput("DalcPlot"),
                                       plotOutput("WalcPlot"),
                              ),
                              tabPanel("Table", dataTableOutput("results"))
                              )
                            )
                        )
                ),
               
               tabPanel("Prediction",
                 mainPanel(h3("Grades Prediction for MS/GP Students"),
                   numericInput("failures1", "How many times have you failed the course:", 0, min = 0, max = 3),
                   selectInput("higheryes1","Are you going for higher education:", 
                                                             c("Yes" = 1, 
                                                               "No" = 0)),
                   selectInput("schoolGP1", "Which school are you going to?", 
                                                              c("Gabriel Pereira" = 1,
                                                                "Mousinho da Silveira" = 0)),
                   conditionalPanel(
                     condition = "input.schoolGP1 == 'Gabriel Pereira'",
                     selectInput("schoolMS1", "Are you going to the MS school:",
                                 c("No" = 0))
                   ),
                   conditionalPanel(
                     condition = "input.schoolGP1 == 'Mousinho da Silveira'",
                     selectInput("schoolMS1" , "Are you going to the MS school:",
                                 c("Yes" = 1))
                   ),
                   selectInput("Medu1", "Your mother's education:",
                                                             c("None" = 0,
                                                               "Primary education" = 1,
                                                               "5th to 9th grade" = 2,
                                                               "Secondary education" = 3,
                                                               "Higher Education" = 4)),
                   selectInput("Fedu1", "Your father's education:",
                                                             c("None" = 0,
                                                               "Primary education" = 1,
                                                               "5th to 9th grade" = 2,
                                                               "Secondary education" = 3,
                                                               "Higher Education" = 4)),
                   selectInput("studytime1", "Studytime (hour/week):",
                                                             c("less than 2 hours" = 1,
                                                               "2-5 hours" = 2,
                                                               "5-10 hours" = 3,
                                                               "More than 10 hours" = 4)),
                   selectInput("Dalc1", "Daily alcohol consumption:",
                                                             c("Very low" = 1,
                                                               "Low" = 2,
                                                               "Medium" = 3,
                                                               "High" = 4,
                                                               "Very high" = 5)),
                   actionButton("Calculate", "Calculate")
                 ),
                 mainPanel(h3("\n", "Your average grade:", "\n"),
      
                  h2(textOutput("result")))
                 )

)
)     
#Code for Shiny Server

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
  
  output$DalcPlot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(Dalc, G3)) +
      geom_boxplot() +
      labs(y="Final Grade", x="Alcohol Consumption during Workdays") +
      facet_grid(~ sex)
      
    
  })
  
  output$WalcPlot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(Walc, G3)) +
      geom_boxplot() +
      labs(y="Final Grade", x="Alcohol Consumption during Weekend") +
      facet_grid(~ sex)
    
    
  })
  
  output$results <- renderDataTable({
    filtered()
  })
  
  #Function that predicts avg grade
  observeEvent(input$Calculate, {
    
    #make prediction with input variables
    predLinear <- predict(lm5, newdata = data.frame(failures=input$failures1 ,higheryes=as.numeric(input$higheryes1) 
                                                    ,schoolGP=as.numeric(input$schoolGP1) ,schoolMS=as.numeric(input$schoolMS1)
                                                    ,Medu=as.numeric(input$Medu1) ,studytime=as.numeric(input$studytime1) ,
                                                    Fedu=as.numeric(input$Fedu1) ,Dalc=as.numeric(input$Dalc1 )))
    #show result in rendered Text and round it
    output$result <- renderText({ 
      paste(round(predLinear, digits = 2))
    })
  })
}

shinyApp(ui = ui, server = server)
