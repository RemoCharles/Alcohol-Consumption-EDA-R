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
             subject %in% input$subjectInput,
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
      geom_bar(aes(x=failures, y = ..prop.., stat="count", fill=subject, group=subject), position="dodge") +
      geom_text(aes(x=failures, label = scales::percent(..prop..),
                    y= ..prop.., group=subject ), stat= "count", position = position_dodge(width = 1), vjust = -.5) +
      labs(y="Percent", x="Number of failed Exams") +
      facet_grid(~ sex)+
      scale_y_continuous(labels=scales::percent)
  })
  
  
  output$Grade3Plot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G3, color=sex, fill=sex)) +
      geom_bar(aes(y = ..prop.., stat="count"), position="dodge") +
      scale_y_continuous(labels=scales::percent) +
      labs(y="Percent", x="Final grade") +
      facet_grid(~ subject)
  })
  
  output$Grade2Plot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G2, color=sex, fill=sex)) +
      geom_bar(aes(y = ..prop.., stat="count"), position="dodge") +
      
      scale_y_continuous(labels=scales::percent) +
      labs(y="Percent", x="Second period grade") +
      facet_grid(~ subject)
  })
  
  output$Grade1Plot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(G1, color=sex, fill=sex)) +
      geom_bar(aes(y = ..prop.., stat="count"), position="dodge") +
      scale_y_continuous(labels=scales::percent) +
      labs(y="Percent", x="First period grade") +
      facet_grid(~ subject)
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
    predLinear <- predict(lm7, newdata = data.frame(failures=input$failures1 ,higheryes=as.numeric(input$higheryes1) 
                                                    ,schoolGP=as.numeric(input$schoolGP1),Medu=as.numeric(input$Medu1), subjectPor=as.numeric(input$subjectPor1) ,studytime=as.numeric(input$studytime1) ,
                                                    Fedu=as.numeric(input$Fedu1) ,Dalc=as.numeric(input$Dalc1 )))
    #show result in rendered Text and round it
    output$result <- renderText({ 
      paste(round(predLinear, digits = 2))
    })
  })
}