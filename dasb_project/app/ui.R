fluidPage(theme = shinytheme("flatly"),
          titlePanel("Student Alcohol Consumption"),
          navbarPage("DASB Project - Group 4",
                     
                     tabPanel("Project proposal",
                              uiOutput("pdfview")
                     ),
                     
                     tabPanel("Information about Data", mainPanel(
                       HTML("<h2>Data insights</h2>
<h4>The following list will help you understand all variables of the dataset. This will provide you with everything you need to build an exploratory data analysis by yourself.</h4>
<br></br>
<ul>
<li>school - student's school (binary: 'GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)</li>
<li>sex - student's sex (binary: 'F' - female or 'M' - male) </li>
<li>age - student's age (numeric: from 15 to 22) </li>
<li>address - student's home address type (binary: 'U' - urban or 'R' - rural) </li>
<li>famsize - family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3) </li>
<li>Pstatus - parent's cohabitation status (binary: 'T' - living together or 'A' - apart)</li>
<li>Medu - mother's education (numeric: 0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)</li>
<li>Fedu - father's education (numeric: 0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)</li>
<li>Mjob - mother's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other') </li>
<li>Fjob - father's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other') </li>
<li>reason - reason to choose this school (nominal: close to 'home', school 'reputation', 'course' preference or 'other') </li>
<li>guardian - student's guardian (nominal: 'mother', 'father' or 'other') </li>
<li>traveltime - home to school travel time (numeric: 1 - 1 hour) </li>
<li>studytime - weekly study time (numeric: 1 - 10 hours) </li>
<li>failures - number of past class failures (numeric: n if 1&lt;=n&lt;3, else 4) </li>
<li>schoolsup - extra educational support (binary: yes or no) </li>
<li>famsup - family educational support (binary: yes or no) </li>
<li>paid - extra paid classes within the course subject (Math or Portuguese) (binary: yes or no) </li>
<li>activities - extra-curricular activities (binary: yes or no) </li>
<li>nursery - attended nursery school (binary: yes or no) </li>
<li>higher - wants to take higher education (binary: yes or no) </li>
<li>internet - Internet access at home (binary: yes or no) </li>
<li>romantic - with a romantic relationship (binary: yes or no) </li>
<li>famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent) </li>
<li>freetime - free time after school (numeric: from 1 - very low to 5 - very high) </li>
<li>goout - going out with friends (numeric: from 1 - very low to 5 - very high) </li>
<li>Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high) </li>
<li>Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high) </li>
<li>health - current health status (numeric: from 1 - very bad to 5 - very good) </li>
<li>absences - number of school absences (numeric: from 0 to 93) </li>
<li>subject - specific Subject (binary: Por=Portguese or Mat=Mathematics) </li>
</ul>")
                     )
                     ),
                     
                     tabPanel("Exploratory Data Analysis",
                              sidebarLayout(
                                sidebarPanel(
                                  titlePanel("Exploratory Data Analysis"),
                                  sliderInput("ageInput", "Age", min = 15, max = 22,
                                              value = c(15, 22)),
                                  checkboxGroupInput("subjectInput", "Subject",
                                                     choiceNames = c("Mathematics", "Portuguese"),
                                                     choiceValues = c("Mat", "Por"),
                                                     selected = c("Mat", "Por")
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
                                        selectInput("subjectPor1", "Which subject are you visiting:",
                                                    c("Portuguese" = 1,
                                                      "Mathematics" = 0)),
                                        actionButton("Calculate", "Calculate")
                              ),
                              mainPanel(h3("\n", "Your average grade:", "\n"),
                                        
                                        h2(textOutput("result")))
                     )
                     
          )
)     
