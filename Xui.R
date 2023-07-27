library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
library(shinyjs)
dashboardPage(#skin="orange",
              
              #Title
              dashboardHeader(title="Chi-Square Goodness-fit-Test and Simulation ",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 221,
                sidebarMenu(
                  id = "tabs",
                  menuItem("Overview", tabName = "over", icon = icon("dashboard")),
                  menuItem("View an Example", tabName = "first", icon = icon("wpexplorer")),
                  menuItem("Test Your Own Dataset", tabName = "second", icon = icon("wpexplorer")),
                  menuItem("Let's Practice!", tabName = 'third', icon = icon('gamepad'))
                )),
              
              #Content within the tabs
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css")
                ),
                
                useShinyjs(),
                
                tags$head(
                  tags$style(HTML("
                                  .shiny-output-error-validation {
                                  color: #FF5733;
                                  font-size: 30px; 
                                  
                                  }
                                  ")),
                  
                  
                  tags$style(HTML("
                                  .tabbable > .nav > li > a {background-color: grey;  color:white}
                                  .tabbable > .nav > li > a[data-value='Simulated p-values plot'] {background-color: #FF5733;  color:white}
                                  .tabbable > .nav > li > a[data-value=' Comparsion to null distribution '] {background-color: #FF5733;  color:white}
                                  .tabbable > .nav > li[class=active]    > a {background-color: #FF5733; color:white}
                                  "))),
                
                
                useShinyjs(),
                
                tabItems(
                  
                  tabItem(tabName = "over",
                          tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                          br(),br(),br(),
                          h3(tags$b("About:")),
                          h4("In this app you will explore Chi-Square Goodness-fit-Test with simulations.
                                        The test is applied when you have categorical variables from a population." 
                          ),
                          h4(tags$div("When an analyst attempts to fit a statistical model to observed data, he or she may wonder
                                  how well the model actually reflects the data. How close are the observed values to those which would be expected under the fitted model? 
                                  One statistical test that addresses this issue is the chi-square goodness of fit test. In general, the chi-square test statistic is of the form:"
                          ),
                          tags$img(src = "fit.jpg", width = "384px", height = "86px", style="display: block; margin-left: auto; margin-right: auto;"),
                          
                          h4("If the computed test statistic is large, then the observed and expected 
                                               values are not close and the model is a poor fit to the data. "
                                   
                          )),
                          br(),
                          h3(tags$b("Instructions:")),
                          h4(tags$li("Select one of the scenarios for the proportion in each category (Equal Probabilities or Different Probabilities).")),
                          h4(tags$li("Move the sliders to change the values of number of observations, number of categories and number of simulations.")),
                          
                          h4(tags$li("A p-value is calculated and plotted for each simulation. You 
                                               can click a p-value on the plot to see the summary table for that 
                                               dataset.")),
                          h4(tags$li("When there are more than 50 simulations, only a histogram of p-values is shown.")),
                          h4(tags$li("Click the ", tags$strong("link on the bottom-left "),
                                      "if you have your own data and null hypothesis to explore.")), 
                          
                          div(style = "text-align: center",bsButton("explore", "Explore", icon("bolt"), size = "large")),
                          br(),
                          h3(tags$b("Acknowledgements:")),
                          h4("This app was developed and coded by Jinglin Feng. Special thanks to Alex Chen and Yuxin Zhang for help on some programming issues. "),
                          h4('This app was last modified by Anna (Yinqi) Zhang.')
                          
                  ),
                  
                  #Define the content contained within part 1 ie. tabname "first"
                  tabItem(tabName = "first",
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                          ),
                          fluidRow(
                            withMathJax(),
                            column(4,
                                   h3("Introduction:"),
                                   box(width ="10.5%",background = "orange",
                                      "Use the sliders below for simulating example data from either an equiprobable or different probability null (one p-value calculated for each simulation). Hit the Link below if you have your own data and hypothesis to examine."),
                                   
                                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #FF5733}")),
                                   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #FF5733}")),
                                   tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #FF5733}")),
                              
                                   radioButtons("random", "Proportion in each category", choices = c("Equiprobable Null", "Different Null Probabilities")),
                                   
                                   sliderInput("n", "Sample Size:", min = 200, max = 2000, value = 1100 ,
                                               step = 1),
                                   bsPopover("n", "", "Number of Observations", place="right"),
                                   
                                   sliderInput("n2", "The number of Categories:", min = 2, max = 8, value = 5 ,
                                               step = 1) ,
                                   
                                   bsPopover("n2", "", "Number of Categories", place="right"),
                                   
                                   sliderInput("n3", "The number of Simulations:", min = 1, max = 1000, value = 5 ,
                                               step = 1),
                                   
                                   bsPopover("n3", "", "For the first 50 simulations, you will see a p-value scatterplot; For the number of simulations greater than 50, you will see a histogram of p-values.", place="right", options = list(container = "body")),
                                   
                                   div(style = "text-align: left" ,bsButton("test", tags$strong("Click here if you have real data to test"), icon("hand-o-right"), size = "large", style = "link")),
                                   
                                   
                                   conditionalPanel(
                                   
                                   condition="input.n3 <= 50",
                                   #tags$img(src = "arrow.gif", width="15%", style="display: block; margin-left: 115px; margin-right: 4px;"),
                                   
                                   textOutput("hint"),
                                   tags$head(tags$style("#hint{color: #FF5733 ;
                                                        font-size: 18px;
                                                        
                                                        
                                                        }"
                                                          ))
                                   )
                                   
                                   ),
                          
                                 h3("Table and Plot:"),
                                 column(7,align="center",
                                        
                                 conditionalPanel(condition = "input.random == 'Equiprobable Null'",
                                                  tableOutput("values2"),
                                                  bsPopover("values2","","An example of a summary table of population values", placement = "bottom", options = list(container = "body")),
                                   
                                       conditionalPanel(condition = "input.random == 'Equiprobable Null'",             
                                                  plotOutput("plot2", width='90%', click = "plot_click"),
                                                  bsPopover("plot2","","For the number of simulations less than or equal to 50, click a point on the scatterplot to see the table behind it; For the number of simulations greater than 50, you will see a histogram of p-values. The red line denotes the uniform density of p-values under the null ", place="bottom", options = list(container = "body")),
                                                  tableOutput("plot_clickedpoints"), 
                                                  bsPopover("plot_clickedpoints","","An example of a summary table of sample values", placement = "right", options = list(container = "body")),
                                                  htmlOutput("text2", class="text-center")
                                                  
                          ))),
                          
                          column(7,align="center",
                                 
                                 conditionalPanel(condition = "input.random == 'Different Null Probabilities'",
                                                  tableOutput("values1"),
                                                  bsPopover("values1","","An example of a summary table of population values", placement = "bottom", options = list(container = "body")),
                                               
                                                  plotOutput("plot1", width="90%", click = "plot_click"),
                                                  bsPopover("plot1","","For the number of simulations less than or equal to 50, click a point on the scatterplot to see the table behind it; For the number of simulations greater than 50, you will see a histogram of p-values. The red line denotes the uniform density of p-values under the null )", place="right", options = list(container = "body")),
                                                 
                                                  tableOutput("plot_clickedpoints2"), 
                                                        bsPopover("plot_clickedpoints2","","An example of a summary table of sample values", placement = "right", options = list(container = "body")),
                                                        htmlOutput("text1", class="text-center")))                 
                                       )
                          
                  
                  
                 
                                   
                                   
                            ),
                  tabItem(tabName = "second",
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                          ),
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(4,
                                   
                                   
                                   textInput("names",h4(tags$strong("Level Names")),
                                             ""),
                                   br(),
                                   
                                   textInput("nulls",h4(tags$strong("Null Probabilities")),
                                             ""),
                                   bsPopover(id = 'nulls', title = 'Prob Info', content = 'All the null probabilities should add up to 1. For example, if there are four levels, the null probabilities could be 0.2, 0.2, 0.3, 0.3'),
                                   br(),
                                   
                                   textInput("obs",h4(tags$strong("Observed Counts")),
                                             ""),
                                   bsPopover(id = 'obs', title = 'Observation Info', content = 'The observed counts entered should have the same levels as the null probabilities. For example, if the null probabilities are 0.25, 0.3, 0.2, and 0.25, the observed counts entered could be 13, 24, 4, and 10'), 
                                   br(), 
                                   
                                   numericInput("sims",h4(tags$strong("Number of simulations from null model")),1,min=0,step=1),
                                   br(),
                                   actionButton("resample",h4(tags$div(tags$strong("Simulate Now")))),
                                   conditionalPanel(
                                     condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
                                     actionButton("reset",h4(tags$div(tags$strong("Start Over"))))
                                   )
                                   
                            ),
                            
                            column(8,
                                   conditionalPanel(
                                     condition="input.resample == 0 || output.totalPrev == output.total",
                                     plotOutput("barGraphInitial"),
                                     p(textOutput("remarksInitial")),
                                     tags$head(tags$style("#remarksInitial{color: black ;
                                                          font-size: 17px;
                                                          }"
                                                          )),
                                     tableOutput("obsTable"),
                                     tags$head(tags$style("#obsTable{color: black ;
                                                          font-size: 16px;
                                                          }"
                                                          ))
                                     ),
                                   
                                   conditionalPanel(
                                     condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
                                     tabsetPanel(
                                       tabPanel(h5(tags$div(tags$strong("Latest Simulation"),style = "color:white" )),
                                                plotOutput("barGraphLatest"),
                                                bsPopover("barGraphLatest", "", "This plot shows you the comparison of latest resample data verse actual data", placement = "left", options = list(container="body")),
                                                p(textOutput("remarksLatest1")),
                                                tags$head(tags$style("#remarksLatest1{color: black ;
                                                                     font-size: 17px;
                                                                     }"
                                                          )),
                                                tableOutput("summary1"),
                                                tags$head(tags$style("#summary1{color: black ;
                                                                     font-size: 16px;
                                                                     }"
                                                          ))), 
                                       
                                       tabPanel(h5(tags$div(tags$strong("Simulated p-values plot"),style = "color:white" )),
                                                plotOutput("pvalueplot",height=400,width=630)),                                                                                                                                                                                                                                                                                     
                                       
                                       
                                       tabPanel(h5(tags$div(tags$strong("
                                                                        Comparsion to null distribution "), style = "color:white" )),
                                                plotOutput("chisqCurve"),
                                                br(),
                                                conditionalPanel(
                                                  
                                                  condition="input.sims <= 5",
                                                  p(textOutput("remarksProb2")),
                                                  tags$head(tags$style("#remarksProb2{color: #FF5733 ;
                                                                       font-size: 18.5px;
                                                                       font-style: italic;
                                                                       }"
                                                          ))),
                                                
                                                conditionalPanel(
                                                  
                                                  condition="input.sims > 5",
                                                  p(textOutput("remarksProb")),
                                                  tags$head(tags$style("#remarksProb{color: #FF5733 ;
                                                                       font-size: 18.5px;
                                                                       font-style: italic;
                                                                       }"
                                                          )))
                                                
                                                  ))),
                                   
                                   
                                   
                                   id="myPanel"
                                                  )
                                                  )),
                  tabItem(tabName = "third",
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                          ),
                          tabsetPanel(id = "game",
                                      tabPanel(title = h4(strong("Instructions")), value = "instr",
                                               fluidPage(theme = 'Muted',
                                                         titlePanel('Instructions to Answering the Practice Questions'),
                                                         h3(p('Click on the GO! button to start the game.')),
                                                         h3(p('Select from the dropdown menu the answer you think correct.')),
                                                         br(),
                                                         div(style = "text-align: center", 
                                                             bsButton(inputId = "bsButton4",label = "GO!", icon('bolt'),  size = "median"))
                                                         
                                               )
                                      ),
                                      tabPanel(title = h4(strong("Multiple Choice")), value = "fib",
                                               fluidRow(
                                                 box(width = 12, style = 'color: #FFFFFF; background-color: #ff8080', textOutput('question'))
                                               ),
                                               
                                               fluidRow(
                                                 box(width = 12, style = 'color: #000000; background-color: #ff8080', htmlOutput('questionChoice'))
                                               ),
                                               
                                               conditionalPanel('input.submitX != 0',
                                                                htmlOutput('challengeFeedback'),
                                                                htmlOutput('textFeedback')),
                                               br(),
                                               div(style = "text-align: center",
                                                   bsButton(inputId = 'submitX', label = 'Check Answer',size = 'median'),
                                                   bsButton(inputId = 'nextX', label = 'Next',size = 'median'))
                                               
                                               #div(style = "text-align: center", )
                                               
                                      ))))
                          
                  )
)


                  
            
    