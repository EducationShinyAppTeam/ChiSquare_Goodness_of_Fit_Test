# Load Packages ----
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(boastUtils) 
library(ggplot2)
# test

# Load additional dependencies and setup functions
source("chisqplot.R") # Loads the script to make the chi sq plot.

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "yellow",
    ### Create the app header ----
    dashboardHeader(
      title = "Goodness-of-Fit Test",
      titleWidth = 250,
      tags$li(
        class = "dropdown", 
        actionLink("info", 
                   icon("info"))
        ),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(
          name = "Goodness_of_Fit_Test"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/', 
               icon("home"))
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Example", tabName = "example", icon = icon("wpexplorer")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Chi-Square Goodness-of-Fit Test"),
          # Please reformat long lines of code; the line below is an example
          p("In this app you will explore the Chi-Square Goodness-fit-Test with
            simulations. The test is applied when you have categorical variables
            from a population."),
          br(),
          h2("Instructions"),
          tags$ul(
            tags$li("Select one of the scenarios for the proportion in each 
                    category (Equal Probabilities or Different Probabilities)."),
            tags$li("Move the sliders to change the values of number of 
                    observations, number of categories, and number of 
                    simulations."),
            tags$li("A p-value is calculated and plotted for each simulation. 
                    You can click a p-value on the plot to see the summary 
                    table for that dataset."),
            tags$li("When there are more than 50 simulations, only a histogram 
                    of p-values is shown."),
            tags$li("You can use the Explore page to explore your own data and 
                    null hypothesis.")
          ),
          br(),
          ##### Prerequisites Button ----
          div(
            style = "text-align: center",
            bsButton(
              inputId = "goToOverview",
              label = "Prerequisites",
              size = "large",
              icon = icon("book"),
              style = "default"
            )
          ),
          ##### Set Acknowledgements Part ----
          br(),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by Jinglin Feng. Later modified 
            by Anna (Yinqi) Zhang. Special thanks to Alex Chen, Yuxin Zhang and 
            Dr. Neil Hatfield for help on some programming issues. The app was 
            further updated by Yudan Zhang in 2021 and Sean Burke in 2023."),
          br(),
          br(),
          "Cite this app as:",
          br(),
          citeApp(),
          br(),
          br(),
          div(class = "updated", "Last Update: 6/29/2023 by SB.")
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h1("Prerequisites"),
          p("When an analyst attempts to fit a statistical model to observed 
            data, they may wonder how close are the observed values to those 
            which would be expected. One statistical test that addresses this 
            issue for categorical data is the chi-square goodness-of-fit test."),
         p("If the observed and expected values are not close, 
           then the computed test statistic is large and the model is a poor 
           fit to the data."),
         box(
           title = tags$strong("Hypotheses"), 
           status = "primary",
           collapsible = TRUE,
           collapsed = FALSE,
           width = '100%',
           tags$ul(
             tags$li("Null: The distribution of the variable follows the 
                     expected distribution."),
             tags$li("Alternative: The distribution of the variable differs 
                     from the expected distribution.")
           )
         ),
         box(
           title = tags$strong("Form of Statistics"), # Spell check
           status = "primary",
           collapsible = TRUE,
           collapsed = FALSE,
           width = '100%',
           "The statistics has this form 
           \\[X^2 = \\sum\\frac{(observed-expected)^2}{expected}\\]
           For large samples, this follows the chi-square distribution with
           degrees of freedom: number of categories - 1. Alternatively, the
           distribution may be simulated for any size samples."
           ),
         # Prereq Button: 
         div(
           style = "text-align: center",
           bsButton(
             inputId = "goToPrereq",
             label = "Examples",
             size = "large",
             icon = icon("bolt"),
             style = "default"
           )
         )
        ),
        #### Set up the Example Page ----
        # Define the content contained within part 1 ie. tabname "first"
        tabItem(
          tabName = "example",
          withMathJax(),
          
          # Introduction
          h2("Introduction"),
          p("Use the sliders below for simulating example data from either an 
            equiprobable or different probability null 
             (one p-value calculated for each simulation). 
            Try different number of simulations to see how the distribution 
            of p-value will behave, 
            and read the captions below the sliders for more interpretation of 
            the plot.
            Go to the Explore page if you have your own data and hypothesis to 
            explore."),
          br(),
          ##### sideBar on the left ----
          # This page needs redesigned/coded
          fluidRow(
            column(
              width = 4,
              wellPanel(
                div(
                  align = "left",
                   radioButtons(
                     inputId = "random",
                     label = "Proportion in each category",
                     choices = c("Null with equal probabilities", 
                                 "Null with different probabilities")
                   ),
                  sliderInput(
                    inputId = "sampleBar",
                    label = "Sample size",
                    min = 200,
                    max = 2000,
                    value = 1100 ,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "categoriesBar",
                    label = "Number of categories",
                    min = 2,
                    max = 8,
                    value = 5 ,
                    step = 1
                  ) ,
                  sliderInput(
                    inputId = "simulationsBar",
                    label = "Number of simulations",
                    min = 1,
                    max = 1000,
                    value = 5 ,
                    step = 1
                  )
                )
              ),
              ##### Captions based simulationsBar ----
              conditionalPanel(
                condition = "input.simulationsBar <= 50",
                textOutput("hint1")
              ),
              conditionalPanel(
                condition = "input.simulationsBar >= 50",
                textOutput("hint2")
              )
            ),
               ##### Table and Plot on the right ----
            column(
              width = 8,
              align = "center",
              conditionalPanel(
                condition = "input.random == 'Null with equal probabilities'",
                tableOutput("equalValues"),
                
                conditionalPanel(
                  condition = "input.random == 'Null with equal probabilities'",
                  plotOutput(
                    outputId = "equalPlot",
                    width = '90%',
                    click = "plot_click"
                  ),
                  tableOutput("equalPlotClickedpoints"),
                  htmlOutput(
                    outputId = "equalText",
                    class = "text-center"
                  )
                )
              )
            ),
            column(
              width = 8,
              align = "center",
              conditionalPanel(
                condition = "input.random == 'Null with different probabilities'",
                tableOutput("diffValues"),
                plotOutput(
                  outputId = "diffPlot",
                  width = "90%",
                  click = "plot_click"
                ),
                tableOutput("diffPlotClickedpoints"),
                htmlOutput(
                  outputId = "diffText",
                  class = "text-center"
                )
              )
            )
          )
        ),
        #### Set up the Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          # Use h2 here
          h2("Explore Your Own Data"),
          p("You can use this page to see the behavior of distribution of the 
            chi-square statistic for your data. If you need guidence for each part, 
            put arrow on that box. When you finished, click on Simulate Now to 
            see the plots."),
          fluidRow(
            #column of length 12 which is the whole width
            #I include everthing in a column though because this way there are margins and it looks better
            ##### sideBar on the left ----
            column(
              width = 4,
              wellPanel(
                align = "left",
                textInput(
                  inputId = "names",
                  label = HTML(
                    "Level names", 
                    as.character(
                      actionLink(
                        inputId = "namesPopover", 
                        label = "", 
                        icon = icon("info")
                      )
                    )
                  ),
                  value = ""
                ),
                textInput(
                  inputId = "nulls",
                  label =  HTML(
                    "Null probabilities",
                    as.character(
                      actionLink(
                        inputId = "nullsPopover", 
                        label = "", 
                        icon = icon("info")
                      )
                    )
                  ),
                  value = ""
                ),
                textInput(
                  inputId = "obs",
                  label = HTML(
                    "Observed counts",
                    as.character(
                      actionLink(
                        inputId = "obsPopover", 
                        label = "", 
                        icon = icon("info")
                      )
                    )
                  ),
                  value = ""
                ),
                sliderInput(
                  inputId = "sims",
                  label = "Additional number of simulations from null model",
                  min = 50,
                  max = 5000,
                  value = 100,
                  step = NULL
                ),
                br(),
                ##### resample & reset Bottons ----
                # This should be bsButton
                bsButton(
                  inputId = "resample",
                  label = tags$div("Simulate Now"),
                  size = "large"
                ),
                br(),
                # We need to get rid of these conditional panels 
                bsButton(
                  inputId = "reset",
                  label =  tags$div("Start Over"),
                  size = "large"
                )
              )
            ),
                  ##### Plots on right ----
            column(
                  width = 8,
                  
                  ###### Initial Plot "barGraphInitial" & Table "obsTable" ----
                   conditionalPanel(
                     condition = "input.resample == 0 || output.totalPrev == output.total",
                     plotOutput(outputId = "barGraphInitial"),
                     p(textOutput("remarksInitial")),
                       tableOutput("obsTable"),
                   ),
                   conditionalPanel(
                     condition = "(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
                     tabsetPanel(
                       ###### 1st Plot "barGraphLatest" & Table "summary1"----
                       tabPanel(
                         h5(
                           tags$div("Latest Simulation", style = "color:black" )
                         ),
                         plotOutput(outputId = "barGraphLatest"),
                         bsPopover(
                           id = "barGraphLatest",
                           title = "",
                           content = "This plot shows you the comparison of latest 
                           resample data verse actual data",
                           placement = "left",
                           options = list(container = "body")
                         ),
                         p(textOutput("remarksLatest1")),
                         tableOutput("summary1"),
                       ),
                   ###### 2nd Plot "pValuePlot" ----
                   tabPanel(
                     h5(
                       tags$div("Simulated p-values plot", style = "color:black" )
                     ),
                     plotOutput(
                       outputId = "pValuePlot",
                       height = 400,
                       width = 630
                     )
                   ),
                   ###### 3rd Plot "chisqCurve" (remarksProb2 & remarksProb)----
                   tabPanel(
                     h5(
                       tags$div("Comparsion to null distribution", style = "color:black" )),
                     plotOutput(outputId = "chisqCurve"),
                     br(),
                     # no. of simulation:
                     # conditionalPanel(
                     #  condition = "input.sims <= 5",
                     # p(textOutput("remarksProb2")),
                     # ),
                     
                     conditionalPanel(
                       condition = "input.sims > 5",
                       p(textOutput("remarksProb")
                       ),
                     )
                   )
                     )
                   ),
                  id = "myPanel"
            )
          )
        ),

        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p( # shinyBS
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter Bootstrap Components for Shiny. 
            R package version 0.61. Available from 
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p( # boastUtils
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. 
            R package version 0.1.6.3. Available from 
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p( # shiny
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. 
            (2020). shiny: Web Application Framework for R. R package 
            version 1.5.0. Available from 
            https://CRAN.R-project.org/package=shiny"
          ),
          p( # shinydashboard
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard: Create
            Dashboards with 'Shiny'. R package version 0.7.1. Available from 
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p( 
            class = "hangingindent",
            "Chi-Square Goodness of Fit.” Statistics Online Support, 
            sites.utexas.edu/sos/guided/inferential/categorical/univariate/chi2/"
          ),
          p( # shinyjs
            class = "hangingindent",
            "Dean Attali (2020). shinyjs: Easily Improve the User Experience of 
            Your Shiny Apps in Seconds. R package version 2.0.0. 
            https://CRAN.R-project.org/package=shinyjs"
          ),
          p( # shinyWidgets
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets: 
            Custom Inputs Widgets for Shiny. R package version 0.5.3. 
            Available from https://CRAN.R-project.org/package=shinyWidgets"
            ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
    ## Bottons Output ----
    ### Info button
    observeEvent(
      eventExpr = input$info,
      handlerExpr = {
        sendSweetAlert(
          session = session,
          type = "info",
          title = "Information",
          text = "You will explore Chi-Square Goodness-fit-Test with simulations in this app",
          btn_colors = boastUtils::boastPalette[5]
        )
      }
    )
  
    ### Button on each page
   # Overview
  observeEvent(
    eventExpr = input$goToOverview,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
  
  # Prerequisites
  observeEvent(
    eventExpr = input$goToPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "example"
      )
    }
  )
  
    # Explore
  
  # Remove
    # observeEvent(
    #   eventExpr = input$namesPopover,
    #   handlerExpr = {
    #     sendSweetAlert(
    #       session = session,
    #       type = "info",
    #       title = "Level Info",
    #       text = "Enter level names for each category, 
    #     seperated by commas ( e.g. A,B,C,D ).",
    #     btn_colors = boastUtils::boastPalette[5]
    #     )
    #   }
    # )
    # 
    # observeEvent(
    #   eventExpr = input$nullsPopover,
    #   handlerExpr = {
    #     sendSweetAlert(
    #       session = session,
    #       type = "info",
    #       title = "Prob Info",
    #       text = "All the null probabilities should add up to 1. 
    #     For example, if there are four levels, 
    #     the null probabilities could be 0.2, 0.2, 0.3, 0.3",
    #     btn_colors = boastUtils::boastPalette[5]
    #     )
    #   }
    # )
    # 
    # observeEvent(
    #   eventExpr = input$obsPopover,
    #   handlerExpr = {
    #     sendSweetAlert(
    #       session = session,
    #       type = "info",
    #       title = "Observation Info",
    #       text = "The observed counts entered should have the same levels as the null probabilities. 
    #     For example, if the null probabilities are 0.25, 0.3, 0.2, and 0.25, 
    #     the observed counts entered could be 13, 24, 4, and 10.",
    #     btn_colors = boastUtils::boastPalette[5]
    #     )
    #   }
    # )
    
    ## Example Page ---- 
    ### tableData Set Up ----
    # For diffProb:
    firstdata <- reactive(
      x = {
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        ss = input$simulationsBar
        v$click1 <- NULL
        mytable <- list(0)
        for (i in 1:ss) {
          x <- sample(1:nn,num_of_samples,replace = T)
          mytable[i] <- list(x)
        }
        mytable
      }
    )
    
    # For equalProb:
    firstdata2 <- reactive(
      x = {
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        ss = input$simulationsBar
        v$click1 <- NULL
        mytable <- list(0)
        for (i in 1:ss) {
          x <- sample(1:nn,num_of_samples,replace = T)
          mytable[i] <- list(x)
        }
        mytable
      }
    )
    
    #### table Output ----
    # For diffProb：
    sliderValues <- reactive(
      x = {
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        # pp=numeric(0)
        x1 <- sample(1:nn,num_of_samples,replace = TRUE)
        gen <- runif(input$categoriesBar)
        trueProp = gen/sum(gen)
        total = table(x1)
        expected = trueProp*input$sampleBar
        
        # Compose data frame for diffProb:
        xx = cbind(paste0(LETTERS[1:nn]),round(expected,2), round(round(expected,2)/sum(round(expected,2)),3))
        xx = as.data.frame(xx,stringsAsFactors = FALSE)
        colnames(xx) = c("Categories","Expected Value", "Expected Proportion")
        xx[nrow(xx) + 1,] <- c("Total", round(sum(round(expected,2)),0),"1")
        xx
        
      }
    )
    
    # For equalProb：
    sliderValues2 <- reactive(
      x = {
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        # pp=numeric(0)
        x1 <- sample(1:nn,num_of_samples,replace = TRUE)
        
        # Compose data frame for equalProb:
        xx = cbind(
          paste0(LETTERS[1:nn]),
          round(rep(num_of_samples/nn,nn),2), 
          round(
            round(
              rep(num_of_samples/nn,nn),
              2)/sum(round(rep(num_of_samples/nn,nn),2)),3))
        xx = as.data.frame(xx,stringsAsFactors = FALSE)
        colnames(xx) = c("Categories","Expected Value", "Expected Proportion")
        xx[nrow(xx) + 1,] <- c("Total", round(sum(round(rep(num_of_samples/nn,nn),2)),0),"1")
        xx
      }
    )
    
    # For diffProb
    output$diffValues <- renderTable(
      expr = {
        sliderValues()
      },
      align = "c",
      caption = "Example Population Summary Table",
      caption.placement = "top",
      style = "bootstrap4",
      width = "150%"
    )
    
    # For equalProb
    output$equalValues <- renderTable(
      expr = {
        sliderValues2()
      },
      align = "c",
      caption = "Example Population Summary Table",
      caption.placement = "top",
      style = "bootstrap4",
      width = "150%"
    )
    
    #### Captions Output ----
    output$hint1 <- renderText(
      expr = {
        paste0("For the number of simulations less than or equal to 50, the plot shows the individual p-value of each simulation.
           Click any point on the scatterplot and scroll down to see a more detailed table")
      }
    )
    
    output$hint2 <- renderText(
      expr = {
        paste0("For the number of simulations greater than 50, 
            the plot shows a histogram of the p-value observed. 
            The red line denotes the uniform density of p-values under the null.")
      }
    )
    
    ### plotData Set Up ----
    # For diffProb
    plotdata <- reactive(
      x = {
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        ss = input$simulationsBar
        v$click1 <- NULL
        pp = numeric(0)
        xx <- firstdata()
        for (i in 1:ss) {
          x <- unlist(xx[i])
          gen <- runif(input$categoriesBar)
          trueProp = gen/sum(gen)
          total = table(x)
          expected = trueProp*input$sampleBar
          a <- chisq.test(table(x), correct = FALSE, rescale.p = TRUE )
          pp[i] = a$p.value
          
        }
        if (ss <= 50) {
          index = seq(1,length(pp))
          data = data.frame(index,pp)
        }
        else
        {
          data = data.frame(pp)
        }
        list(x = data, y = gen)
      }
    )

    # For equalProb
    plotdata2 <- reactive(
      x = {
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        ss = input$simulationsBar
        v$click1 <- NULL
        pp = numeric(0)
        xx <- firstdata2()
        for (i in 1:ss) {
          x <- unlist(xx[i])
          nulls = 1/(1:nn)
          total = table(x)
          expected = nulls*total
          a <- chisq.test(table(x), correct = FALSE, rescale.p = TRUE )
          pp[i] = a$p.value
          
        }
        if (ss <= 50) {
          index = seq(1,length(pp))
          data = data.frame(index,pp)
        }
        else
        {
          data = data.frame(pp)
        }
        data
      }
    )
    
    # clickPoints on plot for each prob 
    v <- reactiveValues(click1 = NULL)
    
    observeEvent(
      eventExpr = input$plot_click,
      handlerExpr = {
        v$click1 <- input$plot_click
      }
    )
    
    #### scatterPlot and histogram Output ---- 
    # For diffProb
    # output$diffPlot <- renderPlot(
    #   expr = {
    #     ss = input$simulationsBar
    #     nn = input$categoriesBar
    #     d <- plotdata()$x
    #     coordinatex <- v$click1$x
    #     coordinatey <- v$click1$y
    #     if (ss <= 50)
    #     {
    #       plot(d$index,
    #            d$pp,
    #            pch = 20,
    #            col = boastUtils::boastPalette[5], 
    #            cex = 1.5,
    #            font.lab = 2,
    #            xlab = "Simulation Number",
    #            ylab = "P-value",
    #            main = "Scatterplot of Simulation Number and P-value")
    #       if (!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01)
    #         points(x = round(coordinatex),
    #                y = d$pp[round(coordinatex)],
    #                col =  boastUtils::psuPalette[4],
    #                pch = 20,
    #                cex = 2.5)
    #     }
    #     # histgram
    #     else {
    #       par(xpd = F)
    #       hist(d$pp,
    #            breaks = 5,
    #            xlab = "P-value",
    #            font.lab = 2,
    #            main = "P-value Distribution from the simulations")
    #       abline(h = ss/5, col = boastUtils::psuPalette[2])}
    #   }
    # )
    
    output$diffPlot <- renderPlot(
      expr = {
        ss <- input$simulationsBar
        nn <- input$categoriesBar
        d <- plotdata()$x
        coordinatex <- as.numeric(v$click1$x)
        coordinatey <- v$click1$y
        if (ss <= 50) {
          plt <- ggplot(data = d, aes(x = index, y = pp)) +
            geom_point(
              col = boastUtils::boastPalette[5],
              size = 4
            ) +
            labs(
              x = "Simulation Number", 
              y = "P-value",
              title = "Scatterplot of Simulation Number and P-value"
            ) +
            theme_bw() +
            theme(
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 13),
              plot.title = element_text(size = 15, face = "bold")
            ) +
            coord_cartesian(clip = "off") 
          
          if (!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 
              && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) {
            plt <- plt + geom_point(
              data = d[round(coordinatex), , drop = FALSE],
              aes(x = index, y = pp),
              col = boastUtils::psuPalette[4],
              size = 5.5
            )
          }
          plt
        } else {
          plt <- ggplot(data = d, aes(x = pp)) +
            geom_histogram(
              bins = 5,
              fill = boastUtils::boastPalette[7]
              , col = boastUtils::boastPalette[5]
            ) +
            labs(
              x = "P-value", 
              y = "Frequency",
              title = "P-value Distribution from the simulations"
            ) +
            theme_bw() +
            theme(
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 13),
              plot.title = element_text(size = 15, face = "bold")
            ) +
            geom_hline(
              yintercept = ss/5,
              col = boastUtils::psuPalette[2]
            )
          plt
        }
      }
    )
    
    # For equalProb
    output$equalPlot <- renderPlot(
      # expr = {
      #   coordinateatex <- v$click1$x
      #   coordinatey <- v$click1$y
      #   ss = input$simulationsBar
      #   nn = input$categoriesBar
      #   d <- plotdata2()
      #   if (ss <= 50)
      #   {
      #     plot(d$index,
      #          d$pp,
      #          pch = 20,
      #          col =  boastUtils::boastPalette[5],
      #          cex = 1.5,
      #          font.lab = 2,
      #          xlab = "Simulation Number",
      #          ylab = "P-value",
      #          main = "Scatterplot of Simulation Number and P-value")
      #     if (!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01)
      #       points(
      #         x = round(v$click1$x),
      #         y = d$pp[round(v$click1$x)],
      #         col = boastUtils::psuPalette[4],
      #         pch = 20,
      #         cex = 2.5
      #       )
      #   }
      #   # histgram
      #   else {
      #     par(xpd = F)
      #     hist(d$pp,
      #          breaks = 5,
      #          xlab = "P-value",
      #          font.lab = 2,
      #          main = "P-value Distribution from the simulations")
      #     abline(h = ss/5, col = boastUtils::psuPalette[2])}
      # }
      
      expr = {
        ss <- input$simulationsBar
        nn <- input$categoriesBar
        d <- plotdata2()
        coordinatex <- as.numeric(v$click1$x)
        coordinatey <- v$click1$y
        if (ss <= 50) {
          plt <- ggplot(data = d, aes(x = index, y = pp)) +
            geom_point(
              col = boastUtils::boastPalette[5],
              size = 4
            ) +
            labs(
              x = "Simulation Number", 
              y = "P-value",
              title = "Scatterplot of Simulation Number and P-value"
            ) +
            theme_bw() +
            theme(
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 13),
              plot.title = element_text(size = 15, face = "bold")
            ) +
            coord_cartesian(clip = "off") 
          
          if (!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 
              && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) {
            plt <- plt + geom_point(
              data = d[round(coordinatex), , drop = FALSE],
              aes(x = index, y = pp),
              col = boastUtils::psuPalette[4],
              size = 5.5
            )
          }
          plt
        } else {
          plt <- ggplot(data = d, aes(x = pp)) +
            geom_histogram(
              bins = 5,
              fill = boastUtils::boastPalette[7]
              , col = boastUtils::boastPalette[5]
            ) +
            labs(
              x = "P-value", 
              y = "Frequency",
              title = "P-value Distribution from the simulations"
            ) +
            theme_bw() +
            theme(
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 13),
              plot.title = element_text(size = 15, face = "bold")
            ) +
            geom_hline(
              yintercept = ss/5,
              col = boastUtils::psuPalette[2]
            )
          plt
        }
      }
    )
    
    #### following table for clickedPoint ----
    # For diffProb
    clickedpoints1 <- reactive(
      x = {
        # For base graphics, I need to specify columns, though for ggplot2,
        # it's usually not necessary.
        coordinatex <- v$click1$x
        coordinatey <- v$click1$y
        ss = input$simulationsBar
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        mytable <- firstdata()
        d <- plotdata()$x
        
        data <- plotdata()$x
        if (!(!is.null(v$click1$x) && 
              abs(coordinatex - round(coordinatex)) < 0.1 && 
              abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) || ss > 50)
          return()
        i <- round(v$click1$x)
        pvalue <- round(data$pp[round(v$click1$x)],3)
        
        x1 <- unlist(mytable[i])
        # gen<-runif(input$categoriesBar)
        trueProp = plotdata()$y/sum(plotdata()$y)
        total = table(x1)
        expected = trueProp*input$sampleBar
        xx = cbind(paste0(LETTERS[1:nn]),table(x1),round(expected,2),
                   round(table(x1)/sum(table(x1)),2),
                   round(round(expected,2)/sum(round(expected,2)),3))
        
        xx = as.data.frame(xx,stringsAsFactors = FALSE)
        colnames(xx) = c(
          "Categories",
          "Observed Value",
          "Expected Value", 
          "Observed Proportion",
          "Expected Proportion"
        )
        xx[nrow(xx) + 1,] <- c(
          "Total",
          sum(table(x1)),
          round(sum(round(rep(num_of_samples/nn,nn),2)),0),
          "1",
          "1"
        )
        xx
      }
    )

      # For equalProb
    clickedpoints2 <- reactive(
      x = {
        # For base graphics, I need to specify columns, though for ggplot2,
        # it's usually not necessary.
        coordinatex <- v$click1$x
        coordinatey <- v$click1$y
        ss = input$simulationsBar
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        mytable <- firstdata2()
        d <- plotdata2()
        
        data <- plotdata2()
        if (!(!is.null(v$click1$x) && 
              abs(coordinatex - round(coordinatex)) < 0.1 && 
              abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) || ss > 50)
          return()
        i <- round(v$click1$x)
        pvalue <- round(data$pp[round(v$click1$x)],3)
        x1 <- unlist(mytable[i])
        
        xx = cbind(paste0(LETTERS[1:nn]),table(x1),round(rep(num_of_samples/nn,nn),2),
                   round(table(x1)/sum(table(x1)),2),
                   round(round(rep(num_of_samples/nn,nn),2)/sum(round(rep(num_of_samples/nn,nn),2)),3))
        xx = as.data.frame(xx,stringsAsFactors = FALSE)
        colnames(xx) = c("Categories","Observed Value","Expected Value", "Observed Proportion", "Expected Proportion")
        xx[nrow(xx) + 1,] <- c("Total", sum(table(x1)),round(sum(round(rep(num_of_samples/nn,nn),2)),0),"1", "1")
        xx
      }
    )
    
    #### P-value of clickedPoint ----
    # For diffProb
    clickedpoints21 <- reactive(
      x = {
        # For base graphics, I need to specify columns, though for ggplot2,
        # it's usually not necessary.
        coordinatex <- v$click1$x
        coordinatey <- v$click1$y
        ss = input$simulationsBar
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        mytable <- firstdata()
        d <- plotdata()$x
        data <- plotdata()$x
        if (!(!is.null(v$click1$x) && 
              abs(coordinatex - round(coordinatex)) < 0.1 && 
              abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) || ss > 50)
          return()
        i <- round(v$click1$x)
        pvalue <- round(data$pp[round(v$click1$x)],3)
        paste("P-value =  ",
              as.character(pvalue) )
      }
    )

    # For equalProb
    clickedpoints22 <- reactive(
      x = {
        # For base graphics, I need to specify columns, though for ggplot2,
        # it's usually not necessary.
        coordinatex <- v$click1$x
        coordinatey <- v$click1$y
        ss = input$simulationsBar
        num_of_samples = input$sampleBar
        nn = input$categoriesBar
        mytable <- firstdata2()
        d <- plotdata2()
        data <- plotdata2()
        if (!(!is.null(v$click1$x) && 
              abs(coordinatex - round(coordinatex)) < 0.1 && 
              abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) || ss > 50)
          return()
        i <- round(v$click1$x)
        pvalue <- round(data$pp[round(v$click1$x)],3)
        paste("P-value =  ",
              as.character(pvalue) )
      }
    )
    
    #### following table and p-value text Output ----
    # following table:
    # For equalProb
    output$equalPlotClickedpoints <- renderTable(
      expr = {
        clickedpoints2()
      },
      align = "c",
      caption = "Example Sample Summary Table",
      caption.placement = "top",
      style = "bootstrap4"
    )

    # For diffProb
    output$diffPlotClickedpoints <- renderTable(
      expr = {
        clickedpoints1()
      },
      align = "c",
      caption = "Example Sample Summary Table",
      caption.placement = "top",
      style = "bootstrap4"
    )
    
    # p-value text:
    # For equalProb
    output$equalText <- renderText(
      expr = {
        clickedpoints22()
      }
    )
    
    # For diffProb
    output$diffText <- renderText(
      expr = {
        clickedpoints21()
      }
    )
    
    ## Explore Page ----
    simLimit <- 10000
    numberSims <- 0
    chisqSims <- numeric()
    latestSim <- NULL
    fullSim <- character()
    total <- 0     #total number of sims over all set-ups including current one
    totalPrev <- 0 #total number of sims over all set-ups excluding current one
    
    ##### sideBar Inputs Set Up ----
    namesInput <- reactive(
      x = {
        unlist(strsplit(input$names,split = ","))
      }
    )
    
    nullsInput <- reactive(
      x = {
        probs <- as.numeric(unlist(strsplit(input$nulls,split = ",")))
        probs
      }
    )
    
    obsInput <- reactive(
      x = {
        observed <- as.numeric(unlist(strsplit(input$obs,split = ",")))
        observed
      }
    )

    goodNames <- reactive(
      x = {
        names <- namesInput()
        goodNames <- TRUE
        if (length(names) <= 1) goodNames <- FALSE
        if (any(is.na(names))) goodNames <- FALSE
        if (!goodNames) disable("resample")
        goodNames
      }
    )

    goodNulls <- reactive(
      x = {
        nulls <- nullsInput()
        goodNulls <- TRUE
        if (length(nulls) < 1) goodNulls <- FALSE
        if (any(is.na(nulls))) goodNulls <- FALSE
        if (!any(is.na(nulls)) && any(nulls <= 0)) goodNulls <- FALSE
        if (!goodNulls) disable("resample")
        goodNulls
      }
    )

    goodNulls2 <- reactive(
      x = {
        nulls <- nullsInput()
        goodNulls <- TRUE
        if (sum(nulls) != 1) goodNulls <- FALSE
        if (!goodNulls) disable("resample")
        goodNulls
      }
    )
    
    goodObs <- reactive(
      x = {
        obs <- obsInput()
        goodObs <- TRUE
        if (length(obs) < 1) goodObs <- FALSE
        if (any(is.na(obs))) goodObs <- FALSE
        if (!any(is.na(obs)) && any(obs < 0)) goodObs <- FALSE
        if (!goodObs) disable("resample")
        goodObs
      }
    )

    obschisqInput <- reactive(
      x = {
        nulls <- nullsInput()/sum(nullsInput())
        totalCounts <- sum(obsInput())
        expected <- nulls*totalCounts
        sum(obsInput()^2/expected) - totalCounts
      }
    )

    simsUpdate <- reactive(
      x = {
        if (input$resample > 0) {
          nullProbs <- isolate(nullsInput()/sum(nullsInput()))
          totalCounts <- isolate(sum(obsInput()))
          expCounts <- nullProbs*totalCounts
          reps <- min(simLimit,isolate(input$sims))
          # The first argument, n, was misnamed sampleBar
          newSims <- rmultinom(n = reps, size = totalCounts, prob = nullProbs)
          chisqNew <- colSums(newSims^2/expCounts) - totalCounts
          chisqSims <<- c(chisqSims,chisqNew)
          latestSim <<- newSims[,reps]
          numberSims <<- numberSims + reps
          total <<- total + reps
          hide(
            id = "setup",
            anim = T,
            animType = "slide"
          )
          if (total - totalPrev == 1) {
            updateTabsetPanel(
              session = session,
              inputId = "myPanel",
              selected = "Latest Simulation"
              )
          }
          varLevels <- isolate(namesInput())
          namesList <- rep(varLevels,times = latestSim)
          fullSim <<- sample(namesList,size = totalCounts, replace = FALSE)
          list(numberSims,latestSim)
        }
      }
    )

    simsReset <- reactive(
      x = {
        input$reset
        totalPrev <<- totalPrev + numberSims
        numberSims <<- 0
        chisqSims <<- numeric()
        latestSim <<- NULL
        
        show(
          id = "setup",
          anim = T,
          animType = "slide"
          )
        return(totalPrev)
      }
    )
    
    dfInput <- reactive(
      x = {
        length(obsInput()) - 1
      }
    )
    
    xmaxInput <- reactive(
      x = {
        qchisq(0.999,df = dfInput())
      }
    )
    
    output$totalPrev <- reactive(
      x = {
        simsReset()
      }
    )
    
    outputOptions(output, 'totalPrev', suspendWhenHidden = FALSE)
    output$total <- reactive(
      x = {
        simsUpdate() #for dependency
        total
      }
    )
    
    ###### reset function ----
    observeEvent(
      eventExpr = input$reset,
      handlerExpr = {
       updateTextInput(
         session = session,
         inputId = "names",
         value = ""
       )
        updateTextInput(
          session = session,
          inputId = "nulls",
          value = ""
        )
        updateTextInput(
          session = session,
          inputId = "obs",
          value = ""
        )
        simsReset()
      }
    )

    ###### Hints for Inputs ---- 
    outputOptions(output, 'total', suspendWhenHidden = FALSE)
    
    output$barGraphInitial <- renderPlot(
      expr = {
        if (goodNames()) enable("resample") else disable("resample")
        validate(
          need(goodNames(),"To Start, enter level names for each category, 
             seperated by commas ( e.g. A,B,C,D )."
          ))
        
        if (goodNulls()) enable("resample") else disable("resample")
        validate(
          need(
            goodNulls(),
            "Enter your null probabilities as decimals separated 
             by commas. They should all be positive numbers that add to one."
          )
        )
        
        if (goodNulls2()) enable("resample") else disable("resample")
        validate(
          need(goodNulls2(),"Your probabilities should be added up to 1.")
        )
        
        names <- namesInput()
        nulls <- nullsInput()/sum(nullsInput())
        observed <- obsInput()
        
        # Names and Null Check
        lengthCheck <- length(names) == length(nulls)
        if (lengthCheck) enable("resample") else disable("resample")
        validate(
          need(lengthCheck,
               "Make sure that you enter the exact same number of level names 
               and null probabilities")
        )
        if (goodObs()) enable("resample") else disable("resample")
        validate(
          need(goodObs(),"Enter your observed counts separated by commas. 
               All counts must be non-negative whole numbers.")
        )
        
        lengthCheck2 <- (
          length(nulls) == length(observed)) && (length(observed) == length(names)
          )
        if (lengthCheck2) enable("resample") else disable("resample")
        validate(
          need(lengthCheck2,
               "Make sure that you enter the exact same number of level names, 
               null probabilities and observed counts")
        )
        ##### Plots Set Up ----
        ###### Initial Plot & Table ----
        # Histogram "remarksInitial":
        observed <- obsInput()
        expected <- nulls*sum(observed)
        tab <- rbind(observed,expected)
        rownames(tab) <- c("Observed","Expected")
        colnames(tab) <- names
        data <- data.frame(
          group = rep(colnames(tab), each = nrow(tab)),
          category = rep(rownames(tab), times = ncol(tab)),
          counts = as.vector(tab)
        )
        
        ggplot(data, aes(x = group, y = counts, fill = category)) +
          geom_bar(stat = "identity", position = "dodge", color = boastUtils::boastPalette[5]) +
          scale_fill_manual(
            values = c(
              boastUtils::boastPalette[3],
              boastUtils::boastPalette[6]
            )
          ) +
          labs(
            x = "",
            y = "Counts",
            title = "Bargraph of Observed and Expected Counts",
          ) +
          theme_bw() +
          theme(
            legend.position = "bottom",
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 15, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(size = 13)
          )
      }
    )
    
    output$remarksInitial <- renderText(
      expr = {
        observed <- obsInput()
        nulls <- nullsInput()/sum(nullsInput())
        names <- namesInput()
        allGood <- (goodNulls() && goodObs() && goodNulls2()) && goodNames()
        lengthCheck <- (length(nulls) == length(observed)) && (length(observed) == length(names))
        validate(
          need(allGood && lengthCheck,"")
        )
        # Table "obsTable":
        chisq <- obschisqInput()
        rounded1 <- round(chisq,2)
        p.value <- pchisq(chisq, length(nulls) - 1, lower.tail = FALSE)
        rounded2 <- round(p.value,3)
        paste("Observed Chi-Square Statistic =  ",
              as.character(rounded1),
              sep = "\n",
              ",",
              "P-value =  ",
              as.character(rounded2)
        )
      }
    )

    output$obsTable <- renderTable(
      expr = {
        observed <- obsInput()
        nulls <- nullsInput()/sum(nullsInput())
        names <- namesInput()
        allGood <- (goodNulls() && goodObs() && goodNulls2()) && goodNames()
        lengthCheck <- (length(nulls) == length(observed)) && (length(observed) == length(names))
        validate(
          need(allGood && lengthCheck,"")
        )
        
        expected <- nulls*sum(observed)
        contribs <- (observed - expected)^2/expected
        df <- data.frame(
          Levels = names,
          Observed = observed,
          Expected = round(expected,2),
          cont = round(contribs,2)
        )
        names(df)[4] <- c("Contribution to Chi-Square")
        df
      }, 
      align = "c"
    )
    
    ###### 1st Plot & Table Output ----
    # Histogram "barGraphLatest":
    output$barGraphLatest <- renderPlot(
      expr = {
        input$resample
        if (length(chisqSims) > 0) {
          totalCounts <- isolate(sum(obsInput()))
          nulls <- isolate(nullsInput()/sum(nullsInput()))
          expected <- totalCounts*nulls
          tab <- rbind(obsInput(),expected,latestSim)
          rownames(tab) <- c("Observed","Expected","Resampled")
          colnames(tab) <- isolate(namesInput())
          data <- data.frame(
            group = rep(colnames(tab), each = nrow(tab)),
            category = rep(rownames(tab), times = ncol(tab)),
            counts = as.vector(tab)
          )
          
          ggplot(data, aes(x = group, y = counts, fill = category)) +
            geom_bar(stat = "identity", position = "dodge", color = boastUtils::boastPalette[5]) +
            scale_fill_manual(
              values = c(
                boastUtils::boastPalette[3],
                boastUtils::boastPalette[6],
                boastUtils::boastPalette[7]
              )
            ) +
            labs(
              x = "",
              y = "Counts",
              title = "Bargraph of Observed, Expected, and Latest Resample",
            ) +
            theme_bw() +
            theme(
              legend.position = "bottom",
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 14),
              plot.title = element_text(size = 15, face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 13)
            )
        }
      }
    )
    
    # Table "summary1":
    output$remarksLatest1 <- renderText(
      expr = {
        input$resample
        chisq <- obschisqInput()
        rounded1 <- round(chisq,2)
        rounded2 <- round(chisqSims[length(chisqSims)],2)
        paste(
          "Observed chi-square statistic =  ",
          as.character(rounded1),sep = "\n",
          ",",
          "Latest resampled chi-square statistics = ",
          as.character(abs(rounded2))
        )
      }
    )
    output$summary1 <- renderTable(
      expr = {
        input$resample
        observed <- obsInput()
        nulls <- nullsInput()/sum(nullsInput())
        names <- namesInput()
        expected <- nulls*sum(observed)
        
        df <- data.frame(
          Levels = names,
          Observed = observed,
          Expected = round(expected,2),
          Resampled = latestSim
        )
        df
      }, 
      align = "c"
    )
    
    ###### 2nd Plot Output ----
    output$pValuePlot <- renderPlot(
      expr = {
        input$resample
        nulls <- nullsInput()/sum(nullsInput())
        observed <- obsInput()
        chisq <- obschisqInput()
        obs <- isolate(obschisqInput())
        n <- length(chisqSims)
        latest <- chisqSims[n]
        p.value <- pchisq(chisqSims, length(observed) - 1, lower.tail = FALSE)
        data <- data.frame(pvalue = p.value)
        ggplot(data = data, aes(x = pvalue)) +
          geom_histogram(
            bins = 10,
            fill = boastUtils::boastPalette[7],
            color = boastUtils::boastPalette[5],
            na.rm = TRUE
          ) +
          labs(
            x = "p-value",
            y = "Count",
            title = "P-value Distribution Histogram from the simulations"
          ) +
          scale_x_continuous(
            breaks = seq(0, 1, 0.2),
            limits = c(0,1)
          ) +
          theme_bw() +
          theme(
            axis.title = element_text(size = 14, face = "bold"),
            plot.title = element_text(size = 15, face = "bold")
          )
      }
    )
    
    ###### 3rd Plot Output ----
    chisqDensities <- reactive(
      x = {
        input$resample
        if (length(chisqSims) == 1) {
          band <- 1
          } else {
          band <- "nrd0"
          }
        density(
          chisqSims,
          n = 500,
          from = 0,
          to = xmaxInput(),
          bw = band
        )
      }
    )
    
    output$chisqCurve <- renderPlot(
      expr = {
        sim <- input$sims
        if (sim <= 5) {
          obs <- obschisqInput()
          degFreedom <- dfInput()
          chisqGraph(
            bound = round(obs,3),
            region = "above",
            df = degFreedom,
            xlab = "Chi-Square Values",
            graph = TRUE)
          abline(v = obs)
          abline(v = 0)
        }
        else{
          obs <- obschisqInput()
          degFreedom <- dfInput()
          chisqGraph(
            bound = round(obs,3),
            region = "above",
            df = degFreedom,
            xlab = "Chi-Square Values",
            graph = TRUE)
          abline(v = obs)
          abline(v = 0)
          lines(chisqDensities(),col = boastUtils::psuPalette[4],lwd = 3)
        }
      }
    )

    ####### diff. interpretation based on no. of simulation ----
    output$remarksProb <- renderText(
      expr = {
        obs <- obschisqInput()
        paste0("The orange curve approximates the true probability distribution 
             of the chi-square statistic based on simulations.",
             " The black curve shows the large sample chi-square density.",
             " For your last simulation, you get a chi-square statistic of ",
             round(obs,2),
             " The shaded area gives the approximate probability of getting a 
             chi-square statistic of ",
             round(obs,2),
             " or more, if the probability of each outcome is under Null 
             probabilities (i.e. the p-value ).")
      }
    )
    
   # output$remarksProb2 <- renderText({
   #  obs <- obschisqInput()
   #   paste0(" The black curve is the large sample chi-square density.",
   #          " The shaded area gives the approximate probability of getting a chi-square statistic of ",
   #         round(obs,2),
   #         " or more, if the probability of each outcome is under Null probabilities (i.e. the p-value ).")
   # })
  }

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
