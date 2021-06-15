# Load Packages ----
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(boastUtils) 

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
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(
          name = "Chi-Square Goodness-fit-Test and Simulation"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home"))
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
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
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Chi-Square Goodness-fit-Test"),
          # Please reformat long lines of code; the line below is an example
          p("In this app you will explore Chi-Square Goodness-fit-Test with
            simulations. The test is applied when you have categorical variables
            from a population."),
          br(),
          h2("Instructions"),
          tags$ul(
            tags$li("Select one of the scenarios for the proportion in each category (Equal Probabilities or Different Probabilities)."),
            tags$li("Move the sliders to change the values of number of observations, number of categories and number of simulations."),
            tags$li("A p-value is calculated and plotted for each simulation. You can click a p-value on the plot to see the summary table for that dataset."),
            tags$li("When there are more than 50 simulations, only a histogram of p-values is shown."),
            tags$li("You can use Explore page to explore your own data and null hypothesis.")
          ),
          br(),
          ##### Go Button ----
          div(
            style = "text-align: center",
            bsButton(inputId = "overviewBotton",
                     label = "Prerequisites",
                     size = "large",
                     icon = icon("bolt"),
                     style = "default"
            )
          ),
          ##### Set Acknowledgements Part ----
          br(),
          br(),
          h2("Acknowledgements:"),
          p("This app was developed and coded by Jinglin Feng. 
            Special thanks to Alex Chen and Yuxin Zhang for help on some programming issues.",
            br(),
            'This app was last modified by Anna (Yinqi) Zhang.',
            div(class = "updated", "Last Update: 5/26/2021 by ZYD.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h1("Prerequisites"),
          p("When an analyst attempts to fit a statistical model to observed data,
         they may wonder how close are the observed values to those which would be expected.
         One statistical test that addresses this issue for categorical data is the chi-square goodness-of-fit test."),
         p("If the computed test statistic is large, then the observed and expected
         values are not close and the model is a poor fit to the data."),
  
         box(
           title = strong("Hypotheses"), # Remove all instances of strong in titles
           status = "primary",
           collapsible = TRUE,
           collapsed = FALSE,
           width = '100%',
           tags$ul(
             tags$li("Null: The observed distribution of the variable follows the expected distribution."),
             tags$li("Alternative: The observed distribution of the variable differs from the expected distribution.")
           )
         ),
         
         box(
           title = strong("Form of Statistics"), # Spell check
           status = "primary",
           collapsible = TRUE,
           collapsed = FALSE,
           width = '100%',
           "For large samples, this follows the chi-square distribution with
           degrees of freedom: number of categories - 1. Alternatively, the
           distribution may be simulated for any size samples.
           \\[X^2 = \\sum\\frac{(observed-expected)^2}{expected}\\]"
           ),
         
         # Go Bottom: 
         div(
           style = "text-align: center",
           bsButton(inputId = "prerequisitesBotton",
                    label = "Go",
                    size = "large",
                    icon = icon("bolt"),
                    style = "default")
           )
        ),

        #### Set up the Example Page ----
        # Define the content contained within part 1 ie. tabname "first"
        tabItem(
          tabName = "example",
          withMathJax(),
          
          # Introduction
          h2("Introduction"),
          p("Use the sliders below for simulating example data from either an equiprobable or different probability null 
             (one p-value calculated for each simulation). 
            The number of simulations less or greater than 50 will effect the type of p-value plot. 
            Try different number of simulations to see the change of plot type and read the captions below the sliders for more interpretation of the plot.
            Hit the Link below if you have your own data and hypothesis to explore."),
          
          ##### Button leads to the Explore page ---- 
          div(style = "text-align: left" ,
              bsButton(inputId = "exampleBotton",
                       tags$strong("Click here to explore your own data"),
                       icon = icon("hand-o-right"),
                       size = "large",
                       style = "link")),
          br(),
          
          ##### sideBar on the left ----
          # This page needs redesigned/coded
          fluidRow(
            column(
              width = 4,
              wellPanel(
                div(
                  align = "left",
                   radioButtons(inputId = "random",
                               label = "Proportion in each category",
                               choices = c("Null with equal probabilities", "Null with different probabilities")
                               ),
                
                   sliderInput(inputId = "sampleBar",
                               label = "Sample Size",
                                     min = 200,
                                     max = 2000,
                                     value = 1100 ,
                                     step = 1),

                         sliderInput(inputId = "categoriesBar",
                                     label = "The number of Categories",
                                     min = 2,
                                     max = 8,
                                     value = 5 ,
                                     step = 1) ,

                         sliderInput(inputId = "simulationsBar",
                                     label = "The number of Simulations",
                                     min = 1,
                                     max = 1000,
                                     value = 5 ,
                                     step = 1)
    
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
                  # h3("Table and Plot"),
                  column(
                    width = 8,
                         align = "center",
                         conditionalPanel(
                           condition = "input.random == 'Null with equal probabilities'",
                            tableOutput("equalValues"),
                           
                           conditionalPanel(
                             condition = "input.random == 'Null with equal probabilities'",
                             plotOutput(outputId = "equalPlot",
                                        width = '90%',
                                        click = "plot_click"),
                             
                             tableOutput("equalPlotClickedpoints"),
  
                             htmlOutput(
                               outputId = "equalText",
                               class = "text-center")
                                          ))),

                  column(
                    width = 8,
                         align = "center",
                    
                         conditionalPanel(
                           condition = "input.random == 'Null with different probabilities'",
                           tableOutput("diffValues"),
                           
                           plotOutput(outputId = "diffPlot",
                                      width = "90%",
                                      click = "plot_click"),
                           
                           tableOutput("diffPlotClickedpoints"),
                           
                            htmlOutput(
                              outputId = "diffText",
                              class = "text-center")
                           )))
        ),

        #### Set up the Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          # Use h2 here
          h2("Explore Your Own Data"),
          p("You can use this page to explore your own dataset.
                  If you need guidence for each part, put arrow on that box.
                  When you finished, click on Simulate Now to see the plots."),
          fluidRow(
            #column of length 12 which is the whole width
            #I include everthing in a column though because this way there are margins and it looks better
            ##### sideBar on the left ----
            column(
                   width = 4,
                   wellPanel(
                     align = "left",
                   textInput(inputId = "names",
                             label = tags$strong("Level Names"), 
                             value = ""),
                   bsPopover(id = 'names',
                             title = 'Level Info',
                             content = 'Enter level names for each category, seperated by commas ( e.g. A,B,C,D ).'),
                   br(),
                   textInput(inputId = "nulls",
                             label =  tags$strong("Null Probabilities"),
                             value = ""),
                   bsPopover(id = 'nulls',
                             title = 'Prob Info',
                             content = 'All the null probabilities should add up to 1. For example, if there are four levels, the null probabilities could be 0.2, 0.2, 0.3, 0.3'),
                   br(),
                   textInput(inputId = "obs",
                             label = tags$strong("Observed Counts"),
                             value = ""),
                   bsPopover(id = 'obs',
                             title = 'Observation Info',
                             content = 'The observed counts entered should have the same levels as the null probabilities. For example, if the null probabilities are 0.25, 0.3, 0.2, and 0.25, the observed counts entered could be 13, 24, 4, and 10'),
                   br(),
                   sliderInput(inputId = "sims",
                               label = tags$strong("Additional Number of simulations from null model"),
                               min = 50,
                               max = 5000,
                               value = 100,
                               step = NULL),
                   
                    #numericInput(inputId = "sims",
                                  #label = tags$strong("Number of simulations from null model"),
                                  #value = 1,
                                  #min = 0,
                                  #step = 1),
                   br(),
                   
                   ##### resample & reset Bottons ----
                   # This should be bsButton
                   bsButton(inputId = "resample",
                            label = tags$div(tags$strong("Simulate Now")),
                            size = "large"
                   ),
                  br(),
                   # We need to get rid of these conditional panels 
                   # conditionalPanel(
                     # condition = "(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
                     bsButton(inputId = "reset",
                              label =  tags$div(tags$strong("Start Over")),
                              size = "large"
                   )
                   # )
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
                       tabPanel(h5(tags$div(tags$strong("Latest Simulation"),
                                            style = "color:black" )),
                                plotOutput(outputId = "barGraphLatest"),
                                bsPopover(id = "barGraphLatest",
                                          title = "",
                                          content = "This plot shows you the comparison of latest resample data verse actual data",
                                          placement = "left",
                                          options = list(container = "body")),
                                
                                p(textOutput("remarksLatest1")),
                                tableOutput("summary1"),
                       ),
                       
                   ###### 2nd Plot "pvalueplot" ----
                       tabPanel(h5(tags$div(tags$strong("Simulated p-values plot"),
                                            style = "color:black" )),
                                plotOutput(outputId = "pvalueplot",
                                           height = 400,
                                           width = 630)),
                   
                   ###### 3rd Plot "chisqCurve" (remarksProb2 & remarksProb)----
                       tabPanel(h5(tags$div(tags$strong("Comparsion to null distribution"),
                                            style = "color:black" )),
                                plotOutput(outputId = "chisqCurve"),
                                br(),
                                
                                # no. of simulation:
                                conditionalPanel(
                                  condition = "input.sims <= 5",
                                  p(textOutput("remarksProb2")),
                                ),
                                
                                conditionalPanel(
                                  condition = "input.sims > 5",
                                  p(textOutput("remarksProb")),
                                )
                       ))),
                  
                   id = "myPanel"))
        ),

        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p( # shinyjs
            class = "hangingindent",
            "Attali, Dean. (2020). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds (v2.0.0). [R package]. Available from https://CRAN.R-project.org/package=shinyjs"
          ),
          p( # shinyBS
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.(v0.6.1). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p( # boastUtils
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p( # shiny
            class = "hangingindent",
            "Chang W, Cheng J, Allaire J, Xie Y and Mcpherson J (2017). shiny: Web Application Framework for R. R package version 1.0.3"
          ),
          p( # shinydashboard
            class = "hangingindent",
            "Chang W and Borges Ribeiro B (2017). shinydashboard: Create Dashboards with ‘Shiny’. R package version 0.6.1"
          ),
          p( 
            class = "hangingindent",
            "Chi-Square Goodness of Fit.” Statistics Online Support, sites.utexas.edu/sos/guided/inferential/categorical/univariate/chi2/"
          ),
          p( # shinyWidgets
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets: Custom Inputs Widgets for shiny.(v0.5.3). [R package]. Available from https://CRAN.R-project.org/package=shinyWidgets"
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
          text = "You will explore Chi-Square Goodness-fit-Test with simulations in this app"
        )
      }
    )
    ### Button on each page
    observeEvent(
      eventExpr = input$overviewBotton,
      handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites")
    })
  
    observeEvent(
      eventExpr = input$prerequisitesBotton,
      handlerExpr = {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "example")
  })

    observeEvent(
      eventExpr = input$exampleBotton,
      handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore")
    })
    
    # Example Page ---- 
    ### tableData Set Up ----
    # For diffProb:
    firstdata <- reactive({
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
    })

    # For equalProb:
    firstdata2 <- reactive({
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
    })
    
    #### table Output ----
    # For diffProb：
    sliderValues <- reactive({
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
      
    })
    
    # For equalProb：
    sliderValues2 <- reactive({
      num_of_samples = input$sampleBar
      nn = input$categoriesBar
      # pp=numeric(0)
      x1 <- sample(1:nn,num_of_samples,replace = TRUE)
      
      # Compose data frame for equalProb:
      xx = cbind(paste0(LETTERS[1:nn]),round(rep(num_of_samples/nn,nn),2), round(round(rep(num_of_samples/nn,nn),2)/sum(round(rep(num_of_samples/nn,nn),2)),3))
      xx = as.data.frame(xx,stringsAsFactors = FALSE)
      colnames(xx) = c("Categories","Expected Value", "Expected Proportion")
      xx[nrow(xx) + 1,] <- c("Total", round(sum(round(rep(num_of_samples/nn,nn),2)),0),"1")
      xx
    })
    
    # For diffProb
    output$diffValues <- renderTable({
      sliderValues()},
      align = "c",
      caption = "Example Population Summary Table",
      caption.placement = "top",
      style = "bootstrap4",
      width = "150%"
    )
    # For equalProb
    output$equalValues <- renderTable({
      sliderValues2()},
      align = "c",
      caption = "Example Population Summary Table",
      caption.placement = "top",
      style = "bootstrap4",
      width = "150%"
    )
    
    #### Captions Output ----
    output$hint1 <- renderText({
    paste0("For the number of simulations less than or equal to 50, the plot of p-value is a scatterplot with no trend. 
           Click any point on the scatterplot and scroll down to see a more detailed table")
     })
    
    output$hint2 <- renderText({
      paste0("For the number of simulations greater than 50, 
            the plot of p-value is a histogram. The red line denotes the uniform density of p-values under the null).")
    })
    
    ### plotData Set Up ----
    # For diffProb
    plotdata <- reactive({
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
    })

    # For equalProb
    plotdata2 <- reactive({
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
    })

    # clickPoints on plot for each prob 
    v <- reactiveValues(click1 = NULL)

    observeEvent(input$plot_click, {
      v$click1 <- input$plot_click
    })
    
    #### scatterPlot and histogram Output ---- 
    # For diffProb
    output$diffPlot <- renderPlot({
      ss = input$simulationsBar
      nn = input$categoriesBar
      d <- plotdata()$x
      coordinatex <- v$click1$x
      coordinatey <- v$click1$y
      if (ss <= 50)
      {
        plot(d$index,
             d$pp,
             pch = 20,
             col = "black", 
             cex = 1.5,
             font.lab = 2,
             xlab = "Simulation Number",
             ylab = "P-value",
             main = "Scatterplot of Simulation Number and P-value")
        if (!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01)
          points(x = round(coordinatex),
                 y = d$pp[round(coordinatex)],
                 col = "orange",
                 pch = 20,
                 cex = 2.5)
      }
      # histgram
      else {
        par(xpd = F)
        hist(d$pp,
             breaks = 5,
             xlab = "P-value",
             font.lab = 2,
             main = "P-value Distribution from the simulations")
        abline(h = ss/5, col = "red")}
    })

    # For equalProb
    output$equalPlot <- renderPlot({
      coordinatex <- v$click1$x
      coordinatey <- v$click1$y
      ss = input$simulationsBar
      nn = input$categoriesBar
      d <- plotdata2()
      if (ss <= 50)
      {
        plot(d$index,
             d$pp,
             pch = 20,
             col = "black",
             cex = 1.5,
             font.lab = 2,
             xlab = "Simulation Number",
             ylab = "P-value",
             main = "Scatterplot of Simulation Number and P-value")
        if (!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01)
          points(x = round(v$click1$x),
                 y = d$pp[round(v$click1$x)],
                 col = "orange",
                 pch = 20,
                 cex = 2.5)

      }
      
      # histgram
      else {
        par(xpd = F)
        hist(d$pp,
             breaks = 5,
             xlab = "P-value",
             font.lab = 2,
             main = "P-value Distribution from the simulations")
        abline(h = ss/5, col = "red")}
    })

    #### following table for clickedPoint ----
    # For diffProb
    clickedpoints1 <- reactive({
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
      if (!(!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) || ss > 50)
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
      colnames(xx) = c("Categories","Observed Value","Expected Value", "Observed Proportion", "Expected Proportion")
      xx[nrow(xx) + 1,] <- c("Total", sum(table(x1)),round(sum(round(rep(num_of_samples/nn,nn),2)),0),"1","1")
      xx


    })

      # For equalProb
    clickedpoints2 <- reactive({
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
      if (!(!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) || ss > 50)
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
    })
    
    #### P-value of clickedPoint ----
    # For diffProb
    clickedpoints21 <- reactive({
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
      if (!(!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) || ss > 50)
        return()
      i <- round(v$click1$x)
      pvalue <- round(data$pp[round(v$click1$x)],3)
      paste("P-value =  ",
            as.character(pvalue) )

    })

    # For equalProb
    clickedpoints22 <- reactive({
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
      if (!(!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01) || ss > 50)
        return()
      i <- round(v$click1$x)
      pvalue <- round(data$pp[round(v$click1$x)],3)
      paste("P-value =  ",
            as.character(pvalue) )
    })
    
    #### following table and p-value text Output ----
    # following table:
    # For equalProb
    output$equalPlotClickedpoints <- renderTable({
      clickedpoints2()},
      align = "c",
      caption = "Example Sample Summary Table",
      caption.placement = "top",
      style = "bootstrap4"
    )

    # For diffProb
    output$diffPlotClickedpoints <- renderTable({
      clickedpoints1()},
      align = "c",
      caption = "Example Sample Summary Table",
      caption.placement = "top",
      style = "bootstrap4"
    )
    
    # p-value text:
    # For equalProb
    output$equalText <- renderText({
      clickedpoints22()

    })
    
    # For diffProb
    output$diffText <- renderText({
      clickedpoints21()
      
    })
    
  
    # Explore Page ----
    simLimit <- 10000
    numberSims <- 0
    chisqSims <- numeric()
    latestSim <- NULL
    fullSim <- character()
    total <- 0     #total number of sims over all set-ups including current one
    totalPrev <- 0 #total number of sims over all set-ups excluding current one
    
    ##### sideBar Inputs Set Up ----
    namesInput <- reactive({
      unlist(strsplit(input$names,split = ","))
    })

    nullsInput <- reactive({
      probs <- as.numeric(unlist(strsplit(input$nulls,split = ",")))
      probs
    })

    obsInput <- reactive({
      observed <- as.numeric(unlist(strsplit(input$obs,split = ",")))
      observed
    })

    goodNames <- reactive({
      names <- namesInput()
      goodNames <- TRUE
      if (length(names) <= 1) goodNames <- FALSE
      if (any(is.na(names))) goodNames <- FALSE
      if (!goodNames) disable("resample")
      goodNames
    })

    goodNulls <- reactive({
      nulls <- nullsInput()
      goodNulls <- TRUE
      if (length(nulls) < 1) goodNulls <- FALSE
      if (any(is.na(nulls))) goodNulls <- FALSE
      if (!any(is.na(nulls)) && any(nulls <= 0)) goodNulls <- FALSE
      if (!goodNulls) disable("resample")
      goodNulls
    })

    goodNulls2 <- reactive({
        nulls <- nullsInput()
        goodNulls <- TRUE
        if (sum(nulls) != 1) goodNulls <- FALSE
        if (!goodNulls) disable("resample")
        goodNulls
      })

    goodObs <- reactive({
      obs <- obsInput()
      goodObs <- TRUE
      if (length(obs) < 1) goodObs <- FALSE
      if (any(is.na(obs))) goodObs <- FALSE
      if (!any(is.na(obs)) && any(obs < 0)) goodObs <- FALSE
      if (!goodObs) disable("resample")
      goodObs
    })


    obschisqInput <- reactive({
      nulls <- nullsInput()/sum(nullsInput())
      totalCounts <- sum(obsInput())
      expected <- nulls*totalCounts
      sum(obsInput()^2/expected) - totalCounts
    })

    simsUpdate <- reactive({
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

        hide(id = "setup",
             anim = T,
             animType = "slide")

        if (total - totalPrev == 1) {
          updateTabsetPanel(session,"myPanel",selected = "Latest Simulation")
        }

        varLevels <- isolate(namesInput())
        namesList <- rep(varLevels,times = latestSim)
        fullSim <<- sample(namesList,size = totalCounts,replace = FALSE)
        list(numberSims,latestSim)
      }
    })

    simsReset <- reactive({
      input$reset
      totalPrev <<- totalPrev + numberSims
      numberSims <<- 0
      chisqSims <<- numeric()
      latestSim <<- NULL

      show(id = "setup",
           anim = T,
           animType = "slide")
      return(totalPrev)
    })


    dfInput <- reactive({
      length(obsInput()) - 1
    })


    xmaxInput <- reactive({
      qchisq(0.999,df = dfInput())
    })


    output$totalPrev <- reactive({
      simsReset()
    })

    outputOptions(output, 'totalPrev', suspendWhenHidden = FALSE)

    output$total <- reactive({
      simsUpdate() #for dependency
      total
    })


    ###### Hints for Inputs ---- 
    outputOptions(output, 'total', suspendWhenHidden = FALSE)

    output$barGraphInitial <- renderPlot({
      if (goodNames()) enable("resample") else disable("resample")
      validate(
        need(goodNames(),"To Start, enter level names for each category, seperated by commas ( e.g. A,B,C,D )."
        ))

      if (goodNulls()) enable("resample") else disable("resample")
      validate(
        need(goodNulls(),"Enter your null probabilities as decimals separated by commas. They should all be positive numbers that add to one.")
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
             "Make sure that you enter the exact same number of level names and null probabilities")
      )

      if (goodObs()) enable("resample") else disable("resample")
      validate(
        need(goodObs(),"Enter your observed counts separated by commas. All counts must be non-negative whole numbers.")
      )

      lengthCheck2 <- (length(nulls) == length(observed)) && (length(observed) == length(names))
      if (lengthCheck2) enable("resample") else disable("resample")
      validate(
        need(lengthCheck2,
             "Make sure that you enter the exact same number of level names, null probabilities and observed counts")
      )
      
      ##### Plots Set Up ----
      ###### Initial Plot & Table ----
      # Histogram "remarksInitial":
      observed <- obsInput()
      expected <- nulls*sum(observed)
      tab <- rbind(observed,expected)
      rownames(tab) <- c("Observed","Expected")
      colnames(tab) <- names
      barplot(tab,
              beside = T,
              col = c("dark green","yellow"),
              main = "Bargraph of Observed and Expected Counts",
              xlab = "",
              ylab = "Counts",
              legend.text = TRUE)
    })
    
    output$remarksInitial <- renderText({
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
            as.character(rounded2) )
    })

    output$obsTable <- renderTable({
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
      df <- data.frame(Levels = names,
                       Observed = observed,
                       Expected = round(expected,2),
                       cont = round(contribs,2)
      )
      names(df)[4] <- c("Contribution to Chi-Square")
      df
    }, align = "c"
    )
    
    ###### 1st Plot & Table Output ----
    # Histogram "barGraphLatest":
    output$barGraphLatest <- renderPlot({
      input$resample
      if (length(chisqSims) > 0) {
        totalCounts <- isolate(sum(obsInput()))
        nulls <- isolate(nullsInput()/sum(nullsInput()))
        expected <- totalCounts*nulls
        tab <- rbind(obsInput(),expected,latestSim)
        rownames(tab) <- c("Observed","Expected","Resampled")
        colnames(tab) <- isolate(namesInput())
        barplot(tab,
                beside = T,
                col = c("dark green","yellow","grey"),
                main = "Bargraph of Observed, Expected, and Latest Resample",
                xlab = "",
                ylab = "Counts",
                legend.text = TRUE)
      }
    })
    
    # Table "summary1":
    output$remarksLatest1 <- renderText({
      input$resample
      chisq <- obschisqInput()
      rounded1 <- round(chisq,2)
      rounded2 <- round(chisqSims[length(chisqSims)],2)
      paste("Observed chi-square statistic =  ",
            as.character(rounded1),sep = "\n",
            ",",
            "Latest resampled chi-square statistics = ",
            as.character(abs(rounded2)))
    })
    
    output$summary1 <- renderTable({
      input$resample
      observed <- obsInput()
      nulls <- nullsInput()/sum(nullsInput())
      names <- namesInput()
      expected <- nulls*sum(observed)

      df <- data.frame(Levels = names,
                       Observed = observed,
                       Expected = round(expected,2),
                       Resampled = latestSim
      )
      df
    }, align = "c")
    
    ###### 2nd Plot Output ----
    output$pvalueplot <- renderPlot({
        input$resample
        nulls <- nullsInput()/sum(nullsInput())
        observed <- obsInput()
        chisq <- obschisqInput()
        obs <- isolate(obschisqInput())
        n <- length(chisqSims)
        latest <- chisqSims[n]
        p.value <- pchisq(chisqSims, length(observed) - 1, lower.tail = FALSE)
        hist(p.value,breaks = 10,
             main = "P-value Distribution Histogram from the simulations",
             xlab = "p-value",
             xlim = c(0,1),
             font.lab = 2)

      })
    
    ###### 3rd Plot Output ----
    chisqDensities <- reactive({
      input$resample
      if (length(chisqSims) == 1) band <- 1
      else band <- "nrd0"
      density(chisqSims,
              n = 500,
              from = 0,
              to = xmaxInput(),
              bw = band)
    })
    
    output$chisqCurve <- renderPlot({
      sim <- input$sims
      if (sim <= 5) {
        obs <- obschisqInput()
        degFreedom <- dfInput()
        chisqGraph(bound = obs,
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
        chisqGraph(bound = obs,
                   region = "above",
                   df = degFreedom,
                   xlab = "Chi-Square Values",
                   graph = TRUE)
        abline(v = obs)
        abline(v = 0)
        lines(chisqDensities(),col = "orange",lwd = 3)

      }
    })
    
           ####### diff. interpretation based on no. of simulation ----
    output$remarksProb <- renderText({
      obs <- obschisqInput()
      paste0("The orange curve approximates the true probability distribution of the chi-square statistic based on simulations.",
             " The black curve shows the large sample chi-square density.",
             " The shaded area gives the approximate probability of getting a chi-square statistic of ",
             round(obs,2),
             " or more, if the probability of each outcome is under Null probabilities (i.e. the p-value ).")
    })

    output$remarksProb2 <- renderText({
      obs <- obschisqInput()
      paste0(" The black curve is the large sample chi-square density.",
             " The shaded area gives the approximate probability of getting a chi-square statistic of ",
             round(obs,2),
             " or more, if the probability of each outcome is under Null probabilities (i.e. the p-value ).")
    })
  }

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
