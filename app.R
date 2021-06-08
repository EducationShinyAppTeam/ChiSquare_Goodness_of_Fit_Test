# Load Packages ----
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)


# Load additional dependencies and setup functions
# source("global.R")

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
      tags$li(class = "dropdown", boastUtils::surveyLink(name = "Chi-Square Goodness-fit-Test and Simulation")),
      tags$li(class = "dropdown",tags$a(href = 'https://shinyapps.science.psu.edu/',icon("home")))
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
          p("In this app you will explore Chi-Square Goodness-fit-Test with simulations.
                        The test is applied when you have categorical variables from a population."),
          br(),

          h2("Instructions:"),
          tags$ul(
            tags$li("Select one of the scenarios for the proportion in each category (Equal Probabilities or Different Probabilities)."),
            tags$li("Move the sliders to change the values of number of observations, number of categories and number of simulations."),
            tags$li("A p-value is calculated and plotted for each simulation. You can click a p-value on the plot to see the summary table for that dataset."),
            tags$li("When there are more than 50 simulations, only a histogram of p-values is shown."),
            tags$li("You can use Explore page to explore your own data and null hypothesis.")
          ),
          br(),
          ##### Go Button--location will depend on your goals ----
          div(
            style = "text-align: center",
            bsButton(inputId = "overviewBotton",
                     label = "Go to Prerequisites",
                     size = "large",
                     icon = icon("bolt"),
                     style = "default"
            )
          ),
          ##### Set Acknowledgements Part ----
          br(),
          br(),
          h2("Acknowledgements:"),
          p("This app was developed and coded by Jinglin Feng. Special thanks to Alex Chen and Yuxin Zhang for help on some programming issues.",
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
           title = strong("Hypotheses:"),
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
           title = strong("Form of Satistics:"),
           status = "primary",
           collapsible = TRUE,
           collapsed = FALSE,
           width = '100%',
           "For large sample, this follows the chi-square distribution with degrees of freedom: number of categories - 1.
            Alternatively, the distribution may be simulated for any size samples.",
           withMathJax(
             helpText('$$X^2 = \\sum\\frac{(observed-expected)^2}{expected}$$'))
         ),
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
        #Define the content contained within part 1 ie. tabname "first"
        tabItem(tabName = "example",
                withMathJax(),
                fluidRow(
                  column(4,
                         h3("Introduction:"),
                         box(
                           width = "10.5%",
                           background = "orange",
                          "Use the sliders below for simulating example data from either an equiprobable or different probability null (one p-value calculated for each simulation). 
                           Hit the Link below if you have your own data and hypothesis to examine."),
                         radioButtons(inputId = "random", 
                                      label = "Proportion in each category", 
                                      choices = c("Null with equal probabilities", "Null with different probabilities")),

                         sliderInput(inputId = "sampleBar", 
                                     label = "Sample Size:", 
                                     min = 200, 
                                     max = 2000, 
                                     value = 1100 ,
                                     step = 1),
                         bsPopover(id = "sampleBar", 
                                   title = "", 
                                   content = "Number of Observations", 
                                   placement = "right"),

                         sliderInput(inputId = "categoriesBar", 
                                     label = "The number of Categories:", 
                                     min = 2, 
                                     max = 8, 
                                     value = 5 ,
                                     step = 1) ,

                         bsPopover(id = "categoriesBar", 
                                   title = "", 
                                   content = "Number of Categories", 
                                   placement = "right"),

                         sliderInput(inputId = "simulationsBar", 
                                     label = "The number of Simulations:", 
                                     min = 1, 
                                     max = 1000, 
                                     value = 5 ,
                                     step = 1),

                         bsPopover(id = "simulationsBar", 
                                   title = "", 
                                   content = "For the first 50 simulations, you will see a p-value scatterplot; 
                                              For the number of simulations greater than 50, you will see a histogram of p-values.",
                                   placement = "right", 
                                   options = list(container = "body")),

                         div(style = "text-align: left" ,
                             bsButton(inputId = "exampleBotton", 
                                      tags$strong("Click here if you have real data to test"),
                                      icon = icon("hand-o-right"),
                                      size = "large",
                                      style = "link")),
                         conditionalPanel(
                           condition = "input.simulationsBar <= 50",
                           textOutput("hint"),
                         )
                         ),
                  
                  h3("Table and Plot:"),
                  column(7,align = "center",
                         conditionalPanel(
                           condition = "input.random == 'Null with equal probabilities'",
                            tableOutput("equalValues"),
                            bsPopover(
                              id = "equalValues",
                              title = "",
                              content =  "An example of a summary table of population values", 
                              placement = "bottom", 
                              options = list(container = "body")),
                           conditionalPanel(
                             condition = "input.random == 'Null with equal probabilities'",
                             plotOutput("equalPlot", 
                                        width = '90%', 
                                        click = "plot_click"),
                             bsPopover(
                               id = "equalPlot",
                               title = "",
                               content = "For the number of simulations less than or equal to 50, click a point on the scatterplot to see the table behind it;
                                          For the number of simulations greater than 50, you will see a histogram of p-values. The red line denotes the uniform density of p-values under the null",
                               placement = "bottom", 
                               options = list(container = "body")),
                              tableOutput("equalPlotClickedpoints"),
                             bsPopover(
                               id = "equalPlotClickedpoints",
                               title = "",
                               content = "An example of a summary table of sample values", 
                               placement = "right", 
                               options = list(container = "body")),
                             htmlOutput("equalText", 
                                        class = "text-center")
                                          ))),

                  column(7,align = "center",
                         conditionalPanel(
                           condition = "input.random == 'Null with different probabilities'",
                           tableOutput("diffValues"),
                           bsPopover(
                             id = "diffValues",
                             title = "",
                             content = "An example of a summary table of population values", 
                             placement = "bottom", 
                             options = list(container = "body")),
                           plotOutput("diffPlot", 
                                      width = "90%", 
                                      click = "plot_click"),
                           bsPopover(
                             id = "diffPlot",
                             title = "",
                             content = "For the number of simulations less than or equal to 50, click a point on the scatterplot to see the table behind it; 
                                        For the number of simulations greater than 50, you will see a histogram of p-values. The red line denotes the uniform density of p-values under the null )",
                             placement = "right", 
                             options = list(container = "body")),
                           tableOutput("diffPlotClickedpoints"),
                           bsPopover(
                             id = "diffPlotClickedpoints",
                             title = "",
                             content = "An example of a summary table of sample values",
                             placement = "right", 
                             options = list(container = "body")),
                           htmlOutput("diffText",
                                      class = "text-center")
                           )))
        ),
        
        #### Set up the Explore Page ----
        tabItem(tabName = "explore",
                withMathJax(),
                h1("Explore Your Own Data"),
                p("You can use this page to explore your own dataset.
                  If you need guidence for each part, put arrow on that box.
                  When you finished, click on Simulate Now to see the plots."),
                fluidRow(
                  #column of length 12 which is the whole width
                  #I include everthing in a column though because this way there are margins and it looks better
                  column(4,
                         textInput(inputId = "names",
                                   h4(tags$strong("Level Names")),
                                   value = ""),
                         bsPopover(id = 'names', 
                                   title = 'Level Info', 
                                   content = 'Enter level names for each category, seperated by commas ( e.g. A,B,C,D ).'),
                         br(),
                         textInput(inputId = "nulls",
                                   h4(tags$strong("Null Probabilities")),
                                   value = ""),
                         bsPopover(id = 'nulls', 
                                   title = 'Prob Info', 
                                   content = 'All the null probabilities should add up to 1. For example, if there are four levels, the null probabilities could be 0.2, 0.2, 0.3, 0.3'),
                         br(),
                         textInput(inputId = "obs",
                                   h4(tags$strong("Observed Counts")),
                                   value = ""),
                         bsPopover(id = 'obs',
                                   title = 'Observation Info',
                                   content = 'The observed counts entered should have the same levels as the null probabilities. For example, if the null probabilities are 0.25, 0.3, 0.2, and 0.25, the observed counts entered could be 13, 24, 4, and 10'),
                         br(),
                         numericInput(inputId = "sims",
                                      h4(tags$strong("Number of simulations from null model")),
                                      value = 1,
                                      min = 0,
                                      step = 1),
                         br(),
                         actionButton(inputId = "resample",
                                      h4(tags$div(tags$strong("Simulate Now")))
                                      ),
                         conditionalPanel(
                           condition = "(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
                           actionButton(inputId = "reset",
                                        h4(tags$div(tags$strong("Start Over"))))
                         )
                  ),
                  
                  column(8,
                         conditionalPanel(
                           condition = "input.resample == 0 || output.totalPrev == output.total",
                           plotOutput("barGraphInitial"),
                           p(
                             textOutput("remarksInitial")),
                             tableOutput("obsTable"),
                  ),
                         conditionalPanel(
                           condition = "(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
                           tabsetPanel(
                             tabPanel(h5(tags$div(tags$strong("Latest Simulation"),
                                                  style = "color:black" )),
                                      plotOutput("barGraphLatest"),
                                      bsPopover(id = "barGraphLatest", 
                                                title = "", 
                                                content = "This plot shows you the comparison of latest resample data verse actual data", 
                                                placement = "left", 
                                                options = list(container = "body")),
                                      p(textOutput("remarksLatest1")),
                                      tableOutput("summary1"),
                                      ),

                             tabPanel(h5(tags$div(tags$strong("Simulated p-values plot"),
                                                  style = "color:black" )),
                                      plotOutput("pvalueplot",
                                                 height = 400,
                                                 width = 630)),
                             tabPanel(h5(tags$div(tags$strong("Comparsion to null distribution "), 
                                                  style = "color:black" )),
                                      plotOutput("chisqCurve"),
                                      br(),
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
          p(class = "hangingindent",
            "Attali, Dean. (2020). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds (v2.0.0). [R package]. Available from https://CRAN.R-project.org/package=shinyjs"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.(v0.6.1). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Chang W, Cheng J, Allaire J, Xie Y and Mcpherson J (2017). shiny: Web Application Framework for R. R package version 1.0.3"
          ),
          p(
            class = "hangingindent",
            "Chang W and Borges Ribeiro B (2017). shinydashboard: Create Dashboards with ‘Shiny’. R package version 0.6.1"
          ),
          p(
            class = "hangingindent",
            "Chi-Square Goodness of Fit.” Statistics Online Support, sites.utexas.edu/sos/guided/inferential/categorical/univariate/chi2/"
          ),
          p(
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
    ## Set up Info button ----
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
    #Explore Button ----
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

    #For Random
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

    #For Same
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

    # For Random
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

    # For Same
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

    #click_event
    v <- reactiveValues(click1 = NULL)

    observeEvent(input$plot_click, {
      v$click1 <- input$plot_click
    })



    # For Random
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
             col = "#1C2C5B",
             cex = 1.5,
             font.lab = 2, 
             xlab = "Simulation Number",
             ylab = "P-value")
        if (!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01)
          points(x = round(coordinatex),
                 y = d$pp[round(coordinatex)],
                 col = "#FF4500",
                 pch = 20, 
                 cex = 2.5)
      }
      else {
        par(xpd = F)
        hist(d$pp,breaks = 5,main = "P-value Distribution from the simulations", xlab = "P-value",font.lab = 2 )
        abline(h = ss/5, col = "red")}
    })

    # For Same
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
             col = "#1C2C5B", 
             cex = 1.5,
             font.lab = 2, 
             xlab = "Simulation Number",
             ylab = "P-value")
        if (!is.null(v$click1$x) && abs(coordinatex - round(coordinatex)) < 0.1 && abs(coordinatey - d$pp[round(coordinatex)]) < 0.01)
          points(x = round(v$click1$x),
                 y = d$pp[round(v$click1$x)],
                 col = "#FF4500",
                 pch = 20, 
                 cex = 2.5)

      }
      else {
        par(xpd = F)
        hist(d$pp,breaks = 5,main = "P-value Distribution from the simulations", xlab = "P-value",font.lab = 2)
        abline(h = ss/5, col = "red")}
    })


    # For Random
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

    # For Same
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

    # For Random
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

    # For Same
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

    # For Same
    output$equalPlotClickedpoints <- renderTable({
      clickedpoints2()},
      align = "c"
    )

    # For Random
    output$diffPlotClickedpoints <- renderTable({
      clickedpoints1()},
      align = "c"
    )

    # For Random
    output$diffText <- renderText({
      clickedpoints21()

    })

    # For Same
    output$equalText <- renderText({
      clickedpoints22()

    })

    # For Random
    sliderValues <- reactive({
      num_of_samples = input$sampleBar
      nn = input$categoriesBar
      # pp=numeric(0)
      x1 <- sample(1:nn,num_of_samples,replace = TRUE)
      gen <- runif(input$categoriesBar)
      trueProp = gen/sum(gen)
      total = table(x1)
      expected = trueProp*input$sampleBar

      # Compose data frame ----
      xx = cbind(paste0(LETTERS[1:nn]),round(expected,2), round(round(expected,2)/sum(round(expected,2)),3))
      xx = as.data.frame(xx,stringsAsFactors = FALSE)
      colnames(xx) = c("Categories","Expected Value", "Expected Proportion")
      xx[nrow(xx) + 1,] <- c("Total", round(sum(round(expected,2)),0),"1")
      xx

    })

    # For Same
    sliderValues2 <- reactive({
      num_of_samples = input$sampleBar
      nn = input$categoriesBar
      # pp=numeric(0)
      x1 <- sample(1:nn,num_of_samples,replace = TRUE)

      # Compose data frame
      xx = cbind(paste0(LETTERS[1:nn]),round(rep(num_of_samples/nn,nn),2), round(round(rep(num_of_samples/nn,nn),2)/sum(round(rep(num_of_samples/nn,nn),2)),3))
      xx = as.data.frame(xx,stringsAsFactors = FALSE)
      colnames(xx) = c("Categories","Expected Value", "Expected Proportion")
      xx[nrow(xx) + 1,] <- c("Total", round(sum(round(rep(num_of_samples/nn,nn),2)),0),"1")
      xx
    })
    
    # For Random
    output$diffValues <- renderTable({
      sliderValues()},
      align = "c"
    )
    # For Same
    output$equalValues <- renderTable({
      sliderValues2()},
      align = "c"
    )
    output$hint <- renderText({
      paste0("Scroll down to see the table associated with the point you just clicked")
    })

    simLimit <- 10000


    numberSims <- 0
    chisqSims <- numeric()
    latestSim <- NULL
    fullSim <- character()


    total <- 0 #total number of sims over all set-ups including current one
    totalPrev <- 0 #total number of sims over all set-ups excluding current one

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
        newSims <- rmultinom(sampleBar = reps, size = totalCounts, prob = nullProbs)
        chisqNew <- colSums(newSims^2/expCounts) - totalCounts
        chisqSims <<- c(chisqSims,chisqNew)
        latestSim <<- newSims[,reps]
        numberSims <<- numberSims + reps
        total <<- total + reps

        hide(id = "setup",anim = T,animType = "slide")
        
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

      show(id = "setup",anim = T,animType = "slide")

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


    # needed for the conditional panels to work 
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



      observed <- obsInput()
      nulls <- nullsInput()/sum(nullsInput())
      names <- namesInput()


      # Names and Null Check ----
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
    }, align = "c"
    )

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
        lines(chisqDensities(),col = "#D95F02",lwd = 3)

      }
    })
    
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
