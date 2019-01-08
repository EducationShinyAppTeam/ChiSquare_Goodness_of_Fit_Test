library(shiny)
library(ggplot2)

source("chisqplot.R")

shinyServer(function(input, output,session) {

  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "first")
  })
  
  observeEvent(input$test, {
    
    updateTabItems(session, "tabs", "second")
  })
  
  observeEvent(input$bsButton4, {
    updateTabItems(session, 'game', 'fib')
  })
  
  #For Random
  firstdata<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    v$click1 <-NULL
    mytable<-list(0)
    for(i in 1:ss){
      x <- sample(1:nn,num_of_samples,replace=T)
      mytable[i]<-list(x)
    }
    mytable
  })
  
  #For Same
  firstdata2<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    v$click1 <-NULL
    mytable<-list(0)
    for(i in 1:ss){
      x <- sample(1:nn,num_of_samples,replace=T)
      mytable[i]<-list(x)
    }
    mytable
  })
  
  # For Random
  plotdata<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    v$click1 <-NULL
    pp=numeric(0)
    xx<-firstdata()
    for(i in 1:ss){
      x<-unlist(xx[i])
      gen<-runif(input$n2)
      trueProp = gen/sum(gen)
      total=table(x)
      expected=trueProp*input$n
      a <- chisq.test(table(x), correct= FALSE, rescale.p=TRUE )
      pp[i]=a$p.value
      
    }
    if (ss<=50) {
      index=seq(1,length(pp))
      data=data.frame(index,pp)
    }
    else
    {
      data=data.frame(pp)
    }
    list(x = data, y = gen)
  })
  
  # For Same
  plotdata2<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    v$click1 <-NULL
    pp=numeric(0)
    xx<-firstdata2()
    for(i in 1:ss){
      x<-unlist(xx[i])
      nulls=1/(1:nn)
      total=table(x)
      expected=nulls*total
      a <- chisq.test(table(x), correct= FALSE, rescale.p=TRUE )
      pp[i]=a$p.value
      
    }
    if (ss<=50) {
      index=seq(1,length(pp))
      data=data.frame(index,pp)
    }
    else
    {
      data=data.frame(pp)
    }
    data
  })
  
  #click_event
  v <- reactiveValues(click1 = NULL)
  
  observeEvent(input$plot_click, {
    v$click1 <- input$plot_click
  })
  
  
  
  # For Random
  output$plot1 <- renderPlot({
    ss= input$n3
    nn= input$n2
    d<-plotdata()$x
    coordinatex<-v$click1$x
    coordinatey<-v$click1$y
    if (ss<=50)
    {
      plot(d$index,d$pp,pch = 20,col="#1C2C5B", cex=1.5,font.lab=2, xlab = "Simulation Number",ylab = "P-value")
      if (!is.null(v$click1$x)&&abs(coordinatex-round(coordinatex))<0.1&&abs(coordinatey-d$pp[round(coordinatex)])<0.01)
        points(x=round(coordinatex),y=d$pp[round(coordinatex)],col="#FF4500",pch = 20, cex=2.5)
      
    }
    else {
      par(xpd=F)
      hist(d$pp,breaks=5,main="P-value Distribution from the simulations", xlab="P-value",font.lab=2 )
      abline(h = ss/5, col = "red")}
  })
  
  # For Same
  
  output$plot2 <- renderPlot({
    coordinatex<-v$click1$x
    coordinatey<-v$click1$y
    ss= input$n3
    nn= input$n2
    d<-plotdata2()
    if (ss<=50)
    {
      plot(d$index,d$pp,pch = 20,col="#1C2C5B", cex=1.5,font.lab=2, xlab = "Simulation Number",ylab = "P-value")
      if (!is.null(v$click1$x)&&abs(coordinatex-round(coordinatex))<0.1&&abs(coordinatey-d$pp[round(coordinatex)])<0.01)
        points(x=round(v$click1$x),y=d$pp[round(v$click1$x)],col="#FF4500",pch = 20, cex=2.5)
      
    }
    else {
      par(xpd=F)
      hist(d$pp,breaks=5,main="P-value Distribution from the simulations", xlab="P-value",font.lab=2 )
      abline(h = ss/5, col = "red")}
  })
  
  
  # For Random
  clickedpoints1<- reactive({
    # For base graphics, I need to specify columns, though for ggplot2,
    # it's usually not necessary.
    coordinatex<-v$click1$x
    coordinatey<-v$click1$y
    ss= input$n3
    num_of_samples = input$n
    nn= input$n2
    mytable<-firstdata() 
    d<-plotdata()$x
    
    data<-plotdata()$x
    if (!(!is.null(v$click1$x)&&abs(coordinatex-round(coordinatex))<0.1&&abs(coordinatey-d$pp[round(coordinatex)])<0.01)||ss>50)
      return()
    i<-round(v$click1$x)
    pvalue<-round(data$pp[round(v$click1$x)],3)
    
    x1<-unlist(mytable[i])
    # gen<-runif(input$n2)
    trueProp = plotdata()$y/sum(plotdata()$y)
    total=table(x1)
    expected=trueProp*input$n
    xx=cbind(paste0(LETTERS[1:nn]),table(x1),round(expected,2),
             round(table(x1)/sum(table(x1)),2), 
             round(round(expected,2)/sum(round(expected,2)),3))
    
    xx=as.data.frame(xx,stringsAsFactors=FALSE)
    colnames(xx)=c("Categories","Observed Value","Expected Value", "Observed Proportion", "Expected Proportion")
    xx[nrow(xx)+1,] <- c("Total", sum(table(x1)),round(sum(round(rep(num_of_samples/nn,nn),2)),0),"1","1")
    xx
    
    
  })
  
  # For Same
  clickedpoints2<- reactive({
    # For base graphics, I need to specify columns, though for ggplot2,
    # it's usually not necessary.
    coordinatex<-v$click1$x
    coordinatey<-v$click1$y
    ss= input$n3
    num_of_samples = input$n
    nn= input$n2
    mytable<-firstdata2()  
    d<-plotdata2()
    
    data<-plotdata2()
    if (!(!is.null(v$click1$x)&&abs(coordinatex-round(coordinatex))<0.1&&abs(coordinatey-d$pp[round(coordinatex)])<0.01)||ss>50)
      return()
    i<-round(v$click1$x)
    pvalue<-round(data$pp[round(v$click1$x)],3)
    x1<-unlist(mytable[i])
    
    xx=cbind(paste0(LETTERS[1:nn]),table(x1),round(rep(num_of_samples/nn,nn),2),
             round(table(x1)/sum(table(x1)),2), 
             round(round(rep(num_of_samples/nn,nn),2)/sum(round(rep(num_of_samples/nn,nn),2)),3))
    
    xx=as.data.frame(xx,stringsAsFactors=FALSE)
    colnames(xx)=c("Categories","Observed Value","Expected Value", "Observed Proportion", "Expected Proportion")
    xx[nrow(xx)+1,] <- c("Total", sum(table(x1)),round(sum(round(rep(num_of_samples/nn,nn),2)),0),"1", "1")
    xx
  })
  
  # For Random
  clickedpoints21<- reactive({
    # For base graphics, I need to specify columns, though for ggplot2,
    # it's usually not necessary.
    coordinatex<-v$click1$x
    coordinatey<-v$click1$y
    ss= input$n3
    num_of_samples = input$n
    nn= input$n2
    mytable<-firstdata()  
    d<-plotdata()$x
    data<-plotdata()$x
    if (!(!is.null(v$click1$x)&&abs(coordinatex-round(coordinatex))<0.1&&abs(coordinatey-d$pp[round(coordinatex)])<0.01)||ss>50)
      return()
    i<-round(v$click1$x)
    pvalue<-round(data$pp[round(v$click1$x)],3)
    
    paste("P-value =  ",as.character(pvalue) )
    
  })
  
  # For Same
  clickedpoints22<- reactive({
    # For base graphics, I need to specify columns, though for ggplot2,
    # it's usually not necessary.
    coordinatex<-v$click1$x
    coordinatey<-v$click1$y
    ss= input$n3
    num_of_samples = input$n
    nn= input$n2
    mytable<-firstdata2()  
    d<-plotdata2()
    data<-plotdata2()
    if (!(!is.null(v$click1$x)&&abs(coordinatex-round(coordinatex))<0.1&&abs(coordinatey-d$pp[round(coordinatex)])<0.01)||ss>50)
      return()
    i<-round(v$click1$x)
    pvalue<-round(data$pp[round(v$click1$x)],3)
    
    paste("P-value =  ",as.character(pvalue) )
    
  })
  
  # For Same
  output$plot_clickedpoints<-renderTable({
    clickedpoints2()},
    align="c"
  )
  
  # For Random
  output$plot_clickedpoints2<-renderTable({
    clickedpoints1()},
    align="c"
  )
  
  # For Random
  output$text1<- renderText({
    clickedpoints21()
    
  })
  
  # For Same
  output$text2<- renderText({
    clickedpoints22()
    
  })
  
  # For Random
  sliderValues <- reactive({
    num_of_samples = input$n
    nn= input$n2
    # pp=numeric(0)
    x1 <- sample(1:nn,num_of_samples,replace=T)
    gen<-runif(input$n2)
    trueProp = gen/sum(gen)
    total=table(x1)
    expected=trueProp*input$n
    
    # Compose data frame
    xx=cbind(paste0(LETTERS[1:nn]),round(expected,2), round(round(expected,2)/sum(round(expected,2)),3)) 
    xx=as.data.frame(xx,stringsAsFactors=FALSE)
    colnames(xx)=c("Categories","Expected Value", "Expected Proportion")
    xx[nrow(xx)+1,] <- c("Total", round(sum(round(expected,2)),0),"1")
    xx
    
  })
  
  # For Same
  
  sliderValues2 <- reactive({
    num_of_samples = input$n
    nn= input$n2
    # pp=numeric(0)
    x1 <- sample(1:nn,num_of_samples,replace=T)
    
    # Compose data frame
    xx=cbind(paste0(LETTERS[1:nn]),round(rep(num_of_samples/nn,nn),2), round(round(rep(num_of_samples/nn,nn),2)/sum(round(rep(num_of_samples/nn,nn),2)),3)) 
    xx=as.data.frame(xx,stringsAsFactors=FALSE)
    colnames(xx)=c("Categories","Expected Value", "Expected Proportion")
    xx[nrow(xx)+1,] <- c("Total", round(sum(round(rep(num_of_samples/nn,nn),2)),0),"1")
    xx
  })
  # For Random
  output$values1 <- renderTable({
    sliderValues()},
    align="c"
  )
  
  # For Same
  
  output$values2 <- renderTable({
    sliderValues2()},
    align="c"
  )
  
  output$hint <- renderText({
    paste0("Scroll down to see the table associated with the point you just clicked")
  })
  
  simLimit <- 10000
  
  
  numberSims <- 0
  chisqSims <- numeric()
  latestSim <- NULL
  fullSim <-character()
  
  
  total <- 0 #total number of sims over all set-ups including current one
  totalPrev <- 0 #total number of sims over all set-ups excluding current one
  
  namesInput <- reactive({
    unlist(strsplit(input$names,split=","))  
  })
  
  nullsInput <- reactive({
    probs <- as.numeric(unlist(strsplit(input$nulls,split=",")))
    probs
  })
  
  obsInput <- reactive({
    observed <- as.numeric(unlist(strsplit(input$obs,split=",")))
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
  
  goodNulls2<-
    reactive({
      nulls <- nullsInput()
      goodNulls <- TRUE
      if (sum(nulls)!=1) goodNulls<-FALSE
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
    sum(obsInput()^2/expected)-totalCounts
  })
  
  simsUpdate <- reactive({
    if (input$resample > 0) {
      nullProbs <- isolate(nullsInput()/sum(nullsInput()))
      totalCounts <- isolate(sum(obsInput()))
      expCounts <- nullProbs*totalCounts
      reps <- min(simLimit,isolate(input$sims))
      newSims <- rmultinom(n=reps,size=totalCounts,prob=nullProbs)
      chisqNew <- colSums(newSims^2/expCounts)-totalCounts
      chisqSims <<- c(chisqSims,chisqNew)
      latestSim <<- newSims[,reps]
      numberSims <<- numberSims + reps
      total <<- total+reps
      
      
      hide(id="setup",anim=T,animType="slide")
      
      if (total - totalPrev == 1) {
        updateTabsetPanel(session,"myPanel",selected="Latest Simulation")
      }
      
      varLevels <- isolate(namesInput())
      namesList <- rep(varLevels,times=latestSim)
      fullSim <<- sample(namesList,size=totalCounts,replace=FALSE)
      list(numberSims,latestSim)
    }
  })
  
  
  
  simsReset <- reactive({
    input$reset
    totalPrev <<- totalPrev + numberSims
    numberSims <<- 0
    chisqSims <<- numeric()
    latestSim <<- NULL
    
    show(id="setup",anim=T,animType="slide")
    
    return(totalPrev)
  })
  
  
  
  dfInput <- reactive({
    length(obsInput())-1
  })
  
  
  xmaxInput <- reactive({
    
    qchisq(0.999,df=dfInput())
  })
  
  
  output$totalPrev <- reactive({
    simsReset()
  })
  
  outputOptions(output, 'totalPrev', suspendWhenHidden=FALSE)
  
  output$total <- reactive({
    simsUpdate() #for dependency
    total
  })
  
  
  # needed for the conditional panels to work
  outputOptions(output, 'total', suspendWhenHidden=FALSE)
  
  output$barGraphInitial <- renderPlot({
    if (goodNames()) enable("resample") else disable("resample")
    validate(
      need(goodNames(),"Enter level names for each category, seperated by commas ( e.g. A,B,C,D )."
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
    
    
    lengthCheck2 <- (length(nulls) == length(observed)) && (length(observed)==length(names))
    if (lengthCheck2) enable("resample") else disable("resample")
    validate(
      need(lengthCheck2,
           "Make sure that you enter the exact same number of level names, null probabilities and observed counts")
    )
    
    
    
    observed <- obsInput()
    expected <- nulls*sum(observed)
    tab <- rbind(observed,expected)
    rownames(tab) <-c("Observed","Expected")
    colnames(tab) <- names
    barplot(tab,beside=T,col=c("dark green","yellow"),
            main="Bargraph of Observed and Expected Counts",xlab="",ylab="Counts",
            legend.text=TRUE)
  })
  
  output$remarksInitial <- renderText({
    
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    
    allGood <- (goodNulls() && goodObs() && goodNulls2()) && goodNames() 
    lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
    
    validate(
      need(allGood && lengthCheck,"")
      
    )
    
    
    
    chisq <- obschisqInput()
    rounded1 <- round(chisq,2)
    p.value <- pchisq(chisq, length(nulls)-1, lower.tail=FALSE) 
    rounded2<- round(p.value,3)
    paste("Observed Chi-Square Statistic =  ",as.character(rounded1),sep="\n",",",
          "P-value =  ",as.character(rounded2) )
  })
  
  output$obsTable <- renderTable({
    
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    
    allGood <- (goodNulls() && goodObs() && goodNulls2()) && goodNames()
    lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
    
    validate(
      need(allGood && lengthCheck,"")
    )
    
    
    
    expected <- nulls*sum(observed)
    contribs <- (observed-expected)^2/expected
    df <- data.frame(Levels=names,
                     Observed=observed,
                     Expected=round(expected,2),
                     cont=round(contribs,2)
    )
    names(df)[4] <- c("Contribution to Chi-Square")
    df
  }, align="c"
  )
  
  output$remarksLatest1 <- renderText({
    input$resample
    chisq <- obschisqInput()
    rounded1 <- round(chisq,2)
    rounded2 <- round(chisqSims[length(chisqSims)],2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),sep="\n",",",
          "Latest resampled chi-square statistics = ",as.character(abs(rounded2)))
  })
  
  
  output$barGraphLatest <- renderPlot({
    input$resample
    if (length(chisqSims) > 0) {
      totalCounts <- isolate(sum(obsInput()))
      nulls <- isolate(nullsInput()/sum(nullsInput()))
      expected <- totalCounts*nulls
      tab <- rbind(obsInput(),expected,latestSim)
      rownames(tab) <-c("Observed","Expected","Resampled")
      colnames(tab) <- isolate(namesInput())
      barplot(tab,beside=T,col=c("dark green","yellow","grey"),
              main="Bargraph of Observed, Expected, and Latest Resample",xlab="",
              ylab="Counts",
              legend.text=TRUE)
    }
    
  })
  
  chisqDensities <- reactive({
    input$resample
    if (length(chisqSims)==1) band <- 1 else band <- "nrd0"
    density(chisqSims,n=500, from=0,to=xmaxInput(),bw=band)
  })
  
  
  output$summary1 <- renderTable({
    input$resample
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    expected <- nulls*sum(observed)
    
    df <- data.frame(Levels=names,
                     Observed=observed,
                     Expected=round(expected,2),
                     Resampled=latestSim
    )
    df
  }, align = "c"
  
  )
  
  
  
  output$pvalueplot <-
    renderPlot({
      input$resample
      nulls <- nullsInput()/sum(nullsInput())
      observed <- obsInput()
      chisq <- obschisqInput()
      obs <- isolate(obschisqInput())
      n <- length(chisqSims)
      latest <- chisqSims[n]
      p.value <- pchisq(chisqSims, length(observed)-1, lower.tail=FALSE)
      hist(p.value,breaks=10,main="P-value Distribution Histogram from the simulations", xlab="p-value", xlim=c(0,1),font.lab=2)
      
    })  
  
  output$chisqCurve <- renderPlot({
    
    sim<-input$sims
    
    if(sim<=5){
      obs <- obschisqInput()
      degFreedom <- dfInput()
      chisqGraph(bound=obs,region="above",df=degFreedom,xlab="Chi-Square Values",
                 graph=TRUE)
      abline(v=obs)
      abline(v=0)
    }
    
    else{
      obs <- obschisqInput()
      degFreedom <- dfInput()
      
      chisqGraph(bound=obs,region="above",df=degFreedom,xlab="Chi-Square Values",
                 graph=TRUE)
      abline(v=obs)
      abline(v=0)
      lines(chisqDensities(),col="#D95F02",lwd=3)
      
    }
    
    
    
  })
  
  output$remarksProb <- renderText({
    obs <- obschisqInput()
    paste0("The orange curve approximates the true probability distribution of the chi-square statistic based on simulations.", 
           " The black curve shows the large sample chi-square density.",
           " The shaded area gives the approximate probability of getting a chi-square statistic of ",
           round(obs,2)," or more, if the probability of each outcome is under Null probabilities (i.e. the p-value ).")
  })
  
  output$remarksProb2 <- renderText({
    obs <- obschisqInput()
    paste0(" The black curve is the large sample chi-square density.",
           " The shaded area gives the approximate probability of getting a chi-square statistic of ",
           round(obs,2)," or more, if the probability of each outcome is under Null probabilities (i.e. the p-value ).")
  })
  
############ Question Bank ############
############ ############ ############ ############
question <- read.csv('bank.csv')
question$Question=as.character(question$Question)
question$A=as.character(question$A)
question$B=as.character(question$B)
question$C=as.character(question$C)
question$D=as.character(question$D)
question$Correct=as.character(question$Correct)
question$Feedback=as.character(question$Feedback)
sapply(question, class)

values <- reactiveValues()

  output$question <- renderText ({
    values$num <- sample(1:6, 1, replace = FALSE)
    cquestion <- question$Question[values$num]
    cquestion
  })
  
  output$questionChoice <- renderUI ({
    selectInput('challengeChoice', label = '',
                choices = c("Please select your response",question$A[values$num], question$B[values$num], question$C[values$num], question$D[values$num])
                )
  })
  
  observeEvent(input$nextX, {
    values$num = sample(1:8, 1, replace = FALSE)
    questionUpdate <- question$Question[values$num]
    questionUpdate
  })
  
  observeEvent(input$submitX, {
    if (input$challengeChoice == question$Correct[values$num]) {
      output$challengeFeedback <- renderUI ({
        div(style = "text-align: center", img(src = 'correct.png', height = 100, width = 100))
      })
      output$textFeedback <- renderUI ({ #UI
        div(style = "text-align: center", tags$h4('Congratulations!'))
      })
    }
    else {
      output$challengeFeedback <- renderUI ({
        div(style = "text-align: center", img(src = 'incorrect.png', height = 100, width = 100))
      })
      output$textFeedback <- renderUI ({
        div(style = "text-align: center", tags$h4(question$Feedback[values$num]))
      })
    }
  })
  
  observeEvent(input$nextX, {
    shinyjs::hideElement("challengeFeedback")
    shinyjs::hideElement('textFeedback')
  })
  
  observeEvent(input$submitX, {
    shinyjs::showElement("challengeFeedback")
    shinyjs::showElement('textFeedback')
  })
  
})