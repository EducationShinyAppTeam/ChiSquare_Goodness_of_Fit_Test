
#Line 1319
###### return later ----
# observeEvent(
#   eventExpr = c(input$names,input$nulls,input$obs),
#   handlerExpr = {
#     simsReset()
#   }
# )

# Replaced with hints
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

#Code for initial plot in base r (refer to if there are issues with ggplot)
# barplot(
#   tab,
#   beside = T,
#   col = c(
#     boastUtils::boastPalette[3],
#     boastUtils::boastPalette[6]
#   ),
#   main = "Bargraph of Observed and Expected Counts",
#   xlab = "",
#   ylab = "Counts",
#   legend.text = TRUE)


# For diffProb in base r (refer to if there are issues with ggplot)
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

# For equalProb in base r (refer to if there are issues with ggplot)
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

#Code for first plot in base r (refer to if there are issues with ggplot)
# barplot(
#   tab,
#   beside = T,
#   col = c(
#     boastUtils::boastPalette[3],
#     boastUtils::boastPalette[6],
#     boastUtils::boastPalette[7]
#   ),
#   main = "Bargraph of Observed, Expected, and Latest Resample",
#   xlab = "",
#   ylab = "Counts",
#   legend.text = TRUE
# )


#Code for second plot in base r (refer to if there are issues with ggplot)
# input$resample
# nulls <- nullsInput()/sum(nullsInput())
# observed <- obsInput()
# chisq <- obschisqInput()
# obs <- isolate(obschisqInput())
# n <- length(chisqSims)
# latest <- chisqSims[n]
# p.value <- pchisq(chisqSims, length(observed) - 1, lower.tail = FALSE)
# hist(
#   p.value,breaks = 10,
#   main = "P-value Distribution Histogram from the simulations",
#   xlab = "p-value",
#   xlim = c(0,1),
#   font.lab = 2)

#Code of third plot in base r (refer to if there are issues with ggplot)
# chisqDensities <- reactive(
#   x = {
#     input$resample
#     if (length(chisqSims) == 1) {
#       band <- 1
#     } else {
#       band <- "nrd0"
#     }
#     density(
#       chisqSims,
#       n = 500,
#       from = 0,
#       to = xmaxInput(),
#       bw = band
#     )
#   }
# )
# output$chisqCurve <- renderPlot(
#   expr = {
#     sim <- input$sims
#     if (sim <= 5) {
#       obs <- obschisqInput()
#       degFreedom <- dfInput()
#       chisqGraph(
#         bound = round(obs,3),
#         region = "above",
#         df = degFreedom,
#         xlab = "Chi-Square Values",
#         graph = TRUE)
#       abline(v = obs)
#       abline(v = 0)
#     }
#     else{
#       obs <- obschisqInput()
#       degFreedom <- dfInput()
#       chisqGraph(
#         bound = round(obs,3),
#         region = "above",
#         df = degFreedom,
#         xlab = "Chi-Square Values",
#         graph = TRUE)
#       abline(v = obs)
#       abline(v = 0)
#       lines(chisqDensities(),col = boastUtils::psuPalette[4],lwd = 3)
#     }
#   }
# )


#THE SIMS INPUT MINIMUM NOW 50
# no. of simulation:
# conditionalPanel(
#  condition = "input.sims <= 5",
# p(textOutput("remarksProb2")),
# ),

#Code from  ####### diff. interpretation based on no. of simulation section
# output$remarksProb2 <- renderText({
#  obs <- obschisqInput()
#   paste0(" The black curve is the large sample chi-square density.",
#          " The shaded area gives the approximate probability of getting a chi-square statistic of ",
#         round(obs,2),
#         " or more, if the probability of each outcome is under Null probabilities (i.e. the p-value ).")
# })