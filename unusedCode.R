
#Line 1319
###### return later ----
# observeEvent(
#   eventExpr = c(input$names,input$nulls,input$obs),
#   handlerExpr = {
#     simsReset()
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

