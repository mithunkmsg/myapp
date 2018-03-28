library(shiny)
insurance<-readRDS("insurance.rda")
shinyServer(function(input, output) {

  output$sum <-renderPrint(
    summary(insurance)
  ) 

})




