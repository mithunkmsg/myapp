library(shiny)

shinyServer(function(input, output) {

  output$sum <-renderPrint(
    summary(insurance)
  ) 

})




