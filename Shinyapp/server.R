library(shiny)
speciality_wise<-readRDS("speciality_wise.rda")
shinyServer(function(input, output) {

  output$sum <-renderPrint(
    summary(speciality_wise)
  ) 

})




