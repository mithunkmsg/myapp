library(shiny)
speciality_wise<-readRDS("city_wise.rda")
shinyServer(function(input, output) {

  output$sum <-renderPrint(
    summary(speciality_wise)
  ) 

})




