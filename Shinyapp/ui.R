

library(shiny)
speciality_wise<-readRDS("city_wise.rda")
shinyUI(fluidPage(

 
  titlePanel("Old Faithful Geyser Data"),

  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    
    mainPanel(
      verbatimTextOutput("sum")
    )
  )
))
