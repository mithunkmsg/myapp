library(shiny)
library(lubridate)
library(dygraphs)


ui <- fluidPage(
  theme = "style.css",
  img(src='logo_omega_.png', align = 'center',height='60px',class='d-inline-block align-top',alt=""
      ,href="#"),
  titlePanel(div(h4("NEW PATIENT VISIT APPOINTMENT ANALYSIS", align = "center"), style = "color:roboto"),windowTitle = "Appt Feedback Analysis"),
  #theme = shinytheme("Roboto"),
  fluidRow(
    column(3,wellPanel(
      
      tags$head(tags$style(type="text/css", 
                           ".test_type {color : Roboto;
                           font-size: 12px; 
                           }"
      )
      ),
      
      br(),
      
      
      div(class="test_type",dateRangeInput(inputId = "dateRange",  
                                           label =  "1. Please Select a Date Range:",
                                           start = Sys.Date()-10,
                                           end = Sys.Date()-1,
                                           #min = "2018-07-01"),
                                           format = "dd-mm-yyyy"
                                           
      )),
      
      div(class="test_type",selectInput("appt_input", label = "2. Type of Analysis:", 
                                        c("Date Wise"= "date",
                                          "City Wise" = "city",
                                          "Hospital Wise" = "hospital",
                                          "Treatment Wise" = 'treatment',
                                          "Agent Wise" ="agent"
                                        ),
                                        width = validateCssUnit("100%"))),
      
      
      #uiOutput("submit")
      
      uiOutput("filter_box"),
      
      submitButton()
      
      )),
    column(9,tabsetPanel(type="tab",
                         tabPanel("Date Wise",br(),
                                  span(dataTableOutput("table1"),style="color:Roboto"
                                  )),
                         tabPanel("City Wise",br(),
                                  span(dataTableOutput("table2"),style="color:Roboto"
                                  )),
                         tabPanel("Hospital Wise",br(),
                                  span(dataTableOutput("table3"),style="color:Roboto"
                                  )),
                         tabPanel("Treatment Wise",br(),
                                  span(dataTableOutput("table4"),style="color:Roboto"
                                  )),
                         tabPanel("Sales Agent Wise",br(),
                                  span(dataTableOutput("table5"),style="color:Roboto"
                                  ))
                         
                         
                         
    ))
    
    )
)



