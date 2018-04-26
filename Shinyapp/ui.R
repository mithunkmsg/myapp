library(shiny)
library(zoo)
library(dygraphs)
library(xts)
library(dplyr)
library(tidyr)
library(reshape2)

request_data<-read.csv("request_data.csv")
request_data$Ind_search<-as.numeric(request_data$Ind_search)
request_data[is.na(request_data)]<-3968
request_data$Month<-as.Date(request_data$Month,format="%m/%d/%Y")

df1<-melt(request_data,id.vars = c("Month","Speciality"))



shinyUI(
  pageWithSidebar(
    
    titlePanel(div(h4("TREND GRAPH FOR IND_SEARCH,CREDI_REQUEST, CREDI_OPD AND CREDI_IPD IN DIFFERENT MONTH", align = "center"), style = "color:red"),windowTitle = "Request Analysis"),
    
    sidebarPanel(
      
      selectInput("Speciality","1. Please select a Keyword:",choices = levels(df1$Speciality),selected = "Abortion Surgery")
      
    ),
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Trend Graph",br(),
                           h4(dygraphOutput("dygraph")),
                           #h4(textOutput("legendDivID"), title = "Legend", collapsible = F, width=2),
                           br(),
                           h4(dygraphOutput("dygraph2"))),
                  tabPanel("Forecast",fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"),plotOutput("plotgraph3"))
                  ),
                  fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"), verbatimTextOutput("text1"), verbatimTextOutput("text2"),verbatimTextOutput("text3"))
                  ))
    )
  )
)
)


