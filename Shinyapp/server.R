library(reshape2)
library(dplyr)
library(xts)
library(dygraphs)
library(ggplot2)
library(forecast)


request_data<-read.csv("E:/DATA SCIENCE/credihealth/vaibhav project/request_data.csv")
request_data$Ind_search<-as.numeric(request_data$Ind_search)
request_data[is.na(request_data)]<-3968
request_data$Month<-as.Date(request_data$Month,format="%m/%d/%Y")

df1<-melt(request_data,id.vars = c("Month","Speciality"))

search<-df1 %>% filter(variable=="Ind_search")
request<-df1 %>% filter(variable=="Request")
opd<-df1 %>% filter(variable=="Opd")
ipd<-df1 %>% filter(variable=="Ipd") 




shinyServer(
  function(input,output){
    
    selected1 <- reactive({search %>% 
        filter(Speciality==input$Speciality) %>% 
        group_by(Month) %>%
        summarise(n = value)})
    
    selected2 <- reactive({request %>% 
        filter(Speciality==input$Speciality) %>% 
        group_by(Month) %>%
        summarise(n = value)})
    
    selected3 <- reactive({opd %>% 
        filter(Speciality==input$Speciality) %>% 
        group_by(Month) %>%
        summarise(n = value)})
    
    selected4 <- reactive({ipd %>% 
        filter(Speciality==input$Speciality) %>% 
        group_by(Month) %>%
        summarise(n = value)})
    
    output$plotgraph1<-renderPlot({
         ts1<-ts(selected2()$n,start = c(2017,5),frequency = 12)
         fit1 = Arima(ts1, order = c(4,1,1),include.drift = T)
         future<-forecast(fit1,h=6)
         plot(future,xlab = "Month",ylab = "value",main = "Forecast For Request")
         summary(future)
    })
    
    output$text1<-renderPrint({
      ts1<-ts(selected2()$n,start = c(2017,5),frequency = 12)
      fit1 = auto.arima(ts1)
      future<-forecast(fit1,h=6)
      future$upper
      
    })
    
    output$plotgraph2<-renderPlot({
      ts1<-ts(selected3()$n,start = c(2017,5),frequency = 12)
      fit1 = Arima(ts1, order = c(4,1,1),include.drift = T)
      future<-forecast(fit1,h=6)
      plot(future,xlab = "Month",ylab = "value",main = "Forecast For Opd")
      summary(future)
    })
    
    output$text2<-renderPrint({
      ts1<-ts(selected3()$n,start = c(2017,5),frequency = 12)
      fit1 = auto.arima(ts1)
      future<-forecast(fit1,h=6)
      future$upper
      
    })
    
    output$plotgraph3<-renderPlot({
      ts1<-ts(selected4()$n,start = c(2017,5),frequency = 12)
      fit1 = Arima(ts1, order = c(4,1,1),include.drift = T)
      future<-forecast(fit1,h=6)
      plot(future,xlab = "Month",ylab = "value",main = "Forecast For Ipd")
      summary(future)
    })
    
    output$text3<-renderPrint({
      ts1<-ts(selected4()$n,start = c(2017,5),frequency = 12)
      fit1 = auto.arima(ts1)
      future<-forecast(fit1,h=6)
      future$upper
      
    })
    
    
    
    
    
    
    output$dygraph<-renderDygraph({
      spe_xts <- xts(selected1()$n, order.by = as.Date(selected1()$Month))
      dygraph(spe_xts,xlab = "Month",ylab = "Value")%>%
      dySeries("V1",label="Ind_search",color="red", fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)  
    })
    
      
    output$dygraph2<-renderDygraph({
      spe_xts <- xts(cbind(selected2()$n,selected3()$n,selected4()$n), order.by = as.Date(selected2()$Month))
      dygraph(spe_xts,xlab = "Month",ylab = "Value")%>%
        #dySeries("V1",label="Ind_search",color="red", fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dySeries("V1",label="Request",color="green",fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dySeries("V2",label="Opd",color="purple",fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dySeries("V3",label="Ipd",color="orange",fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)#%>%
      #dyLegend(labelsDiv = "legendDivID",labelsSeparateLines = T)
      
      
    })
 }
)