library(shiny)
library(lubridate)
library(reshape2)
library(dplyr)
library(RMySQL)
library(dbConnect)
library(DBI)
library(DT)
library(janitor)
library(Hmisc)
library(zoo)
library(dygraphs)
library(xts)


#tl<-read.csv("tl_team.csv")

#tl$agent<-tolower(tl$agent)
#tl$tl<-capitalize(tolower(tl$tl))

con = dbConnect(MySQL(), user='', password='careongo',
                dbname = '', host='13.233.21.31')



shinyServer(
  function(input,output){
    
    convert_data<-reactive({
      
      
        
        start<-input$dateRange[1]
        end<-input$dateRange[2]
        
        convert_data<-dbGetQuery(con,paste("select x.lead_id,x.conversion_date,x.center_id,x.hospital,panel_cities.name as city,
                                           x.package_cost,x.agent,x.treatment from
                                           (select y.lead_id,y.conversion_date,y.latest_status,y.center_id,
                                           panel_centers.name as hospital,panel_centers.city_id,y.package_cost, y.agent,y.treatment from
                                           (select z.lead_id, z.conversion_date, appointment_appointment.latest_status,
                                           appointment_appointment.center_id,z.package_cost, lead_lead.manual_agent_field as agent,
                                           appointment_appointment.treatment from 
                                           (select conversion_treatment.conversion_date,lead_leadconverted.lead_id,
                                           conversion_treatment.package_cost 
                                           from conversion_treatment left outer join lead_leadconverted on 
                                           conversion_treatment.lead_converted_id=lead_leadconverted.id where 
                                           conversion_treatment.conversion_date between '",start,"' and '",end,"') as z 
                                           left outer join appointment_appointment on z.lead_id=appointment_appointment.lead_id
                                           left outer join lead_lead on lead_lead.id=z.lead_id
                                           where appointment_appointment.latest_status in ('appointment_done_hot')) as y
                                           left outer join panel_centers on panel_centers.id=y.center_id) as x
                                           left outer join panel_cities on panel_cities.id=x.city_id"))
        
        
        convert_data<-unique(convert_data)
        
        convert_data$month<-months(as.Date(convert_data$conversion_date))
        
        convert_data$hospital<-paste(convert_data$hospital,convert_data$city,sep=", ")
        
        convert_data$treatment<-capitalize(tolower(convert_data$treatment))
        
        convert_data$treatment[which(convert_data$treatment %in% c('Ivf_icsi','Ivf_imsi','Ivf_pgd',
                                                                   'Ivf_egg_donor','Ivf_with_egg_donor'))]<-'Ivf'
        
        convert_data$agent<-tolower(convert_data$agent)
        
        #convert_data<-convert_data %>% left_join(tl,by="agent") %>% droplevels()
        
        #convert_data$tl[grep(".*international",tolower(convert_data$agent))]<-"International"
        
        convert_data<-as.data.frame(convert_data)
        
      
      
    })
    
    appt_booked<-reactive({
      
      
        
        start<-input$dateRange[1]
        end<-input$dateRange[2]
        
        appt_booked<-dbGetQuery(con,paste("select appointment_appointment.id as appt_id, appointment_appointment.lead_id,
                                          appointment_appointment.center_id, appointment_appointment.latest_status,
                                          appointment_appointment.appointment_date as appt_date,panel_centers.city_id,
                                          lead_lead.manual_agent_field as agent,panel_centers.name as hospital,
                                          appointment_appointment.treatment from appointment_appointment
                                          left outer join panel_centers on appointment_appointment.center_id=panel_centers.id
                                          left outer join lead_lead on appointment_appointment.lead_id=lead_lead.id
                                          where appointment_appointment.appointment_date between '",start,"' and '",end,"'"))
        
        panel_city<-dbGetQuery(con,"select id as city_id, name as city from panel_cities")
        
        appt_booked<-appt_booked %>% left_join(panel_city,by="city_id") %>% droplevels()
        
        appt_booked$treatment<-capitalize(tolower(appt_booked$treatment))
        
        appt_booked$treatment[which(appt_booked$treatment %in% c('Ivf_icsi','Ivf_imsi','Ivf_pgd',
                                                                 'Ivf_egg_donor','Ivf_with_egg_donor'))]<-'Ivf'
        
        appt_booked$hospital<-paste(appt_booked$hospital,appt_booked$city,sep=", ")
        
        appt_booked$agent<-tolower(appt_booked$agent)
        
        #appt_booked<-appt_booked %>% left_join(tl,by="agent") %>% droplevels()
        
        #appt_booked$tl[grep(".*international",tolower(appt_booked$agent))]<-"International"
        
        appt_booked<-as.data.frame(appt_booked)
        
      
      
    })
    
    
    first_visited_lead<-reactive({
      
      
        
        start<-input$dateRange[1]
        end<-input$dateRange[2]
        
        first_visited_lead<-dbGetQuery(con,paste("select appointment_appointment.lead_id as lead_id,
                                                 appointment_appointment.center_id, min(appointment_appointment.appointment_date) as first_visit_date
                                                 from
                                                 (select appointment_appointment.id as appt_id, appointment_appointment.lead_id,
                                                 appointment_appointment.center_id, appointment_appointment.latest_status,
                                                 appointment_appointment.appointment_date from appointment_appointment
                                                 where appointment_appointment.appointment_date between '",start,"' and '",end,"') as z
                                                 left outer join appointment_appointment on z.lead_id=appointment_appointment.lead_id
                                                 
                                                 where appointment_appointment.latest_status in ('appointment_done',
                                                 'appointment_done_hot','appointment_done_cold')
                                                 group by appointment_appointment.lead_id,appointment_appointment.center_id"))
        
        
        
        
      
      
    })
    
    
    first_booked_lead<-reactive({
      
      
        
        start<-input$dateRange[1]
        end<-input$dateRange[2]
        
        first_booked_lead<-dbGetQuery(con,paste("select appointment_appointment.lead_id as lead_id,appointment_appointment.center_id
                                                ,min(appointment_appointment.appointment_date) as first_appt_date
                                                from
                                                (select appointment_appointment.id as appt_id, appointment_appointment.lead_id,
                                                appointment_appointment.center_id, appointment_appointment.latest_status,
                                                appointment_appointment.appointment_date from appointment_appointment
                                                where appointment_appointment.appointment_date between '",start,"' and '",end,"') as z
                                                left outer join appointment_appointment on z.lead_id=appointment_appointment.lead_id
                                                group by appointment_appointment.lead_id,appointment_appointment.center_id"))
        
        
        
      
      
    })
    
    
    
    #########################   appt analysis report ###################################
    
    ### date wise
    
    # date tab
    
    appt_report_date_date<-reactive({
      
       
        mydata<-appt_booked() %>% left_join(first_visited_lead(),by=c('lead_id','center_id')) %>% droplevels()
        mydata<-mydata %>% left_join(first_booked_lead(),by=c('lead_id','center_id')) %>% droplevels()
        
        
        mydata$visit_diff<-as.integer(as.Date(mydata$first_visit_date)-as.Date(mydata$appt_date))
        mydata$booking_diff<-as.integer(as.Date(mydata$first_appt_date)-as.Date(mydata$appt_date))
        
        ## unique visit
        
        unique_done<-mydata[-which(mydata$booking_diff<0),]
        
        unique_done<-unique_done[which(unique_done$visit_diff>=0),]
        
        mydata$latest_status[which(mydata$appt_id %in% c(unique_done[which(unique_done$latest_status=="appointment_missed"),1]))]<-"appointment_done"
        
        
        unique_done<-unique_done[!duplicated(unique_done[c(2,3)]),]
        
        if(input$appt_input=="date"){
          
          unique_visit<-unique_done %>% group_by(appt_date) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="city"){
          
          unique_visit<-unique_done %>% filter(city %in% c(input$mycity)) %>% group_by(appt_date) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          unique_visit<-unique_done %>% filter(treatment %in% c(input$mytreat)) %>% group_by(appt_date) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          unique_visit<-unique_done %>% filter(agent %in% c(input$myagent)) %>% group_by(appt_date) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          unique_visit<-unique_done %>% filter(hospital %in% c(input$myhos)) %>% group_by(appt_date) %>% summarise(new_visit=n())
          
        }
        
        ## total visit
        
        done<-mydata %>% filter(latest_status %in% c('appointment_done','appointment_done_hot',
                                                     'appointment_done_cold')) %>% droplevels()
        
        done<-done[!duplicated(done[c(2,3)]),]
        
        
        if(input$appt_input=="date"){
          
          total_visit<-done %>% group_by(appt_date) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="city"){
          
          total_visit<-done %>% filter(city %in% c(input$mycity)) %>% group_by(appt_date) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_visit<-done %>% filter(treatment %in% c(input$mytreat)) %>% group_by(appt_date) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          total_visit<-done %>% filter(agent %in% c(input$myagent)) %>% group_by(appt_date) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_visit<-done %>% filter(hospital %in% c(input$myhos)) %>% group_by(appt_date) %>% summarise(total_visit=n())
          
        }
        
        ## total convert
        
        total_convert<-convert_data()[!duplicated(convert_data()[c(1,4)]),]
        
        if(input$appt_input=="date"){
          
          total_convert<-total_convert %>% group_by(conversion_date) %>% summarise(convert=n())
          
        }else if(input$appt_input=="city"){
          
          total_convert<-total_convert %>% filter(city %in% c(input$mycity)) %>% group_by(conversion_date) %>% summarise(convert=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_convert<-total_convert %>% filter(treatment %in% c(input$mytreat)) %>% group_by(conversion_date) %>% summarise(convert=n())
          
        }else if(input$appt_input=="agent"){
          
          total_convert<-total_convert %>% filter(agent %in% c(input$myagent)) %>% group_by(conversion_date) %>% summarise(convert=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_convert<-total_convert %>% filter(hospital %in% c(input$myhos)) %>% group_by(conversion_date) %>% summarise(convert=n())
          
        }
        
        colnames(total_convert)[1]<-"appt_date"
        
        ## total package cost
        
        if(input$appt_input=="date"){
          
          total_package_cost<-convert_data() %>% group_by(conversion_date) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="city"){
          
          total_package_cost<-convert_data() %>% filter(city %in% c(input$mycity)) %>% group_by(conversion_date) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="treatment"){
          
          total_package_cost<-convert_data() %>% filter(treatment %in% c(input$mytreat)) %>% group_by(conversion_date) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="hospital"){
          
          total_package_cost<-convert_data() %>% filter(hospital %in% c(input$myhos)) %>% group_by(conversion_date) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="agent"){
          
          total_package_cost<-convert_data() %>% filter(agent %in% c(input$magent)) %>% group_by(conversion_date) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }
        
        colnames(total_package_cost)[1]<-"appt_date"
        
        #### combine all data
        
        final_data<-merge.data.frame(total_visit,unique_visit,by="appt_date",all = T)
        final_data<-merge.data.frame(final_data,total_convert,by="appt_date",all = T)
        final_data<-merge.data.frame(final_data,total_package_cost,by="appt_date",all = T)
        
        final_data$day<-weekdays(as.Date(final_data$appt_date))
         
        final_data<-final_data[,c(1,6,2:5)]
      
        final_data$new_pt_p<-round(((final_data$new_visit)/(final_data$total_visit))*100,digits = 2)
        
        final_data<-final_data[,c(1:4,7,5,6)]
        
        final_data<-as.data.frame(final_data)
        
    })
    
    # city tab
    
    appt_report_date_city<-reactive({
      
      
        
        mydata<-appt_booked() %>% left_join(first_visited_lead(),by=c('lead_id','center_id')) %>% droplevels()
        mydata<-mydata %>% left_join(first_booked_lead(),by=c('lead_id','center_id')) %>% droplevels()
        
        
        mydata$visit_diff<-as.integer(as.Date(mydata$first_visit_date)-as.Date(mydata$appt_date))
        mydata$booking_diff<-as.integer(as.Date(mydata$first_appt_date)-as.Date(mydata$appt_date))
        
        ## unique visit
        
        unique_done<-mydata[-which(mydata$booking_diff<0),]
        
        unique_done<-unique_done[which(unique_done$visit_diff>=0),]
        
        mydata$latest_status[which(mydata$appt_id %in% c(unique_done[which(unique_done$latest_status=="appointment_missed"),1]))]<-"appointment_done"
        
        
        unique_done<-unique_done[!duplicated(unique_done[c(2,3)]),]
        
        if(input$appt_input=="date"){
          
          unique_visit<-unique_done %>% group_by(city) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="city"){
          
          unique_visit<-unique_done %>% filter(city %in% c(input$mycity)) %>% group_by(city) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          unique_visit<-unique_done %>% filter(treatment %in% c(input$mytreat)) %>% group_by(city) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          unique_visit<-unique_done %>% filter(agent %in% c(input$myagent)) %>% group_by(city) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          unique_visit<-unique_done %>% filter(hospital %in% c(input$myhos)) %>% group_by(city) %>% summarise(new_visit=n())
          
        }
        
        ## total visit
        
        done<-mydata %>% filter(latest_status %in% c('appointment_done','appointment_done_hot',
                                                     'appointment_done_cold')) %>% droplevels()
        
        done<-done[!duplicated(done[c(2,3)]),]
        
        done1<-done %>% group_by(agent,city) %>% summarise(total_visit=n())
        
        if(input$appt_input=="date"){
          
          total_visit<-done %>% group_by(city) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="city"){
          
          total_visit<-done %>% filter(city %in% c(input$mycity)) %>%  group_by(city) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_visit<-done %>% filter(treatment %in% c(input$mytreat)) %>%  group_by(city) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_visit<-done %>% filter(hospital %in% c(input$myhos)) %>%  group_by(city) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          total_visit<-done %>% filter(agent %in% c(input$myagent)) %>%  group_by(city) %>% summarise(total_visit=n())
          
        }
        
        ## total convert
        
        total_convert<-convert_data()[!duplicated(convert_data()[c(1,4)]),]
        
        if(input$appt_input=="date"){
          
          total_convert<-total_convert %>% group_by(city) %>% summarise(convert=n())
          
        }else if(input$appt_input=="city"){
          
          total_convert<-total_convert %>% filter(city %in% c(input$mycity)) %>% group_by(city) %>% summarise(convert=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_convert<-total_convert %>% filter(treatment %in% c(input$mytreat)) %>% group_by(city) %>% summarise(convert=n())
          
        }else if(input$appt_input=="agent"){
          
          total_convert<-total_convert %>% filter(agent %in% c(input$myagent)) %>% group_by(city) %>% summarise(convert=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_convert<-total_convert %>% filter(hospital %in% c(input$myhos)) %>% group_by(city) %>% summarise(convert=n())
          
        }
        
        ## total package cost
        
        if(input$appt_input=="date"){
          
          total_package_cost<-convert_data() %>% group_by(city) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="city"){
          
          total_package_cost<-convert_data() %>% filter(city %in% c(input$mycity)) %>% group_by(city) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="agent"){
          
          total_package_cost<-convert_data() %>% filter(agent %in% c(input$myagent)) %>% group_by(city) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="treatment"){
          
          total_package_cost<-convert_data() %>% filter(treatment %in% c(input$mytreat)) %>% group_by(city) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="hospital"){
          
          total_package_cost<-convert_data() %>% filter(hospital %in% c(input$myhos)) %>% group_by(city) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }
        
        #### combine all data
        
        final_data<-merge.data.frame(total_visit,unique_visit,by="city",all = T)
        final_data<-merge.data.frame(final_data,total_convert,by="city",all = T)
        final_data<-merge.data.frame(final_data,total_package_cost,by="city",all = T)
        
        final_data$new_pt_p<-round(((final_data$new_visit)/(final_data$total_visit))*100,digits = 2)
        
        final_data<-final_data[,c(1:3,6,4,5)]      
        
        final_data<-as.data.frame(final_data)
        
    })
    
    appt_report_date_hospital<-reactive({
      
      
        
        mydata<-appt_booked() %>% left_join(first_visited_lead(),by=c('lead_id','center_id')) %>% droplevels()
        mydata<-mydata %>% left_join(first_booked_lead(),by=c('lead_id','center_id')) %>% droplevels()
        
        
        mydata$visit_diff<-as.integer(as.Date(mydata$first_visit_date)-as.Date(mydata$appt_date))
        mydata$booking_diff<-as.integer(as.Date(mydata$first_appt_date)-as.Date(mydata$appt_date))
        
        ## unique visit
        
        unique_done<-mydata[-which(mydata$booking_diff<0),]
        
        unique_done<-unique_done[which(unique_done$visit_diff>=0),]
        
        mydata$latest_status[which(mydata$appt_id %in% c(unique_done[which(unique_done$latest_status=="appointment_missed"),1]))]<-"appointment_done"
        
        
        unique_done<-unique_done[!duplicated(unique_done[c(2,3)]),]
        
        if(input$appt_input=="date"){
          
          unique_visit<-unique_done %>% group_by(hospital) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="city"){
          
          unique_visit<-unique_done %>% filter(city %in% c(input$mycity)) %>% group_by(hospital) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          unique_visit<-unique_done %>% filter(treatment %in% c(input$mytreat)) %>% group_by(hospital) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          unique_visit<-unique_done %>% filter(agent %in% c(input$myagent)) %>% group_by(hospital) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          unique_visit<-unique_done %>% filter(hospital %in% c(input$myhos)) %>% group_by(hospital) %>% summarise(new_visit=n())
          
        }
        
        ## total visit
        
        done<-mydata %>% filter(latest_status %in% c('appointment_done','appointment_done_hot',
                                                     'appointment_done_cold')) %>% droplevels()
        
        done<-done[!duplicated(done[c(2,3)]),]
        
        done1<-done %>% group_by(agent,city) %>% summarise(total_visit=n())
        
        if(input$appt_input=="date"){
          
          total_visit<-done %>% group_by(hospital) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="city"){
          
          total_visit<-done %>% filter(city %in% c(input$mycity)) %>% group_by(hospital) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_visit<-done %>% filter(treatment %in% c(input$mytreat)) %>% group_by(hospital) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_visit<-done %>% filter(hospital %in% c(input$myhos)) %>% group_by(hospital) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          total_visit<-done %>% filter(agent %in% c(input$myagent)) %>% group_by(hospital) %>% summarise(total_visit=n())
          
        }
        
        ## total convert
        
        total_convert<-convert_data()[!duplicated(convert_data()[c(1,4)]),]
        
        if(input$appt_input=="date"){
          
          total_convert<-total_convert %>% group_by(hospital) %>% summarise(convert=n())
          
        }else if(input$appt_input=="city"){
          
          total_convert<-total_convert %>% filter(city %in% c(input$mycity)) %>% group_by(hospital) %>% summarise(convert=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_convert<-total_convert %>% filter(treatment %in% c(input$mytreat)) %>% group_by(hospital) %>% summarise(convert=n())
          
        }else if(input$appt_input=="agent"){
          
          total_convert<-total_convert %>% filter(agent %in% c(input$myagent)) %>% group_by(hospital) %>% summarise(convert=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_convert<-total_convert %>% filter(hospital %in% c(input$myhos)) %>% group_by(hospital) %>% summarise(convert=n())
          
        }
        
        ## total package cost
        
        if(input$appt_input=="date"){
          
          total_package_cost<-convert_data() %>% group_by(hospital) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="city"){
          
          total_package_cost<-convert_data() %>% filter(city %in% c(input$mycity)) %>% group_by(hospital) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="treatment"){
          
          total_package_cost<-convert_data() %>% filter(treatment %in% c(input$mytreat)) %>% group_by(hospital) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="agent"){
          
          total_package_cost<-convert_data() %>% filter(agent %in% c(input$myagent)) %>% group_by(hospital) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="hospital"){
          
          total_package_cost<-convert_data() %>% filter(hospital %in% c(input$myhos)) %>% group_by(hospital) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }
        
        #### combine all data
        
        final_data<-merge.data.frame(total_visit,unique_visit,by="hospital",all = T)
        final_data<-merge.data.frame(final_data,total_convert,by="hospital",all = T)
        final_data<-merge.data.frame(final_data,total_package_cost,by="hospital",all = T)
        
        final_data$new_pt_p<-round(((final_data$new_visit)/(final_data$total_visit))*100,digits = 2)
        
        final_data<-final_data[,c(1:3,6,4,5)] 
        
        final_data<-as.data.frame(final_data)
      
    })
    
    appt_report_date_treatment<-reactive({
      
       
        
        mydata<-appt_booked() %>% left_join(first_visited_lead(),by=c('lead_id','center_id')) %>% droplevels()
        mydata<-mydata %>% left_join(first_booked_lead(),by=c('lead_id','center_id')) %>% droplevels()
        
        
        mydata$visit_diff<-as.integer(as.Date(mydata$first_visit_date)-as.Date(mydata$appt_date))
        mydata$booking_diff<-as.integer(as.Date(mydata$first_appt_date)-as.Date(mydata$appt_date))
        
        ## unique visit
        
        unique_done<-mydata[-which(mydata$booking_diff<0),]
        
        unique_done<-unique_done[which(unique_done$visit_diff>=0),]
        
        mydata$latest_status[which(mydata$appt_id %in% c(unique_done[which(unique_done$latest_status=="appointment_missed"),1]))]<-"appointment_done"
        
        
        unique_done<-unique_done[!duplicated(unique_done[c(2,3)]),]
        
        if(input$appt_input=="date"){
          
          unique_visit<-unique_done %>% group_by(treatment) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="city"){
          
          unique_visit<-unique_done %>% filter(city %in% c(input$mycity)) %>% group_by(treatment) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          unique_visit<-unique_done %>% filter(treatment %in% c(input$mytreat)) %>% group_by(treatment) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          unique_visit<-unique_done %>% filter(agent %in% c(input$myagent)) %>% group_by(treatment) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          unique_visit<-unique_done %>% filter(hospital %in% c(input$myhos)) %>% group_by(treatment) %>% summarise(new_visit=n())
          
        }
        
        ## total visit
        
        done<-mydata %>% filter(latest_status %in% c('appointment_done','appointment_done_hot',
                                                     'appointment_done_cold')) %>% droplevels()
        
        done<-done[!duplicated(done[c(2,3)]),]
        
        done1<-done %>% group_by(agent,city) %>% summarise(total_visit=n())
        
        if(input$appt_input=="date"){
          
          total_visit<-done %>% group_by(treatment) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="city"){
          
          total_visit<-done %>% filter(city %in% c(input$mycity)) %>% group_by(treatment) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_visit<-done %>% filter(treatment %in% c(input$mytreat)) %>% group_by(treatment) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          total_visit<-done %>% filter(agent %in% c(input$myagent)) %>% group_by(treatment) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_visit<-done %>% filter(hospital %in% c(input$myhos)) %>% group_by(treatment) %>% summarise(total_visit=n())
          
        }    
        
        ## total convert
        
        total_convert<-convert_data()[!duplicated(convert_data()[c(1,4)]),]
        
        if(input$appt_input=="date"){
          
          total_convert<-total_convert %>% group_by(treatment) %>% summarise(convert=n())
          
        }else if(input$appt_input=="city"){
          
          total_convert<-total_convert %>% filter(city %in% c(input$mycity)) %>% group_by(treatment) %>% summarise(convert=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_convert<-total_convert %>% filter(treatment %in% c(input$mytreat)) %>% group_by(treatment) %>% summarise(convert=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_convert<-total_convert %>% filter(hospital %in% c(input$myhos)) %>% group_by(treatment) %>% summarise(convert=n())
          
        }else if(input$appt_input=="agent"){
          
          total_convert<-total_convert %>% filter(agent %in% c(input$myagent)) %>% group_by(treatment) %>% summarise(convert=n())
          
        }
        
        ## total package cost
        
        if(input$appt_input=="date"){
          
          total_package_cost<-convert_data() %>% group_by(treatment) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="city"){
          
          total_package_cost<-convert_data() %>% filter(city %in% c(input$mycity)) %>% group_by(treatment) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="treatment"){
          
          total_package_cost<-convert_data() %>% filter(treatment %in% c(input$mytreat)) %>% group_by(treatment) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="hospital"){
          
          total_package_cost<-convert_data() %>% filter(hospital %in% c(input$myhos)) %>% group_by(treatment) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="agent"){
          
          total_package_cost<-convert_data() %>% filter(agent %in% c(input$myagent)) %>% group_by(treatment) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }
        
        
        #### combine all data
        
        final_data<-merge.data.frame(total_visit,unique_visit,by="treatment",all = T)
        final_data<-merge.data.frame(final_data,total_convert,by="treatment",all = T)
        final_data<-merge.data.frame(final_data,total_package_cost,by="treatment",all = T)
        
        final_data$new_pt_p<-round(((final_data$new_visit)/(final_data$total_visit))*100,digits = 2)
        
        final_data<-final_data[,c(1:3,6,4,5)] 
        
        final_data<-as.data.frame(final_data)
      
    })
    
    appt_report_date_agent<-reactive({
      
      
        
        mydata<-appt_booked() %>% left_join(first_visited_lead(),by=c('lead_id','center_id')) %>% droplevels()
        mydata<-mydata %>% left_join(first_booked_lead(),by=c('lead_id','center_id')) %>% droplevels()
        
        
        mydata$visit_diff<-as.integer(as.Date(mydata$first_visit_date)-as.Date(mydata$appt_date))
        mydata$booking_diff<-as.integer(as.Date(mydata$first_appt_date)-as.Date(mydata$appt_date))
        
        ## unique visit
        
        unique_done<-mydata[-which(mydata$booking_diff<0),]
        
        unique_done<-unique_done[which(unique_done$visit_diff>=0),]
        
        mydata$latest_status[which(mydata$appt_id %in% c(unique_done[which(unique_done$latest_status=="appointment_missed"),1]))]<-"appointment_done"
        
        
        unique_done<-unique_done[!duplicated(unique_done[c(2,3)]),]
        
        if(input$appt_input=="date"){
          
          unique_visit<-unique_done %>% group_by(agent) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="city"){
          
          unique_visit<-unique_done %>% filter(city %in% c(input$mycity)) %>% group_by(agent) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          unique_visit<-unique_done %>% filter(agent %in% c(input$myagent)) %>% group_by(agent) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          unique_visit<-unique_done %>% filter(hospital %in% c(input$myhos)) %>% group_by(agent) %>% summarise(new_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          unique_visit<-unique_done %>% filter(treatment %in% c(input$mytreat)) %>% group_by(agent) %>% summarise(new_visit=n())
          
        }
        
        ## total visit
        
        done<-mydata %>% filter(latest_status %in% c('appointment_done','appointment_done_hot',
                                                     'appointment_done_cold')) %>% droplevels()
        
        done<-done[!duplicated(done[c(2,3)]),]
        
        done1<-done %>% group_by(agent,city) %>% summarise(total_visit=n())
        
        if(input$appt_input=="date"){
          
          total_visit<-done %>% group_by(agent) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="city"){
          
          total_visit<-done %>% filter(city %in% c(input$mycity)) %>% group_by(agent) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="agent"){
          
          total_visit<-done %>% filter(agent %in% c(input$myagent)) %>% group_by(agent) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_visit<-done %>% filter(treatment %in% c(input$mytreat)) %>% group_by(agent) %>% summarise(total_visit=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_visit<-done %>% filter(hospital %in% c(input$myhos)) %>% group_by(agent) %>% summarise(total_visit=n())
          
        }
        
        ## total convert
        
        total_convert<-convert_data()[!duplicated(convert_data()[c(1,4)]),]
        
        if(input$appt_input=="date"){
          
          total_convert<-total_convert %>% group_by(agent) %>% summarise(convert=n())
          
        }else if(input$appt_input=="city"){
          
          total_convert<-total_convert %>% filter(city %in% c(input$mycity)) %>% group_by(agent) %>% summarise(convert=n())
          
        }else if(input$appt_input=="agent"){
          
          total_convert<-total_convert %>% filter(agent %in% c(input$myagent)) %>% group_by(agent) %>% summarise(convert=n())
          
        }else if(input$appt_input=="treatment"){
          
          total_convert<-total_convert %>% filter(treatment %in% c(input$mytreat)) %>% group_by(agent) %>% summarise(convert=n())
          
        }else if(input$appt_input=="hospital"){
          
          total_convert<-total_convert %>% filter(hospital %in% c(input$myhos)) %>% group_by(agent) %>% summarise(convert=n())
          
        }
        
        ## total package cost
        
        if(input$appt_input=="date"){
          
          total_package_cost<-convert_data() %>% group_by(agent) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="city"){
          
          total_package_cost<-convert_data() %>% filter(city %in% c(input$mycity)) %>% group_by(agent) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="hospital"){
          
          total_package_cost<-convert_data() %>% filter(hospital %in% c(input$myhos)) %>% group_by(agent) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="treatment"){
          
          total_package_cost<-convert_data() %>% filter(treatment %in% c(input$mytreat)) %>% group_by(agent) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }else if(input$appt_input=="agent"){
          
          total_package_cost<-convert_data() %>% filter(agent %in% c(input$myagent)) %>% group_by(agent) %>% summarise(package_cost=sum(package_cost,na.rm = T))
          
        }
        
        #### combine all data
        
        final_data<-merge.data.frame(total_visit,unique_visit,by="agent",all = T)
        final_data<-merge.data.frame(final_data,total_convert,by="agent",all = T)
        final_data<-merge.data.frame(final_data,total_package_cost,by="agent",all = T)
        
        final_data$new_pt_p<-round(((final_data$new_visit)/(final_data$total_visit))*100,digits = 2)
        
        final_data<-final_data[,c(1:3,6,4,5)] 
        
        final_data<-as.data.frame(final_data)
      
    })
    
    
    #### table sketch
    
    sketch_date<-reactive({
      
        sketch = htmltools::withTags(table(
          class = 'display',
          tableFooter(c("","","",0,0,0,0,0)),
          thead(
            tr(
              th(rowspan = 2,""),
              th(rowspan = 2, 'Date'),
              th(rowspan = 2, 'Day'),
              th(rowspan = 2, 'Total Visit(D)'),
              th(colspan = 2, 'New Patient Visit(V)'),
              th(rowspan = 2, 'Total Conversion'),
              th(rowspan = 2, 'Package Cost')
              
            ),
            tr(
              lapply(c('Nm', '%(V/D)'), th)
            )
          )
          
        ))
        
    })
    
    
    sketch_city<-reactive({
      
      sketch = htmltools::withTags(table(
        class = 'display',
        #tableFooter(c("","","",0,0,0,0,0)),
        thead(
          tr(
            #th(rowspan = 2,""),
            #th(rowspan = 2, 'Date'),
            th(rowspan = 2, 'City'),
            th(rowspan = 2, 'Total Visit(D)'),
            th(colspan = 2, 'New Patient Visit(V)'),
            th(rowspan = 2, 'Total Conversion'),
            th(rowspan = 2, 'Package Cost')
            
          ),
          tr(
            lapply(c('Nm', '%(V/D)'), th)
          )
        )
        
      ))
      
    })
    
    
    sketch_hospital<-reactive({
      
      sketch = htmltools::withTags(table(
        class = 'display',
        #tableFooter(c("","","",0,0,0,0,0)),
        thead(
          tr(
            #th(rowspan = 2,""),
            #th(rowspan = 2, 'Date'),
            th(rowspan = 2, 'Hospital'),
            th(rowspan = 2, 'Total Visit(D)'),
            th(colspan = 2, 'New Patient Visit(V)'),
            th(rowspan = 2, 'Total Conversion'),
            th(rowspan = 2, 'Package Cost')
            
          ),
          tr(
            lapply(c('Nm', '%(V/D)'), th)
          )
        )
        
      ))
      
    })
    
    sketch_treatment<-reactive({
      
      sketch = htmltools::withTags(table(
        class = 'display',
        #tableFooter(c("","","",0,0,0,0,0)),
        thead(
          tr(
            #th(rowspan = 2,""),
            #th(rowspan = 2, 'Date'),
            th(rowspan = 2, 'Treatment'),
            th(rowspan = 2, 'Total Visit(D)'),
            th(colspan = 2, 'New Patient Visit(V)'),
            th(rowspan = 2, 'Total Conversion'),
            th(rowspan = 2, 'Package Cost')
            
          ),
          tr(
            lapply(c('Nm', '%(V/D)'), th)
          )
        )
        
      ))
      
    })
    
    sketch_agent<-reactive({
      
      sketch = htmltools::withTags(table(
        class = 'display',
        #tableFooter(c("","","",0,0,0,0,0)),
        thead(
          tr(
            #th(rowspan = 2,""),
            #th(rowspan = 2, 'Date'),
            th(rowspan = 2, 'Agent'),
            th(rowspan = 2, 'Total Visit(D)'),
            th(colspan = 2, 'New Patient Visit(V)'),
            th(rowspan = 2, 'Total Conversion'),
            th(rowspan = 2, 'Package Cost')
            
          ),
          tr(
            lapply(c('Nm', '%(V/D)'), th)
          )
        )
        
      ))
      
    })
    
    
    
    #### ui output
    
    output$filter_box<-renderUI({
      
      if(input$appt_input=="date"){
        
        
        
      }else if(input$appt_input=="city"){
        
        tagList(
          div(class="test_type",selectInput("mycity","3. Please Select a City:",choices = sort(unique(appt_booked()$city)),selected = " ",multiple = T))
        )
        
      }else if(input$appt_input=="treatment"){
        
        tagList(
          div(class="test_type",selectInput("mytreat","3. Please Select a Treatment:",choices = sort(unique(appt_booked()$treatment)),selected = " ",multiple = T))
        )
        
      }else if(input$appt_input=="agent"){
        
        tagList(
          div(class="test_type",selectInput("myagent","3. Please Select a Agent:",choices = sort(unique(appt_booked()$agent)),selected = " ",multiple = T))
        )
        
      }else if(input$appt_input=="hospital"){
        
        tagList(
          div(class="test_type",selectInput("myhos","3. Please Select a Hospital:",choices = sort(unique(appt_booked()$hospital)),selected = " ",multiple = T))
        )
        
      }
      
    })
    
    
    ################# server output  #######################
    
    output$table1<-renderDataTable({
      
      
        datatable(appt_report_date_date(),container = sketch_date(),
                  style = "bootstrap",class = 'cell-border stripe',extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 scrollY = "400px",
                                 pageLength = 5000,
                                 #order = list(list(1, 'asc')),
                                 fixedHeader = TRUE,
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 footerCallback =  JS(
                                   "function( tfoot, data, start, end, display ) {",
                                   "var api = this.api();",
                                   
                                   
                                   "var3 = $( api.column( 3 ).footer() ).html(",
                                   "api.column( 3 ).data().reduce( function ( a, b ) {",
                                   "return a + b;",
                                   "} )",
                                   ");",
                                   "var4 = $( api.column( 4 ).footer() ).html(",
                                   "api.column( 4 ).data().reduce( function ( a, b ) {",
                                   "return a + b;",
                                   "} )",
                                   ");",
                                   
                                   "varsum_total_visit = api.column( 3 ).data().reduce( function ( a, b ) { return a + b;})",
                                   "varsum_unique_visit = api.column( 4 ).data().reduce( function ( a, b ) { return a + b;})",
                                   
                                   "varsum_unique_per = ((varsum_unique_visit/varsum_total_visit)*100).toFixed(2)",
                                   
                                   "var5 = $( api.column( 5 ).footer() ).html(varsum_unique_per)",
                                   
                                   "var6 = $( api.column( 6 ).footer() ).html(",
                                   "api.column( 6 ).data().reduce( function ( a, b ) {",
                                   "return a + b;",
                                   "} )",
                                   ");",
                                   
                                   "var7 = $( api.column( 7 ).footer() ).html(",
                                   "api.column( 7 ).data().reduce( function ( a, b ) {",
                                   "return a + b;",
                                   "} )",
                                   ");",
                                   
                                   "}"))
                  
        ) %>% formatStyle(columns = c(1,2),
                         backgroundColor = "pink") %>% formatStyle(columns = c(4,5),
                                                                   backgroundColor = "yellow")
        
      
      
    })
    
    
    output$table2<-renderDataTable({
      
      
        
      datatable(appt_report_date_city(),style = "bootstrap",class = 'cell-border stripe',extensions = 'Buttons',
                rownames = F,container = sketch_city(),
                options = list(dom = 'Bfrtip',
                               scrollX = TRUE,
                               scrollY = "450px",
                               pageLength = 30,
                               fixedHeader = TRUE,
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               order = list(list(1, 'desc'))
                )) %>% formatStyle(columns = c(1),
                                   backgroundColor = "pink") %>% formatStyle(columns = c(3,4),
                                                                             backgroundColor = "yellow")
        
      
    })
    
    
    output$table3<-renderDataTable({
      
      
        
      datatable(appt_report_date_hospital(),style = "bootstrap",class = 'cell-border stripe',extensions = 'Buttons',
                rownames = F,container = sketch_hospital(),
                options = list(dom = 'Bfrtip',
                               scrollX = TRUE,
                               scrollY = "450px",
                               pageLength = 30,
                               fixedHeader = TRUE,
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               order = list(list(1, 'desc'))
                )) %>% formatStyle(columns = c(1),
                                  backgroundColor = "pink") %>% formatStyle(columns = c(3,4),
                                                                            backgroundColor = "yellow")
        
        
      
      
    })
    
    output$table4<-renderDataTable({
      
      
      datatable(appt_report_date_treatment(),style = "bootstrap",class = 'cell-border stripe',extensions = 'Buttons',
                rownames = F,container = sketch_treatment(),
                options = list(dom = 'Bfrtip',
                               scrollX = TRUE,
                               scrollY = "450px",
                               pageLength = 30,
                               fixedHeader = TRUE,
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               order = list(list(1, 'desc'))
                )) %>% formatStyle(columns = c(1),
                                   backgroundColor = "pink") %>% formatStyle(columns = c(3,4),
                                                                             backgroundColor = "yellow")
        
      
      
    })
    
    output$table5<-renderDataTable({
      
      
      datatable(appt_report_date_agent(),style = "bootstrap",class = 'cell-border stripe',extensions = 'Buttons',
                rownames = F,container = sketch_agent(),
                options = list(dom = 'Bfrtip',
                               scrollX = TRUE,
                               scrollY = "450px",
                               pageLength = 30,
                               fixedHeader = TRUE,
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               order = list(list(1, 'desc'))
                )) %>% formatStyle(columns = c(1),
                                   backgroundColor = "pink") %>% formatStyle(columns = c(3,4),
                                                                             backgroundColor = "yellow")
        
      
      
    })
    
   
    
    
  }
)

