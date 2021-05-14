

library(shiny)
library(shinythemes)
library(knitr)
library(kableExtra)
library(highcharter)
library(XML)
library(sp)
#library(tidyverse)
library(dplyr)
library(tidyr)
library(forcats)
library(naniar)
library(readr)
library(shinyWidgets)

library(leaflet)
library(htmlwidgets)
library(htmltools)

load("./data/dmms.Rdata")
load("./data/countries.Rdata")
SAMPLING<-read_csv("./data/SAMPLING.csv")
load("./data/SHP.Rdata")




# functions ##### 
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

a <- function(x) format(x, big.mark = ",", scientific = FALSE)


##### shiny server ######
shinyServer(function(input, output,session) {
  options(shiny.sanitize.errors = FALSE)
  
  
  output$sampling <-renderText({
    SAMPLING<-SAMPLING[,c(2:6)]
    kable(SAMPLING)%>%
      kable_styling(
        font_size = 15,
        bootstrap_options = c("striped", "hover", "condensed"))
  })
  
  #### A1 A2 Piramids #####
  output$pyramidrel <- renderHighchart({
    
    piramids <- lapply(dmms, function(x){
      
      x$A1<- with(x,paste(A1,(year-A2),sep=""))
      #x <- x%>%
      # mutate(A1=paste(A1,(year-A2),sep=""))
      
      x<-x[,c("A1", input$mag,"year")]
      colnames(x)<-c("A1","pop","year")
      
      x$sex<-as.numeric(substr(x$A1,1,1))
      x$age<-as.numeric(substr(x$A1,2,3))
      
      x<-x %>%
        group_by(year,sex,age) %>% 
        summarise(pop=sum(pop))
      
      x$age_group<-with(x, ifelse(age<30, "20-30", 
                                  ifelse((age>=30 & age <40), "30-40",
                                         ifelse((age>=40 & age <50), "40-50",
                                                ifelse((age>=50 & age <60), "50-60",
                                                       ifelse(age>=60, "60-64",0))))))
      
      
      
      
      
      
      x<-x[,c("pop","year", "sex","age_group")]
      x[is.na(x)] <- -7
      
      
      colnames(x)<-c("n_nw","year","sex","age_group")
      
      x<-x %>%
        group_by(year,sex,age_group) %>% 
        filter(sex!=-7)%>%
        summarise(pop=sum(n_nw))
      
      
      x$sex<-as.factor(x$sex)
      x<-x %>%
        group_by(year,sex,age_group) %>% 
        summarise(pop=round(sum(pop),0))%>% 
        mutate(pop_rel=ifelse(year==2016,pop/sum(x[x$year==2016,"pop"]),
                              ifelse(year==2018,pop/sum(x[x$year==2018,"pop"]),    
                                     pop/sum(x[x$year==2020,"pop"]))))%>%
        mutate(pop2=ifelse(sex==1, pop*-1,pop),
               pop_rel2=ifelse(sex==1, pop_rel*-1,pop_rel),
               sex1=fct_recode(as.factor(sex), Male = "1", Female = "2"))
      
      
      x$pop_rel2<-round(x$pop_rel2*100,1)
      x
      
    })
    
    pir<- as.data.frame(do.call("rbind", piramids))
    
    pir<-pir[pir$year==input$year,]
    
    xaxis <- list(categories = as.factor(sort(unique(pir$age_group))),
                  reversed = FALSE, 
                  #tickInterval =5,
                  labels = list(step = 1))
    cols = rev(gg_color_hue(2))
    
    
    formatter<- ifelse(input$mag2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    
    hc_yAxis<-ifelse(input$mag=="n_nw", 2000,175000)
    
    highchart() %>%
      hc_chart(type = "bar",zoomType= 'xy') %>%
      hc_xAxis(xaxis) %>%
      hc_add_series(name = 'Males', 
                    data = c(pir[(pir$sex==1),ifelse(input$mag2=="Relative",7,6)]))%>%
      hc_add_series(name = 'Females', 
                    data = c(pir[(pir$sex==2),ifelse(input$mag2=="Relative",7,6)]))%>%
      hc_plotOptions(series = list(stacking = "normal",animation=FALSE),
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth =.25))%>%
      hc_colors(c(cols)) %>%
      hc_title(text =paste(input$year, "Population structure", 
                           "n:", a(sum(pir[,"pop"])),sep=" "))%>%
      hc_subtitle(text =paste(paste("Males: ",a(sum(abs(c(pir[(pir$sex==1),"pop2"])))),sep=""),
                              paste("Females: ",a(sum(abs(c(pir[(pir$sex==2),"pop2"])))),sep=""),sep=" - "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_yAxis(labels = list(formatter = JS(formatter)),min=ifelse(input$mag2=="Absolute",-hc_yAxis,-25),
               max=ifelse(input$mag2=="Absolute",hc_yAxis,25))%>%
      hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
      hc_tooltip(shared = FALSE,
                 formatter = JS("function () { return '<b>' + this.series.name + '<br>' +'Age: ' + this.point.category + '</b><br/>' + 'N: ' + Math.abs(this.point.y);}"))
  })   
  
  
  
  #### A3 RANKING TOP 20 countrie of birth ###### 
  
  output$barras1 <-renderHighchart({ 
    
    nac <- lapply(dmms, function(x){
      
      x<-x[,c(input$terms,input$mag,"year")]
      colnames(x)<-c("A3","pop","year")
      x<-as.data.frame(x)
      x = data.frame(x, countries[match(x[,"A3"],
                                        countries[,"A3"]),c("official_title_en")])
      colnames(x)[length(names(x))]<-paste("A3","B",sep="")
      x$A3B<-as.factor(x$A3B)
      x$A3B<-fct_explicit_na(x$A3B)
      
      colnames(x)<-c("A3","pop","year","A3B")
      
      x<-x %>%
        group_by(year,A3B) %>% 
        summarise(pop=round(sum(pop),0))%>%
        mutate(prop=round(pop/sum(pop)*100,1))%>%
        arrange(desc(pop))%>% 
        head(20)
      x
    })
    
    
    nacdf<- as.data.frame(do.call("rbind", nac))
    
    nacdf<-nacdf[nacdf$year==input$year,]
    
    data<- if(input$mag2=="Absolute"){ 
      nacdf[,c("year","A3B","pop")]}else{
        nacdf[,c("year","A3B", "prop")]}
    
    formatter<- ifelse(input$mag2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    hc_yAxis<-ifelse(input$mag=="n_nw", 700,175000)
    
    color<-ifelse(input$year==2016,1,
                  ifelse(input$year==2018,2,3))
    
    rank <- highchart() %>%
      hc_chart(type = 'bar',zoomType= 'xy') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = as.character(data$A3B), title = list(text = '')) %>%
      #hc_yAxis(title = list(text = 'N')) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(data = data[,3])%>%
      hc_title(text = paste("Top 20 countries of birth:",input$year,sep=" "),
               align = 'left')  %>%
      hc_subtitle(text =  paste("\nN",a(sum(c(nac[[
        
        if(input$year==2016){ 
          1}else{
            if(input$year==2018){   
              2}else{3}}
        
      ]]$pop))),sep=": "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_yAxis(labels = list(formatter = JS(formatter)),
               min=ifelse(input$mag2=="Absolute",0,0),
               max=ifelse(input$mag2=="Absolute",hc_yAxis,35))%>%
      hc_exporting(enabled = TRUE)%>%
      hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
      hc_tooltip(enabled = TRUE)%>%
      hc_colors(c(gg_color_hue(3)[color]))
    rank
  }) 
  
  
  #### A4 RANKING TOP 20 countrie of birth ###### 
  
  output$barras3 <-renderHighchart({ 
    
    nac <- lapply(dmms, function(x){
      
      x<-x[,c(input$terms,input$mag,"year")]
      colnames(x)<-c("A4_1","pop","year")
      x<-as.data.frame(x)
      x = data.frame(x, countries[match(x[,"A4_1"],
                                        countries[,"A3"]),c("official_title_en")])
      colnames(x)[length(names(x))]<-paste("A4_1","B",sep="")
      x$A4_1<-as.factor(x$A4_1)
      x$A4_1<-fct_explicit_na(x$A4_1)
      
      colnames(x)<-c("A3","pop","year","A3B")
      
      x<-x %>%
        group_by(year,A3B) %>% 
        summarise(pop=round(sum(pop),0))%>%
        mutate(prop=round(pop/sum(pop)*100,1))%>%
        arrange(desc(pop))%>% 
        head(20)
      x
    })
    
    
    nacdf<- as.data.frame(do.call("rbind", nac))
    
    nacdf<-nacdf[nacdf$year==input$year,]
    
    data<- if(input$mag2=="Absolute"){ 
      nacdf[,c("year","A3B","pop")]}else{
        nacdf[,c("year","A3B", "prop")]}
    
    formatter<- ifelse(input$mag2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    hc_yAxis<-ifelse(input$mag=="n_nw", 700,175000)
    color<-ifelse(input$year==2016,1,
                  ifelse(input$year==2018,2,3))
    rank <- highchart() %>%
      hc_chart(type = 'bar',zoomType= 'xy') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = as.character(data$A3B), title = list(text = '')) %>%
      #hc_yAxis(title = list(text = 'N')) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(data = data[,3])%>%
      hc_title(text = paste("Top 20 countries of nationality:",input$year,sep=" "),
               align = 'left')  %>%
      hc_subtitle(text =  paste("\nN",a(sum(c(nac[[
        
        if(input$year==2016){ 
          1}else{
            if(input$year==2018){   
              2}else{3}}
        
      ]]$pop))),sep=": "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_yAxis(labels = list(formatter = JS(formatter)),
               min=ifelse(input$mag2=="Absolute",0,0),
               max=ifelse(input$mag2=="Absolute",hc_yAxis,35))%>%
      hc_exporting(enabled = TRUE)%>%
      hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
      hc_tooltip(enabled = TRUE)%>%
      hc_colors(c(gg_color_hue(3)[color]))
    rank
  }) 
  
  
  
  #### A5 RANKING TOP 20 countrie of birth ###### 
  
  output$barras5 <-renderHighchart({ 
    dmms[[3]]<-NULL
    nac <- lapply(dmms, function(x){
      
      x<-x[,c(input$terms,input$mag,"year")]
      colnames(x)<-c("A5_1","pop","year")
      x<-as.data.frame(x)
      x = data.frame(x, countries[match(x[,"A5_1"],
                                        countries[,"A3"]),c("official_title_en")])
      colnames(x)[length(names(x))]<-paste("A5_1","B",sep="")
      x$A5_1B<-as.factor(x$A5_1B)
      x$A5_1B<-fct_explicit_na(x$A5_1B)
      
      colnames(x)<-c("A3","pop","year","A3B")
      
      x<-x %>%
        group_by(year,A3B) %>% 
        summarise(pop=round(sum(pop),0))%>%
        mutate(prop=round(pop/sum(pop)*100,1))%>%
        arrange(desc(pop))%>% 
        head(20)
      x
    })
    
    
    nacdf<- as.data.frame(do.call("rbind", nac))
    
    nacdf<-nacdf[nacdf$year==input$year1,]
    
    data<- if(input$mag2=="Absolute"){ 
      nacdf[,c("year","A3B","pop")]}else{
        nacdf[,c("year","A3B", "prop")]}
    
    formatter<- ifelse(input$mag2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    hc_yAxis<-ifelse(input$mag=="n_nw", 700,175000)
    color<-ifelse(input$year1==2016,1,
                  ifelse(input$year1==2018,2,3))
    rank <- highchart() %>%
      hc_chart(type = 'bar',zoomType= 'xy') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = as.character(data$A3B), title = list(text = '')) %>%
      #hc_yAxis(title = list(text = 'N')) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(data = data[,3])%>%
      hc_title(text = paste("Top 20 countries of nationality:",input$year1,sep=" "),
               align = 'left')  %>%
      hc_subtitle(text =  paste("\nN",a(sum(c(nac[[
        
        if(input$year1==2016){ 
          1}else{
            if(input$year1==2018){   
              2}else{3}}
        
      ]]$pop))),sep=": "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_yAxis(labels = list(formatter = JS(formatter)),
               min=ifelse(input$mag2=="Absolute",0,0),
               max=ifelse(input$mag2=="Absolute",hc_yAxis,35))%>%
      hc_exporting(enabled = TRUE)%>%
      hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
      hc_tooltip(enabled = TRUE)%>%
      hc_colors(c(gg_color_hue(3)[color]))
    rank
  })
  
  
  #### B1 RANKING TOP 20 countrie of ORRIGIN ###### 
  
  output$B1_16 <-renderHighchart({ 
    
    nac <- lapply(dmms, function(x){
      
      x<-x[,c(input$BQ,input$mag1,"year")]
      colnames(x)<-c("B1","pop","year")
      x<-as.data.frame(x)
      x = data.frame(x, countries[match(x[,"B1"],
                                        countries[,"A3"]),c("official_title_en")])
      colnames(x)[length(names(x))]<-paste("B1","B",sep="")
      x$B1B<-as.factor(x$B1B)
      x$B1B<-fct_explicit_na(x$B1B)
      
      colnames(x)<-c("A3","pop","year","A3B")
      
      x<-x %>%
        group_by(year,A3B) %>% 
        summarise(pop=round(sum(pop),0))%>%
        mutate(prop=round(pop/sum(pop)*100,1))%>%
        arrange(desc(pop))%>% 
        head(20)
      x
    })
    
    
    nacdf<- as.data.frame(do.call("rbind", nac))
    
    nacdf<-nacdf[nacdf$year==input$year3,]
    
    data<- if(input$mag3=="Absolute"){ 
      nacdf[,c("year","A3B","pop")]}else{
        nacdf[,c("year","A3B", "prop")]}
    
    formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    hc_yAxis<-ifelse(input$mag1=="n_nw", 700,175000)
    color<-ifelse(input$year3==2016,1,
                  ifelse(input$year3==2018,2,3))
    rank <- highchart() %>%
      hc_chart(type = 'bar',zoomType= 'xy') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = as.character(data$A3B), title = list(text = '')) %>%
      hc_subtitle(text =  paste("\nN",a(sum(c(nac[[
        
        if(input$year3==2016){ 
          1}else{
            if(input$year3==2018){   
              2}else{3}}
        
      ]]$pop))),sep=": "))%>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(data = data[,3])%>%
      hc_title(text = paste("Top 20 countries of origin:",input$year3,sep=" "),
               align = 'left')  %>%
      # hc_subtitle(text =  paste("\nN",a(sum(c(data$pop))),sep=": "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_yAxis(labels = list(formatter = JS(formatter)),
               min=ifelse(input$mag3=="Absolute",0,0),
               max=ifelse(input$mag3=="Absolute",hc_yAxis,35))%>%
      hc_exporting(enabled = TRUE)%>%
      hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
      hc_tooltip(enabled = TRUE)%>%
      hc_colors(c(gg_color_hue(3)[color]))
    rank
  })
  
  
  #### year of arrival #####   
  output$barras7 <-renderHighchart({ 
    
    tem <- lapply(dmms, function(x){
      
      x<-x[,c(input$terms,input$mag,"year")]
      
      colnames(x)<-c("A6","pop","year")
      
      x<-x %>%
        group_by(year,A6) %>% 
        replace_with_na(replace = list(A6 = -9))%>% 
        summarise(pop=round(sum(pop),0))%>%
        mutate(prop=round(pop/sum(pop)*100,1))%>%
        arrange(desc(A6))
      
      x$A6<-as.factor(x$A6)
      x$A6<-fct_explicit_na(x$A6)
      #levels(doble_presence$A6)
      
      #levels(x$A6)<-c(c(2016:2006),"Before 2006",13, "(Missing)")
      x
    })
    
    levels(tem[["D16"]]$A6)<-c(c(2016:2006))
    levels(tem[["D18"]]$A6)<-c(c(2018:2006),"missing")
    levels(tem[["D20"]]$A6)<-c(c(2020:2006))
    
    
    temDF<- as.data.frame(do.call("rbind", tem))
    temDF$A6<-as.factor(temDF$A6)
    
    temDF$A6<-factor(temDF$A6, levels=c( "2006","2007","2008","2009","2010","2011","2012",
                                         "2013","2014","2015","2016","2017","2018","2019","2020"))
    
    #temDF<-temDF[complete.cases(temDF), ]
    
    
    data<- if(input$mag2=="Absolute"){ 
      temDF[,c("year","A6", "pop")]}else{
        temDF[,c("year","A6", "prop")]}
    
    hc_yAxis<-ifelse(input$mag=="n_nw", 1000,85000)
    
    formatter<- ifelse(input$mag2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    
    rank <- highchart() %>% 
      hc_chart(type = "column",zoomType= 'xy') %>%
      hc_xAxis(categories = levels(data$A6), title = list(text = '')) %>%
      hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
      hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
      hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
      # hc_chart(type = 'column') %>%
      hc_legend(enabled =TRUE ) %>%
      
      hc_yAxis(labels = list(formatter = JS(formatter)),
               min=ifelse(input$mag2=="Absolute",0,0),
               max=ifelse(input$mag2=="Absolute",hc_yAxis,15))%>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      #hc_add_series(name = "Value", data = data) %>%
      hc_title(text = "Year of emigration to Switzerland",align = 'left')  %>%
      hc_subtitle(text =paste(
        paste("\n2016 N",a(sum(c(tem[[1]]$pop))),sep=": "),
        paste("\n2018 N",a(sum(c(tem[[2]]$pop))),sep=": "),
        paste("\n2020 N",a(sum(c(tem[[3]]$pop))),sep=": "),
        sep=" | "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_exporting(enabled = TRUE)%>%
      hc_tooltip(enabled = TRUE) %>%
      hc_colors(c(gg_color_hue(3)))%>%
      hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
    
    rank
    
  })
  
  #### A7 RESIDENCE PERMIT   ######     
  
  output$A7_16 <-renderHighchart({ 
    
    mar <- lapply(dmms, function(x){
      
      x<-x[,c("A7",input$mag,"year")]
      
      colnames(x)<-c("A7", "pop","year")
      x[is.na(x)] <- -7
      
      x<-x %>%
        group_by(year,A7) %>% 
        filter(A7!=-7)%>% 
        filter(A7!=-9)%>% 
        #replace_with_na(replace = list(A6 = -9))%>% 
        summarise(pop=round(sum(pop),0))%>%
        mutate(prop=round(pop/sum(pop)*100,1))%>%
        arrange((A7))
      
      x$A7<-as.factor(x$A7)
      x$A7<-fct_explicit_na(x$A7)
      
      x
    })
    
    levels(mar[["D16"]]$A7)<-c("Settlement permit (C permit)",
                               "Residence permit (B permit)",
                               "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                               "Short-term residence permit (L permit)")
    levels(mar[["D18"]]$A7)<-c("Settlement permit (C permit)",
                               "Residence permit (B permit)",
                               "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                               "Short-term residence permit (L permit)",
                               "Other permit")
    levels(mar[["D20"]]$A7)<-c("Settlement permit (C permit)",
                               "Residence permit (B permit)",
                               "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                               "Short-term residence permit (L permit)",
                               "Other permit",
                               "I have the Swiss citizenship")
    
    
    marDF<- as.data.frame(do.call("rbind", mar))
    marDF$A7<-as.factor(marDF$A7)
    levels(marDF$A7)
    
    data<- if(input$mag2=="Absolute"){ 
      marDF[,c("year","A7", "pop")]}else{
        marDF[,c("year","A7", "prop")]}
    
    hc_yAxis<-ifelse(input$mag=="n_nw", 6000,475000)
    
    formatter<- ifelse(input$mag2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    rank <- highchart() %>% 
      #hc_chart(type = "bar") %>%
      hc_xAxis(categories = levels(data$A7), title = list(text = '')) %>%
      hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
      hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
      hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
      hc_chart(type = 'bar',zoomType= 'xy') %>%
      hc_legend(enabled =TRUE ) %>%
      hc_yAxis(labels = list(formatter = JS(formatter)),
               min=ifelse(input$mag2=="Absolute",0,0),
               max=ifelse(input$mag2=="Absolute",hc_yAxis,70))%>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      # hc_add_series(name = "Value", data = data) %>%
      hc_title(text = "Residence Permit",
               align = 'left')  %>%
      hc_subtitle(text =paste(
        paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
        paste("\n2018 N",a(sum(c(mar[[2]]$pop))),sep=": "),
        paste("\n2020 N",a(sum(c(mar[[3]]$pop))),sep=": "),
        sep=" | "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      
      hc_exporting(enabled = TRUE)%>%
      hc_tooltip(enabled = TRUE)%>%
      hc_colors(c(gg_color_hue(3)))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
    
    rank
    
    
  }) 
  
  #### A8 Marital status ##### 
  
  output$barras9 <-renderHighchart({ 
    
    mar <- lapply(dmms, function(x){
      
      x<-x[,c(input$terms,input$mag,"year")]
      #x<-x[,c("A8","weight","year")]
      colnames(x)<-c("A8", "pop","year")
      x<-x %>%
        group_by(year,A8) %>% 
        #replace_with_na(replace = list(A6 = -9))%>% 
        summarise(pop=round(sum(pop),0))%>%
        mutate(prop=round(pop/sum(pop)*100,1))%>%
        arrange((A8))
      
      x$A8<-as.factor(x$A8)
      x$A8<-fct_explicit_na(x$A8)
      
      x
    })
    
    levels(mar[["D16"]]$A8)<-c("Single, never married",
                               "Married",
                               "Separated",
                               "Divorced",
                               "Widowed",
                               "Registered partnership",
                               "Dissolved partnership")
    
    levels(mar[["D18"]]$A8)<-c("Single, never married",
                               "Married",
                               "Separated",
                               "Divorced",
                               "Widowed",
                               "Registered partnership",
                               "Dissolved partnership")
    
    levels(mar[["D20"]]$A8)<-c("Single, never married",
                               "Married",
                               "Separated",
                               "Divorced",
                               "Widowed",
                               "Registered partnership",
                               "Dissolved partnership")
    
    marDF<- as.data.frame(do.call("rbind", mar))
    marDF$A8<-as.factor(marDF$A8)
    
    data<- if(input$mag2=="Absolute"){ 
      marDF[,c("year","A8", "pop")]}else{
        marDF[,c("year","A8", "prop")]}
    
    hc_yAxis<-ifelse(input$mag=="n_nw", 6000,475000)
    
    formatter<- ifelse(input$mag2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    rank <- highchart() %>% 
      #hc_chart(type = "bar") %>%
      hc_xAxis(categories = levels(marDF$A8), title = list(text = '')) %>%
      hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
      hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
      hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
      hc_chart(type = 'bar',zoomType= 'xy') %>%
      hc_legend(enabled =TRUE ) %>%
      hc_yAxis(labels = list(formatter = JS(formatter)),
               min=ifelse(input$mag2=="Absolute",0,0),
               max=ifelse(input$mag2=="Absolute",hc_yAxis,70))%>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      #hc_add_series(name = "Value", data = data) %>%
      hc_title(text = "Marital status",
               align = 'left')  %>%
      hc_subtitle(text =paste(
        paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
        paste("\n2018 N",a(sum(c(mar[[2]]$pop))),sep=": "),
        paste("\n2020 N",a(sum(c(mar[[3]]$pop))),sep=": "),
        sep=" | "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      
      hc_exporting(enabled = TRUE)%>%
      hc_tooltip(enabled = TRUE)%>%
      hc_colors(c(gg_color_hue(3)))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
    
    rank
    
    
  }) 
  
  #### A9 Marital status 2 ##### 
  
  
  output$ComposicionPlotly20 <-renderHighchart({ 
    mar3 <- lapply(dmms, function(x){
      
      x<-x[,c("A8",input$terms,input$mag,"year")]
      colnames(x)<-c("A8", "A9", "pop", "year")
      x[is.na(x)] <- -7
      x$A8<-as.factor(x$A8)
      levels(x$A8)<-c("Single, never married",
                      "Married",
                      "Separated",
                      "Divorced",
                      "Widowed",
                      "Registered partnership",
                      "Dissolved partnership")
      
      x<-x%>%
        group_by(year)%>%
        filter(A9 !=-7)
      
      
      x<-x %>%
        filter(A8 %in% c("Single, never married",
                         "Separated",
                         "Divorced",
                         "Widowed",
                         "Dissolved partnership"))%>%
        group_by(year,A8,A9) %>% 
        summarise(pop=round(sum(pop),0))%>%
        mutate(prop=round(pop/sum(pop)*100,1))
      x
    })
    
    marDF<- as.data.frame(do.call("rbind", mar3))
    marDF<-marDF[marDF$A9==1,]
    marDF$A8<-as.factor(marDF$A8)
    
    data<- if(input$mag2=="Absolute"){ 
      marDF[,c("year","A8", "pop")]}else{
        marDF[,c("year","A8", "prop")]}
    
    
    hc_yAxis<-ifelse(input$mag=="n_nw", 1500,150000)
    
    formatter<- ifelse(input$mag2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    #levels(marDF$A8)
    
    hc <-  highchart() %>% 
      #hc_chart(type = "bar") %>%
      hc_xAxis(categories = (data$A8), title = list(text = '')) %>%
      hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
      hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
      hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
      hc_chart(type = 'bar',zoomType= 'xy') %>%
      hc_legend(enabled =TRUE ) %>%
      
      hc_yAxis(labels = list(formatter = JS(formatter)),
               min=ifelse(input$mag2=="Absolute",0,0),
               max=ifelse(input$mag2=="Absolute",hc_yAxis,100))%>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      #hc_add_series(name = "Value", data = data) %>%
      hc_title(text = "Do you have a partner?",align = 'left')  %>%
      hc_subtitle(text =paste(
        paste("\n2016 N",a(sum(c(mar3[[1]]$pop))),sep=": "),
        paste("\n2018 N",a(sum(c(mar3[[2]]$pop))),sep=": "),
        paste("\n2020 N",a(sum(c(mar3[[3]]$pop))),sep=": "),
        sep=" | "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_exporting(enabled = TRUE)%>%
      hc_tooltip(enabled = TRUE) %>%
      hc_colors(c(gg_color_hue(3)))%>%  
      hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
    hc
    
    
  }) 
  
  
  
  
  #### A11 STILLS LIVE IN SWITZERLAND ######
  
  output$A11_18 <-renderHighchart({ 
    dmms[[1]] <- NULL
    mar <- lapply(dmms, function(x){
      
      x<-x[,c(input$terms,input$mag,"year")]
      
      colnames(x)<-c("A11", "pop","year")
      x<-x %>%
        group_by(year,A11) %>% 
        filter(A11!=-9)%>% 
        filter(A11!=-7)%>% 
        #replace_with_na(replace = list(A6 = -9))%>% 
        summarise(pop=round(sum(pop),0))%>%
        mutate(prop=round(pop/sum(pop)*100,1))%>%
        arrange((A11))
      
      x$A11<-as.factor(x$A11)
      x$A11<-fct_explicit_na(x$A11)
      
      x
    })
    
    levels(mar[["D18"]]$A11)<-c("Yes, I live in Switzerland",
                                "I live partly in Switzerland, partly abroad")
    
    levels(mar[["D20"]]$A11)<-c("Yes, I live in Switzerland",
                                "I live partly in Switzerland, partly abroad")
    
    marDF<- as.data.frame(do.call("rbind", mar))
    marDF$A11<-as.factor(marDF$A11)
    levels(marDF$A11)
    
    
    data<- if(input$mag2=="Absolute"){ 
      marDF[,c("year","A11", "pop")]}else{
        marDF[,c("year","A11", "prop")]}
    
    hc_yAxis<-ifelse(input$mag=="n_nw", 3500,350000)
    
    formatter<- ifelse(input$mag2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                       "function(){ return Math.abs(this.value); }")
    
    rank <- highchart() %>%
      #hc_chart(type = "bar") %>%
      hc_xAxis(categories = levels(data$A11), title = list(text = '')) %>%
      #hc_add_series(name= "2016",data = marDF[marDF$year==2016,3])%>%
      hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
      hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
      hc_chart(type = 'bar',zoomType= 'xy') %>%
      hc_legend(enabled =TRUE ) %>%
      hc_yAxis(labels = list(formatter = JS(formatter)),
               min=ifelse(input$mag2=="Absolute",0,0),
               max=ifelse(input$mag2=="Absolute",hc_yAxis,100))%>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      #hc_add_series(name = "Value", data = data) %>%
      hc_title(text = "Do you still live in Switzerland?",
               align = 'left')  %>%
      hc_subtitle(text =paste(
        # paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
        paste("\n2018 N",a(sum(c(mar[[1]]$pop))),sep=": "),
        paste("\n2020 N",a(sum(c(mar[[2]]$pop))),sep=": "),
        sep=" | "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
      hc_exporting(enabled = TRUE)%>%
      hc_tooltip(enabled = TRUE)%>%
      hc_colors(c(gg_color_hue(3)[2:3]))
    
    rank
    
    
  })  
  
  