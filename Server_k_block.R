#####K1  During the confinement period beginning in March 2020, where did you spend most of your time (working and free time)? #####

output$K1_20 <-renderHighchart({ 
  
  dmms1820 <-  dmms[3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BK,input$magK,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("K1", "pop","year")
    
    
    x<-x %>%
      group_by(year,K1) %>% 
      filter(K1!="(Missing)")%>%
      filter(K1!=-7)%>%
      filter(K1!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((K1))
    
    x$K1<-as.factor(x$K1)
    x$K1<-fct_explicit_na(x$K1)
    
    x
  })
  
  
  
  levels(mar[["D20"]]$K1)<-c("In my usual place of residence in Switzerland",
                             "In another place in Switzerland",
                             "In my country of origin",
                             "In another country")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$K1<-as.factor(marDF$K1)
  
  data<- if(input$magK2=="Absolute"){ 
    marDF[,c("year","K1", "pop")]}else{
      marDF[,c("year","K1", "prop")]}
  
  hc_yAxis<-ifelse(input$magK=="n_nw",  8000,800000)
  
  
  formatter<- ifelse(input$magK2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$K1), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magK2=="Absolute",0,0),
             max=ifelse(input$magK2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "During the confinement period beginning in March 2020, where did you spend most of your time (working and free time)?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})


#####K2  Which statement best describes how you felt at that moment? #####

output$K2_20 <-renderHighchart({ 
  
  dmms1820 <-  dmms[3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BK,input$magK,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("K2", "pop","year")
    
    
    x<-x %>%
      group_by(year,K2) %>% 
      filter(K2!="(Missing)")%>%
      filter(K2!=-7)%>%
      filter(K2!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((K2))
    
    x$K2<-as.factor(x$K2)
    x$K2<-fct_explicit_na(x$K2)
    
    x
  })
  
  
  
  levels(mar[["D20"]]$K2)<-c("My life is in Switzerland, so I felt I was in the right place",
                             "Being in Switzerland or in my home country was the same for me",
                             "I would have preferred to be in my home country")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$K2<-as.factor(marDF$K2)
  
  data<- if(input$magK2=="Absolute"){ 
    marDF[,c("year","K2", "pop")]}else{
      marDF[,c("year","K2", "prop")]}
  
  hc_yAxis<-ifelse(input$magK=="n_nw",  8000,800000)
  
  
  formatter<- ifelse(input$magK2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$K2), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magK2=="Absolute",0,0),
             max=ifelse(input$magK2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Which statement best describes how you felt at that moment?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#####K3  WWhich impact did the partial lockdown in spring have on your professional situation in Switzerland? #####

output$K3_20 <-renderHighchart({ 
  
  dmms1820 <-  dmms[3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BK,input$magK,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("K3", "pop","year")
    
    
    x<-x %>%
      group_by(year,K3) %>% 
      filter(K3!="(Missing)")%>%
      filter(K3!=-7)%>%
      filter(K3!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((K3))
    
    x$K3<-as.factor(x$K3)
    x$K3<-fct_explicit_na(x$K3)
    
    x
  })
  
  
  
  levels(mar[["D20"]]$K3)<-c("It had (almost) no impact",
                             "I kept my job, but I had to telework",
                             "I had to temporarily reduce my work time",
                             "I was temporarily put out of work",
                             "I lost my job, as an employee")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$K3<-as.factor(marDF$K3)
  
  data<- if(input$magK2=="Absolute"){ 
    marDF[,c("year","K3", "pop")]}else{
      marDF[,c("year","K3", "prop")]}
  
  hc_yAxis<-ifelse(input$magK=="n_nw",  4000,400000)
  
  
  formatter<- ifelse(input$magK2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$K3), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magK2=="Absolute",0,0),
             max=ifelse(input$magK2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Which impact did the partial lockdown in spring have on your professional situation in Switzerland?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#####K4  WWhich impact did the partial lockdown have on your professional situation in Switzerland?#####

output$K4_20 <-renderHighchart({ 
  
  dmms1820 <-  dmms[3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BK,input$magK,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("K4", "pop","year")
    
    
    x<-x %>%
      group_by(year,K4) %>% 
      filter(K4!="(Missing)")%>%
      filter(K4!=-7)%>%
      filter(K4!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((K4))
    
    x$K4<-as.factor(x$K4)
    x$K4<-fct_explicit_na(x$K4)
    
    x
  })
  
  
  
  levels(mar[["D20"]]$K4)<-c("I could benefit economically from the situation",
                             "It had almost no impact",
                             "I kept most of my activities, but I had to organize my work differently",
                             "I kept only a part of my activities, and suffered from the situation",
                             "I was no longer able to work")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$K4<-as.factor(marDF$K4)
  
  data<- if(input$magK2=="Absolute"){ 
    marDF[,c("year","K4", "pop")]}else{
      marDF[,c("year","K4", "prop")]}
  
  hc_yAxis<-ifelse(input$magK=="n_nw",  500,25000)
  
  
  formatter<- ifelse(input$magK2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$K4), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magK2=="Absolute",0,0),
             max=ifelse(input$magK2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #   paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Which impact did the partial lockdown have on your professional situation in Switzerland?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#####K5  Which statement best fits how you felt about your residence status during that period?#####

output$K5_20 <-renderHighchart({ 
  
  dmms1820 <-  dmms[3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BK,input$magK,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("K5", "pop","year")
    
    
    x<-x %>%
      group_by(year,K5) %>% 
      filter(K5!="(Missing)")%>%
      filter(K5!=-7)%>%
      filter(K5!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((K5))
    
    x$K5<-as.factor(x$K5)
    x$K5<-fct_explicit_na(x$K5)
    
    x
  })
  
  
  
  levels(mar[["D20"]]$K5)<-c("I did not fear losing my residence permit",
                             "I was concerned about the renewal of my permit",
                             "I lost my residence permit")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$K5<-as.factor(marDF$K5)
  
  data<- if(input$magK2=="Absolute"){ 
    marDF[,c("year","K5", "pop")]}else{
      marDF[,c("year","K5", "prop")]}
  
  hc_yAxis<-ifelse(input$magK=="n_nw",  7000,700000)
  
  
  formatter<- ifelse(input$magK2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$K5), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magK2=="Absolute",0,0),
             max=ifelse(input$magK2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #    paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #   paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Which statement best fits how you felt about your residence status during that period?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#####K6  Did you experience discriminatory or unfair treatment based on your nationality in relation to the Covid-19 outbreak?#####

output$K6_20 <-renderHighchart({ 
  
  dmms1820 <-  dmms[3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BK,input$magK,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("K6", "pop","year")
    
    
    x<-x %>%
      group_by(year,K6) %>% 
      filter(K6!="(Missing)")%>%
      filter(K6!=-7)%>%
      filter(K6!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((K6))
    
    x$K6<-as.factor(x$K6)
    x$K6<-fct_explicit_na(x$K6)
    
    x
  })
  
  
  
  levels(mar[["D20"]]$K6)<-c("Yes, I did experience such kind of treatment",
                             "No, I do not think so")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$K6<-as.factor(marDF$K6)
  
  data<- if(input$magK2=="Absolute"){ 
    marDF[,c("year","K6", "pop")]}else{
      marDF[,c("year","K6", "prop")]}
  
  hc_yAxis<-ifelse(input$magK=="n_nw",  7500,750000)
  
  
  formatter<- ifelse(input$magK2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$K6), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magK2=="Absolute",0,0),
             max=ifelse(input$magK2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #   paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Did you experience discriminatory or unfair treatment based on your nationality in relation to the Covid-19 outbreak?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#####K7  Did you experience support or empathy from the population living in Switzerland because of how your country of origin was affected by the Covid-19 outbreak ?#####

output$K7_20 <-renderHighchart({ 
  
  dmms1820 <-  dmms[3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BK,input$magK,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("K7", "pop","year")
    
    
    x<-x %>%
      group_by(year,K7) %>% 
      filter(K7!="(Missing)")%>%
      filter(K7!=-7)%>%
      filter(K7!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((K7))
    
    x$K7<-as.factor(x$K7)
    x$K7<-fct_explicit_na(x$K7)
    
    x
  })
  
  
  
  levels(mar[["D20"]]$K7)<-c("Yes, I did experience marks of support or empathy",
                             "No, I do not think so")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$K7<-as.factor(marDF$K7)
  
  data<- if(input$magK2=="Absolute"){ 
    marDF[,c("year","K7", "pop")]}else{
      marDF[,c("year","K7", "prop")]}
  
  hc_yAxis<-ifelse(input$magK=="n_nw",  7500,750000)
  
  
  formatter<- ifelse(input$magK2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$K7), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magK2=="Absolute",0,0),
             max=ifelse(input$magK2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Did you experience support or empathy from the population living in Switzerland because of how your country of origin was affected by the Covid-19 outbreak?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#####K8 Did the Covid-19 outbreak affect your plans regarding your stay in Switzerland ?#####

output$K8_20 <-renderHighchart({ 
  
  dmms1820 <-  dmms[3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BK,input$magK,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("K8", "pop","year")
    
    
    x<-x %>%
      group_by(year,K8) %>% 
      filter(K8!="(Missing)")%>%
      filter(K8!=-7)%>%
      filter(K8!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((K8))
    
    x$K8<-as.factor(x$K8)
    x$K8<-fct_explicit_na(x$K8)
    
    x
  })
  
  
  
  levels(mar[["D20"]]$K8)<-c("Yes, I decided/had to anticipate my departure",
                             "Yes, I decided/had to postpone my departure",
                             "No")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$K8<-as.factor(marDF$K8)
  
  data<- if(input$magK2=="Absolute"){ 
    marDF[,c("year","K8", "pop")]}else{
      marDF[,c("year","K8", "prop")]}
  
  hc_yAxis<-ifelse(input$magK=="n_nw",  7500,750000)
  
  
  formatter<- ifelse(input$magK2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$K8), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magK2=="Absolute",0,0),
             max=ifelse(input$magK2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #    paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #   paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Did the Covid-19 outbreak affect your plans regarding your stay in Switzerland?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#####K9 Did the Covid-19 outbreak affect your plans regarding naturalization?#####

output$K9_20 <-renderHighchart({ 
  
  dmms1820 <-  dmms[3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BK,input$magK,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("K9", "pop","year")
    
    
    x<-x %>%
      group_by(year,K9) %>% 
      filter(K9!="(Missing)")%>%
      filter(K9!=-7)%>%
      filter(K9!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((K9))
    
    x$K9<-as.factor(x$K9)
    x$K9<-fct_explicit_na(x$K9)
    
    x
  })
  
  
  
  levels(mar[["D20"]]$K9)<-c("Yes, I realized that naturalization is a way to stabilize my stay in Switzerland",
                             "Yes, I realized that I no longer want to naturalize in Switzerland.",
                             "Yes, for other reasons",
                             "No")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$K9<-as.factor(marDF$K9)
  
  data<- if(input$magK2=="Absolute"){ 
    marDF[,c("year","K9", "pop")]}else{
      marDF[,c("year","K9", "prop")]}
  
  hc_yAxis<-ifelse(input$magK=="n_nw",  7500,750000)
  
  
  formatter<- ifelse(input$magK2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$K9), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magK2=="Absolute",0,0),
             max=ifelse(input$magK2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #   paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Did the Covid-19 outbreak affect your plans regarding naturalization?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})




##### crosstable ######

output$cross <-renderText({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$A, input$B,input$C,"year")] 
    
    colnames(x)<-c("A","B","pop","year")
    x[is.na(x)] <- -7
    
    x
  })
  
  ##### 16 A ####     
  df<-mar[["D16"]]%>%
    group_by(A, B)%>%
    filter(A!=-9)%>% 
    filter(A!=-8)%>% 
    filter(A!=99)%>% 
    filter(A!=-7)%>% 
    filter(B!=-9)%>% 
    filter(A!=-8)%>% 
    filter(B!=99)%>% 
    filter(B!=-7)%>% 
    summarise(n=round(sum(pop),0))%>%
    mutate(prop=round((n/sum(n)*100),2))
  
  
  df[[1]]<-as.factor(df[[1]])
  levels(df[[1]])<- if(input$A=="sex1"){c("Male", "Female")}else
  {if(input$A=="A8"){c("Single, never married",
                       "Married",
                       "Separated",
                       "Divorced",
                       "Widowed",
                       "Registered partnership",
                       "Dissolved partnership")}else
                       {if((input$A=="A9"|
                            input$A=="B8_1"|
                            input$A=="B8_2"|
                            input$A=="B8_3"|
                            input$A=="B8_4"|
                            input$A=="B8_5"|
                            input$A=="B8_6"|
                            input$A=="B8_7"|
                            input$A=="B8_8"|
                            input$A=="B8_9"|
                            input$A=="B8_10"|
                            input$A=="B13_1"|
                            input$A=="B13_2"|
                            input$A=="B13_3"|
                            input$A=="B13_4"|
                            input$A=="B13_5"|
                            input$A=="B13_6"|
                            input$A=="B8_10"|
                            input$A=="C2_1"|
                            input$A=="C2_2"|
                            input$A=="C2_3"|
                            input$A=="C2_4"|
                            input$A=="C2_5"|
                            input$A=="C2_6"|
                            input$A=="C2_7"|
                            input$A=="C3_1"|
                            input$A=="C3_2"|
                            input$A=="C3_3"|
                            input$A=="C3_4"|
                            input$A=="C3_5"|
                            input$A=="C3_6"|
                            input$A=="C3_7"|
                            input$A=="C3_8"|
                            input$A=="E1_1"|
                            input$A=="E1_2"|
                            input$A=="E1_3"|
                            input$A=="E1_4"|
                            input$A=="E1_5"|
                            input$A=="E1_6"|
                            input$A=="E1_7"|
                            input$A=="E1_8"|
                            input$A=="E1_9"|
                            input$A=="E11_1"|
                            input$A=="E11_2"|
                            input$A=="E11_3"|
                            input$A=="E11_4"|
                            input$A=="E11_5"|
                            input$A=="E11_6"|
                            input$A=="E11_7"|
                            input$A=="E11_8"|
                            input$A=="E11_9"|
                            input$A=="E16_1"|
                            input$A=="E16_2"|
                            input$A=="E16_3"|
                            input$A=="E16_4"|
                            input$A=="E16_5"|
                            input$A=="E16_6"|
                            input$A=="E16_7"|
                            input$A=="E16_8"|
                            input$A=="E16_9"|
                            input$A=="F7_1"|
                            input$A=="F7_2"|
                            input$A=="F7_3"|
                            input$A=="F7_4"|
                            input$A=="F7_5"|
                            input$A=="F7_6"|
                            input$A=="F7_7"|
                            input$A=="F7_8"|
                            input$A=="F7_9"|
                            input$A=="H8_1"|
                            input$A=="H8_2"|
                            input$A=="H8_3"|
                            input$A=="H8_4"|
                            input$A=="H8_5"|
                            input$A=="H8_6"|
                            input$A=="H8_7"|
                            input$A=="H8_8"|
                            input$A=="H9_1"|
                            input$A=="H9_2"|
                            input$A=="H9_3"|
                            input$A=="H9_4"|
                            input$A=="H9_6")){c("Yes","No")}else
                            {if(input$A=="age_group"){c("20-30",
                                                        "30-40",
                                                        "40_50",
                                                        "50-60",
                                                        "60-64")}else
                                                        {if(input$A=="A6"){c("2006",
                                                                             "2007",
                                                                             "2008",
                                                                             "2009",
                                                                             "2010",
                                                                             "2011",
                                                                             "2012",
                                                                             "2013",
                                                                             "2014",
                                                                             "2015",
                                                                             "2016")}else
                                                                             {if(input$A=="B2CAT"){c("In no other country",
                                                                                                     "In 1 other country",
                                                                                                     "In 2 other countries",
                                                                                                     "In 3 other countries",
                                                                                                     "In 4 or more other countries")}else
                                                                                                     {if((input$A=="B4"|
                                                                                                          input$A=="B9"|
                                                                                                          input$A=="E4"|
                                                                                                          input$A=="E5"|
                                                                                                          input$A=="E15"|
                                                                                                          input$A=="G7"|
                                                                                                          input$A=="G11")){c("Yes","No")}else
                                                                                                          {if(input$A=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                "You moved together",
                                                                                                                                "Your spouse/partner moved before you",
                                                                                                                                "Your spouse/partner moved after you",
                                                                                                                                "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                {if(input$A=="C1"){c("Yes, certainly",
                                                                                                                                                     "Yes, probably",
                                                                                                                                                     "No, probably not",
                                                                                                                                                     "No, certainly not",
                                                                                                                                                     "I do not know yet",
                                                                                                                                                     "I have already applied for the Swiss nationality")}else
                                                                                                                                                     {if((input$A=="D1"|input$A=="D3"|input$A=="E23"|input$A=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                       "Compulsory education",
                                                                                                                                                                                                                       "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                       "Vocational education and/or training",
                                                                                                                                                                                                                       "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                       "Advanced technical and professional training",
                                                                                                                                                                                                                       "Bachelor or equivalent",
                                                                                                                                                                                                                       "Master or equivalent",
                                                                                                                                                                                                                       "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                       {if(input$A=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                            "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                            "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                            "No, it was not necessary",
                                                                                                                                                                                                                                            "No, other reasons")}else
                                                                                                                                                                                                                                            {if((input$A=="E2"|input$A=="E12"|input$A=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                 "A company owner",
                                                                                                                                                                                                                                                                                                 "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                 "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                 "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                 "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                 "An apprentice",
                                                                                                                                                                                                                                                                                                 "Master or equivalent",
                                                                                                                                                                                                                                                                                                 "A PhD student")}else
                                                                                                                                                                                                                                                                                                 {if(input$A=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                       "Limited duration",
                                                                                                                                                                                                                                                                                                                       "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                       {if(input$A=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                             "Improved slightly",
                                                                                                                                                                                                                                                                                                                                             "Remained the same",
                                                                                                                                                                                                                                                                                                                                             "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                             "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                             {if(input$A=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                    "German",
                                                                                                                                                                                                                                                                                                                                                                    "French",
                                                                                                                                                                                                                                                                                                                                                                    "Romansh",
                                                                                                                                                                                                                                                                                                                                                                    "Italian",
                                                                                                                                                                                                                                                                                                                                                                    "English",
                                                                                                                                                                                                                                                                                                                                                                    "Spanish",
                                                                                                                                                                                                                                                                                                                                                                    "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                    "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                    {if((input$A=="E25"|input$A=="G13_1"|input$A=="G13_2"|input$A=="H11_1"|input$A=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                    {if(input$A=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                         "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                         "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                         {if(input$A=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                              "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                              "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                              {if(input$A=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$A=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if((input$A=="G16_1"|input$A=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$A=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if(input$A=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Short-term residence permit (L permit)")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             {if(input$A=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             }}}}}}}}}}}}}}}}}}}}}}}
  
  ##### 16 B ####      
  df[[2]]<-as.factor(df[[2]])
  levels(df[[2]])<- if(input$B=="sex1"){c("Male", "Female")}else
  {if(input$B=="A8"){c("Single, never married",
                       "Married",
                       "Separated",
                       "Divorced",
                       "Widowed",
                       "Registered partnership",
                       "Dissolved partnership")}else
                       {if((input$B=="A9"|
                            input$B=="B8_1"|
                            input$B=="B8_2"|
                            input$B=="B8_3"|
                            input$B=="B8_4"|
                            input$B=="B8_5"|
                            input$B=="B8_6"|
                            input$B=="B8_7"|
                            input$B=="B8_8"|
                            input$B=="B8_9"|
                            input$B=="B8_10"|
                            input$B=="B13_1"|
                            input$B=="B13_2"|
                            input$B=="B13_3"|
                            input$B=="B13_4"|
                            input$B=="B13_5"|
                            input$B=="B13_6"|
                            input$B=="B8_10"|
                            input$B=="C2_1"|
                            input$B=="C2_2"|
                            input$B=="C2_3"|
                            input$B=="C2_4"|
                            input$B=="C2_5"|
                            input$B=="C2_6"|
                            input$B=="C2_7"|
                            input$B=="C3_1"|
                            input$B=="C3_2"|
                            input$B=="C3_3"|
                            input$B=="C3_4"|
                            input$B=="C3_5"|
                            input$B=="C3_6"|
                            input$B=="C3_7"|
                            input$B=="C3_8"|
                            input$B=="E1_1"|
                            input$B=="E1_2"|
                            input$B=="E1_3"|
                            input$B=="E1_4"|
                            input$B=="E1_5"|
                            input$B=="E1_6"|
                            input$B=="E1_7"|
                            input$B=="E1_8"|
                            input$B=="E1_9"|
                            input$B=="E11_1"|
                            input$B=="E11_2"|
                            input$B=="E11_3"|
                            input$B=="E11_4"|
                            input$B=="E11_5"|
                            input$B=="E11_6"|
                            input$B=="E11_7"|
                            input$B=="E11_8"|
                            input$B=="E11_9"|
                            input$B=="E16_1"|
                            input$B=="E16_2"|
                            input$B=="E16_3"|
                            input$B=="E16_4"|
                            input$B=="E16_5"|
                            input$B=="E16_6"|
                            input$B=="E16_7"|
                            input$B=="E16_8"|
                            input$B=="E16_9"|
                            input$B=="F7_1"|
                            input$B=="F7_2"|
                            input$B=="F7_3"|
                            input$B=="F7_4"|
                            input$B=="F7_5"|
                            input$B=="F7_6"|
                            input$B=="F7_7"|
                            input$B=="F7_8"|
                            input$B=="F7_9"|
                            input$B=="H8_1"|
                            input$B=="H8_2"|
                            input$B=="H8_3"|
                            input$B=="H8_4"|
                            input$B=="H8_5"|
                            input$B=="H8_6"|
                            input$B=="H8_7"|
                            input$B=="H8_8"|
                            input$B=="H9_1"|
                            input$B=="H9_2"|
                            input$B=="H9_3"|
                            input$B=="H9_4"|
                            input$B=="H9_6")){c("Yes","No")}else
                            {if(input$B=="age_group"){c("20-30",
                                                        "30-40",
                                                        "40_50",
                                                        "50-60",
                                                        "60-64")}else
                                                        {if(input$B=="A6"){c("2006",
                                                                             "2007",
                                                                             "2008",
                                                                             "2009",
                                                                             "2010",
                                                                             "2011",
                                                                             "2012",
                                                                             "2013",
                                                                             "2014",
                                                                             "2015",
                                                                             "2016")}else
                                                                             {if(input$B=="B2CAT"){c("In no other country",
                                                                                                     "In 1 other country",
                                                                                                     "In 2 other countries",
                                                                                                     "In 3 other countries",
                                                                                                     "In 4 or more other countries")}else
                                                                                                     {if((input$B=="B4"|
                                                                                                          input$B=="B9"|
                                                                                                          input$B=="E4"|
                                                                                                          input$B=="E5"|
                                                                                                          input$B=="E15"|
                                                                                                          input$B=="G7"|
                                                                                                          input$B=="G11")){c("Yes","No")}else
                                                                                                          {if(input$B=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                "You moved together",
                                                                                                                                "Your spouse/partner moved before you",
                                                                                                                                "Your spouse/partner moved after you",
                                                                                                                                "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                {if(input$B=="C1"){c("Yes, certainly",
                                                                                                                                                     "Yes, probably",
                                                                                                                                                     "No, probably not",
                                                                                                                                                     "No, certainly not",
                                                                                                                                                     "I do not know yet",
                                                                                                                                                     "I have already applied for the Swiss nationality")}else
                                                                                                                                                     {if((input$B=="D1"|input$B=="D3"|input$B=="E23"|input$B=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                       "Compulsory education",
                                                                                                                                                                                                                       "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                       "Vocational education and/or training",
                                                                                                                                                                                                                       "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                       "Advanced technical and professional training",
                                                                                                                                                                                                                       "Bachelor or equivalent",
                                                                                                                                                                                                                       "Master or equivalent",
                                                                                                                                                                                                                       "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                       {if(input$B=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                            "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                            "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                            "No, it was not necessary",
                                                                                                                                                                                                                                            "No, other reasons")}else
                                                                                                                                                                                                                                            {if((input$B=="E2"|input$B=="E12"|input$B=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                 "A company owner",
                                                                                                                                                                                                                                                                                                 "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                 "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                 "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                 "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                 "An apprentice",
                                                                                                                                                                                                                                                                                                 "Master or equivalent",
                                                                                                                                                                                                                                                                                                 "A PhD student")}else
                                                                                                                                                                                                                                                                                                 {if(input$B=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                       "Limited duration",
                                                                                                                                                                                                                                                                                                                       "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                       {if(input$B=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                             "Improved slightly",
                                                                                                                                                                                                                                                                                                                                             "Remained the same",
                                                                                                                                                                                                                                                                                                                                             "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                             "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                             {if(input$B=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                    "German",
                                                                                                                                                                                                                                                                                                                                                                    "French",
                                                                                                                                                                                                                                                                                                                                                                    "Romansh",
                                                                                                                                                                                                                                                                                                                                                                    "Italian",
                                                                                                                                                                                                                                                                                                                                                                    "English",
                                                                                                                                                                                                                                                                                                                                                                    "Spanish",
                                                                                                                                                                                                                                                                                                                                                                    "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                    "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                    {if((input$B=="E25"|input$B=="G13_1"|input$B=="G13_2"|input$B=="H11_1"|input$B=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                    {if(input$B=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                         "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                         "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                         {if(input$B=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                              "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                              "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                              {if(input$B=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$B=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if((input$B=="G16_1"|input$B=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$B=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if(input$B=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Short-term residence permit (L permit)")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             {if(input$B=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             }}}}}}}}}}}}}}}}}}}}}}}
  
  kable(df %>%
          subset(select=if(input$D=="Absolute"){c(1:3)}else{c(1,2,4)})%>% 
          rename("Table" = 1,"B"=2,"C"=3)%>% 
          spread(B, C),caption = paste(paste(ifelse(input$C=="weight","Weighted data","Non weighted data"),
                                             ifelse(input$D=="Relative","(%)","(N)"), sep=" "),"2016",sep=": "))%>%
    kable_styling(
      font_size = 15,
      bootstrap_options = c("striped", "hover", "condensed"))
})    

output$cross2 <-renderText({ 
  
  ##### 18 A ####
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$A, input$B,input$C,"year")] 
    
    colnames(x)<-c("A","B","pop","year")
    
    x
  })
  
  
  df<-mar[["D18"]]%>%
    group_by(A, B)%>%
    filter(A!=-9)%>% 
    filter(A!=-8)%>% 
    filter(A!=99)%>% 
    filter(A!=-7)%>% 
    filter(B!=-9)%>% 
    filter(A!=-8)%>% 
    filter(B!=99)%>% 
    filter(B!=-7)%>% 
    summarise(n=round(sum(pop),0))%>%
    mutate(prop=round((n/sum(n)*100),2))
  
  df[[1]]<-as.factor(df[[1]])
  levels(df[[1]])<- if(input$A=="sex1"){c("Male", "Female")}else
  {if(input$A=="A8"){c("Single, never married",
                       "Married",
                       "Separated",
                       "Divorced",
                       "Widowed",
                       "Registered partnership",
                       "Dissolved partnership")}else
                       {if((input$A=="A9"|
                            input$A=="B8_1"|
                            input$A=="B8_2"|
                            input$A=="B8_3"|
                            input$A=="B8_4"|
                            input$A=="B8_5"|
                            input$A=="B8_6"|
                            input$A=="B8_7"|
                            input$A=="B8_8"|
                            input$A=="B8_9"|
                            input$A=="B8_10"|
                            input$A=="B13_1"|
                            input$A=="B13_2"|
                            input$A=="B13_3"|
                            input$A=="B13_4"|
                            input$A=="B13_5"|
                            input$A=="B13_6"|
                            input$A=="B8_10"|
                            input$A=="C2_1"|
                            input$A=="C2_2"|
                            input$A=="C2_3"|
                            input$A=="C2_4"|
                            input$A=="C2_5"|
                            input$A=="C2_6"|
                            input$A=="C2_7"|
                            input$A=="C3_1"|
                            input$A=="C3_2"|
                            input$A=="C3_3"|
                            input$A=="C3_4"|
                            input$A=="C3_5"|
                            input$A=="C3_6"|
                            input$A=="C3_7"|
                            input$A=="C3_8"|
                            input$A=="E1_1"|
                            input$A=="E1_2"|
                            input$A=="E1_3"|
                            input$A=="E1_4"|
                            input$A=="E1_5"|
                            input$A=="E1_6"|
                            input$A=="E1_7"|
                            input$A=="E1_8"|
                            input$A=="E1_9"|
                            input$A=="E11_1"|
                            input$A=="E11_2"|
                            input$A=="E11_3"|
                            input$A=="E11_4"|
                            input$A=="E11_5"|
                            input$A=="E11_6"|
                            input$A=="E11_7"|
                            input$A=="E11_8"|
                            input$A=="E11_9"|
                            input$A=="E16_1"|
                            input$A=="E16_2"|
                            input$A=="E16_3"|
                            input$A=="E16_4"|
                            input$A=="E16_5"|
                            input$A=="E16_6"|
                            input$A=="E16_7"|
                            input$A=="E16_8"|
                            input$A=="E16_9"|
                            input$A=="F7_1"|
                            input$A=="F7_2"|
                            input$A=="F7_3"|
                            input$A=="F7_4"|
                            input$A=="F7_5"|
                            input$A=="F7_6"|
                            input$A=="F7_7"|
                            input$A=="F7_8"|
                            input$A=="F7_9"|
                            input$A=="H8_1"|
                            input$A=="H8_2"|
                            input$A=="H8_3"|
                            input$A=="H8_4"|
                            input$A=="H8_5"|
                            input$A=="H8_6"|
                            input$A=="H8_7"|
                            input$A=="H8_8"|
                            input$A=="H9_1"|
                            input$A=="H9_2"|
                            input$A=="H9_3"|
                            input$A=="H9_4"|
                            input$A=="H9_6")){c("Yes","No")}else
                            {if(input$A=="age_group"){c("20-30",
                                                        "30-40",
                                                        "40_50",
                                                        "50-60",
                                                        "60-64")}else
                                                        {if(input$A=="A6"){c("2006",
                                                                             "2007",
                                                                             "2008",
                                                                             "2009",
                                                                             "2010",
                                                                             "2011",
                                                                             "2012",
                                                                             "2013",
                                                                             "2014",
                                                                             "2015",
                                                                             "2016", 
                                                                             "2017",
                                                                             "2018")}else
                                                                             {if(input$A=="B2CAT"){c("In no other country",
                                                                                                     "In 1 other country",
                                                                                                     "In 2 other countries",
                                                                                                     "In 3 other countries",
                                                                                                     "In 4 or more other countries")}else
                                                                                                     {if((input$A=="B4"|
                                                                                                          input$A=="B9"|
                                                                                                          input$A=="E4"|
                                                                                                          input$A=="E5"|
                                                                                                          input$A=="E15"|
                                                                                                          input$A=="G7"|
                                                                                                          input$A=="G11")){c("Yes","No")}else
                                                                                                          {if(input$A=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                "You moved together",
                                                                                                                                "Your spouse/partner moved before you",
                                                                                                                                "Your spouse/partner moved after you",
                                                                                                                                "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                {if(input$A=="C1"){c("Yes, certainly",
                                                                                                                                                     "Yes, probably",
                                                                                                                                                     "No, probably not",
                                                                                                                                                     "No, certainly not",
                                                                                                                                                     "I do not know yet",
                                                                                                                                                     "I have already applied for the Swiss nationality")}else
                                                                                                                                                     {if((input$A=="D1"|input$A=="D3"|input$A=="E23"|input$A=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                       "Compulsory education",
                                                                                                                                                                                                                       "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                       "Vocational education and/or training",
                                                                                                                                                                                                                       "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                       "Advanced technical and professional training",
                                                                                                                                                                                                                       "Bachelor or equivalent",
                                                                                                                                                                                                                       "Master or equivalent",
                                                                                                                                                                                                                       "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                       {if(input$A=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                            "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                            "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                            "No, it was not necessary",
                                                                                                                                                                                                                                            "No, other reasons")}else
                                                                                                                                                                                                                                            {if((input$A=="E2"|input$A=="E12"|input$A=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                 "A company owner",
                                                                                                                                                                                                                                                                                                 "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                 "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                 "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                 "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                 "An apprentice",
                                                                                                                                                                                                                                                                                                 "Master or equivalent",
                                                                                                                                                                                                                                                                                                 "A PhD student")}else
                                                                                                                                                                                                                                                                                                 {if(input$A=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                       "Limited duration",
                                                                                                                                                                                                                                                                                                                       "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                       {if(input$A=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                             "Improved slightly",
                                                                                                                                                                                                                                                                                                                                             "Remained the same",
                                                                                                                                                                                                                                                                                                                                             "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                             "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                             {if(input$A=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                    "German",
                                                                                                                                                                                                                                                                                                                                                                    "French",
                                                                                                                                                                                                                                                                                                                                                                    "Romansh",
                                                                                                                                                                                                                                                                                                                                                                    "Italian",
                                                                                                                                                                                                                                                                                                                                                                    "English",
                                                                                                                                                                                                                                                                                                                                                                    "Spanish",
                                                                                                                                                                                                                                                                                                                                                                    "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                    "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                    {if((input$A=="E25"|input$A=="G13_1"|input$A=="G13_2"|input$A=="H11_1"|input$A=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                    {if(input$A=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                         "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                         "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                         {if(input$A=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                              "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                              "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                              {if(input$A=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$A=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if((input$A=="G16_1"|input$A=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$A=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if(input$A=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Short-term residence permit (L permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Other")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             {if(input$A=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             }}}}}}}}}}}}}}}}}}}}}}}
  ##### 18 B ####    
  df[[2]]<-as.factor(df[[2]])
  levels(df[[2]])<- if(input$B=="sex1"){c("Male", "Female")}else
  {if(input$B=="A8"){c("Single, never married",
                       "Married",
                       "Separated",
                       "Divorced",
                       "Widowed",
                       "Registered partnership",
                       "Dissolved partnership")}else
                       {if((input$B=="A9"|
                            input$B=="B8_1"|
                            input$B=="B8_2"|
                            input$B=="B8_3"|
                            input$B=="B8_4"|
                            input$B=="B8_5"|
                            input$B=="B8_6"|
                            input$B=="B8_7"|
                            input$B=="B8_8"|
                            input$B=="B8_9"|
                            input$B=="B8_10"|
                            input$B=="B13_1"|
                            input$B=="B13_2"|
                            input$B=="B13_3"|
                            input$B=="B13_4"|
                            input$B=="B13_5"|
                            input$B=="B13_6"|
                            input$B=="B8_10"|
                            input$B=="C2_1"|
                            input$B=="C2_2"|
                            input$B=="C2_3"|
                            input$B=="C2_4"|
                            input$B=="C2_5"|
                            input$B=="C2_6"|
                            input$B=="C2_7"|
                            input$B=="C3_1"|
                            input$B=="C3_2"|
                            input$B=="C3_3"|
                            input$B=="C3_4"|
                            input$B=="C3_5"|
                            input$B=="C3_6"|
                            input$B=="C3_7"|
                            input$B=="C3_8"|
                            input$B=="E1_1"|
                            input$B=="E1_2"|
                            input$B=="E1_3"|
                            input$B=="E1_4"|
                            input$B=="E1_5"|
                            input$B=="E1_6"|
                            input$B=="E1_7"|
                            input$B=="E1_8"|
                            input$B=="E1_9"|
                            input$B=="E11_1"|
                            input$B=="E11_2"|
                            input$B=="E11_3"|
                            input$B=="E11_4"|
                            input$B=="E11_5"|
                            input$B=="E11_6"|
                            input$B=="E11_7"|
                            input$B=="E11_8"|
                            input$B=="E11_9"|
                            input$B=="E16_1"|
                            input$B=="E16_2"|
                            input$B=="E16_3"|
                            input$B=="E16_4"|
                            input$B=="E16_5"|
                            input$B=="E16_6"|
                            input$B=="E16_7"|
                            input$B=="E16_8"|
                            input$B=="E16_9"|
                            input$B=="F7_1"|
                            input$B=="F7_2"|
                            input$B=="F7_3"|
                            input$B=="F7_4"|
                            input$B=="F7_5"|
                            input$B=="F7_6"|
                            input$B=="F7_7"|
                            input$B=="F7_8"|
                            input$B=="F7_9"|
                            input$B=="H8_1"|
                            input$B=="H8_2"|
                            input$B=="H8_3"|
                            input$B=="H8_4"|
                            input$B=="H8_5"|
                            input$B=="H8_6"|
                            input$B=="H8_7"|
                            input$B=="H8_8"|
                            input$B=="H9_1"|
                            input$B=="H9_2"|
                            input$B=="H9_3"|
                            input$B=="H9_4"|
                            input$B=="H9_6")){c("Yes","No")}else
                            {if(input$B=="age_group"){c("20-30",
                                                        "30-40",
                                                        "40_50",
                                                        "50-60",
                                                        "60-64")}else
                                                        {if(input$B=="A6"){c("2006",
                                                                             "2007",
                                                                             "2008",
                                                                             "2009",
                                                                             "2010",
                                                                             "2011",
                                                                             "2012",
                                                                             "2013",
                                                                             "2014",
                                                                             "2015",
                                                                             "2016",
                                                                             "2017",
                                                                             "2018")}else
                                                                             {if(input$B=="B2CAT"){c("In no other country",
                                                                                                     "In 1 other country",
                                                                                                     "In 2 other countries",
                                                                                                     "In 3 other countries",
                                                                                                     "In 4 or more other countries")}else
                                                                                                     {if((input$B=="B4"|
                                                                                                          input$B=="B9"|
                                                                                                          input$B=="E4"|
                                                                                                          input$B=="E5"|
                                                                                                          input$B=="E15"|
                                                                                                          input$B=="G7"|
                                                                                                          input$B=="G11")){c("Yes","No")}else
                                                                                                          {if(input$B=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                "You moved together",
                                                                                                                                "Your spouse/partner moved before you",
                                                                                                                                "Your spouse/partner moved after you",
                                                                                                                                "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                {if(input$B=="C1"){c("Yes, certainly",
                                                                                                                                                     "Yes, probably",
                                                                                                                                                     "No, probably not",
                                                                                                                                                     "No, certainly not",
                                                                                                                                                     "I do not know yet",
                                                                                                                                                     "I have already applied for the Swiss nationality")}else
                                                                                                                                                     {if((input$B=="D1"|input$B=="D3"|input$B=="E23"|input$B=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                       "Compulsory education",
                                                                                                                                                                                                                       "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                       "Vocational education and/or training",
                                                                                                                                                                                                                       "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                       "Advanced technical and professional training",
                                                                                                                                                                                                                       "Bachelor or equivalent",
                                                                                                                                                                                                                       "Master or equivalent",
                                                                                                                                                                                                                       "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                       {if(input$B=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                            "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                            "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                            "No, it was not necessary",
                                                                                                                                                                                                                                            "No, other reasons")}else
                                                                                                                                                                                                                                            {if((input$B=="E2"|input$B=="E12"|input$B=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                 "A company owner",
                                                                                                                                                                                                                                                                                                 "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                 "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                 "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                 "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                 "An apprentice",
                                                                                                                                                                                                                                                                                                 "Master or equivalent",
                                                                                                                                                                                                                                                                                                 "A PhD student")}else
                                                                                                                                                                                                                                                                                                 {if(input$B=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                       "Limited duration",
                                                                                                                                                                                                                                                                                                                       "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                       {if(input$B=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                             "Improved slightly",
                                                                                                                                                                                                                                                                                                                                             "Remained the same",
                                                                                                                                                                                                                                                                                                                                             "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                             "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                             {if(input$B=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                    "German",
                                                                                                                                                                                                                                                                                                                                                                    "French",
                                                                                                                                                                                                                                                                                                                                                                    "Romansh",
                                                                                                                                                                                                                                                                                                                                                                    "Italian",
                                                                                                                                                                                                                                                                                                                                                                    "English",
                                                                                                                                                                                                                                                                                                                                                                    "Spanish",
                                                                                                                                                                                                                                                                                                                                                                    "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                    "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                    {if((input$B=="E25"|input$B=="G13_1"|input$B=="G13_2"|input$B=="H11_1"|input$B=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                    {if(input$B=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                         "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                         "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                         {if(input$B=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                              "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                              "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                              {if(input$B=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$B=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if((input$B=="G16_1"|input$B=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$B=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if(input$B=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Short-term residence permit (L permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Other")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             {if(input$B=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             }}}}}}}}}}}}}}}}}}}}}}}
  
  
  
  kable(df %>%
          subset(select=if(input$D=="Absolute"){c(1:3)}else{c(1,2,4)})%>% 
          rename("Table" = 1,"B"=2,"C"=3)%>% 
          spread(B, C),caption = paste(paste(ifelse(input$C=="weight","Weighted data","Non weighted data"),
                                             ifelse(input$D=="Relative","(%)","(N)"), sep=" "),"2018",sep=": "))%>%
    kable_styling(
      font_size = 15,
      bootstrap_options = c("striped", "hover", "condensed"))
}) 


output$cross3 <-renderText({ 
  
  ##### 20 A ####
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$A, input$B,input$C,"year")] 
    
    colnames(x)<-c("A","B","pop","year")
    
    x
  })
  
  
  df<-mar[["D20"]]%>%
    group_by(A, B)%>%
    filter(A!=-9)%>% 
    filter(A!=-8)%>% 
    filter(A!=99)%>% 
    filter(A!=-7)%>% 
    filter(B!=-9)%>% 
    filter(A!=-8)%>% 
    filter(B!=99)%>% 
    filter(B!=-7)%>% 
    summarise(n=round(sum(pop),0))%>%
    mutate(prop=round((n/sum(n)*100),2))
  
  df[[1]]<-as.factor(df[[1]])
  levels(df[[1]])<- if(input$A=="sex1"){c("Male", "Female")}else
  {if(input$A=="A8"){c("Single, never married",
                       "Married",
                       "Separated",
                       "Divorced",
                       "Widowed",
                       "Registered partnership",
                       "Dissolved partnership")}else
                       {if((input$A=="A9"|
                            input$A=="B8_1"|
                            input$A=="B8_2"|
                            input$A=="B8_3"|
                            input$A=="B8_4"|
                            input$A=="B8_5"|
                            input$A=="B8_6"|
                            input$A=="B8_7"|
                            input$A=="B8_8"|
                            input$A=="B8_9"|
                            input$A=="B8_10"|
                            input$A=="B13_1"|
                            input$A=="B13_2"|
                            input$A=="B13_3"|
                            input$A=="B13_4"|
                            input$A=="B13_5"|
                            input$A=="B13_6"|
                            input$A=="B8_10"|
                            input$A=="C2_1"|
                            input$A=="C2_2"|
                            input$A=="C2_3"|
                            input$A=="C2_4"|
                            input$A=="C2_5"|
                            input$A=="C2_6"|
                            input$A=="C2_7"|
                            input$A=="C3_1"|
                            input$A=="C3_2"|
                            input$A=="C3_3"|
                            input$A=="C3_4"|
                            input$A=="C3_5"|
                            input$A=="C3_6"|
                            input$A=="C3_7"|
                            input$A=="C3_8"|
                            input$A=="E1_1"|
                            input$A=="E1_2"|
                            input$A=="E1_3"|
                            input$A=="E1_4"|
                            input$A=="E1_5"|
                            input$A=="E1_6"|
                            input$A=="E1_7"|
                            input$A=="E1_8"|
                            input$A=="E1_9"|
                            input$A=="E11_1"|
                            input$A=="E11_2"|
                            input$A=="E11_3"|
                            input$A=="E11_4"|
                            input$A=="E11_5"|
                            input$A=="E11_6"|
                            input$A=="E11_7"|
                            input$A=="E11_8"|
                            input$A=="E11_9"|
                            input$A=="E16_1"|
                            input$A=="E16_2"|
                            input$A=="E16_3"|
                            input$A=="E16_4"|
                            input$A=="E16_5"|
                            input$A=="E16_6"|
                            input$A=="E16_7"|
                            input$A=="E16_8"|
                            input$A=="E16_9"|
                            input$A=="F7_1"|
                            input$A=="F7_2"|
                            input$A=="F7_3"|
                            input$A=="F7_4"|
                            input$A=="F7_5"|
                            input$A=="F7_6"|
                            input$A=="F7_7"|
                            input$A=="F7_8"|
                            input$A=="F7_9"|
                            input$A=="H8_1"|
                            input$A=="H8_2"|
                            input$A=="H8_3"|
                            input$A=="H8_4"|
                            input$A=="H8_5"|
                            input$A=="H8_6"|
                            input$A=="H8_7"|
                            input$A=="H8_8"|
                            input$A=="H9_1"|
                            input$A=="H9_2"|
                            input$A=="H9_3"|
                            input$A=="H9_4"|
                            input$A=="H9_6")){c("Yes","No")}else
                            {if(input$A=="age_group"){c("20-30",
                                                        "30-40",
                                                        "40_50",
                                                        "50-60",
                                                        "60-64")}else
                                                        {if(input$A=="A6"){c("2006",
                                                                             "2007",
                                                                             "2008",
                                                                             "2009",
                                                                             "2010",
                                                                             "2011",
                                                                             "2012",
                                                                             "2013",
                                                                             "2014",
                                                                             "2015",
                                                                             "2016", 
                                                                             "2017",
                                                                             "2018",
                                                                             "2019",
                                                                             "2020")}else
                                                                             {if(input$A=="B2CAT"){c("In no other country",
                                                                                                     "In 1 other country",
                                                                                                     "In 2 other countries",
                                                                                                     "In 3 other countries",
                                                                                                     "In 4 or more other countries")}else
                                                                                                     {if((input$A=="B4"|
                                                                                                          input$A=="B9"|
                                                                                                          input$A=="E4"|
                                                                                                          input$A=="E5"|
                                                                                                          input$A=="E15"|
                                                                                                          input$A=="G7"|
                                                                                                          input$A=="G11")){c("Yes","No")}else
                                                                                                          {if(input$A=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                "You moved together",
                                                                                                                                "Your spouse/partner moved before you",
                                                                                                                                "Your spouse/partner moved after you",
                                                                                                                                "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                {if(input$A=="C1"){c("Yes, certainly",
                                                                                                                                                     "Yes, probably",
                                                                                                                                                     "No, probably not",
                                                                                                                                                     "No, certainly not",
                                                                                                                                                     "I do not know yet",
                                                                                                                                                     "I have already applied for the Swiss nationality",
                                                                                                                                                     "I have already being naturalized")}else
                                                                                                                                                     {if((input$A=="D1"|input$A=="D3"|input$A=="E23"|input$A=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                       "Compulsory education",
                                                                                                                                                                                                                       "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                       "Vocational education and/or training",
                                                                                                                                                                                                                       "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                       "Advanced technical and professional training",
                                                                                                                                                                                                                       "Bachelor or equivalent",
                                                                                                                                                                                                                       "Master or equivalent",
                                                                                                                                                                                                                       "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                       {if(input$A=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                            "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                            "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                            "No, it was not necessary",
                                                                                                                                                                                                                                            "No, other reasons")}else
                                                                                                                                                                                                                                            {if((input$A=="E2"|input$A=="E12"|input$A=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                 "A company owner",
                                                                                                                                                                                                                                                                                                 "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                 "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                 "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                 "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                 "An apprentice",
                                                                                                                                                                                                                                                                                                 "Master or equivalent",
                                                                                                                                                                                                                                                                                                 "A PhD student")}else
                                                                                                                                                                                                                                                                                                 {if(input$A=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                       "Limited duration",
                                                                                                                                                                                                                                                                                                                       "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                       {if(input$A=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                             "Improved slightly",
                                                                                                                                                                                                                                                                                                                                             "Remained the same",
                                                                                                                                                                                                                                                                                                                                             "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                             "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                             {if(input$A=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                    "German",
                                                                                                                                                                                                                                                                                                                                                                    "French",
                                                                                                                                                                                                                                                                                                                                                                    "Romansh",
                                                                                                                                                                                                                                                                                                                                                                    "Italian",
                                                                                                                                                                                                                                                                                                                                                                    "English",
                                                                                                                                                                                                                                                                                                                                                                    "Spanish",
                                                                                                                                                                                                                                                                                                                                                                    "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                    "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                    {if((input$A=="E25"|input$A=="G13_1"|input$A=="G13_2"|input$A=="H11_1"|input$A=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                    {if(input$A=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                         "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                         "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                         {if(input$A=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                              "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                              "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                              {if(input$A=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$A=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if((input$A=="G16_1"|input$A=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$A=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if(input$A=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Short-term residence permit (L permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Other",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "I have the Swiss citizenship")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             {if(input$A=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             }}}}}}}}}}}}}}}}}}}}}}}
  ##### 20 B ####    
  df[[2]]<-as.factor(df[[2]])
  levels(df[[2]])<- if(input$B=="sex1"){c("Male", "Female")}else
  {if(input$B=="A8"){c("Single, never married",
                       "Married",
                       "Separated",
                       "Divorced",
                       "Widowed",
                       "Registered partnership",
                       "Dissolved partnership")}else
                       {if((input$B=="A9"|
                            input$B=="B8_1"|
                            input$B=="B8_2"|
                            input$B=="B8_3"|
                            input$B=="B8_4"|
                            input$B=="B8_5"|
                            input$B=="B8_6"|
                            input$B=="B8_7"|
                            input$B=="B8_8"|
                            input$B=="B8_9"|
                            input$B=="B8_10"|
                            input$B=="B13_1"|
                            input$B=="B13_2"|
                            input$B=="B13_3"|
                            input$B=="B13_4"|
                            input$B=="B13_5"|
                            input$B=="B13_6"|
                            input$B=="B8_10"|
                            input$B=="C2_1"|
                            input$B=="C2_2"|
                            input$B=="C2_3"|
                            input$B=="C2_4"|
                            input$B=="C2_5"|
                            input$B=="C2_6"|
                            input$B=="C2_7"|
                            input$B=="C3_1"|
                            input$B=="C3_2"|
                            input$B=="C3_3"|
                            input$B=="C3_4"|
                            input$B=="C3_5"|
                            input$B=="C3_6"|
                            input$B=="C3_7"|
                            input$B=="C3_8"|
                            input$B=="E1_1"|
                            input$B=="E1_2"|
                            input$B=="E1_3"|
                            input$B=="E1_4"|
                            input$B=="E1_5"|
                            input$B=="E1_6"|
                            input$B=="E1_7"|
                            input$B=="E1_8"|
                            input$B=="E1_9"|
                            input$B=="E11_1"|
                            input$B=="E11_2"|
                            input$B=="E11_3"|
                            input$B=="E11_4"|
                            input$B=="E11_5"|
                            input$B=="E11_6"|
                            input$B=="E11_7"|
                            input$B=="E11_8"|
                            input$B=="E11_9"|
                            input$B=="E16_1"|
                            input$B=="E16_2"|
                            input$B=="E16_3"|
                            input$B=="E16_4"|
                            input$B=="E16_5"|
                            input$B=="E16_6"|
                            input$B=="E16_7"|
                            input$B=="E16_8"|
                            input$B=="E16_9"|
                            input$B=="F7_1"|
                            input$B=="F7_2"|
                            input$B=="F7_3"|
                            input$B=="F7_4"|
                            input$B=="F7_5"|
                            input$B=="F7_6"|
                            input$B=="F7_7"|
                            input$B=="F7_8"|
                            input$B=="F7_9"|
                            input$B=="H8_1"|
                            input$B=="H8_2"|
                            input$B=="H8_3"|
                            input$B=="H8_4"|
                            input$B=="H8_5"|
                            input$B=="H8_6"|
                            input$B=="H8_7"|
                            input$B=="H8_8"|
                            input$B=="H9_1"|
                            input$B=="H9_2"|
                            input$B=="H9_3"|
                            input$B=="H9_4"|
                            input$B=="H9_6")){c("Yes","No")}else
                            {if(input$B=="age_group"){c("20-30",
                                                        "30-40",
                                                        "40_50",
                                                        "50-60",
                                                        "60-64")}else
                                                        {if(input$B=="A6"){c("2006",
                                                                             "2007",
                                                                             "2008",
                                                                             "2009",
                                                                             "2010",
                                                                             "2011",
                                                                             "2012",
                                                                             "2013",
                                                                             "2014",
                                                                             "2015",
                                                                             "2016",
                                                                             "2017",
                                                                             "2018",
                                                                             "2019",
                                                                             "2020")}else
                                                                             {if(input$B=="B2CAT"){c("In no other country",
                                                                                                     "In 1 other country",
                                                                                                     "In 2 other countries",
                                                                                                     "In 3 other countries",
                                                                                                     "In 4 or more other countries")}else
                                                                                                     {if((input$B=="B4"|
                                                                                                          input$B=="B9"|
                                                                                                          input$B=="E4"|
                                                                                                          input$B=="E5"|
                                                                                                          input$B=="E15"|
                                                                                                          input$B=="G7"|
                                                                                                          input$B=="G11")){c("Yes","No")}else
                                                                                                          {if(input$B=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                "You moved together",
                                                                                                                                "Your spouse/partner moved before you",
                                                                                                                                "Your spouse/partner moved after you",
                                                                                                                                "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                {if(input$B=="C1"){c("Yes, certainly",
                                                                                                                                                     "Yes, probably",
                                                                                                                                                     "No, probably not",
                                                                                                                                                     "No, certainly not",
                                                                                                                                                     "I do not know yet",
                                                                                                                                                     "I have already applied for the Swiss nationality",
                                                                                                                                                     "I have already being naturalized")}else
                                                                                                                                                     {if((input$B=="D1"|input$B=="D3"|input$B=="E23"|input$B=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                       "Compulsory education",
                                                                                                                                                                                                                       "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                       "Vocational education and/or training",
                                                                                                                                                                                                                       "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                       "Advanced technical and professional training",
                                                                                                                                                                                                                       "Bachelor or equivalent",
                                                                                                                                                                                                                       "Master or equivalent",
                                                                                                                                                                                                                       "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                       {if(input$B=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                            "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                            "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                            "No, it was not necessary",
                                                                                                                                                                                                                                            "No, other reasons")}else
                                                                                                                                                                                                                                            {if((input$B=="E2"|input$B=="E12"|input$B=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                 "A company owner",
                                                                                                                                                                                                                                                                                                 "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                 "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                 "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                 "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                 "An apprentice",
                                                                                                                                                                                                                                                                                                 "Master or equivalent",
                                                                                                                                                                                                                                                                                                 "A PhD student")}else
                                                                                                                                                                                                                                                                                                 {if(input$B=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                       "Limited duration",
                                                                                                                                                                                                                                                                                                                       "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                       {if(input$B=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                             "Improved slightly",
                                                                                                                                                                                                                                                                                                                                             "Remained the same",
                                                                                                                                                                                                                                                                                                                                             "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                             "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                             {if(input$B=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                    "German",
                                                                                                                                                                                                                                                                                                                                                                    "French",
                                                                                                                                                                                                                                                                                                                                                                    "Romansh",
                                                                                                                                                                                                                                                                                                                                                                    "Italian",
                                                                                                                                                                                                                                                                                                                                                                    "English",
                                                                                                                                                                                                                                                                                                                                                                    "Spanish",
                                                                                                                                                                                                                                                                                                                                                                    "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                    "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                    {if((input$B=="E25"|input$B=="G13_1"|input$B=="G13_2"|input$B=="H11_1"|input$B=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                    {if(input$B=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                         "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                         "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                         "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                         {if(input$B=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                              "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                              "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                              "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                              {if(input$B=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$B=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if((input$B=="G16_1"|input$B=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   {if(input$B=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        {if(input$B=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Short-term residence permit (L permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Other",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "I have the Swiss citizenship")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             {if(input$B=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             }}}}}}}}}}}}}}}}}}}}}}}
  
  
  
  kable(df %>%
          subset(select=if(input$D=="Absolute"){c(1:3)}else{c(1,2,4)})%>% 
          rename("Table" = 1,"B"=2,"C"=3)%>% 
          spread(B, C),caption = paste(paste(ifelse(input$C=="weight","Weighted data","Non weighted data"),
                                             ifelse(input$D=="Relative","(%)","(N)"), sep=" "),"2020",sep=": "))%>%
    kable_styling(
      font_size = 15,
      bootstrap_options = c("striped", "hover", "condensed"))
}) 

#### DOWNLOAD DATA SECTION CROOS ###### 

output$downloadDataCT <- downloadHandler(
  
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste(paste("nccr","data",sep=" "), input$filetypeCT, sep = ".")
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    sep <- switch(input$filetypeCT, "csv" = ",", "txt" = "\t")
    
    mar <- lapply(dmms, function(x){
      
      x<-x[,c(input$A, input$B,input$C,"year")] 
      
      colnames(x)<-c("A","B","pop","year")
      x[is.na(x)] <- -7
      
      x
    })
    
    ##### 16 A ####  
    df<-mar[["D16"]]%>%
      group_by(A, B)%>%
      filter(A!=-9)%>% 
      filter(A!=-8)%>% 
      filter(A!=99)%>% 
      filter(A!=-7)%>% 
      filter(B!=-9)%>% 
      filter(A!=-8)%>% 
      filter(B!=99)%>% 
      filter(B!=-7)%>% 
      summarise(n=round(sum(pop),0))%>%
      mutate(prop=round((n/sum(n)*100),2))
    
    df[[1]]<-as.factor(df[[1]])
    levels(df[[1]])<- if(input$A=="sex1"){c("Male", "Female")}else
    {if(input$A=="A8"){c("Single, never married",
                         "Married",
                         "Separated",
                         "Divorced",
                         "Widowed",
                         "Registered partnership",
                         "Dissolved partnership")}else
                         {if((input$A=="A9"|
                              input$A=="B8_1"|
                              input$A=="B8_2"|
                              input$A=="B8_3"|
                              input$A=="B8_4"|
                              input$A=="B8_5"|
                              input$A=="B8_6"|
                              input$A=="B8_7"|
                              input$A=="B8_8"|
                              input$A=="B8_9"|
                              input$A=="B8_10"|
                              input$A=="B13_1"|
                              input$A=="B13_2"|
                              input$A=="B13_3"|
                              input$A=="B13_4"|
                              input$A=="B13_5"|
                              input$A=="B13_6"|
                              input$A=="B8_10"|
                              input$A=="C2_1"|
                              input$A=="C2_2"|
                              input$A=="C2_3"|
                              input$A=="C2_4"|
                              input$A=="C2_5"|
                              input$A=="C2_6"|
                              input$A=="C2_7"|
                              input$A=="C3_1"|
                              input$A=="C3_2"|
                              input$A=="C3_3"|
                              input$A=="C3_4"|
                              input$A=="C3_5"|
                              input$A=="C3_6"|
                              input$A=="C3_7"|
                              input$A=="C3_8"|
                              input$A=="E1_1"|
                              input$A=="E1_2"|
                              input$A=="E1_3"|
                              input$A=="E1_4"|
                              input$A=="E1_5"|
                              input$A=="E1_6"|
                              input$A=="E1_7"|
                              input$A=="E1_8"|
                              input$A=="E1_9"|
                              input$A=="E11_1"|
                              input$A=="E11_2"|
                              input$A=="E11_3"|
                              input$A=="E11_4"|
                              input$A=="E11_5"|
                              input$A=="E11_6"|
                              input$A=="E11_7"|
                              input$A=="E11_8"|
                              input$A=="E11_9"|
                              input$A=="E16_1"|
                              input$A=="E16_2"|
                              input$A=="E16_3"|
                              input$A=="E16_4"|
                              input$A=="E16_5"|
                              input$A=="E16_6"|
                              input$A=="E16_7"|
                              input$A=="E16_8"|
                              input$A=="E16_9"|
                              input$A=="F7_1"|
                              input$A=="F7_2"|
                              input$A=="F7_3"|
                              input$A=="F7_4"|
                              input$A=="F7_5"|
                              input$A=="F7_6"|
                              input$A=="F7_7"|
                              input$A=="F7_8"|
                              input$A=="F7_9"|
                              input$A=="H8_1"|
                              input$A=="H8_2"|
                              input$A=="H8_3"|
                              input$A=="H8_4"|
                              input$A=="H8_5"|
                              input$A=="H8_6"|
                              input$A=="H8_7"|
                              input$A=="H8_8"|
                              input$A=="H9_1"|
                              input$A=="H9_2"|
                              input$A=="H9_3"|
                              input$A=="H9_4"|
                              input$A=="H9_6")){c("Yes","No")}else
                              {if(input$A=="age_group"){c("20-30",
                                                          "30-40",
                                                          "40_50",
                                                          "50-60",
                                                          "60-64")}else
                                                          {if(input$A=="A6"){c("2006",
                                                                               "2007",
                                                                               "2008",
                                                                               "2009",
                                                                               "2010",
                                                                               "2011",
                                                                               "2012",
                                                                               "2013",
                                                                               "2014",
                                                                               "2015",
                                                                               "2016")}else
                                                                               {if(input$A=="B2CAT"){c("In no other country",
                                                                                                       "In 1 other country",
                                                                                                       "In 2 other countries",
                                                                                                       "In 3 other countries",
                                                                                                       "In 4 or more other countries")}else
                                                                                                       {if((input$A=="B4"|
                                                                                                            input$A=="B9"|
                                                                                                            input$A=="E4"|
                                                                                                            input$A=="E5"|
                                                                                                            input$A=="E15"|
                                                                                                            input$A=="G7"|
                                                                                                            input$A=="G11")){c("Yes","No")}else
                                                                                                            {if(input$A=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                  "You moved together",
                                                                                                                                  "Your spouse/partner moved before you",
                                                                                                                                  "Your spouse/partner moved after you",
                                                                                                                                  "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                  {if(input$A=="C1"){c("Yes, certainly",
                                                                                                                                                       "Yes, probably",
                                                                                                                                                       "No, probably not",
                                                                                                                                                       "No, certainly not",
                                                                                                                                                       "I do not know yet",
                                                                                                                                                       "I have already applied for the Swiss nationality")}else
                                                                                                                                                       {if((input$A=="D1"|input$A=="D3"|input$A=="E23"|input$A=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                         "Compulsory education",
                                                                                                                                                                                                                         "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                         "Vocational education and/or training",
                                                                                                                                                                                                                         "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                         "Advanced technical and professional training",
                                                                                                                                                                                                                         "Bachelor or equivalent",
                                                                                                                                                                                                                         "Master or equivalent",
                                                                                                                                                                                                                         "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                         {if(input$A=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                              "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                              "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                              "No, it was not necessary",
                                                                                                                                                                                                                                              "No, other reasons")}else
                                                                                                                                                                                                                                              {if((input$A=="E2"|input$A=="E12"|input$A=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                   "A company owner",
                                                                                                                                                                                                                                                                                                   "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                   "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                   "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                   "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                   "An apprentice",
                                                                                                                                                                                                                                                                                                   "Master or equivalent",
                                                                                                                                                                                                                                                                                                   "A PhD student")}else
                                                                                                                                                                                                                                                                                                   {if(input$A=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                         "Limited duration",
                                                                                                                                                                                                                                                                                                                         "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                         {if(input$A=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                               "Improved slightly",
                                                                                                                                                                                                                                                                                                                                               "Remained the same",
                                                                                                                                                                                                                                                                                                                                               "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                               "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                               {if(input$A=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                      "German",
                                                                                                                                                                                                                                                                                                                                                                      "French",
                                                                                                                                                                                                                                                                                                                                                                      "Romansh",
                                                                                                                                                                                                                                                                                                                                                                      "Italian",
                                                                                                                                                                                                                                                                                                                                                                      "English",
                                                                                                                                                                                                                                                                                                                                                                      "Spanish",
                                                                                                                                                                                                                                                                                                                                                                      "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                      "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                      {if((input$A=="E25"|input$A=="G13_1"|input$A=="G13_2"|input$A=="H11_1"|input$A=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                      {if(input$A=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                           "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                           "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                           {if(input$A=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                                "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                                "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                                {if(input$A=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$A=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if((input$A=="G16_1"|input$A=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$A=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if(input$A=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Short-term residence permit (L permit)")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               {if(input$A=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               }}}}}}}}}}}}}}}}}}}}}}}
    ##### 16 B ####         
    df[[2]]<-as.factor(df[[2]])
    levels(df[[2]])<- if(input$B=="sex1"){c("Male", "Female")}else
    {if(input$B=="A8"){c("Single, never married",
                         "Married",
                         "Separated",
                         "Divorced",
                         "Widowed",
                         "Registered partnership",
                         "Dissolved partnership")}else
                         {if((input$B=="A9"|
                              input$B=="B8_1"|
                              input$B=="B8_2"|
                              input$B=="B8_3"|
                              input$B=="B8_4"|
                              input$B=="B8_5"|
                              input$B=="B8_6"|
                              input$B=="B8_7"|
                              input$B=="B8_8"|
                              input$B=="B8_9"|
                              input$B=="B8_10"|
                              input$B=="B13_1"|
                              input$B=="B13_2"|
                              input$B=="B13_3"|
                              input$B=="B13_4"|
                              input$B=="B13_5"|
                              input$B=="B13_6"|
                              input$B=="B8_10"|
                              input$B=="C2_1"|
                              input$B=="C2_2"|
                              input$B=="C2_3"|
                              input$B=="C2_4"|
                              input$B=="C2_5"|
                              input$B=="C2_6"|
                              input$B=="C2_7"|
                              input$B=="C3_1"|
                              input$B=="C3_2"|
                              input$B=="C3_3"|
                              input$B=="C3_4"|
                              input$B=="C3_5"|
                              input$B=="C3_6"|
                              input$B=="C3_7"|
                              input$B=="C3_8"|
                              input$B=="E1_1"|
                              input$B=="E1_2"|
                              input$B=="E1_3"|
                              input$B=="E1_4"|
                              input$B=="E1_5"|
                              input$B=="E1_6"|
                              input$B=="E1_7"|
                              input$B=="E1_8"|
                              input$B=="E1_9"|
                              input$B=="E11_1"|
                              input$B=="E11_2"|
                              input$B=="E11_3"|
                              input$B=="E11_4"|
                              input$B=="E11_5"|
                              input$B=="E11_6"|
                              input$B=="E11_7"|
                              input$B=="E11_8"|
                              input$B=="E11_9"|
                              input$B=="E16_1"|
                              input$B=="E16_2"|
                              input$B=="E16_3"|
                              input$B=="E16_4"|
                              input$B=="E16_5"|
                              input$B=="E16_6"|
                              input$B=="E16_7"|
                              input$B=="E16_8"|
                              input$B=="E16_9"|
                              input$B=="F7_1"|
                              input$B=="F7_2"|
                              input$B=="F7_3"|
                              input$B=="F7_4"|
                              input$B=="F7_5"|
                              input$B=="F7_6"|
                              input$B=="F7_7"|
                              input$B=="F7_8"|
                              input$B=="F7_9"|
                              input$B=="H8_1"|
                              input$B=="H8_2"|
                              input$B=="H8_3"|
                              input$B=="H8_4"|
                              input$B=="H8_5"|
                              input$B=="H8_6"|
                              input$B=="H8_7"|
                              input$B=="H8_8"|
                              input$B=="H9_1"|
                              input$B=="H9_2"|
                              input$B=="H9_3"|
                              input$B=="H9_4"|
                              input$B=="H9_6")){c("Yes","No")}else
                              {if(input$B=="age_group"){c("20-30",
                                                          "30-40",
                                                          "40_50",
                                                          "50-60",
                                                          "60-64")}else
                                                          {if(input$B=="A6"){c("2006",
                                                                               "2007",
                                                                               "2008",
                                                                               "2009",
                                                                               "2010",
                                                                               "2011",
                                                                               "2012",
                                                                               "2013",
                                                                               "2014",
                                                                               "2015",
                                                                               "2016")}else
                                                                               {if(input$B=="B2CAT"){c("In no other country",
                                                                                                       "In 1 other country",
                                                                                                       "In 2 other countries",
                                                                                                       "In 3 other countries",
                                                                                                       "In 4 or more other countries")}else
                                                                                                       {if((input$B=="B4"|
                                                                                                            input$B=="B9"|
                                                                                                            input$B=="E4"|
                                                                                                            input$B=="E5"|
                                                                                                            input$B=="E15"|
                                                                                                            input$B=="G7"|
                                                                                                            input$B=="G11")){c("Yes","No")}else
                                                                                                            {if(input$B=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                  "You moved together",
                                                                                                                                  "Your spouse/partner moved before you",
                                                                                                                                  "Your spouse/partner moved after you",
                                                                                                                                  "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                  {if(input$B=="C1"){c("Yes, certainly",
                                                                                                                                                       "Yes, probably",
                                                                                                                                                       "No, probably not",
                                                                                                                                                       "No, certainly not",
                                                                                                                                                       "I do not know yet",
                                                                                                                                                       "I have already applied for the Swiss nationality")}else
                                                                                                                                                       {if((input$B=="D1"|input$B=="D3"|input$B=="E23"|input$B=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                         "Compulsory education",
                                                                                                                                                                                                                         "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                         "Vocational education and/or training",
                                                                                                                                                                                                                         "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                         "Advanced technical and professional training",
                                                                                                                                                                                                                         "Bachelor or equivalent",
                                                                                                                                                                                                                         "Master or equivalent",
                                                                                                                                                                                                                         "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                         {if(input$B=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                              "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                              "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                              "No, it was not necessary",
                                                                                                                                                                                                                                              "No, other reasons")}else
                                                                                                                                                                                                                                              {if((input$B=="E2"|input$B=="E12"|input$B=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                   "A company owner",
                                                                                                                                                                                                                                                                                                   "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                   "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                   "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                   "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                   "An apprentice",
                                                                                                                                                                                                                                                                                                   "Master or equivalent",
                                                                                                                                                                                                                                                                                                   "A PhD student")}else
                                                                                                                                                                                                                                                                                                   {if(input$B=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                         "Limited duration",
                                                                                                                                                                                                                                                                                                                         "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                         {if(input$B=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                               "Improved slightly",
                                                                                                                                                                                                                                                                                                                                               "Remained the same",
                                                                                                                                                                                                                                                                                                                                               "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                               "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                               {if(input$B=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                      "German",
                                                                                                                                                                                                                                                                                                                                                                      "French",
                                                                                                                                                                                                                                                                                                                                                                      "Romansh",
                                                                                                                                                                                                                                                                                                                                                                      "Italian",
                                                                                                                                                                                                                                                                                                                                                                      "English",
                                                                                                                                                                                                                                                                                                                                                                      "Spanish",
                                                                                                                                                                                                                                                                                                                                                                      "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                      "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                      {if((input$B=="E25"|input$B=="G13_1"|input$B=="G13_2"|input$B=="H11_1"|input$B=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                      {if(input$B=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                           "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                           "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                           {if(input$B=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                                "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                                "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                                {if(input$B=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$B=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if((input$B=="G16_1"|input$B=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$B=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if(input$B=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Short-term residence permit (L permit)")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               {if(input$B=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               }}}}}}}}}}}}}}}}}}}}}}}
    
    dffinal0<-df %>%
      subset(select=if(input$D=="Absolute"){c(1:3)}else{c(1,2,4)})%>% 
      rename("Table" = 1,"B"=2,"C"=3)%>% 
      spread(B, C)%>%
      mutate(year=2016)
    ##### 18 A ####         
    df1<-mar[["D18"]]%>%
      group_by(A, B)%>%
      filter(A!=-9)%>% 
      filter(A!=-8)%>% 
      filter(A!=99)%>% 
      filter(A!=-7)%>% 
      filter(B!=-9)%>% 
      filter(A!=-8)%>% 
      filter(B!=99)%>% 
      filter(B!=-7)%>% 
      summarise(n=round(sum(pop),0))%>%
      mutate(prop=round((n/sum(n)*100),2))
    
    df1[[1]]<-as.factor(df1[[1]])
    levels(df1[[1]])<- if(input$A=="sex1"){c("Male", "Female")}else
    {if(input$A=="A8"){c("Single, never married",
                         "Married",
                         "Separated",
                         "Divorced",
                         "Widowed",
                         "Registered partnership",
                         "Dissolved partnership")}else
                         {if((input$A=="A9"|
                              input$A=="B8_1"|
                              input$A=="B8_2"|
                              input$A=="B8_3"|
                              input$A=="B8_4"|
                              input$A=="B8_5"|
                              input$A=="B8_6"|
                              input$A=="B8_7"|
                              input$A=="B8_8"|
                              input$A=="B8_9"|
                              input$A=="B8_10"|
                              input$A=="B13_1"|
                              input$A=="B13_2"|
                              input$A=="B13_3"|
                              input$A=="B13_4"|
                              input$A=="B13_5"|
                              input$A=="B13_6"|
                              input$A=="B8_10"|
                              input$A=="C2_1"|
                              input$A=="C2_2"|
                              input$A=="C2_3"|
                              input$A=="C2_4"|
                              input$A=="C2_5"|
                              input$A=="C2_6"|
                              input$A=="C2_7"|
                              input$A=="C3_1"|
                              input$A=="C3_2"|
                              input$A=="C3_3"|
                              input$A=="C3_4"|
                              input$A=="C3_5"|
                              input$A=="C3_6"|
                              input$A=="C3_7"|
                              input$A=="C3_8"|
                              input$A=="E1_1"|
                              input$A=="E1_2"|
                              input$A=="E1_3"|
                              input$A=="E1_4"|
                              input$A=="E1_5"|
                              input$A=="E1_6"|
                              input$A=="E1_7"|
                              input$A=="E1_8"|
                              input$A=="E1_9"|
                              input$A=="E11_1"|
                              input$A=="E11_2"|
                              input$A=="E11_3"|
                              input$A=="E11_4"|
                              input$A=="E11_5"|
                              input$A=="E11_6"|
                              input$A=="E11_7"|
                              input$A=="E11_8"|
                              input$A=="E11_9"|
                              input$A=="E16_1"|
                              input$A=="E16_2"|
                              input$A=="E16_3"|
                              input$A=="E16_4"|
                              input$A=="E16_5"|
                              input$A=="E16_6"|
                              input$A=="E16_7"|
                              input$A=="E16_8"|
                              input$A=="E16_9"|
                              input$A=="F7_1"|
                              input$A=="F7_2"|
                              input$A=="F7_3"|
                              input$A=="F7_4"|
                              input$A=="F7_5"|
                              input$A=="F7_6"|
                              input$A=="F7_7"|
                              input$A=="F7_8"|
                              input$A=="F7_9"|
                              input$A=="H8_1"|
                              input$A=="H8_2"|
                              input$A=="H8_3"|
                              input$A=="H8_4"|
                              input$A=="H8_5"|
                              input$A=="H8_6"|
                              input$A=="H8_7"|
                              input$A=="H8_8"|
                              input$A=="H9_1"|
                              input$A=="H9_2"|
                              input$A=="H9_3"|
                              input$A=="H9_4"|
                              input$A=="H9_6")){c("Yes","No")}else
                              {if(input$A=="age_group"){c("20-30",
                                                          "30-40",
                                                          "40_50",
                                                          "50-60",
                                                          "60-64")}else
                                                          {if(input$A=="A6"){c("2006",
                                                                               "2007",
                                                                               "2008",
                                                                               "2009",
                                                                               "2010",
                                                                               "2011",
                                                                               "2012",
                                                                               "2013",
                                                                               "2014",
                                                                               "2015",
                                                                               "2016",
                                                                               "2017",
                                                                               "2018")}else
                                                                               {if(input$A=="B2CAT"){c("In no other country",
                                                                                                       "In 1 other country",
                                                                                                       "In 2 other countries",
                                                                                                       "In 3 other countries",
                                                                                                       "In 4 or more other countries")}else
                                                                                                       {if((input$A=="B4"|
                                                                                                            input$A=="B9"|
                                                                                                            input$A=="E4"|
                                                                                                            input$A=="E5"|
                                                                                                            input$A=="E15"|
                                                                                                            input$A=="G7"|
                                                                                                            input$A=="G11")){c("Yes","No")}else
                                                                                                            {if(input$A=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                  "You moved together",
                                                                                                                                  "Your spouse/partner moved before you",
                                                                                                                                  "Your spouse/partner moved after you",
                                                                                                                                  "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                  {if(input$A=="C1"){c("Yes, certainly",
                                                                                                                                                       "Yes, probably",
                                                                                                                                                       "No, probably not",
                                                                                                                                                       "No, certainly not",
                                                                                                                                                       "I do not know yet",
                                                                                                                                                       "I have already applied for the Swiss nationality")}else
                                                                                                                                                       {if((input$A=="D1"|input$A=="D3"|input$A=="E23"|input$A=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                         "Compulsory education",
                                                                                                                                                                                                                         "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                         "Vocational education and/or training",
                                                                                                                                                                                                                         "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                         "Advanced technical and professional training",
                                                                                                                                                                                                                         "Bachelor or equivalent",
                                                                                                                                                                                                                         "Master or equivalent",
                                                                                                                                                                                                                         "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                         {if(input$A=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                              "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                              "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                              "No, it was not necessary",
                                                                                                                                                                                                                                              "No, other reasons")}else
                                                                                                                                                                                                                                              {if((input$A=="E2"|input$A=="E12"|input$A=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                   "A company owner",
                                                                                                                                                                                                                                                                                                   "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                   "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                   "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                   "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                   "An apprentice",
                                                                                                                                                                                                                                                                                                   "Master or equivalent",
                                                                                                                                                                                                                                                                                                   "A PhD student")}else
                                                                                                                                                                                                                                                                                                   {if(input$A=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                         "Limited duration",
                                                                                                                                                                                                                                                                                                                         "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                         {if(input$A=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                               "Improved slightly",
                                                                                                                                                                                                                                                                                                                                               "Remained the same",
                                                                                                                                                                                                                                                                                                                                               "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                               "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                               {if(input$A=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                      "German",
                                                                                                                                                                                                                                                                                                                                                                      "French",
                                                                                                                                                                                                                                                                                                                                                                      "Romansh",
                                                                                                                                                                                                                                                                                                                                                                      "Italian",
                                                                                                                                                                                                                                                                                                                                                                      "English",
                                                                                                                                                                                                                                                                                                                                                                      "Spanish",
                                                                                                                                                                                                                                                                                                                                                                      "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                      "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                      {if((input$A=="E25"|input$A=="G13_1"|input$A=="G13_2"|input$A=="H11_1"|input$A=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                      {if(input$A=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                           "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                           "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                           {if(input$A=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                                "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                                "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                                {if(input$A=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$A=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if((input$A=="G16_1"|input$A=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$A=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if(input$A=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Short-term residence permit (L permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Other")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               {if(input$A=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               }}}}}}}}}}}}}}}}}}}}}}}
    ##### 18 B ####           
    df1[[2]]<-as.factor(df1[[2]])
    levels(df1[[2]])<- if(input$B=="sex1"){c("Male", "Female")}else
    {if(input$B=="A8"){c("Single, never married",
                         "Married",
                         "Separated",
                         "Divorced",
                         "Widowed",
                         "Registered partnership",
                         "Dissolved partnership")}else
                         {if((input$B=="A9"|
                              input$B=="B8_1"|
                              input$B=="B8_2"|
                              input$B=="B8_3"|
                              input$B=="B8_4"|
                              input$B=="B8_5"|
                              input$B=="B8_6"|
                              input$B=="B8_7"|
                              input$B=="B8_8"|
                              input$B=="B8_9"|
                              input$B=="B8_10"|
                              input$B=="B13_1"|
                              input$B=="B13_2"|
                              input$B=="B13_3"|
                              input$B=="B13_4"|
                              input$B=="B13_5"|
                              input$B=="B13_6"|
                              input$B=="B8_10"|
                              input$B=="C2_1"|
                              input$B=="C2_2"|
                              input$B=="C2_3"|
                              input$B=="C2_4"|
                              input$B=="C2_5"|
                              input$B=="C2_6"|
                              input$B=="C2_7"|
                              input$B=="C3_1"|
                              input$B=="C3_2"|
                              input$B=="C3_3"|
                              input$B=="C3_4"|
                              input$B=="C3_5"|
                              input$B=="C3_6"|
                              input$B=="C3_7"|
                              input$B=="C3_8"|
                              input$B=="E1_1"|
                              input$B=="E1_2"|
                              input$B=="E1_3"|
                              input$B=="E1_4"|
                              input$B=="E1_5"|
                              input$B=="E1_6"|
                              input$B=="E1_7"|
                              input$B=="E1_8"|
                              input$B=="E1_9"|
                              input$B=="E11_1"|
                              input$B=="E11_2"|
                              input$B=="E11_3"|
                              input$B=="E11_4"|
                              input$B=="E11_5"|
                              input$B=="E11_6"|
                              input$B=="E11_7"|
                              input$B=="E11_8"|
                              input$B=="E11_9"|
                              input$B=="E16_1"|
                              input$B=="E16_2"|
                              input$B=="E16_3"|
                              input$B=="E16_4"|
                              input$B=="E16_5"|
                              input$B=="E16_6"|
                              input$B=="E16_7"|
                              input$B=="E16_8"|
                              input$B=="E16_9"|
                              input$B=="F7_1"|
                              input$B=="F7_2"|
                              input$B=="F7_3"|
                              input$B=="F7_4"|
                              input$B=="F7_5"|
                              input$B=="F7_6"|
                              input$B=="F7_7"|
                              input$B=="F7_8"|
                              input$B=="F7_9"|
                              input$B=="H8_1"|
                              input$B=="H8_2"|
                              input$B=="H8_3"|
                              input$B=="H8_4"|
                              input$B=="H8_5"|
                              input$B=="H8_6"|
                              input$B=="H8_7"|
                              input$B=="H8_8"|
                              input$B=="H9_1"|
                              input$B=="H9_2"|
                              input$B=="H9_3"|
                              input$B=="H9_4"|
                              input$B=="H9_6")){c("Yes","No")}else
                              {if(input$B=="age_group"){c("20-30",
                                                          "30-40",
                                                          "40_50",
                                                          "50-60",
                                                          "60-64")}else
                                                          {if(input$B=="A6"){c("2006",
                                                                               "2007",
                                                                               "2008",
                                                                               "2009",
                                                                               "2010",
                                                                               "2011",
                                                                               "2012",
                                                                               "2013",
                                                                               "2014",
                                                                               "2015",
                                                                               "2016",
                                                                               "2017",
                                                                               "2018")}else
                                                                               {if(input$B=="B2CAT"){c("In no other country",
                                                                                                       "In 1 other country",
                                                                                                       "In 2 other countries",
                                                                                                       "In 3 other countries",
                                                                                                       "In 4 or more other countries")}else
                                                                                                       {if((input$B=="B4"|
                                                                                                            input$B=="B9"|
                                                                                                            input$B=="E4"|
                                                                                                            input$B=="E5"|
                                                                                                            input$B=="E15"|
                                                                                                            input$B=="G7"|
                                                                                                            input$B=="G11")){c("Yes","No")}else
                                                                                                            {if(input$B=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                  "You moved together",
                                                                                                                                  "Your spouse/partner moved before you",
                                                                                                                                  "Your spouse/partner moved after you",
                                                                                                                                  "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                  {if(input$B=="C1"){c("Yes, certainly",
                                                                                                                                                       "Yes, probably",
                                                                                                                                                       "No, probably not",
                                                                                                                                                       "No, certainly not",
                                                                                                                                                       "I do not know yet",
                                                                                                                                                       "I have already applied for the Swiss nationality")}else
                                                                                                                                                       {if((input$B=="D1"|input$B=="D3"|input$B=="E23"|input$B=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                         "Compulsory education",
                                                                                                                                                                                                                         "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                         "Vocational education and/or training",
                                                                                                                                                                                                                         "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                         "Advanced technical and professional training",
                                                                                                                                                                                                                         "Bachelor or equivalent",
                                                                                                                                                                                                                         "Master or equivalent",
                                                                                                                                                                                                                         "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                         {if(input$B=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                              "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                              "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                              "No, it was not necessary",
                                                                                                                                                                                                                                              "No, other reasons")}else
                                                                                                                                                                                                                                              {if((input$B=="E2"|input$B=="E12"|input$B=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                   "A company owner",
                                                                                                                                                                                                                                                                                                   "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                   "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                   "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                   "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                   "An apprentice",
                                                                                                                                                                                                                                                                                                   "Master or equivalent",
                                                                                                                                                                                                                                                                                                   "A PhD student")}else
                                                                                                                                                                                                                                                                                                   {if(input$B=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                         "Limited duration",
                                                                                                                                                                                                                                                                                                                         "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                         {if(input$B=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                               "Improved slightly",
                                                                                                                                                                                                                                                                                                                                               "Remained the same",
                                                                                                                                                                                                                                                                                                                                               "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                               "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                               {if(input$B=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                      "German",
                                                                                                                                                                                                                                                                                                                                                                      "French",
                                                                                                                                                                                                                                                                                                                                                                      "Romansh",
                                                                                                                                                                                                                                                                                                                                                                      "Italian",
                                                                                                                                                                                                                                                                                                                                                                      "English",
                                                                                                                                                                                                                                                                                                                                                                      "Spanish",
                                                                                                                                                                                                                                                                                                                                                                      "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                      "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                      {if((input$B=="E25"|input$B=="G13_1"|input$B=="G13_2"|input$B=="H11_1"|input$B=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                      {if(input$B=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                           "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                           "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                           {if(input$B=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                                "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                                "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                                {if(input$B=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$B=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if((input$B=="G16_1"|input$B=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$B=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if(input$B=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Short-term residence permit (L permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Other")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               {if(input$B=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               }}}}}}}}}}}}}}}}}}}}}}}
    
    dffinal1<-df1 %>%
      subset(select=if(input$D=="Absolute"){c(1:3)}else{c(1,2,4)})%>% 
      rename("Table" = 1,"B"=2,"C"=3)%>% 
      spread(B, C)%>%
      mutate(year=2018)
    
    ##### 20 A ####         
    df1<-mar[["D20"]]%>%
      group_by(A, B)%>%
      filter(A!=-9)%>% 
      filter(A!=-8)%>% 
      filter(A!=99)%>% 
      filter(A!=-7)%>% 
      filter(B!=-9)%>% 
      filter(A!=-8)%>% 
      filter(B!=99)%>% 
      filter(B!=-7)%>% 
      summarise(n=round(sum(pop),0))%>%
      mutate(prop=round((n/sum(n)*100),2))
    
    df1[[1]]<-as.factor(df1[[1]])
    levels(df1[[1]])<- if(input$A=="sex1"){c("Male", "Female")}else
    {if(input$A=="A8"){c("Single, never married",
                         "Married",
                         "Separated",
                         "Divorced",
                         "Widowed",
                         "Registered partnership",
                         "Dissolved partnership")}else
                         {if((input$A=="A9"|
                              input$A=="B8_1"|
                              input$A=="B8_2"|
                              input$A=="B8_3"|
                              input$A=="B8_4"|
                              input$A=="B8_5"|
                              input$A=="B8_6"|
                              input$A=="B8_7"|
                              input$A=="B8_8"|
                              input$A=="B8_9"|
                              input$A=="B8_10"|
                              input$A=="B13_1"|
                              input$A=="B13_2"|
                              input$A=="B13_3"|
                              input$A=="B13_4"|
                              input$A=="B13_5"|
                              input$A=="B13_6"|
                              input$A=="B8_10"|
                              input$A=="C2_1"|
                              input$A=="C2_2"|
                              input$A=="C2_3"|
                              input$A=="C2_4"|
                              input$A=="C2_5"|
                              input$A=="C2_6"|
                              input$A=="C2_7"|
                              input$A=="C3_1"|
                              input$A=="C3_2"|
                              input$A=="C3_3"|
                              input$A=="C3_4"|
                              input$A=="C3_5"|
                              input$A=="C3_6"|
                              input$A=="C3_7"|
                              input$A=="C3_8"|
                              input$A=="E1_1"|
                              input$A=="E1_2"|
                              input$A=="E1_3"|
                              input$A=="E1_4"|
                              input$A=="E1_5"|
                              input$A=="E1_6"|
                              input$A=="E1_7"|
                              input$A=="E1_8"|
                              input$A=="E1_9"|
                              input$A=="E11_1"|
                              input$A=="E11_2"|
                              input$A=="E11_3"|
                              input$A=="E11_4"|
                              input$A=="E11_5"|
                              input$A=="E11_6"|
                              input$A=="E11_7"|
                              input$A=="E11_8"|
                              input$A=="E11_9"|
                              input$A=="E16_1"|
                              input$A=="E16_2"|
                              input$A=="E16_3"|
                              input$A=="E16_4"|
                              input$A=="E16_5"|
                              input$A=="E16_6"|
                              input$A=="E16_7"|
                              input$A=="E16_8"|
                              input$A=="E16_9"|
                              input$A=="F7_1"|
                              input$A=="F7_2"|
                              input$A=="F7_3"|
                              input$A=="F7_4"|
                              input$A=="F7_5"|
                              input$A=="F7_6"|
                              input$A=="F7_7"|
                              input$A=="F7_8"|
                              input$A=="F7_9"|
                              input$A=="H8_1"|
                              input$A=="H8_2"|
                              input$A=="H8_3"|
                              input$A=="H8_4"|
                              input$A=="H8_5"|
                              input$A=="H8_6"|
                              input$A=="H8_7"|
                              input$A=="H8_8"|
                              input$A=="H9_1"|
                              input$A=="H9_2"|
                              input$A=="H9_3"|
                              input$A=="H9_4"|
                              input$A=="H9_6")){c("Yes","No")}else
                              {if(input$A=="age_group"){c("20-30",
                                                          "30-40",
                                                          "40_50",
                                                          "50-60",
                                                          "60-64")}else
                                                          {if(input$A=="A6"){c("2006",
                                                                               "2007",
                                                                               "2008",
                                                                               "2009",
                                                                               "2010",
                                                                               "2011",
                                                                               "2012",
                                                                               "2013",
                                                                               "2014",
                                                                               "2015",
                                                                               "2016",
                                                                               "2017",
                                                                               "2018")}else
                                                                               {if(input$A=="B2CAT"){c("In no other country",
                                                                                                       "In 1 other country",
                                                                                                       "In 2 other countries",
                                                                                                       "In 3 other countries",
                                                                                                       "In 4 or more other countries")}else
                                                                                                       {if((input$A=="B4"|
                                                                                                            input$A=="B9"|
                                                                                                            input$A=="E4"|
                                                                                                            input$A=="E5"|
                                                                                                            input$A=="E15"|
                                                                                                            input$A=="G7"|
                                                                                                            input$A=="G11")){c("Yes","No")}else
                                                                                                            {if(input$A=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                  "You moved together",
                                                                                                                                  "Your spouse/partner moved before you",
                                                                                                                                  "Your spouse/partner moved after you",
                                                                                                                                  "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                  {if(input$A=="C1"){c("Yes, certainly",
                                                                                                                                                       "Yes, probably",
                                                                                                                                                       "No, probably not",
                                                                                                                                                       "No, certainly not",
                                                                                                                                                       "I do not know yet",
                                                                                                                                                       "I have already applied for the Swiss nationality")}else
                                                                                                                                                       {if((input$A=="D1"|input$A=="D3"|input$A=="E23"|input$A=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                         "Compulsory education",
                                                                                                                                                                                                                         "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                         "Vocational education and/or training",
                                                                                                                                                                                                                         "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                         "Advanced technical and professional training",
                                                                                                                                                                                                                         "Bachelor or equivalent",
                                                                                                                                                                                                                         "Master or equivalent",
                                                                                                                                                                                                                         "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                         {if(input$A=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                              "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                              "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                              "No, it was not necessary",
                                                                                                                                                                                                                                              "No, other reasons")}else
                                                                                                                                                                                                                                              {if((input$A=="E2"|input$A=="E12"|input$A=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                   "A company owner",
                                                                                                                                                                                                                                                                                                   "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                   "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                   "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                   "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                   "An apprentice",
                                                                                                                                                                                                                                                                                                   "Master or equivalent",
                                                                                                                                                                                                                                                                                                   "A PhD student")}else
                                                                                                                                                                                                                                                                                                   {if(input$A=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                         "Limited duration",
                                                                                                                                                                                                                                                                                                                         "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                         {if(input$A=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                               "Improved slightly",
                                                                                                                                                                                                                                                                                                                                               "Remained the same",
                                                                                                                                                                                                                                                                                                                                               "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                               "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                               {if(input$A=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                      "German",
                                                                                                                                                                                                                                                                                                                                                                      "French",
                                                                                                                                                                                                                                                                                                                                                                      "Romansh",
                                                                                                                                                                                                                                                                                                                                                                      "Italian",
                                                                                                                                                                                                                                                                                                                                                                      "English",
                                                                                                                                                                                                                                                                                                                                                                      "Spanish",
                                                                                                                                                                                                                                                                                                                                                                      "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                      "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                      {if((input$A=="E25"|input$A=="G13_1"|input$A=="G13_2"|input$A=="H11_1"|input$A=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                      {if(input$A=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                           "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                           "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                           {if(input$A=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                                "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                                "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                                {if(input$A=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$A=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if((input$A=="G16_1"|input$A=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$A=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if(input$A=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Short-term residence permit (L permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Other")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               {if(input$A=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               }}}}}}}}}}}}}}}}}}}}}}}
    ##### 20 B ####           
    df1[[2]]<-as.factor(df1[[2]])
    levels(df1[[2]])<- if(input$B=="sex1"){c("Male", "Female")}else
    {if(input$B=="A8"){c("Single, never married",
                         "Married",
                         "Separated",
                         "Divorced",
                         "Widowed",
                         "Registered partnership",
                         "Dissolved partnership")}else
                         {if((input$B=="A9"|
                              input$B=="B8_1"|
                              input$B=="B8_2"|
                              input$B=="B8_3"|
                              input$B=="B8_4"|
                              input$B=="B8_5"|
                              input$B=="B8_6"|
                              input$B=="B8_7"|
                              input$B=="B8_8"|
                              input$B=="B8_9"|
                              input$B=="B8_10"|
                              input$B=="B13_1"|
                              input$B=="B13_2"|
                              input$B=="B13_3"|
                              input$B=="B13_4"|
                              input$B=="B13_5"|
                              input$B=="B13_6"|
                              input$B=="B8_10"|
                              input$B=="C2_1"|
                              input$B=="C2_2"|
                              input$B=="C2_3"|
                              input$B=="C2_4"|
                              input$B=="C2_5"|
                              input$B=="C2_6"|
                              input$B=="C2_7"|
                              input$B=="C3_1"|
                              input$B=="C3_2"|
                              input$B=="C3_3"|
                              input$B=="C3_4"|
                              input$B=="C3_5"|
                              input$B=="C3_6"|
                              input$B=="C3_7"|
                              input$B=="C3_8"|
                              input$B=="E1_1"|
                              input$B=="E1_2"|
                              input$B=="E1_3"|
                              input$B=="E1_4"|
                              input$B=="E1_5"|
                              input$B=="E1_6"|
                              input$B=="E1_7"|
                              input$B=="E1_8"|
                              input$B=="E1_9"|
                              input$B=="E11_1"|
                              input$B=="E11_2"|
                              input$B=="E11_3"|
                              input$B=="E11_4"|
                              input$B=="E11_5"|
                              input$B=="E11_6"|
                              input$B=="E11_7"|
                              input$B=="E11_8"|
                              input$B=="E11_9"|
                              input$B=="E16_1"|
                              input$B=="E16_2"|
                              input$B=="E16_3"|
                              input$B=="E16_4"|
                              input$B=="E16_5"|
                              input$B=="E16_6"|
                              input$B=="E16_7"|
                              input$B=="E16_8"|
                              input$B=="E16_9"|
                              input$B=="F7_1"|
                              input$B=="F7_2"|
                              input$B=="F7_3"|
                              input$B=="F7_4"|
                              input$B=="F7_5"|
                              input$B=="F7_6"|
                              input$B=="F7_7"|
                              input$B=="F7_8"|
                              input$B=="F7_9"|
                              input$B=="H8_1"|
                              input$B=="H8_2"|
                              input$B=="H8_3"|
                              input$B=="H8_4"|
                              input$B=="H8_5"|
                              input$B=="H8_6"|
                              input$B=="H8_7"|
                              input$B=="H8_8"|
                              input$B=="H9_1"|
                              input$B=="H9_2"|
                              input$B=="H9_3"|
                              input$B=="H9_4"|
                              input$B=="H9_6")){c("Yes","No")}else
                              {if(input$B=="age_group"){c("20-30",
                                                          "30-40",
                                                          "40_50",
                                                          "50-60",
                                                          "60-64")}else
                                                          {if(input$B=="A6"){c("2006",
                                                                               "2007",
                                                                               "2008",
                                                                               "2009",
                                                                               "2010",
                                                                               "2011",
                                                                               "2012",
                                                                               "2013",
                                                                               "2014",
                                                                               "2015",
                                                                               "2016",
                                                                               "2017",
                                                                               "2018")}else
                                                                               {if(input$B=="B2CAT"){c("In no other country",
                                                                                                       "In 1 other country",
                                                                                                       "In 2 other countries",
                                                                                                       "In 3 other countries",
                                                                                                       "In 4 or more other countries")}else
                                                                                                       {if((input$B=="B4"|
                                                                                                            input$B=="B9"|
                                                                                                            input$B=="E4"|
                                                                                                            input$B=="E5"|
                                                                                                            input$B=="E15"|
                                                                                                            input$B=="G7"|
                                                                                                            input$B=="G11")){c("Yes","No")}else
                                                                                                            {if(input$B=="B10"){c("Your spouse/partner already lived in Switzerland when you met",
                                                                                                                                  "You moved together",
                                                                                                                                  "Your spouse/partner moved before you",
                                                                                                                                  "Your spouse/partner moved after you",
                                                                                                                                  "Your spouse/partner has not yet moved to Switzerland")}else
                                                                                                                                  {if(input$B=="C1"){c("Yes, certainly",
                                                                                                                                                       "Yes, probably",
                                                                                                                                                       "No, probably not",
                                                                                                                                                       "No, certainly not",
                                                                                                                                                       "I do not know yet",
                                                                                                                                                       "I have already applied for the Swiss nationality")}else
                                                                                                                                                       {if((input$B=="D1"|input$B=="D3"|input$B=="E23"|input$B=="F6")){c("No formal educational qualification",
                                                                                                                                                                                                                         "Compulsory education",
                                                                                                                                                                                                                         "Higher secondary education not giving access to universities (or similar)",
                                                                                                                                                                                                                         "Vocational education and/or training",
                                                                                                                                                                                                                         "High school-leaving certificate giving access to universities (or similar)",
                                                                                                                                                                                                                         "Advanced technical and professional training",
                                                                                                                                                                                                                         "Bachelor or equivalent",
                                                                                                                                                                                                                         "Master or equivalent",
                                                                                                                                                                                                                         "Phd Doctoral or equivalent")}else
                                                                                                                                                                                                                         {if(input$B=="D4"){c("Yes, the certificate was obtained",
                                                                                                                                                                                                                                              "Yes, but the certificate was not obtained",
                                                                                                                                                                                                                                              "Yes, but the procedure is not yet complete",
                                                                                                                                                                                                                                              "No, it was not necessary",
                                                                                                                                                                                                                                              "No, other reasons")}else
                                                                                                                                                                                                                                              {if((input$B=="E2"|input$B=="E12"|input$B=="E18")){c("Self-employed",
                                                                                                                                                                                                                                                                                                   "A company owner",
                                                                                                                                                                                                                                                                                                   "A relative employed in a family business",
                                                                                                                                                                                                                                                                                                   "Employed as director or board member and/or with managerial",
                                                                                                                                                                                                                                                                                                   "Employed without managerial responsibility",
                                                                                                                                                                                                                                                                                                   "Employed in a protected workshop (except support staff)",
                                                                                                                                                                                                                                                                                                   "An apprentice",
                                                                                                                                                                                                                                                                                                   "Master or equivalent",
                                                                                                                                                                                                                                                                                                   "A PhD student")}else
                                                                                                                                                                                                                                                                                                   {if(input$B=="E21"){c("Unlimited duration",
                                                                                                                                                                                                                                                                                                                         "Limited duration",
                                                                                                                                                                                                                                                                                                                         "You don't have a contract")}else
                                                                                                                                                                                                                                                                                                                         {if(input$B=="E27"){c("Improved substantially",
                                                                                                                                                                                                                                                                                                                                               "Improved slightly",
                                                                                                                                                                                                                                                                                                                                               "Remained the same",
                                                                                                                                                                                                                                                                                                                                               "Worsened slightly",
                                                                                                                                                                                                                                                                                                                                               "Worsened substantially")}else
                                                                                                                                                                                                                                                                                                                                               {if(input$B=="G1_1"){c("Swiss-German",
                                                                                                                                                                                                                                                                                                                                                                      "German",
                                                                                                                                                                                                                                                                                                                                                                      "French",
                                                                                                                                                                                                                                                                                                                                                                      "Romansh",
                                                                                                                                                                                                                                                                                                                                                                      "Italian",
                                                                                                                                                                                                                                                                                                                                                                      "English",
                                                                                                                                                                                                                                                                                                                                                                      "Spanish",
                                                                                                                                                                                                                                                                                                                                                                      "Portuguese",
                                                                                                                                                                                                                                                                                                                                                                      "Other language")}else
                                                                                                                                                                                                                                                                                                                                                                      {if((input$B=="E25"|input$B=="G13_1"|input$B=="G13_2"|input$B=="H11_1"|input$B=="H11_2")){c("0","1","2","3","4","5","6","7")}else
                                                                                                                                                                                                                                                                                                                                                                      {if(input$B=="G2"){c("Everything",
                                                                                                                                                                                                                                                                                                                                                                                           "Most of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Parts of a conversation",
                                                                                                                                                                                                                                                                                                                                                                                           "Some words and phrases",
                                                                                                                                                                                                                                                                                                                                                                                           "Nothing at all")}else
                                                                                                                                                                                                                                                                                                                                                                                           {if(input$B=="G3"){c("Speak fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak somewhat fluently",
                                                                                                                                                                                                                                                                                                                                                                                                                "Speak not very well",
                                                                                                                                                                                                                                                                                                                                                                                                                "Know some vocabulary",
                                                                                                                                                                                                                                                                                                                                                                                                                "Not speak the language at all")}else
                                                                                                                                                                                                                                                                                                                                                                                                                {if(input$B=="G6"){c("All your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live in Switzerland",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Approximately the same numbers of friends live in Switzerland and abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Most of your good friends live abroad",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "All your good friends live abroad")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$B=="G8"){c("Once or twice a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Three to six times a year",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Once a month",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Twice or more a month")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if((input$B=="G16_1"|input$B=="G16_2")){c("Very interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Quite interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Hardly interested",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Not at all interested")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     {if(input$B=="H3"){c("Very often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Often",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "From time to time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Never")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          {if(input$B=="A7"){c("Settlement permit (C permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Residence permit (B permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Diplomat or International Status or residence permit with gainful employment as member of diploma",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Short-term residence permit (L permit)",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Other")}else
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               {if(input$B=="CONT3"){c("EU/EFTA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "non-EU/EFTA")}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               }}}}}}}}}}}}}}}}}}}}}}}
    
    dffinal2<-df1 %>%
      subset(select=if(input$D=="Absolute"){c(1:3)}else{c(1,2,4)})%>% 
      rename("Table" = 1,"B"=2,"C"=3)%>% 
      spread(B, C)%>%
      mutate(year=2020)
    
    dffinal<-rbind(dffinal0, dffinal1,dffinal2)
    
    write.table(dffinal, file, sep = sep, row.names = FALSE)
    
    
  })

