#### G1 Which language do you relate to and master best? #####

output$G1_1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("G1_1",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G1_1", "pop","year")
    
    
    x<-x %>%
      group_by(year,G1_1) %>% 
      filter(G1_1!="(Missing)")%>%
      filter(G1_1!=-7)%>%
      filter(G1_1!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G1_1))
    
    x$G1_1<-as.factor(x$G1_1)
    x$G1_1<-fct_explicit_na(x$G1_1)
    
    x
  })
  
  levels(mar[["D16"]]$G1_1)<-c("Swiss-German",
                               "German",
                               "French",
                               "Romansh",
                               "Italian",
                               "English",
                               "Spanish",
                               "Portuguese",
                               "Other language")
  
  levels(mar[["D18"]]$G1_1)<-c("Swiss-German",
                               "German",
                               "French",
                               "Romansh",
                               "Italian",
                               "English",
                               "Spanish",
                               "Portuguese",
                               "Other language")
  
  levels(mar[["D20"]]$G1_1)<-c("Swiss-German",
                               "German",
                               "French",
                               "Romansh",
                               "Italian",
                               "English",
                               "Spanish",
                               "Portuguese",
                               "Other language")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G1_1<-as.factor(marDF$G1_1)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G1_1", "pop")]}else{
      marDF[,c("year","G1_1", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 3000,300000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G1_1), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Language mastered best",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 

##### G2 lANGUAGE MASTERED BEST  ######

output$G2_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("G2",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G2", "pop","year")
    
    
    x<-x %>%
      group_by(year,G2) %>% 
      filter(G2!="(Missing)")%>%
      filter(G2!=-7)%>%
      filter(G2!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G2))
    
    x$G2<-as.factor(x$G2)
    x$G2<-fct_explicit_na(x$G2)
    
    x
  })
  
  levels(mar[["D16"]]$G2)<-c("Everything",
                             "Most of a conversation",
                             "Parts of a conversation",
                             "Some words and phrases",
                             "Nothing at all")
  
  levels(mar[["D18"]]$G2)<-c("Everything",
                             "Most of a conversation",
                             "Parts of a conversation",
                             "Some words and phrases",
                             "Nothing at all")
  
  levels(mar[["D20"]]$G2)<-c("Everything",
                             "Most of a conversation",
                             "Parts of a conversation",
                             "Some words and phrases",
                             "Nothing at all")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G2<-as.factor(marDF$G2)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G2", "pop")]}else{
      marDF[,c("year","G2", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 3000,300000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G2), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Language mastered best",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

##### G3 local language #####

output$G3_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("G3",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G3", "pop","year")
    
    
    x<-x %>%
      group_by(year,G3) %>% 
      filter(G3!="(Missing)")%>%
      filter(G3!=-7)%>%
      filter(G3!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G3))
    
    x$G3<-as.factor(x$G3)
    x$G3<-fct_explicit_na(x$G3)
    
    x
  })
  
  levels(mar[["D16"]]$G3)<-c("Speak fluently",
                             "Speak somewhat fluently",
                             "Speak not very well",
                             "Know some vocabulary",
                             "Not speak the language at all")
  
  levels(mar[["D18"]]$G3)<-c("Speak fluently",
                             "Speak somewhat fluently",
                             "Speak not very well",
                             "Know some vocabulary",
                             "Not speak the language at all")
  
  levels(mar[["D20"]]$G3)<-c("Speak fluently",
                             "Speak somewhat fluently",
                             "Speak not very well",
                             "Know some vocabulary",
                             "Not speak the language at all")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G3<-as.factor(marDF$G3)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G3", "pop")]}else{
      marDF[,c("year","G3", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw",  3000,350000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G3), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Do you speak the local language?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})


#### G4 Do you have relatives living in Switzerland? ####

output$G4_16 <-renderHighchart({ 
  
  
  #fst <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("G4",input$magG,"year")]
  
  colnames(x)<-c("G4","pop","year")
  
  x[is.na(x)] <- -7
  
  x<-x %>%
    group_by(year,G4) %>% 
    filter(G4!=-7)%>%
    filter(G4!=-9)%>%
    filter(G4!=-8)%>%
    summarise(pop=round(sum(pop),0))
  
  
  # })
  
  pie_single<- highchart() %>%
    hc_chart(type = 'pie') %>%
    hc_legend(enabled = TRUE) %>%
    hc_plotOptions(column = list(stacking = "normal"),
                   series = list(dataLabels = list(enabled = TRUE, 
                                                   format = '<b>{point.name}</b>: {point.percentage:.1f} %'))) %>%
    hc_add_series(data = list(
      list(y =  round(x[1,3][[1]],2), name = "Yes"),
      list(y =  round(x[2,3][[1]],2), name = "No")))%>%
    
    hc_title(text = "2016: Do you have relatives living in Switzerland?", align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x[1,3][[1]],
                                            x[2,3][[1]]))), 
                              sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    #
    hc_tooltip(pointFormat = "N: {point.y}", enabled = TRUE) 
  pie_single})

output$G4_18 <-renderText({ 
  
  x<-"Question not included in 2018 and 2020"
})

#### G5 hOEW ARE YOU RELATED TO THEM  ####

output$G5_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G5_1","G5_2","G5_3",
            "G5_4","G5_5","G5_6",
            "G5_7","G5_8","G5_9",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G5_1","G5_2","G5_3",
                   "G5_4","G5_5","G5_6",
                   "G5_7","G5_8","G5_9",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G5_1,G5_2,G5_3,
                                       G5_4,G5_5,G5_6,
                                       G5_7,G5_8,G5_9),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    
    
    x2<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))
    
    x2
    
    x1$prop<-round(x1$pop/(x2$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                           "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Parent(s)",
                          "Grandparent(s)",
                          "Brother(s) and/or sister(s)",
                          "Uncle(s) and/or aunt(s)",
                          "Cousin(s)",
                          "In-law(s)",
                          "Niece(s)/Nephew(s)",
                          "Grandchild(ren)",
                          "Other")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 500, 50000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("G5_1",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G5_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G5_1),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  marDF1<- as.data.frame(do.call("rbind", mar1))
  
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "How are they related to you?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      # paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### G20 FRIENDS IN CH  ####

output$G20_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G20",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G20",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G20),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    
    
    x2<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))
    
    x2
    
    x1$prop<-round(x1$pop/(x2$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                           "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Yes")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 7000, 700000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("G20",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G20",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G20),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  marDF1<- as.data.frame(do.call("rbind", mar1))
  
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Do you have friends or relatives living in Switzerland you can count on for practical or emotional support?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})


##### g6 Where do your very good friends live?  ######

output$G6_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("G6",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G6", "pop","year")
    
    
    x<-x %>%
      group_by(year,G6) %>% 
      filter(G6!="(Missing)")%>%
      filter(G6!=-7)%>%
      filter(G6!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G6))
    
    x$G6<-as.factor(x$G6)
    x$G6<-fct_explicit_na(x$G6)
    
    x
  })
  
  levels(mar[["D16"]]$G6)<-c("All your good friends live in Switzerland",
                             "Most of your good friends live in Switzerland",
                             "Approximately the same numbers of friends live in Switzerland and abroad",
                             "Most of your good friends live abroad",
                             "All your good friends live abroad")
  
  levels(mar[["D18"]]$G6)<-c("All your good friends live in Switzerland",
                             "Most of your good friends live in Switzerland",
                             "Approximately the same numbers of friends live in Switzerland and abroad",
                             "Most of your good friends live abroad",
                             "All your good friends live abroad")
  
  levels(mar[["D20"]]$G6)<-c("All your good friends live in Switzerland",
                             "Most of your good friends live in Switzerland",
                             "Approximately the same numbers of friends live in Switzerland and abroad",
                             "Most of your good friends live abroad",
                             "All your good friends live abroad")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G6<-as.factor(marDF$G6)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G6", "pop")]}else{
      marDF[,c("year","G6", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 3000,350000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G6), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Where do your very good friends live?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

###### g21  among the follkowing statementes #####

output$G21_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G21",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G21", "pop","year")
    
    
    x<-x %>%
      group_by(year,G21) %>% 
      filter(G21!="(Missing)")%>%
      filter(G21!=-7)%>%
      filter(G21!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G21))
    
    x$G21<-as.factor(x$G21)
    x$G21<-fct_explicit_na(x$G21)
    
    x
  })
  
  
  levels(mar[["D18"]]$G21)<-c("Most of my friends are from my country of origin",
                              "Most of my friends are foreigners, but not necessarily from my country of origin",
                              "Most of my friends are Swiss",
                              "My friends come from many different countries (including Switzerland, my country of origin, other countries)")
  
  levels(mar[["D20"]]$G21)<-c("Most of my friends are from my country of origin",
                              "Most of my friends are foreigners, but not necessarily from my country of origin",
                              "Most of my friends are Swiss",
                              "My friends come from many different countries (including Switzerland, my country of origin, other countries)")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G21<-as.factor(marDF$G21)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G21", "pop")]}else{
      marDF[,c("year","G21", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 3000,350000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G21), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Among the following statements, which best describes your situation concerning your friends here in Switzerland?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

###### g22  aHow often do you have contacts with friends or family in your country of origin by telephone, email, skype, whatsapp or other forms of online communications?#####

output$G22_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G22",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G22", "pop","year")
    
    
    x<-x %>%
      group_by(year,G22) %>% 
      filter(G22!="(Missing)")%>%
      filter(G22!=-7)%>%
      filter(G22!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G22))
    
    x$G22<-as.factor(x$G22)
    x$G22<-fct_explicit_na(x$G22)
    
    x
  })
  
  
  levels(mar[["D18"]]$G22)<-c("Every day",
                              "Many times a week",
                              "Approximately once time a week",
                              "A few times per month",
                              "Approximately once time a month",
                              "A few times per year",
                              "Once per year",
                              "Never")
  
  levels(mar[["D20"]]$G22)<-c("Every day",
                              "Many times a week",
                              "Approximately once time a week",
                              "A few times per month",
                              "Approximately once time a month",
                              "A few times per year",
                              "Once per year",
                              "Never")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G22<-as.factor(marDF$G22)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G22", "pop")]}else{
      marDF[,c("year","G22", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 3000,350000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G22), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #   paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "How often do you have contacts with friends or family in your country of origin by telephone, email, skype, whatsapp or other forms of online communications?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})


#### g7 have you visited your ghome countru ###### 

output$G7_16 <-renderHighchart({ 
  fst <- lapply(dmms, function(x){
    
    x<-x[,c(input$BG,input$magG,"year")]#input$BQ,input$mag1
    
    x[is.na(x)] <- -7
    
    colnames(x)<-c("G7","pop","year")
    x<-x %>%
      group_by(year,G7) %>% 
      filter(G7!=-7)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))
    x
  })
  
  fstDF<- as.data.frame(do.call("rbind", fst))
  fstDF<-fstDF[fstDF$G7==1,]
  fstDF$G7<-as.factor(fstDF$G7)
  levels(fstDF$G7)<-"Yes"
  
  
  data<- if(input$magG2=="Absolute"){ 
    fstDF[,c("year","G7", "pop")]}else{
      fstDF[,c("year","G7", "prop")]}
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 7000,700000)
  
  rank <- highchart() %>%
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = factor(data$G7), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Have you visited your country of origin?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(c(fst[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(fst[[2]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(fst[[3]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 

###### g8  How often have you visited your home country of origin?#####

output$G8_16 <-renderHighchart({ 
  
  #dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("G8",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G8", "pop","year")
    
    
    x<-x %>%
      group_by(year,G8) %>% 
      filter(G8!="(Missing)")%>%
      filter(G8!=-7)%>%
      filter(G8!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G8))
    
    x$G8<-as.factor(x$G8)
    x$G8<-fct_explicit_na(x$G8)
    
    x
  })
  
  levels(mar[["D16"]]$G8)<-c("Once or twice a year",
                             "Three to six times a year",
                             "Once a month",
                             "Twice or more a month")
  
  
  levels(mar[["D18"]]$G8)<-c("Once or twice a year",
                             "Three to six times a year",
                             "Once a month",
                             "Twice or more a month")
  
  levels(mar[["D20"]]$G8)<-c("Once or twice a year",
                             "Three to six times a year",
                             "Once a month",
                             "Twice or more a month")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G8<-as.factor(marDF$G8)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G8", "pop")]}else{
      marDF[,c("year","G8", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw",4000,400000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G8), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "How often have you visited your home country of origin?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#### g23 Is there a place (family house, apartment) in your country of origin that you own or rent and where you can go when you stay there? ###### 

output$G23_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  fst <- lapply(dmms1820, function(x){
    
    x<-x[,c("G23",input$magG,"year")]#input$BQ,input$mag1
    
    x[is.na(x)] <- -7
    
    colnames(x)<-c("G23","pop","year")
    x<-x %>%
      group_by(year,G23) %>% 
      filter(G23!=-7)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))
    x
  })
  
  fstDF<- as.data.frame(do.call("rbind", fst))
  fstDF<-fstDF[fstDF$G23==1,]
  fstDF$G23<-as.factor(fstDF$G23)
  levels(fstDF$G23)<-"Yes"
  
  
  data<- if(input$magG2=="Absolute"){ 
    fstDF[,c("year","G23", "pop")]}else{
      fstDF[,c("year","G23", "prop")]}
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 7000,700000)
  
  rank <- highchart() %>%
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = factor(data$G23), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Is there a place (family house, apartment) in your country of origin that you own or rent and where you can go when you stay there?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(c(fst[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(fst[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(fst[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 

#### G9 When you visit your home country of origin [B1], do you mainly stay.#####


output$G9_16 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("G9",input$magG,"year")]
  
  colnames(x)<-c("G9", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(G9) %>% 
    filter(G9!=-9)%>% 
    filter(G9!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((G9))
  
  x$G9<-as.factor(x$G9)
  x$G9<-fct_explicit_na(x$G9)
  
  x
  # })
  
  levels(x$G9)<-c("With friends",
                  "In your own apartment",
                  "With family or relatives",
                  "In a hotel or vacation rental")
  
  
  data<- if(input$magG2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 4500,450000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$G9), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "2016: When you visit your country of origin, do you mainly stay",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(1)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank})



#### G10 How at home do you feel on your visits to your home country of origin [B1]? #####

output$G10_16 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("G10",input$magG,"year")]
  
  colnames(x)<-c("G10", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(G10) %>% 
    filter(G10!=-9)%>% 
    filter(G10!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((G10))
  
  x$G10<-as.factor(x$G10)
  x$G10<-fct_explicit_na(x$G10)
  
  x
  # })
  
  levels(x$G10)<-c("I feel right at home from the first day on",
                   "I feel at home after getting acclimatized",
                   "Even after a longer period, I still feel like a tourist or visitor")
  
  
  data<- if(input$magG2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 4000,300000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$G10), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "2016: How do you feel at home on your visits to your home country of origin",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(1)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank})

#### g11 Ihave you ever been visited by family of friends from your home country of origin [B1]? ###### 

output$G11_16 <-renderHighchart({ 
  
  #dmms1820 <-  dmms[2:3]
  fst <- lapply(dmms, function(x){
    
    x<-x[,c("G11",input$magG,"year")]#input$BQ,input$mag1
    
    x[is.na(x)] <- -7
    
    colnames(x)<-c("G11","pop","year")
    x<-x %>%
      group_by(year,G11) %>% 
      filter(G11!=-7)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))
    x
  })
  
  fstDF<- as.data.frame(do.call("rbind", fst))
  fstDF<-fstDF[fstDF$G11==1,]
  fstDF$G11<-as.factor(fstDF$G11)
  levels(fstDF$G11)<-"Yes"
  
  
  data<- if(input$magG2=="Absolute"){ 
    fstDF[,c("year","G11", "pop")]}else{
      fstDF[,c("year","G11", "prop")]}
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 7000,700000)
  
  rank <- highchart() %>%
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = factor(data$G11), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Have you ever been visited by family of friends from your home country of origin?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(c(fst[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(fst[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(fst[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})   

#### G12 Do you have a place of residence abroad outside of Switzerland where you live for three or more months per year? ####

output$G12_16 <-renderHighchart({ 
  
  
  #fst <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("G12",input$magG,"year")]
  
  colnames(x)<-c("G12","pop","year")
  
  x[is.na(x)] <- -7
  
  x<-x %>%
    group_by(year,G12) %>% 
    filter(G12!=-7)%>%
    filter(G12!=-9)%>%
    filter(G12!=-8)%>%
    summarise(pop=round(sum(pop),0))
  
  
  # })
  
  pie_single<- highchart() %>%
    hc_chart(type = 'pie') %>%
    hc_legend(enabled = TRUE) %>%
    hc_plotOptions(column = list(stacking = "normal"),
                   series = list(dataLabels = list(enabled = TRUE, 
                                                   format = '<b>{point.name}</b>: {point.percentage:.1f} %'))) %>%
    hc_add_series(data = list(
      list(y =  round(x[1,3][[1]],2), name = "Yes"),
      list(y =  round(x[2,3][[1]],2), name = "No")))%>%
    
    hc_title(text = "2016: Place of residence abroad outside of Switzerland", align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x[1,3][[1]],
                                            x[2,3][[1]]))), 
                              sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    #
    hc_tooltip(pointFormat = "N: {point.y}", enabled = TRUE) 
  pie_single})

output$G12_18 <-renderText({ 
  
  x<-"Question not included in 2018 and 2020"
})  

#### G13  FROM 0 TO 7 #####

output$G13_1_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("G13_1",input$magG,"year")]
    colnames(x)<-c("G13_1","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G13_1) %>% 
      filter(G13_1!=-7)%>%
      filter(G13_1!=-9)%>%
      filter(G13_1!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G13_1, nesting(year))
  marDF$G13_1<-as.factor(marDF$G13_1)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G13_1)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G13_1),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",2500,250000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Are you interested in news and current events in Switzerland?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

output$G13_2_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("G13_2",input$magG,"year")]
    colnames(x)<-c("G13_2","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G13_2) %>% 
      filter(G13_2!=-7)%>%
      filter(G13_2!=-9)%>%
      filter(G13_2!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G13_2, nesting(year))
  marDF$G13_2<-as.factor(marDF$G13_2)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G13_2)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G13_2),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",2500,250000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Are you interested in news and current events in your country?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

#### G14 Have you engaged in voluntary work for one of the following associations or charity organizations during the last 12 months in .? ####

output$G14_1_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G14_1_1","G14_1_2","G14_1_3",
            "G14_1_4","G14_1_5","G14_1_6",
            "G14_1_7","G14_1_8",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G14_1_1","G14_1_2","G14_1_3",
                   "G14_1_4","G14_1_5","G14_1_6",
                   "G14_1_7","G14_1_8",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G14_1_1,G14_1_2,G14_1_3,
                                       G14_1_4,G14_1_5,G14_1_6,
                                       G14_1_7,G14_1_8),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    
    
    x2<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))
    
    x2
    
    x1$prop<-round(x1$pop/(x2$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                           "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Sports or recreational organizatio",
                          "Cultural association linked to your home country of origin",
                          "Social and charitable institutions or non-profit activity",
                          "Religious organization/community or spiritual group",
                          "Political party, political or public office, interest group, trade unions",
                          "Cultural groups: voluntary work in an orchestra, choir, theatre, film or exhibition association",
                          "Other organizations or associations",
                          "You have not engaged in any voluntary work")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 3500, 300000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("G14_1_1",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G14_1_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G14_1_1),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  marDF1<- as.data.frame(do.call("rbind", mar1))
  
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Have you engaged in voluntary work for one of the following associations or charity organizations during the last 12 months in Switzerland?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      #   paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      #  paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### G14b Have you engaged in voluntary work for one of the following associations or charity organizations during the last 12 months in .? ####

output$G14_2_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G14_2_1","G14_2_2","G14_2_3",
            "G14_2_4","G14_2_5","G14_2_6",
            "G14_2_7","G14_2_8",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G14_2_1","G14_2_2","G14_2_3",
                   "G14_2_4","G14_2_5","G14_2_6",
                   "G14_2_7","G14_2_8",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G14_2_1,G14_2_2,G14_2_3,
                                       G14_2_4,G14_2_5,G14_2_6,
                                       G14_2_7,G14_2_8),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    
    
    x2<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))
    
    x2
    
    x1$prop<-round(x1$pop/(x2$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                           "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Sports or recreational organizatio",
                          "Cultural association linked to your home country of origin",
                          "Social and charitable institutions or non-profit activity",
                          "Religious organization/community or spiritual group",
                          "Political party, political or public office, interest group, trade unions",
                          "Cultural groups: voluntary work in an orchestra, choir, theatre, film or exhibition association",
                          "Other organizations or associations",
                          "You have not engaged in any voluntary work")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 4000, 350000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("G14_2_1",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G14_2_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G14_2_1),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  marDF1<- as.data.frame(do.call("rbind", mar1))
  
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Have you engaged in voluntary work for one of the following associations or charity organizations during the last 12 months in your country?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      # paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})



#### G15 If there was a general election tomorrow in Switzerland [if A3 NE CH (8100) add "and you had the right to vote"] would you vote?#####

output$G15_16 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("G15",input$magG,"year")]
  
  colnames(x)<-c("G15", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(G15) %>% 
    filter(G15!=-9)%>% 
    filter(G15!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((G15))
  
  x$G15<-as.factor(x$G15)
  x$G15<-fct_explicit_na(x$G15)
  
  x
  # })
  
  levels(x$G15)<-c("No, I would not vote",
                   "Yes, I would vote",
                   "I do not know")
  
  
  data<- if(input$magG2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 5000,400000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$G15), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "2016: Would you vote",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(1)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank})

#### G6 How interested would you say you are in politics in ... #####

output$G16_1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("G16_1",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G16_1", "pop","year")
    
    
    x<-x %>%
      group_by(year,G16_1) %>% 
      filter(G16_1!="(Missing)")%>%
      filter(G16_1!=-7)%>%
      filter(G16_1!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G16_1))
    
    x$G16_1<-as.factor(x$G16_1)
    x$G16_1<-fct_explicit_na(x$G16_1)
    
    x
  })
  
  levels(mar[["D16"]]$G16_1)<-c("Very interested",
                                "Quite interested",
                                "Hardly interested",
                                "Not at all interested")
  
  levels(mar[["D18"]]$G16_1)<-c("Very interested",
                                "Quite interested",
                                "Hardly interested",
                                "Not at all interested")
  
  levels(mar[["D20"]]$G16_1)<-c("Very interested",
                                "Quite interested",
                                "Hardly interested",
                                "Not at all interested")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G16_1<-as.factor(marDF$G16_1)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G16_1", "pop")]}else{
      marDF[,c("year","G16_1", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 3000,300000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G16_1), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "How interested would you say you are in politics in Switzerland",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 

output$G16_2_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("G16_2",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G16_2", "pop","year")
    
    
    x<-x %>%
      group_by(year,G16_2) %>% 
      filter(G16_2!="(Missing)")%>%
      filter(G16_2!=-7)%>%
      filter(G16_2!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G16_2))
    
    x$G16_2<-as.factor(x$G16_2)
    x$G16_2<-fct_explicit_na(x$G16_2)
    
    x
  })
  
  levels(mar[["D16"]]$G16_2)<-c("Very interested",
                                "Quite interested",
                                "Hardly interested",
                                "Not at all interested")
  
  levels(mar[["D18"]]$G16_2)<-c("Very interested",
                                "Quite interested",
                                "Hardly interested",
                                "Not at all interested")
  
  levels(mar[["D20"]]$G16_2)<-c("Very interested",
                                "Quite interested",
                                "Hardly interested",
                                "Not at all interested")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G16_2<-as.factor(marDF$G16_2)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G16_2", "pop")]}else{
      marDF[,c("year","G16_2", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 3000,300000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G16_2), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "How interested would you say you are in politics in your country",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 


#### G17 There are different ways of trying to improve things or help prevent things from going wrong  ####

#input<-data.frame(magG="weight",magG2="Relative")
output$G17_1_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G17_1_1","G17_1_2","G17_1_3",
            "G17_1_4","G17_1_5","G17_1_6",
            "G17_1_7","G17_1_8","G17_1_9",
            "G17_1_10","G17_1_11",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G17_1_1","G17_1_2","G17_1_3",
                   "G17_1_4","G17_1_5","G17_1_6",
                   "G17_1_7","G17_1_8","G17_1_9",
                   "G17_1_10","G17_1_11",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G17_1_1,G17_1_2,G17_1_3,
                                       G17_1_4,G17_1_5,G17_1_6,
                                       G17_1_7,G17_1_8,G17_1_9,
                                       G17_1_10,G17_1_11),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    
    
    x2<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))
    
    x2
    
    x1$prop<-round(x1$pop/(x2$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("G17_1_1","G17_1_2","G17_1_3",
                                              "G17_1_4","G17_1_5","G17_1_6",
                                              "G17_1_7","G17_1_8","G17_1_9",
                                              "G17_1_10","G17_1_11"))
  
  levels(marDF$groupz)<-c("Contacted a politician, government or local government official",
                          "Made a donation to a political campaign or to a political party",
                          "Worked in a political party or action group",
                          "Worked in another organization or association",
                          "Worn or displayed a campaign badge/sticker",
                          "Signed a petition",
                          "Taken part in a lawful public demonstration",
                          "Boycotted certain products",
                          "Used the Internet in order to communicate about some activity",
                          "Other",
                          "You did not do anything")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 5000, 350000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("G17_1_1",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G17_1_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G17_1_1),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  marDF1<- as.data.frame(do.call("rbind", mar1))
  
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "During the last 12 months, have you done any of the following? (In Switzerland)",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      # paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### G17b There are different ways of trying to improve things or help prevent things from going wrong  ####

output$G17_2_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G17_2_1","G17_2_2","G17_2_3",
            "G17_2_4","G17_2_5","G17_2_6",
            "G17_2_7","G17_2_8","G17_2_9",
            "G17_2_10","G17_2_11",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G17_2_1","G17_2_2","G17_2_3",
                   "G17_2_4","G17_2_5","G17_2_6",
                   "G17_2_7","G17_2_8","G17_2_9",
                   "G17_2_10","G17_2_11",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G17_2_1,G17_2_2,G17_2_3,
                                       G17_2_4,G17_2_5,G17_2_6,
                                       G17_2_7,G17_2_8,G17_2_9,
                                       G17_2_10,G17_2_11),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    
    
    x2<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))
    
    x2
    
    x1$prop<-round(x1$pop/(x2$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("G17_2_1","G17_2_2","G17_2_3",
                                              "G17_2_4","G17_2_5","G17_2_6",
                                              "G17_2_7","G17_2_8","G17_2_9",
                                              "G17_2_10","G17_2_11"))
  
  levels(marDF$groupz)<-c("Contacted a politician, government or local government official",
                          "Made a donation to a political campaign or to a political party",
                          "Worked in a political party or action group",
                          "Worked in another organization or association",
                          "Worn or displayed a campaign badge/sticker",
                          "Signed a petition",
                          "Taken part in a lawful public demonstration",
                          "Boycotted certain products",
                          "Used the Internet in order to communicate about some activity",
                          "Other",
                          "You did not do anything")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw", 5000, 350000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("G17_2_1",
            input$magG,"year")]#iinput$magG,
    
    colnames(x)<-c("G17_2_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(G17_2_1),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  marDF1<- as.data.frame(do.call("rbind", mar1))
  
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "During the last 12 months, have you done any of the following? (In your country)",align = 'left')  %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      # paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### G18  FROM 0 TO 10 #####

output$G18_1_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms[1], function(x){
    
    x<-x[,c("G18_1",input$magG,"year")]
    colnames(x)<-c("G18_1","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G18_1) %>% 
      filter(G18_1!=-7)%>%
      filter(G18_1!=-9)%>%
      filter(G18_1!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G18_1, nesting(year))
  marDF$G18_1<-as.factor(marDF$G18_1)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G18_1)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G18_1),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",2000,100000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    #hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,25))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "How confident are you in your own ability to participate in politics in your home country? (In Switzerland)",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      # paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      #paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

output$G18_2_16 <-renderHighchart({ 
  
  #input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms[1], function(x){
    
    x<-x[,c("G18_2",input$magG,"year")]
    colnames(x)<-c("G18_2","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G18_2) %>% 
      filter(G18_2!=-7)%>%
      filter(G18_2!=-9)%>%
      filter(G18_2!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G18_2, nesting(year))
  marDF$G18_2<-as.factor(marDF$G18_2)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G18_2)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G18_2),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",2000,100000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    #    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    #   hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,25))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "How confident are you in your own ability to participate in politics in your home country? (In your country)",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #    paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      #   paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

#### G19  FROM 0 TO 10 #####

output$G19_1_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms[1], function(x){
    
    x<-x[,c("G19_1",input$magG,"year")]
    colnames(x)<-c("G19_1","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G19_1) %>% 
      filter(G19_1!=-7)%>%
      filter(G19_1!=-9)%>%
      filter(G19_1!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G19_1, nesting(year))
  marDF$G19_1<-as.factor(marDF$G19_1)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G19_1)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G19_1),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",2000,100000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    #hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    #hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,25))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "How easy do you personally find it to take part in politics ...?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      # paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      #paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

output$G19_2_16 <-renderHighchart({ 
  
  #input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms[1], function(x){
    
    x<-x[,c("G19_2",input$magG,"year")]
    colnames(x)<-c("G19_2","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G19_2) %>% 
      filter(G19_2!=-7)%>%
      filter(G19_2!=-9)%>%
      filter(G19_2!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G19_2, nesting(year))
  marDF$G19_2<-as.factor(marDF$G19_2)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G19_2)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G19_2),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",2000,100000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    #    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    #   hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,25))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "How easy do you personally find it to take part in politics ...?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #    paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      #   paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})


#### G26  FROM 0 TO 7 #####

output$G26_1_18 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  dmms1820<-dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G26_1",input$magG,"year")]
    colnames(x)<-c("G26_1","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G26_1) %>% 
      filter(G26_1!=-7)%>%
      filter(G26_1!=-9)%>%
      filter(G26_1!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G26_1, nesting(year))
  marDF$G26_1<-as.factor(marDF$G26_1)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G26_1)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G26_1),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",3000,300000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Feeling homesick",align = 'left')  %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

output$G26_2_18 <-renderHighchart({ 
  
  #input<-data.frame(magG="weight",magG2="Relative")
  dmms1820<-dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G26_2",input$magG,"year")]
    colnames(x)<-c("G26_2","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G26_2) %>% 
      filter(G26_2!=-7)%>%
      filter(G26_2!=-9)%>%
      filter(G26_2!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G26_2, nesting(year))
  marDF$G26_2<-as.factor(marDF$G26_2)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G26_2)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G26_2),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",3000,300000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Communicating with the local population",align = 'left')  %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

output$G26_3_18 <-renderHighchart({ 
  
  #input<-data.frame(magG="weight",magG2="Relative")
  dmms1820<-dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G26_3",input$magG,"year")]
    colnames(x)<-c("G26_3","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G26_3) %>% 
      filter(G26_3!=-7)%>%
      filter(G26_3!=-9)%>%
      filter(G26_3!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G26_3, nesting(year))
  marDF$G26_3<-as.factor(marDF$G26_3)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G26_3)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G26_3),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",3000,300000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Adapting oneself to the Swiss way of life",align = 'left')  %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

output$G26_4_18 <-renderHighchart({ 
  
  #input<-data.frame(magG="weight",magG2="Relative")
  dmms1820<-dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G26_4",input$magG,"year")]
    colnames(x)<-c("G26_4","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,G26_4) %>% 
      filter(G26_4!=-7)%>%
      filter(G26_4!=-9)%>%
      filter(G26_4!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(G26_4, nesting(year))
  marDF$G26_4<-as.factor(marDF$G26_4)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$G26_4)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(G26_4),])
  
  hc_yAxis<-ifelse(input$magG=="n_nw",3000,300000)
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magG2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Finding friends in Switzerland",align = 'left')  %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})


#####G27  among the follkowing statementes #####

output$G27_1_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G27_1",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G27_1", "pop","year")
    
    
    x<-x %>%
      group_by(year,G27_1) %>% 
      filter(G27_1!="(Missing)")%>%
      filter(G27_1!=-7)%>%
      filter(G27_1!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G27_1))
    
    x$G27_1<-as.factor(x$G27_1)
    x$G27_1<-fct_explicit_na(x$G27_1)
    
    x
  })
  
  
  levels(mar[["D18"]]$G27_1)<-c("Disagree totall",
                                "Disagree partially",
                                "Agree partially",
                                "Agree totally")
  
  levels(mar[["D20"]]$G27_1)<-c("Disagree totall",
                                "Disagree partially",
                                "Agree partially",
                                "Agree totally")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G27_1<-as.factor(marDF$G27_1)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G27_1", "pop")]}else{
      marDF[,c("year","G27_1", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw",  4500,450000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G27_1), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Globally, I feel myself belonging to the Swiss society",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

output$G27_2_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G27_2",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G27_2", "pop","year")
    
    
    x<-x %>%
      group_by(year,G27_2) %>% 
      filter(G27_2!="(Missing)")%>%
      filter(G27_2!=-7)%>%
      filter(G27_2!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G27_2))
    
    x$G27_2<-as.factor(x$G27_2)
    x$G27_2<-fct_explicit_na(x$G27_2)
    
    x
  })
  
  
  levels(mar[["D18"]]$G27_2)<-c("Disagree totall",
                                "Disagree partially",
                                "Agree partially",
                                "Agree totally")
  
  levels(mar[["D20"]]$G27_2)<-c("Disagree totall",
                                "Disagree partially",
                                "Agree partially",
                                "Agree totally")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G27_2<-as.factor(marDF$G27_2)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G27_2", "pop")]}else{
      marDF[,c("year","G27_2", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw",  4500,450000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G27_2), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Globally, I feel myself totally accepted by the society in Switzerland",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#####G29  how is your health #####

output$G28_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("G28",input$magG,"year")]#input$BF,input$magG,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("G28", "pop","year")
    
    
    x<-x %>%
      group_by(year,G28) %>% 
      filter(G28!="(Missing)")%>%
      filter(G28!=-7)%>%
      filter(G28!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((G28))
    
    x$G28<-as.factor(x$G28)
    x$G28<-fct_explicit_na(x$G28)
    
    x
  })
  
  
  levels(mar[["D18"]]$G28)<-c("Very Good",
                              "Good",
                              "Fair",
                              "Bad",
                              "Very bad")
  
  levels(mar[["D20"]]$G28)<-c("Very Good",
                              "Good",
                              "Fair",
                              "Bad",
                              "Very bad",
                              "Do not know")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$G28<-as.factor(marDF$G28)
  
  data<- if(input$magG2=="Absolute"){ 
    marDF[,c("year","G28", "pop")]}else{
      marDF[,c("year","G28", "prop")]}
  
  hc_yAxis<-ifelse(input$magG=="n_nw",  4500,450000)
  
  
  formatter<- ifelse(input$magG2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$G28), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "How is your health in general?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})







