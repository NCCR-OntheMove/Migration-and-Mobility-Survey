#### c1 Do you intend to apply for the Swiss nationality in the future? #####

output$C1_16 <-renderHighchart({ 
  #dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BC,input$magC,"year")] #input$BC,input$magC,
    
    colnames(x)<-c("C1", "pop","year")
    
    x<-x %>%
      group_by(year,C1) %>% 
      filter(C1!=-9)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((C1))
    
    x$C1<-as.factor(x$C1)
    x$C1<-fct_explicit_na(x$C1)
    
    x
  })
  
  levels(mar[["D16"]]$C1)<-c("Yes, certainly",
                             "Yes, probably",
                             "No, probably not",
                             "No, certainly not",
                             "I do not know yet",
                             "I have already applied for the Swiss nationality")
  
  levels(mar[["D18"]]$C1)<-c("Yes, certainly",
                             "Yes, probably",
                             "No, probably not",
                             "No, certainly not",
                             "I do not know yet",
                             "I have already applied for the Swiss nationality")
  
  levels(mar[["D20"]]$C1)<-c("Yes, certainly",
                             "Yes, probably",
                             "No, probably not",
                             "No, certainly not",
                             "I do not know yet",
                             "I have already applied for the Swiss nationality",
                             "I have already being naturalized")
  
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(C1, nesting(year))
  marDF$C1<-as.factor(marDF$C1)
  
  marDF[is.na(marDF)] <- 0
  
  #marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magC=="n_nw", 2500,275000)
  
  formatter<- ifelse(input$magC2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magC2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("groupz","year","prop")
  
  marDF1$year<-paste("Wave ",marDF$year, sep="")
  
  rank <-marDF1 %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magC2=="Absolute",0,0),
             max=ifelse(input$magC2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Do you intend to apply for the Swiss nationality in the future?",align = 'left')  %>%
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

#### c2 Why do you not wish to acquire the Swiss citizenship nationality? #####

output$C2_1_16 <-renderHighchart({ 
  #dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("C2_1","C2_2","C2_3","C2_4",
            "C2_5","C2_6","C2_7","C2_8",input$magC,"year")]#input$magC,
    
    colnames(x)<-c("C2_1","C2_2","C2_3","C2_4",
                   "C2_5","C2_6","C2_7","C2_8","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(C2_1,C2_2,C2_3,C2_4,
                                       C2_5,C2_6,C2_7,C2_8),
                       factor_key=FALSE)
    
    
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
    
    x1$prop<-round(x1$pop/unique(x2$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                           "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("I do not fulfill the requirements",
                          "I do not intend to stay in Switzerland for good",
                          "I do not feel a bond with Switzerland",
                          "I do not see any benefit in it",
                          "I do not want to give up current nationality",
                          "I do not want to lose my rights/benefits of my country of origin",
                          "I do not want to go through the process, which is too expensive/complicated/long",
                          "Other reasons")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magC=="n_nw", 1000,75000)
  
  formatter<- ifelse(input$magC2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magC2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("groupz","year","prop")
  
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("C1",input$magC,"year")] #input$BC,input$magC,
    
    colnames(x)<-c("C1", "pop","year")
    
    x<-x %>%
      group_by(year,C1) %>% 
      filter(C1!=-9)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((C1))
    
    x$C1<-as.factor(x$C1)
    x$C1<-fct_explicit_na(x$C1)
    
    x
  })
  
  levels(mar1[["D16"]]$C1)<-c("Yes, certainly",
                              "Yes, probably",
                              "No, probably not",
                              "No, certainly not",
                              "I do not know yet",
                              "I have already applied for the Swiss nationality")
  
  levels(mar1[["D18"]]$C1)<-c("Yes, certainly",
                              "Yes, probably",
                              "No, probably not",
                              "No, certainly not",
                              "I do not know yet",
                              "I have already applied for the Swiss nationality")
  
  levels(mar1[["D20"]]$C1)<-c("Yes, certainly",
                              "Yes, probably",
                              "No, probably not",
                              "No, certainly not",
                              "I do not know yet",
                              "I have already applied for the Swiss nationality",
                              "I have already being naturalized")
  
  
  marDFtotales<- as.data.frame(do.call("rbind", mar1))
  marDFtotales<-marDFtotales[(marDFtotales$C1=="No, probably not"|
                                marDFtotales$C1=="No, certainly not"), ]
  
  marDF1$year<-paste("Wave ",marDF$year, sep="")
  rank <-marDF1 %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magC2=="Absolute",0,0),
             max=ifelse(input$magC2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Why do you not wish to acquire the Swiss citizenship nationality?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFtotales[marDFtotales$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFtotales[marDFtotales$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFtotales[marDFtotales$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank  
  
}) 

#### C3 why you want the swiss nationality #####

output$C3_1_16 <-renderHighchart({ 
  #dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("C3_1","C3_2","C3_3","C3_4",
            "C3_5","C3_6","C3_7",input$magC,"year")]#input$magC,
    
    colnames(x)<-c("C3_1","C3_2","C3_3","C3_4",
                   "C3_5","C3_6","C3_7","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(C3_1,C3_2,C3_3,C3_4,
                                       C3_5,C3_6,C3_7),
                       factor_key=FALSE)
    
    
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
    
    x1$prop<-round(x1$pop/unique(x2$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  dmms1820 <-  dmms[2:3]
  mar2 <- lapply(dmms1820, function(x){
    
    x<-x[,c("C3_8","C3_9",input$magC,"year")]#input$magC,
    
    colnames(x)<-c("C3_8","C3_9","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(C3_8,C3_9),
                       factor_key=FALSE)
    
    
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
    
    x1$prop<-round(x1$pop/unique(x2$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  
  marDFf<-rbind(marDF,marDF2)
  
  marDFf<-marDFf %>% complete(groupz, nesting(year))
  marDFf$groupz<-as.factor(marDFf$groupz)
  #levels( marDF$groupz)
  #marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                           "B27_4","B27_5","B27_6"))
  
  levels(marDFf$groupz)<-c("My spouse/partner and/or close family members are Swiss",
                           "I wish to vote in national elections, to get involved in my local community",
                           "It makes it easier to visit my country of origin or other countries",
                           "I feel that I belong in Switzerland",
                           "It will give me better professional opportunities",
                           "It will protect me from being expulsed from Switzerland",
                           "It simplifies administrative procedures",
                           "To offer a better future to my children / family",
                           "Other reasons")
  marDFf[is.na(marDFf)] <- 0
  
  marDFf<-with(marDFf, marDFf[order(groupz),])
  
  hc_yAxis<-ifelse(input$magC=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$magC2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDFf<-if(input$magC2=="Relative"){
    marDFf[,c(1,2,4)]
  } else {
    marDFf[,c(1,2,3)]}
  
  colnames(marDFf)<-c("groupz","year","prop")
  marDFf$year<-paste("Wave ",marDFf$year, sep="")
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("C1",input$magC,"year")] #input$BC,input$magC,
    
    colnames(x)<-c("C1", "pop","year")
    
    x<-x %>%
      group_by(year,C1) %>% 
      filter(C1!=-9)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((C1))
    
    x$C1<-as.factor(x$C1)
    x$C1<-fct_explicit_na(x$C1)
    
    x
  })
  
  levels(mar1[["D16"]]$C1)<-c("Yes, certainly",
                              "Yes, probably",
                              "No, probably not",
                              "No, certainly not",
                              "I do not know yet",
                              "I have already applied for the Swiss nationality")
  
  levels(mar1[["D18"]]$C1)<-c("Yes, certainly",
                              "Yes, probably",
                              "No, probably not",
                              "No, certainly not",
                              "I do not know yet",
                              "I have already applied for the Swiss nationality")
  
  levels(mar1[["D20"]]$C1)<-c("Yes, certainly",
                              "Yes, probably",
                              "No, probably not",
                              "No, certainly not",
                              "I do not know yet",
                              "I have already applied for the Swiss nationality",
                              "I have already being naturalized")
  
  
  marDFtotales<- as.data.frame(do.call("rbind", mar1))
  marDFtotales<-marDFtotales[(marDFtotales$C1=="Yes, certainly"|
                                marDFtotales$C1=="Yes, probably"|
                                marDFtotales$C1=="I have already applied for the Swiss nationality"), ]
  
  
  rank <-marDFf %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magC2=="Absolute",0,0),
             max=ifelse(input$magC2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Why would you like to acquire the Swiss citizenship nationality?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFtotales[marDFtotales$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFtotales[marDFtotales$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFtotales[marDFtotales$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%   
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank  
  
}) 





