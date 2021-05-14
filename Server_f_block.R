#### f17    Where does your spouse/partner usually live? ######
output$F17_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BF,input$magF,"year")]
    #x<-x[,c("E33","weight","year")]
    colnames(x)<-c("F17", "pop","year")
    x<-x %>%
      filter(F17!=-7)%>%
      filter(F17!=-9)%>%
      filter(F17!=-8)%>%
      group_by(year,F17) %>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((F17))
    
    x$F17<-as.factor(x$F17)
    x$F17<-fct_explicit_na(x$F17)
    
    x
  })
  
  
  
  
  levels(mar[["D18"]]$F17)<-c("With me, in the same household",
                              "In Switzerland, in another household",
                              "Abroad")
  
  levels(mar[["D20"]]$F17)<-c("With me, in the same household",
                              "In Switzerland, in another household",
                              "Abroad",
                              "Partly in Switzerland with me, partly abroad")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$F17<-as.factor(marDF$F17)
  
  data<- if(input$magF2=="Absolute"){ 
    marDF[,c("year","F17", "pop")]}else{
      marDF[,c("year","F17", "prop")]}
  
  hc_yAxis<-ifelse(input$magF=="n_nw",  6000,600000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$F17), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Where does your spouse/partner live?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 

##### F18 Which is, among the following proposals, the one which fits the best with your current situation? ######
output$F18_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BF,input$magF,"year")]
    #x<-x[,c("E33","weight","year")]
    colnames(x)<-c("F18", "pop","year")
    x<-x %>%
      filter(F18!=-7)%>%
      filter(F18!=-9)%>%
      filter(F18!=-8)%>%
      group_by(year,F18) %>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((F18))
    
    x$F18<-as.factor(x$F18)
    x$F18<-fct_explicit_na(x$F18)
    
    x
  })
  
  
  
  
  levels(mar[["D18"]]$F18)<-c("I go regularly abroad to meet my partner",
                              "My partner regularly travels to Switzerland to meet me",
                              "We meet each other sometimes in Switzerland, sometimes abroad",
                              "We rarely meet, only seeing each other during holidays",
                              "I did not have the opportunity to meet my partner since my arrival in Switzerland",
                              "Other situation")
  
  levels(mar[["D20"]]$F18)<-c("I go regularly abroad to meet my partner",
                              "My partner regularly travels to Switzerland to meet me",
                              "We meet each other sometimes in Switzerland, sometimes abroad",
                              "We rarely meet, only seeing each other during holidays",
                              "I did not have the opportunity to meet my partner since my arrival in Switzerland",
                              "Other situation")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$F18<-as.factor(marDF$F18)
  
  data<- if(input$magF2=="Absolute"){ 
    marDF[,c("year","F18", "pop")]}else{
      marDF[,c("year","F18", "prop")]}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 250,25000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$F18), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Which is, among the following proposals, the one which fits the best with your current situation?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 


#### F19 What are the reasons explaining why your spouse/partner lives abroad ? #####
output$F19_1_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("F19_1","F19_2","F19_3",
            "F19_4","F19_5","F19_6","F19_7",
            input$magF,"year")]#input$magC,
    
    colnames(x)<-c("F19_1","F19_2","F19_3",
                   "F19_4","F19_5","F19_6","F19_7",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F19_1,F19_2,F19_3,
                                       F19_4,F19_5,F19_6,F19_7),
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
  
  levels(marDF$groupz)<-c("For professional or educational reasons",
                          "For family reasons",
                          "He/she (still) has not obtained a residence permit in Switzerland",
                          "He/she wants to stay in his/her country",
                          "He/she does not want to live in Switzerland",
                          "For monetary reasons",
                          "For other reasons")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 250, 25000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magF2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("F19_1",input$magF,"year")]#input$magC,
    
    colnames(x)<-c("F19_1","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F19_1),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    
    
    x1
  })
  
  marDFtotales<- as.data.frame(do.call("rbind", mar1))
  
  
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What are the reasons explaining why your spouse/partner lives abroad?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDFtotales[marDFtotales$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFtotales[marDFtotales$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFtotales[marDFtotales$year==2020,"pop"])),sep=": "),
      sep=" | ")) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(2,3)]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})


##### F1 Including yourself, how many people live in your household? #####
output$F1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BF,input$magF,"year")]
    colnames(x)<-c("F1", "pop", "year")
    
    x[is.na(x)] <- -9
    x<-x%>%
      filter(F1!=-9)%>%
      filter(F1!=-7)%>%
      filter(F1!=0)
    
    x$F1CAT<- with(x, ifelse(F1==1,"1 person",
                             ifelse(F1==2,"2 people",
                                    ifelse(F1==3,"3 people",
                                           ifelse(F1==4,"4 people",
                                                  ifelse(F1 >=5,"5 or more people",0))))))
    
    colnames(x)<-c("F1", "pop", "year","F1CAT")
    
    x<-x %>%
      group_by(year,F1CAT) %>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((F1CAT))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$F1CAT<-as.factor(marDF$F1CAT)
  
  data<- if(input$magF2=="Absolute"){ 
    marDF[,c("year","F1CAT", "pop")]}else{
      marDF[,c("year","F1CAT", "prop")]}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 3000,250000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$F1CAT), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Including yourself, how many people live in your household?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(mar[[2]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(mar[[3]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#### F2 COUNTRY OF LIVING PARTNER/SPOUSE ####

output$F2_16 <-renderHighchart({ 
  
  #hom <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("F2",input$magF,"year")]
  colnames(x)<-c("F2","pop","year")
  x<-as.data.frame(x)
  x = data.frame(x, countries[match(x[,"F2"],
                                    countries[,"A3"]),c("official_title_en")])
  colnames(x)[length(names(x))]<-paste("F2","B",sep="")
  x$F2B<-as.factor(x$F2B)
  x$F2B<-fct_explicit_na(x$F2B)
  x[is.na(x)] <- -9
  colnames(x)<-c("B1","pop","year","B1B")
  
  x<-x %>%
    group_by(year,B1B) %>% 
    filter(B1B!="(Missing)")%>% 
    filter(B1B!="-9")%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange(desc(pop))%>% 
    head(20)
  x
  # })
  
  data<- if(input$magF2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 5000,350000)
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$B1B), title = list(text = '')) %>%
    hc_yAxis(title = list(text = 'N')) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", 
                  data = data) %>%
    hc_title(text = "2016: Top 20 countries partner/spouse currently live",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,100))%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### F3 Does your SPOUSE/PARTENER live in the same household? #####

output$F3_16 <-renderHighchart({ 
  
  
  # fst <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("F3",input$magF,"year")]
  
  colnames(x)<-c("F3","pop","year")
  
  x[is.na(x)] <- -7
  
  x<-x %>%
    group_by(year,F3) %>% 
    filter(F3!=-7)%>%
    filter(F3!=-9)%>%
    filter(F3!=-8)%>%
    summarise(pop=round(sum(pop),0))
  
  
  #})
  
  pie_single<- highchart() %>%
    hc_chart(type = 'pie') %>%
    hc_legend(enabled = TRUE) %>%
    hc_plotOptions(column = list(stacking = "normal"),
                   series = list(dataLabels = list(enabled = TRUE, 
                                                   format = '<b>{point.name}</b>: {point.percentage:.1f} %'))) %>%
    hc_add_series(data = list(
      list(y =  round(x[1,3][[1]],2), name = "Yes"),
      list(y =  round(x[2,3][[1]],2), name = "No")))%>%
    
    hc_title(text = "2016: Spouse/partner living in same household", align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x[1,3][[1]],
                                            x[2,3][[1]]))), 
                              sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    #
    hc_tooltip(pointFormat = "N: {point.y}", enabled = TRUE) 
  pie_single
})

output$F3_18 <-renderText({ 
  
  x<-"Question not included in 2018 and 2020"
})


#### f4 In which country was your  "partner"] born?######

output$F4_16 <-renderHighchart({ 
  
  nac <- lapply(dmms, function(x){
    
    x<-x[,c(input$BF,input$magF,"year")]
    colnames(x)<-c("F4","pop","year")
    x<-as.data.frame(x)
    x = data.frame(x, countries[match(x[,"F4"],
                                      countries[,"A3"]),c("official_title_en")])
    colnames(x)[length(names(x))]<-paste("F4","B",sep="")
    x$F4B<-as.factor(x$F4B)
    x$F4B<-fct_explicit_na(x$F4B)
    
    colnames(x)<-c("F4","pop","year","F4B")
    
    x<-x %>%
      group_by(year,F4B) %>% 
      filter(F4B!="(Missing)")%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange(desc(pop))%>% 
      head(20)
    x
  })
  
  
  nacdf<- as.data.frame(do.call("rbind", nac))
  
  nacdf<-nacdf[nacdf$year==input$yearF,]
  
  data<- if(input$magF2=="Absolute"){ 
    nacdf[,c("year","F4B","pop")]}else{
      nacdf[,c("year","F4B", "prop")]}
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 700,175000)
  
  color<-ifelse(input$yearF==2016,1,
                ifelse(input$yearF==2018,2,3))
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = as.character(data$F4B), title = list(text = '')) %>%
    #hc_yAxis(title = list(text = 'N')) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(data = data[,3])%>%
    hc_title(text = paste("Top 20 countries of birth:",input$yearF,sep=" "),
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(nac[[
      
      if(input$yearF==2016){ 
        1}else{
          if(input$yearF==2018){   
            2}else{3}}
      
    ]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,35))%>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[color]))
  rank
}) 




#### f6 Highest level of education spouse/partne#####

output$F6_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BF,input$magF,"year")]#input$BF,input$magF,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("F6", "pop","year")
    
    
    x<-x %>%
      group_by(year,F6) %>% 
      filter(F6!="(Missing)")%>%
      filter(F6!=-7)%>%
      filter(F6!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((F6))
    
    x$F6<-as.factor(x$F6)
    x$F6<-fct_explicit_na(x$F6)
    
    x
  })
  
  levels(mar[["D16"]]$F6)<-c("No formal educational qualification",
                             "Compulsory education",
                             "Higher secondary education not giving access to universities (or similar)",
                             "Vocational education and/or training",
                             "High school-leaving certificate giving access to universities (or similar)",
                             "Advanced technical and professional training",
                             "Bachelor or equivalent",
                             "Master or equivalent",
                             "Phd Doctoral or equivalent")
  
  levels(mar[["D18"]]$F6)<-c("No formal educational qualification",
                             "Compulsory education",
                             "Higher secondary education not giving access to universities (or similar)",
                             "Vocational education and/or training",
                             "High school-leaving certificate giving access to universities (or similar)",
                             "Advanced technical and professional training",
                             "Bachelor or equivalent",
                             "Master or equivalent",
                             "Phd Doctoral or equivalent")
  
  levels(mar[["D20"]]$F6)<-c("No formal educational qualification",
                             "Compulsory education",
                             "Higher secondary education not giving access to universities (or similar)",
                             "Vocational education and/or training",
                             "High school-leaving certificate giving access to universities (or similar)",
                             "Advanced technical and professional training",
                             "Bachelor or equivalent",
                             "Master or equivalent",
                             "Phd Doctoral or equivalent")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$F6<-as.factor(marDF$F6)
  
  data<- if(input$magF2=="Absolute"){ 
    marDF[,c("year","F6", "pop")]}else{
      marDF[,c("year","F6", "prop")]}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 2500,250000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$F6), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Highest level of education spouse/partner",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 

#### f7 partener current labour situacion ######

output$F7_1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("F7_1","F7_2","F7_3","F7_4",
            "F7_5","F7_6","F7_7","F7_8","F7_9",
            input$magF,"year")]#input$magC,
    
    colnames(x)<-c("F7_1","F7_2","F7_3","F7_4",
                   "F7_5","F7_6","F7_7","F7_8","F7_9","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F7_1,F7_2,F7_3,F7_4,
                                       F7_5,F7_6,F7_7,F7_8,F7_9),
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
  
  levels(marDF$groupz)<-c("In full-time employment",
                          "In part-time employment",
                          "Working in more than one part-time job",
                          "Seeking a job",
                          "Undergoing training",
                          "Looking after home or family",
                          "Disabled or partially disabled",
                          "Retired",
                          "In another non-employed situation")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 5500,550000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  data<-if(input$magF2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(data)<-c("groupz","year","prop")
  
  data$year<-paste("Wave ",data$year, sep="")
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("F7_1",input$magF,"year")]#input$magC,
    
    colnames(x)<-c("F7_1","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F7_1),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    
    
    x1
  })
  
  marDFtotales<- as.data.frame(do.call("rbind", mar1))
  
  rank <-data %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What is your spouse/partner's current labor market situation?",align = 'left')  %>%
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

###### F8 CHILDREN IN HH ######
output$F8_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BF,input$magF,"year")]
    colnames(x)<-c("F8", "pop", "year")
    
    x[is.na(x)] <- -9
    x<-x%>%
      filter(F8!=-9)%>%
      filter(F8!=-7)
    
    x$F8CAT<- with(x, ifelse(F8==0,"0 children",
                             ifelse(F8==1,"1 children",
                                    ifelse(F8==2,"2 children",
                                           ifelse(F8==3,"3 children",
                                                  ifelse(F8==4,"4 children",
                                                         ifelse(F8 >=5,"5 or more children",0)))))))
    
    colnames(x)<-c("F8", "pop", "year","F8CAT")
    
    x<-x %>%
      group_by(year,F8CAT) %>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((F8CAT))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$F8CAT<-as.factor(marDF$F8CAT)
  
  data<- if(input$magF2=="Absolute"){ 
    marDF[,c("year","F8CAT", "pop")]}else{
      marDF[,c("year","F8CAT", "prop")]}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 3500,350000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$F8CAT), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "How many children do you have?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(mar[[2]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(mar[[3]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})




#### F10 WHERE DO YOUR CHILDREN LIVE #####

output$F10_1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("F10_1",input$magF,"year")] #input$magF
    
    colnames(x)<-c("F10_1", "pop","year")
    x[is.na(x)] <- -9
    
    x<-x %>%
      group_by(year, F10_1) %>% 
      filter(F10_1!=-9)%>% 
      filter(F10_1!=-7)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((F10_1))
    
    x$F10_1<-as.factor(x$F10_1)
    x$F10_1<-fct_explicit_na(x$F10_1)
    
    x
  })
  
  levels(mar[["D16"]]$F10_1)<-c("In your household",
                                "In Switzerland but not in your household",
                                "In your country of origin",
                                "In another country")
  
  levels(mar[["D18"]]$F10_1)<-c("In your household",
                                "In Switzerland but not in your household",
                                "In your country of origin",
                                "In another country")
  
  levels(mar[["D20"]]$F10_1)<-c("In your household",
                                "In Switzerland but not in your household",
                                "In your country of origin",
                                "In another country")
  
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$F10_1<-as.factor(marDF$F10_1)
  
  data<- if(input$magF2=="Absolute"){ 
    marDF[,c("year","F10_1", "pop")]}else{
      marDF[,c("year","F10_1", "prop")]}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 3500,350000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$F10_1), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Where do your children live? Child 1",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank})

output$F10_2_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("F10_2",input$magF,"year")] #input$magF
    
    colnames(x)<-c("F10_2", "pop","year")
    x[is.na(x)] <- -9
    
    x<-x %>%
      group_by(year, F10_2) %>% 
      filter(F10_2!=-9)%>% 
      filter(F10_2!=-7)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((F10_2))
    
    x$F10_2<-as.factor(x$F10_2)
    x$F10_2<-fct_explicit_na(x$F10_2)
    
    x
  })
  
  levels(mar[["D16"]]$F10_2)<-c("In your household",
                                "In Switzerland but not in your household",
                                "In your country of origin",
                                "In another country")
  
  levels(mar[["D18"]]$F10_2)<-c("In your household",
                                "In Switzerland but not in your household",
                                "In your country of origin",
                                "In another country")
  
  levels(mar[["D20"]]$F10_2)<-c("In your household",
                                "In Switzerland but not in your household",
                                "In your country of origin",
                                "In another country")
  
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$F10_2<-as.factor(marDF$F10_2)
  
  data<- if(input$magF2=="Absolute"){ 
    marDF[,c("year","F10_2", "pop")]}else{
      marDF[,c("year","F10_2", "prop")]}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 3500,350000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$F10_2), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Where do your children live? Child 2",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank})

output$F10_3_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("F10_3",input$magF,"year")] #input$magF
    
    colnames(x)<-c("F10_3", "pop","year")
    x[is.na(x)] <- -9
    
    x<-x %>%
      group_by(year, F10_3) %>% 
      filter(F10_3!=-9)%>% 
      filter(F10_3!=-7)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((F10_3))
    
    x$F10_3<-as.factor(x$F10_3)
    x$F10_3<-fct_explicit_na(x$F10_3)
    
    x
  })
  
  levels(mar[["D16"]]$F10_3)<-c("In your household",
                                "In Switzerland but not in your household",
                                "In your country of origin",
                                "In another country")
  
  levels(mar[["D18"]]$F10_3)<-c("In your household",
                                "In Switzerland but not in your household",
                                "In your country of origin",
                                "In another country")
  
  levels(mar[["D20"]]$F10_3)<-c("In your household",
                                "In Switzerland but not in your household",
                                "In your country of origin",
                                "In another country")
  
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$F10_3<-as.factor(marDF$F10_3)
  
  data<- if(input$magF2=="Absolute"){ 
    marDF[,c("year","F10_3", "pop")]}else{
      marDF[,c("year","F10_3", "prop")]}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 3500,350000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$F10_3), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Where do your children live? Child 3",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank})


#### f11 childcate  ####

output$F11_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("F11_1","F11_2","F11_3",
            "F11_4","F11_5","F11_6",
            "F11_7","F11_8","F11_9","F11_10",
            input$magF,"year")]#iinput$magF,
    
    colnames(x)<-c("F11_1","F11_2","F11_3",
                   "F11_4","F11_5","F11_6",
                   "F11_7","F11_8","F11_9","F11_10",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F11_1,F11_2,F11_3,
                                       F11_4,F11_5,F11_6,
                                       F11_7,F11_8,F11_9,F11_10),
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
  
  levels(marDF$groupz)<-c("Day care center",
                          "Nursery or pre-school",
                          "A self-organized childcare group",
                          "Babysitter/nanny",
                          "Another institutional or paid arrangement",
                          "Your parents/parents-in-law",
                          "Other family members",
                          "Friends",
                          "Other providers",
                          "I/we do not get regular help")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 500, 50000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magF2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("F11_1",
            input$magF,"year")]#iinput$magF,
    
    colnames(x)<-c("F11_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F11_1),
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
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Do you get regular help with childcare?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      #     paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      #    paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### f12 What type of school  ####

output$F12_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("F12_1","F12_2","F12_3",
            "F12_4","F12_5","F12_6",
            input$magF,"year")]#iinput$magF,
    
    colnames(x)<-c("F12_1","F12_2","F12_3",
                   "F12_4","F12_5","F12_6",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F12_1,F12_2,F12_3,
                                       F12_4,F12_5,F12_6),
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
  
  levels(marDF$groupz)<-c("Public (state-run) and local",
                          "Religious",
                          "Non-profit, including free alternative schools",
                          "Private and local",
                          "International school",
                          "Other school type")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 1500, 100000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magF2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("F12_1",
            input$magF,"year")]#iinput$magF,
    
    colnames(x)<-c("F12_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F12_1),
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
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What type of school do they attend?",align = 'left')  %>%
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

#### f13 Which language   ####

output$F13_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("F13_1","F13_2","F13_3",
            input$magF,"year")]#iinput$magF,
    
    colnames(x)<-c("F13_1","F13_2","F13_3",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F13_1,F13_2,F13_3),
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
  
  levels(marDF$groupz)<-c("Local language",
                          "English",
                          "Other language")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 1500, 100000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magF2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("F13_1",
            input$magF,"year")]#iinput$magF,
    
    colnames(x)<-c("F13_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(F13_1),
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
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Language at school",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      #   paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})


#### F24 When you arrived in Switzerland in [A6], what was your first type of housing? #####

output$F24_18 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c(input$BF,input$magF,"year")]
  
  colnames(x)<-c("F24", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(F24) %>% 
    filter(F24!=-9)%>% 
    filter(F24!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((F24))
  
  x$F24<-as.factor(x$F24)
  x$F24<-fct_explicit_na(x$F24)
  
  x
  # })
  
  levels(x$F24)<-c("A residence that I rented",
                   "A residence that I owned",
                   "A residence that I sublet",
                   "A room in a shared apartment",
                   "A room with a host family",
                   "A friend's or relative's house or apartment",
                   "A home for workers or migrants",
                   "A hotel, hostel or B&B",
                   "Another kind of accommodation")
  
  
  data<- if(input$magF2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 4500,450000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$F24), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,60))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2018", data = data) %>%
    hc_title(text = "First type of housing",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank})

#### F25 DID YOU MOVE SINCE THEN ###### 

output$F25_16 <-renderText({ 
  
  x<-"Question not included in 2016 and 2020"
})

output$F25_18 <-renderHighchart({ 
  
  
  #fst <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c("F25",input$magF,"year")]
  
  colnames(x)<-c("F25","pop","year")
  
  x[is.na(x)] <- -7
  
  x<-x %>%
    group_by(year,F25) %>% 
    filter(F25!=-7)%>%
    filter(F25!=-9)%>%
    filter(F25!=-8)%>%
    summarise(pop=round(sum(pop),0))
  
  
  #})
  
  pie_single<- highchart() %>%
    hc_chart(type = 'pie') %>%
    hc_legend(enabled = TRUE) %>%
    hc_plotOptions(column = list(stacking = "normal"),
                   series = list(dataLabels = list(enabled = TRUE, 
                                                   format = '<b>{point.name}</b>: {point.percentage:.1f} %'))) %>%
    hc_add_series(data = list(
      list(y =  round(x[1,3][[1]],2), name = "Yes"),
      list(y =  round(x[2,3][[1]],2), name = "No")))%>%
    
    hc_title(text = "2018: Did you move since then?", align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x[1,3][[1]],
                                            x[2,3][[1]]))), 
                              sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    #
    hc_tooltip(pointFormat = "N: {point.y}", enabled = TRUE) 
  pie_single
})

#### F26 What kind of housing do you live in now? #####

output$F26_18 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c(input$BF,input$magF,"year")]
  
  colnames(x)<-c("F26", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(F26) %>% 
    filter(F26!=-9)%>% 
    filter(F26!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((F26))
  
  x$F26<-as.factor(x$F26)
  x$F26<-fct_explicit_na(x$F26)
  
  x
  # })
  
  levels(x$F26)<-c("A residence that I rented",
                   "A residence that I owned",
                   "A residence that I sublet",
                   "A room in a shared apartment",
                   "A room with a host family",
                   "A friend's or relative's house or apartment",
                   "A home for workers or migrants",
                   "A hotel, hostel or B&B",
                   "Another kind of accommodation")
  
  
  data<- if(input$magF2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 4500,450000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$F26), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,60))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2018", data = data) %>%
    hc_title(text = "Current type of housing",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank})

#### F27 Before moving to Switzerland in [A6], did you work as a cross-border worker in Switzerland? ####

output$F27_16 <-renderText({ 
  
  x<-"Question not included in 2016 and 2020"
})

output$F27_18 <-renderHighchart({ 
  
  
  # fst <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c("F27",input$magF,"year")]
  
  colnames(x)<-c("F27","pop","year")
  
  x[is.na(x)] <- -7
  
  x<-x %>%
    group_by(year,F27) %>% 
    filter(F27!=-7)%>%
    filter(F27!=-9)%>%
    filter(F27!=-8)%>%
    summarise(pop=round(sum(pop),0))%>% 
    mutate(prop= round((pop/sum(pop))*100,1))
  
  
  #})
  
  pie_single<- highchart() %>%
    hc_chart(type = 'pie') %>%
    hc_legend(enabled = TRUE) %>%
    hc_plotOptions(column = list(stacking = "normal"),
                   series = list(dataLabels = list(enabled = TRUE, 
                                                   format = '<b>{point.name}</b>: {point.percentage:.1f} %'))) %>%
    hc_add_series(data = list(
      list(y =  round(x[1,3][[1]],2), name = "Yes"),
      list(y =  round(x[2,3][[1]],2), name = "No, I moved during my stay in Switzerland")))%>%
    
    hc_title(text = "2018: Do you live since your arrival in Switzerland in the same municipality", align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x[1,3][[1]],
                                            x[2,3][[1]]))), 
                              sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    #
    hc_tooltip(pointFormat = "N: {point.y}", enabled = TRUE) 
  pie_single
}) 



#### F28 satisfied with your move to the municipality you currently live in#####

output$F28_18 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c(input$BF,input$magF,"year")]
  
  colnames(x)<-c("F28", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(F28) %>% 
    filter(F28!=-9)%>% 
    filter(F28!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((F28))
  
  x$F28<-as.factor(x$F28)
  x$F28<-fct_explicit_na(x$F28)
  
  x
  # })
  
  levels(x$F28)<-c("Yes, totally satisfied",
                   "Yes, rather satisfied",
                   "No, rather unsatisfied",
                   "No, totally unsatisfied")
  
  
  data<- if(input$magF2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 2000,200000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$F28), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2018", data = data) %>%
    hc_title(text = "Satisfaction with your move to the municipality you currently live in",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank})

#### F29 satisfied with your move to the municipality you currently live in#####

output$F29_18 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c(input$BF,input$magF,"year")]
  
  colnames(x)<-c("F29", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(F29) %>% 
    filter(F29!=-9)%>% 
    filter(F29!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((F29))
  
  x$F29<-as.factor(x$F29)
  x$F29<-fct_explicit_na(x$F29)
  
  x
  # })
  
  levels(x$F29)<-c("Yes, totally satisfied",
                   "Yes, rather satisfied",
                   "No, rather unsatisfied",
                   "No, totally unsatisfied")
  
  
  data<- if(input$magF2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magF=="n_nw", 4000,300000)
  
  formatter<- ifelse(input$magF2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$F29), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magF2=="Absolute",0,0),
             max=ifelse(input$magF2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2018", data = data) %>%
    hc_title(text = "Satisfaction to have chosen to live in your current municipality",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank})


