#### B16 At the time of our first survey, at the end of 2016, you were living in Switzerland. Can you confirm that you left Switzerland after the 2016 Survey and then you came back in [A6]?#### 

output$B16_16 <-renderHighchart({ 
  
  x<-dmms[["D18"]][,c(input$BQ,input$mag1,"year")]
  
  colnames(x)<-c("B16", "pop","year")
  x<-x %>%
    group_by(B16) %>% 
    filter(B16!=-9)%>% 
    filter(B16!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((B16))
  
  x$B16<-as.factor(x$B16)
  x$B16<-fct_explicit_na(x$B16)
  
  x
  #       })
  
  levels(x$B16)<-c("Yes, I left Switzerland after 2016 then returned",
                   "I did not live anymore in Switzerland at the end of 2016, and returned after",
                   "No, I have stayed continuously in Switzerland since 2016")
  
  
  data<- if(input$mag3=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 50,5000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$B16), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2018", data = data) %>%
    hc_title(text = "2018: Left Switzerland after the 2016 Survey",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[2]))
  
  rank
})    
output$B16_18 <-renderHighchart({ 
  
  x<-dmms[["D20"]][,c(input$BQ,input$mag1,"year")]
  
  colnames(x)<-c("B16", "pop","year")
  x<-x %>%
    group_by(B16) %>% 
    filter(B16!=-9)%>% 
    filter(B16!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((B16))
  
  x$B16<-as.factor(x$B16)
  x$B16<-fct_explicit_na(x$B16)
  
  x
  #       })
  
  levels(x$B16)<-c("Yes, I left Switzerland after 2018 then returned",
                   "I did not live anymore in Switzerland at the end of 2018, and returned after",
                   "No, I have stayed continuously in Switzerland since 2018")
  
  
  data<- if(input$mag3=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 50,5000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$B16), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2020", data = data) %>%
    hc_title(text = "2020: Left Switzerland after the 2018 Survey",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[3]))
  
  rank
  
  
  
  
})  


#### b17 How long did you stay abroad before returning to Switzerland in [A6]?#### 

output$B17_18 <-renderHighchart({ 
  dmms[[1]] <- NULL
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BQ,input$mag1,"year")]
    
    colnames(x)<-c("B17", "pop","year")
    x<-x %>%
      group_by(year,B17) %>% 
      filter(B17!=-9)%>% 
      filter(B17!=-7)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((B17))
    
    x$B17<-as.factor(x$B17)
    x$B17<-fct_explicit_na(x$B17)
    
    x
  })
  
  levels(mar[["D18"]]$B17)<-c("Less than 1 month",
                              "1 to 3 months",
                              "3 months to one year",
                              "More than one year")
  
  levels(mar[["D20"]]$B17)<-c("Less than 1 month",
                              "1 to 3 months",
                              "3 months to one year",
                              "More than one year")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$B17<-as.factor(marDF$B17)
  #levels(marDF$B17)
  
  
  data<- if(input$mag2=="Absolute"){ 
    marDF[,c("year","B17", "pop")]}else{
      marDF[,c("year","B17", "prop")]}
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 3500,350000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$B17), title = list(text = '')) %>%
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
    hc_title(text = "How long did you stay aborad?",
             align = 'left')  %>%
    
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
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


####   reaons #####

output$B18_1_18 <-renderHighchart({ 
  dmms[[1]] <- NULL
  mar <- lapply(dmms, function(x){
    x<-x[,c("B18_1","B18_2","B18_3","B18_4",
            "B18_5","B18_6","B18_7","B18_8",input$mag1,"year")]#input$mag1
    
    
    colnames(x)<-c("B18_1","B18_2","B18_3","B18_4",
                   "B18_5","B18_6","B18_7","B18_8","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B18_1, B18_2,B18_3, B18_4,B18_5,
                                       B18_6,B18_7,B18_8), factor_key=FALSE)
    
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
  marDF<- as.data.frame(do.call("rbind", mar))
  
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  levels(marDF$groupz)<-c("To begin or continue a training abroad",
                          "For professional reasons",
                          "To take care of one or more family members abroad",
                          "For other family reasons",
                          "To expecience new things abroad",
                          "Because my residency permit in Switzerland expired",
                          "For other reasons")
  marDF[is.na(marDF)] <- 0
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 7,750)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$mag3=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  marDF$year <-paste("Wave",marDF$year, sep=" ")
  
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Reasons last stay abroad",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(mar[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2:3]))
  
  rank
  
})

#### B2 HOW MANY COUNTRIES ###### 

output$B2_16 <-renderHighchart({ 
  
  nco <- lapply(dmms, function(x){
    
    x<-x[,c(input$BQ,input$mag1,"year")]#input$BQ,input$mag1,
    
    
    x$B2CAT<- with(x, ifelse(B2==0,"In no other country",
                             ifelse(B2==1,"In 1 other country",
                                    ifelse(B2==2,"In 2 other countries",
                                           ifelse(B2==3,"In 3 other countries",
                                                  ifelse(B2 >3,"In 4 or more other countries",0))))))
    
    colnames(x)<-c("B2", "pop", "year","B2CAT")
    
    x<-x %>%
      group_by(year,B2CAT) %>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange(desc(pop))
    x
  })
  
  ncoDF<- as.data.frame(do.call("rbind", nco))
  
  ncoDF$B2CAT<-as.factor(ncoDF$B2CAT)
  data<- if(input$mag3=="Absolute"){ 
    ncoDF[,c("year","B2CAT", "pop")]}else{
      ncoDF[,c("year","B2CAT", "prop")]}
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 4000,450000)
  
  rank <- highchart() %>%
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$B2CAT), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "In how many other countries have you lived for three or more months",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(c(nco[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(nco[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(nco[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 

#### B4 first time in swisss ###### 

output$B4_16 <-renderHighchart({ 
  fst <- lapply(dmms, function(x){
    
    x<-x[,c(input$BQ,input$mag1,"year")]#input$BQ,input$mag1
    
    x[is.na(x)] <- -7
    
    colnames(x)<-c("B4","pop","year")
    x<-x %>%
      group_by(year,B4) %>% 
      filter(B4!=-7)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))
    x
  })
  
  fstDF<- as.data.frame(do.call("rbind", fst))
  fstDF<-fstDF[fstDF$B4==1,]
  fstDF$B4<-as.factor(fstDF$B4)
  levels(fstDF$B4)<-"Yes"
  
  
  data<- if(input$mag3=="Absolute"){ 
    fstDF[,c("year","B4", "pop")]}else{
      fstDF[,c("year","B4", "prop")]}
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 7000,700000)
  
  rank <- highchart() %>%
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = factor(data$B4), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = " First time in Switzerland?",
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
    hc_colors(c(gg_color_hue(3)))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 




#### B5 HOW MANY TIME IN SWISTZ ####

output$B5_16 <-renderHighchart({ 
  dmms[[3]]<-NULL
  nco <- lapply(dmms, function(x){
    
    x<-x[,c(input$BQ,input$mag1,"year")]
    
    x[is.na(x)] <- -7
    x<-x%>%
      filter(B5!=-7)
    
    x$B5CAT<- with(x, ifelse(B5==0,"0 time",
                             ifelse(B5==1,"1 more time",
                                    ifelse(B5==2,"2 more times",
                                           ifelse(B5==3,"3 more times",
                                                  ifelse(B5 >3,"4 or more more times",0))))))
    
    colnames(x)<-c("B5", "pop", "year","B5CAT")
    
    x<-x %>%
      group_by(year,B5CAT) %>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((B5CAT))
    x
  })
  
  data<- if(input$mag3=="Absolute"){ 
    c(nco[["D16"]]$pop)}else{
      c(nco[["D16"]]$prop)}
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 600,75000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = as.character(nco[["D16"]]$B5CAT), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,80))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "2016: Number of times",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(nco[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[1]))
  rank
}) 

output$B5_18 <-renderHighchart({ 
  dmms[[3]]<-NULL
  nco <- lapply(dmms, function(x){
    
    x<-x[,c(input$BQ,input$mag1,"year")]
    
    x[is.na(x)] <- -7
    x<-x%>%
      filter(B5!=-7)
    
    x$B5CAT<- with(x, ifelse(B5==0,"0 year",
                             ifelse(B5==1,"1 year",
                                    ifelse(B5==2,"2 years",
                                           ifelse(B5==3,"3 years",
                                                  ifelse(B5 >3,"4 or more years",0))))))
    
    colnames(x)<-c("B5", "pop", "year","B5CAT")
    
    x<-x %>%
      group_by(year,B5CAT) %>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((B5CAT))
    x
  })
  
  data<- if(input$mag3=="Absolute"){ 
    c(nco[["D18"]]$pop)}else{
      c(nco[["D18"]]$prop)}
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 600,75000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = as.character(nco[["D18"]]$B5CAT), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,80))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "2018: Number of years",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(nco[["D18"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[2]))
  rank
})


#### B6 Have you ever been a cross-border commuter in Switzerland before your current stay in Switzerland? ####    

output$B6_16 <-renderHighchart({ 
  
  #fst <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c(input$BQ,input$mag1,"year")]
  
  x[is.na(x)] <- -7
  
  colnames(x)<-c("B6","pop","year")
  x<-x %>%
    group_by(year,B6) %>% 
    filter(B6!=-7)%>%
    summarise(pop=round(sum(pop),0))
  x
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
    
    hc_title(text = "2016: Have you ever been a cross-border commuter in Switzerland", align = 'left')  %>%
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

output$B6_18 <-renderText({ 
  
  x<-"Question not included in 2018 and 2020"
})



#### B8 REASON FOR MIGRATING TO SWITZERLAND ####



#input<-data.frame(mag1="weight",mag3="Relative")

output$B8_1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B8_1","B8_2","B8_3","B8_4",
            "B8_5","B8_6","B8_7","B8_8",
            "B8_9","B8_10",input$mag1,"year")]
    
    colnames(x)<-c("B8_1","B8_2","B8_3","B8_4",
                   "B8_5","B8_6","B8_7","B8_8",
                   "B8_9","B8_10","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B8_1, B8_2,B8_3, B8_4,B8_5,
                                       B8_6,B8_7,B8_8,B8_9,B8_10), factor_key=FALSE)
    
    
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
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF1<-marDF[marDF$year==2016,]
  
  dmms18 <-  dmms[[2]]
  dmms18<-list(dmms18)
  mar2 <- lapply(dmms18, function(x){
    
    x<-x[,c("B8_1","B8_2","B8_3","B8_4",
            "B8_5","B8_6","B8_7","B8_8",
            "B8_9","B8_10","B8_11","B8_12",input$mag1,"year")]
    
    colnames(x)<-c("B8_1","B8_2","B8_3","B8_4",
                   "B8_5","B8_6","B8_7","B8_8",
                   "B8_9","B8_10","B8_11","B8_12","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B8_1, B8_2,B8_3, B8_4,B8_5,
                                       B8_6,B8_7,B8_8,B8_9,B8_10,
                                       B8_11,B8_12), factor_key=FALSE)
    
    xlong$groupz<-ifelse(xlong$groupz=="B8_11","B8_3",xlong$groupz)
    
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
    
    x1$prop<-round(x1$pop/(x2$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  
  dmms20 <-  dmms[[3]]
  dmms20<-list(dmms20)
  mar3 <- lapply(dmms20, function(x){
    
    x<-x[,c("B8_1","B8_2","B8_3","B8_4",
            "B8_5","B8_6","B8_7","B8_8",
            "B8_9","B8_10","B8_12",input$mag1,"year")]
    
    colnames(x)<-c("B8_1","B8_2","B8_3","B8_4",
                   "B8_5","B8_6","B8_7","B8_8",
                   "B8_9","B8_10","B8_12","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B8_1, B8_2,B8_3, B8_4,B8_5,
                                       B8_6,B8_7,B8_8,B8_9,B8_10,
                                       B8_12), factor_key=FALSE)
    
    #xlong$groupz<-ifelse(xlong$groupz=="B8_11","B8_3",xlong$groupz)
    
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
    
    x1$prop<-round(x1$pop/(x2$pop)*100,1)
    
    x1
  })
  marDF3<- as.data.frame(do.call("rbind", mar3))
  
  marDF<-rbind(marDF1,marDF2,marDF3)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("B8_1","B8_2","B8_3","B8_4","B8_5",
                                              "B8_6", "B8_7","B8_8", "B8_9",
                                              "B8_12", "B8_10"))
  levels(marDF$groupz)<-c("Professional reasons",
                          "Educational and/or study reasons",
                          "To join a partner and/or to start a family",
                          "To accompany family",
                          "Lifestyle reasons",
                          "Gain new experiences",
                          "Social network in Switzerland",
                          "Tax reasons",
                          "Political reasons",
                          "Health reasons",
                          "Other reasons")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 4500,450000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$mag3=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("groupz","year","prop")
  
  marDF1$year <-paste("Wave",  marDF1$year,sep=" ")
  
  rank <-marDF1 %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Reasons for moving to Switzerland",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(dmms[[1]][input$mag1])),sep=": "),
      paste("\n2018 N",a(sum(dmms[[2]][input$mag1])),sep=": "),
      paste("\n2020 N",a(sum(dmms[[3]][input$mag1])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank  
  
  
}) 


#### b9 married before coming? ######

output$B9_16 <-renderHighchart({ 
  
  
  fst <- lapply(dmms, function(x){
    
    x<-x[,c(input$BQ,input$mag1,"year")]
    
    x[is.na(x)] <- -7
    colnames(x)<-c("B9","pop","year")
    x<-x %>%
      group_by(year,B9) %>% 
      filter(B9!=-7)%>%
      filter(B9!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))
    x
  })
  
  
  fstDF<- as.data.frame(do.call("rbind", fst))
  fstDF<-fstDF[fstDF$B9==1,]
  fstDF$B9<-as.factor(fstDF$B9)
  levels(fstDF$B9)<-"Yes"
  
  
  data<- if(input$mag3=="Absolute"){ 
    fstDF[,c("year","B9", "pop")]}else{
      fstDF[,c("year","B9", "prop")]}
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 7000,700000)
  
  rank <- highchart() %>%
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = factor(data$B9), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(dmms[[1]][input$mag1])),sep=": "),
      paste("\n2018 N",a(sum(dmms[[2]][input$mag1])),sep=": "),
      paste("\n2020 N",a(sum(dmms[[3]][input$mag1])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "Were you married or in a relationship when coming to Switzerland?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 


#### B10 WHAT WAS THE SITUATION BEFORE COMING TO SWITZERLAND ####

output$B10_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BQ,input$mag1,"year")]
    x[is.na(x)] <- -7
    colnames(x)<-c("B10","pop","year")
    x<-x %>%
      group_by(year,B10) %>% 
      filter(B10!=-7)%>%
      filter(B10!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))
    
    x$B10<-as.factor(x$B10)
    x$B10<-fct_explicit_na(x$B10)
    levels(x$B10)<-c(
      "Your spouse/partner already lived in Switzerland when you met.",
      "You moved together.",
      "Your spouse/partner moved before you.",
      "Your spouse/partner moved after you.",
      "Your spouse/partner has not yet moved to Switzerland.")
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar))
  
  data<- if(input$mag3=="Absolute"){ 
    marDF[,c("year","B10", "pop")]}else{
      marDF[,c("year","B10", "prop")]}
  
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 2000,175000)
  
  
  rank <- highchart() %>%
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$B10), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Which of you moved to Switzerland first, or did you move to Switzerland together??",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(mar[[1]]$pop)),sep=": "),
      paste("\n2018 N",a(sum(mar[[2]]$pop)),sep=": "),
      paste("\n2020 N",a(sum(mar[[3]]$pop)),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
  
}) 



#### B11 WWhich of you got had a job here in Switzerland first? ####

output$B11_1_16 <-renderHighchart({ 
  
  x<-dmms[["D16"]][,c("B11_1","B11_2","B11_3","B11_4","B11_5",
                      input$mag1,"year")]#input$mag1,"
  
  colnames(x)<-c("B11_1","B11_2","B11_3","B11_4","B11_5",
                 "pop","year")
  
  x[is.na(x)] <- -7
  
  
  xlong<-x %>%gather(groupz, value,c(B11_1, B11_2,B11_3, B11_4,B11_5),
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
  
  x1$prop<-round(x1$pop/(x2$pop)*100,1)
  x1$groupz<- as.factor(x1$groupz)
  
  levels(x1$groupz)<-c("You got had a job before your spouse/partner",
                       "Your spouse/partner got had a job before you",
                       "You both got had a job around the same time",
                       "You have never had a job in Switzerland",
                       "Your spouse/partner has never had a job in Switzerland")
  
  data<- if(input$mag3=="Absolute"){ 
    x1[,c("year","groupz", "pop")]}else{
      x1[,c("year","groupz", "prop")]}
  
  colnames(data)<-c("year","groupz", "prop")
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 1250,125000)
  
  
  rank <-data %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Which of you got had a job here in Switzerland first?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(x1$pop)),sep=": "),
      #paste("\n2018 N",a(sum(x1$pop)),sep=": "),
      #paste("\n2020 N",a(sum(x1$pop)),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[1]))
  
  rank
  
  
})
#### B12 did you have relatives already living here? ####

output$B12_16 <-renderHighchart({ 
  
  
  #fst <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("B12",input$mag1,"year")]
  
  colnames(x)<-c("B12","pop","year")
  
  x[is.na(x)] <- -7
  
  x<-x %>%
    group_by(year,B12) %>% 
    filter(B12!=-7)%>%
    filter(B12!=-9)%>%
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
    
    hc_title(text = "2016: Relatives already living in Switzerland", align = 'left')  %>%
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

output$B12_18 <-renderText({ 
  
  x<-"Question not included in 2018 and 2020"
})


##### b13 did you receive any support in one of the following areas #####

#input<-data.frame(mag1="weight",mag3="Relative")
output$B13_1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B13_1","B13_2","B13_3",
            "B13_5","B13_6",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B13_1","B13_2","B13_3",
                   "B13_5","B13_6","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B13_1, B13_2,B13_3,B13_5,
                                       B13_6), factor_key=FALSE)
    
    
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
    
    x1$prop<-round(x1$pop/(x2$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  #marDF1<-marDF[marDF$year==2016,]
  
  dmms18 <-  dmms[1:2]
  #dmms18<-list(dmms18)
  mar2 <- lapply(dmms18, function(x){
    
    x<-x[,c("B13_4",input$mag1,"year")] #input$mag1,
    
    colnames(x)<-c("B13_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B13_4), factor_key=FALSE)
    
    #xlong$groupz<-ifelse(xlong$groupz=="B8_11","B8_3",xlong$groupz)
    
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
  
  dmms20 <-  dmms[3]
  #dmms20<-list(dmms20)
  mar3 <- lapply(dmms20, function(x){
    
    x<-x[,c("B13_9",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B13_9","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B13_9), factor_key=FALSE)
    
    #xlong$groupz<-ifelse(xlong$groupz=="B8_11","B8_3",xlong$groupz)
    
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
  marDF3<- as.data.frame(do.call("rbind", mar3))
  
  dmms1 <-  dmms[1]
  #dmms20<-list(dmms20)
  mar4 <- lapply(dmms1, function(x){
    
    x<-x[,c("B13_7","B13_8",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B13_7","B13_8","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B13_7,B13_8), factor_key=FALSE)
    
    #xlong$groupz<-ifelse(xlong$groupz=="B8_11","B8_3",xlong$groupz)
    
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
  marDF4<- as.data.frame(do.call("rbind", mar4))
  
  
  marDF<-rbind(marDF,marDF2,marDF3,marDF4)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("B13_1", "B13_2", "B13_3",
                                              "B13_4", "B13_5", "B13_6",
                                              "B13_7", "B13_8", "B13_9"))
  levels(marDF$groupz)<-c("Allowance for or payment of moving costs",
                          "Housing",
                          "Dealing with administrative issues",
                          "Allowance for or payment of language courses",
                          "School/childcare",
                          "Spouse/partner employment support",
                          "Information about Switzerland",
                          "Other support",
                          "Finding a job after being arrived in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 4500,450000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$mag3=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("groupz","year","prop")
  marDF1$year <-paste("Wave",marDF$year, sep="")
  rank <-marDF1 %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Did you receive any support in one of the following areas?",align = 'left')  %>%
    #hc_subtitle(text =paste(
    # paste("\n2016 N",a(sum(marDF[marDF$year==2016,'pop'])),sep=": "),
    #  paste("\n2018 N",a(sum(marDF[marDF$year==2018,'pop'])),sep=": "),
    #  paste("\n2020 N",a(sum(marDF[marDF$year==2020,'pop'])),sep=": "),
    #  sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank  
  
}) 

#### b13 tables ####

output$TB13_16<- renderText({
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B13_1","B13_2","B13_3",
            "B13_5","B13_6",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B13_1","B13_2","B13_3",
                   "B13_5","B13_6","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B13_1, B13_2,B13_3,B13_5,
                                       B13_6), factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  #marDF1<-marDF[marDF$year==2016,]
  
  dmms18 <-  dmms[1:2]
  #dmms18<-list(dmms18)
  mar2 <- lapply(dmms18, function(x){
    
    x<-x[,c("B13_4",input$mag1,"year")] #input$mag1,
    
    colnames(x)<-c("B13_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B13_4), factor_key=FALSE)
    
    #xlong$groupz<-ifelse(xlong$groupz=="B8_11","B8_3",xlong$groupz)
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  
  
  marDF2<- as.data.frame(do.call("rbind", mar2))
  
  dmms20 <-  dmms[3]
  #dmms20<-list(dmms20)
  mar3 <- lapply(dmms20, function(x){
    
    x<-x[,c("B13_9",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B13_9","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B13_9), factor_key=FALSE)
    
    #xlong$groupz<-ifelse(xlong$groupz=="B8_11","B8_3",xlong$groupz)
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  
  
  marDF3<- as.data.frame(do.call("rbind", mar3))
  
  dmms1 <-  dmms[1]
  #dmms20<-list(dmms20)
  mar4 <- lapply(dmms1, function(x){
    
    x<-x[,c("B13_7","B13_8",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B13_7","B13_8","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B13_7,B13_8), factor_key=FALSE)
    
    #xlong$groupz<-ifelse(xlong$groupz=="B8_11","B8_3",xlong$groupz)
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  
  marDF4<- as.data.frame(do.call("rbind", mar4))
  
  
  marDF<-rbind(marDF,marDF2,marDF3,marDF4)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("B13_1", "B13_2", "B13_3",
                                              "B13_4", "B13_5", "B13_6",
                                              "B13_7", "B13_8", "B13_9"))
  levels(marDF$groupz)<-c("Allowance for or payment of moving costs",
                          "Housing",
                          "Dealing with administrative issues",
                          "Allowance for or payment of language courses",
                          "School/childcare",
                          "Spouse/partner employment support",
                          "Information about Switzerland",
                          "Other support",
                          "Finding a job after being arrived in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  kable(marDF %>%
          # arrange(desc(pop))%>%
          #ungroup %>%
          select(groupz,year,pop)%>%
          rename(Area="groupz", Year = "year", N = "pop"))%>%
    #)%>%
    kable_styling(
      font_size = 15,
      bootstrap_options = c("striped", "hover", "condensed"))
})

#### B14 From whom did you receive support? ####

output$B14_1_16 <-renderHighchart({ 
  
  x<-dmms[["D16"]][,c("B14_1","B14_2","B14_3","B14_4","B14_5",
                      "B14_6","B14_7","B14_8",
                      input$mag1,"year")]#input$mag1,"
  
  colnames(x)<-c("B14_1","B14_2","B14_3","B14_4","B14_5",
                 "B14_6","B14_7","B14_8",
                 "pop","year")
  
  x[is.na(x)] <- -7
  
  xlong<-x %>%gather(groupz, value,c(B14_1,B14_2,B14_3,B14_4,B14_5,
                                     B14_6,B14_7,B14_8),
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
  x1$groupz<- as.factor(x1$groupz)
  
  levels(x1$groupz)<-c("Relatives in Switzerland",
                       "Friends in Switzerland",
                       "Business relations/colleagues in Switzerland",
                       "Your employer",
                       "A private institution",
                       "A public institution",
                       "An online social media/website/blog",
                       "Other")
  
  data<- if(input$mag3=="Absolute"){ 
    x1[,c("year","groupz", "pop")]}else{
      x1[,c("year","groupz", "prop")]}
  
  colnames(data)<-c("year","groupz", "prop")
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 2500,150000)
  
  data$year <-paste("Wave",data$year, sep=" ")
  rank <-data %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "From whom did you receive support?",align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(x1$pop)),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[1]))
  
  rank
  
  
  
})  

#### B21 Who provided you the support for the payment of moving costs? #####

output$B21_1_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("B21_1","B21_2","B21_3","B21_4",
            "B21_5","B21_6",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B21_1","B21_2","B21_3","B21_4",
                   "B21_5","B21_6","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B21_1, B21_2,B21_3,
                                       B21_4,B21_5,B21_6),
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
  
  
  
  #marDF<-rbind(marDF,marDF2,marDF3,marDF4)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("B21_1","B21_2","B21_3",
                                              "B21_4","B21_5","B21_6"))
  
  levels(marDF$groupz)<-c("Relatives or friends already living in Switzerland",
                          "Your employer",
                          "Business relations/colleagues in Switzerland",
                          "A private institution",
                          "A public institution",
                          "Others")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 1500,100000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$mag3=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("groupz","year","prop")
  
  
  marDF1$year <-paste("Wave",marDF1$year, sep=" ")  
  rank <-marDF1 %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Who provided you the support for the payment of moving costs?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(mar[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2:3]))
  
  rank  
  
}) 




#### B22 Who provided you the support for the payment of moving costs? #####

output$B22_1_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("B22_1","B22_2","B22_3","B22_4",
            "B22_5","B22_6",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B22_1","B22_2","B22_3","B22_4",
                   "B22_5","B22_6","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B22_1, B22_2,B22_3,
                                       B22_4,B22_5,B22_6),
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
  
  
  
  #marDF<-rbind(marDF,marDF2,marDF3,marDF4)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("B22_1","B22_2","B22_3",
                                              "B22_4","B22_5","B22_6"))
  
  levels(marDF$groupz)<-c("Relatives or friends already living in Switzerland",
                          "Your employer",
                          "Business relations/colleagues in Switzerland",
                          "A private institution",
                          "A public institution",
                          "Others")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 1500,100000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$mag3=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  marDF$year <-paste("Wave",marDF$year, sep=" ") 
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Who provided you the support for housing?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(mar[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2:3]))
  
  rank  
  
}) 




#### B23 Who provided you the support for the payment of moving costs? #####

output$B23_1_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("B23_1","B23_2","B23_3","B23_4",
            "B23_5","B23_6",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B23_1","B23_2","B23_3","B23_4",
                   "B23_5","B23_6","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B23_1, B23_2,B23_3,
                                       B23_4,B23_5,B23_6),
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
  
  
  
  #marDF<-rbind(marDF,marDF2,marDF3,marDF4)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("B23_1","B23_2","B23_3",
                                              "B23_4","B23_5","B23_6"))
  
  levels(marDF$groupz)<-c("Relatives or friends already living in Switzerland",
                          "Your employer",
                          "Business relations/colleagues in Switzerland",
                          "A private institution",
                          "A public institution",
                          "Others")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 1500,100000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$mag3=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  marDF$year <-paste("Wave",marDF$year, sep=" ") 
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Who provided you the support for dealing with administrative issues?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(mar[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2:3]))
  
  rank  
  
}) 





#### B24 Who provided you the support for the payment of moving costs? #####

output$B24_1_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("B24_1","B24_2","B24_3","B24_4",
            "B24_5","B24_6",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B24_1","B24_2","B24_3","B24_4",
                   "B24_5","B24_6","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B24_1, B24_2,B24_3,
                                       B24_4,B24_5,B24_6),
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
  
  
  
  #marDF<-rbind(marDF,marDF2,marDF3,marDF4)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("B24_1","B24_2","B24_3",
                                              "B24_4","B24_5","B24_6"))
  
  levels(marDF$groupz)<-c("Relatives or friends already living in Switzerland",
                          "Your employer",
                          "Business relations/colleagues in Switzerland",
                          "A private institution",
                          "A public institution",
                          "Others")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 1500,100000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$mag3=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  marDF$year <-paste("Wave",marDF$year, sep=" ") 
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Who provided you the support for school/childcare?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(mar[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2:3]))
  
  rank  
  
}) 





#### B25 Who provided you the support for the payment of moving costs? #####

output$B25_1_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("B25_1","B25_2","B25_3","B25_4",
            "B25_5","B25_6",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B25_1","B25_2","B25_3","B25_4",
                   "B25_5","B25_6","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B25_1, B25_2,B25_3,
                                       B25_4,B25_5,B25_6),
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
  
  
  
  #marDF<-rbind(marDF,marDF2,marDF3,marDF4)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("B25_1","B25_2","B25_3",
                                              "B25_4","B25_5","B25_6"))
  
  levels(marDF$groupz)<-c("Relatives or friends already living in Switzerland",
                          "Your employer",
                          "Business relations/colleagues in Switzerland",
                          "A private institution",
                          "A public institution",
                          "Others")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 1500,100000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$mag3=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  marDF$year <-paste("Wave",marDF$year, sep=" ") 
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Who provided you the support for the spouse/partner employment?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(mar[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2:3]))
  
  rank  
  
}) 




#### B27 Who provided you the support for the payment of moving costs? #####

output$B27_1_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("B27_1","B27_2","B27_3","B27_4",
            "B27_5","B27_6",input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B27_1","B27_2","B27_3","B27_4",
                   "B27_5","B27_6","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B27_1, B27_2,B27_3,
                                       B27_4,B27_5,B27_6),
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
  
  
  
  #marDF<-rbind(marDF,marDF2,marDF3,marDF4)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Relatives or friends already living in Switzerland",
                          "Your employer",
                          "Business relations/colleagues in Switzerland",
                          "A private institution",
                          "A public institution",
                          "Others")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 1500,100000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$mag3=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  marDF$year <-paste("Wave",marDF$year, sep=" ") 
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Who provided you the support to find a job after being arrived in Switzerland?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2018 N",a(sum(c(mar[[1]]$pop))),sep=": "),
      paste("\n2020 N",a(sum(c(mar[[2]]$pop))),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2:3]))
  
  rank  
  
}) 


#### B15 FROM 0 TO 7 #####

#input<-data.frame(mag1="weight",mag3="Relative")

output$B15_1_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B15_1","B15_2","B15_3","B15_4",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_1","B15_2","B15_3","B15_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_1, B15_2,B15_3,B15_4),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz,value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    #x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  dmms1620 <-  dmms[c(1,3)]
  mar2 <- lapply(dmms1620, function(x){
    
    x<-x[,c("B15_5",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_5","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_5),
                       factor_key=FALSE)
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    
    #    x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  #dmms1820 <-  dmms[2:3]
  dmms16 <-  dmms[1]
  mar3 <- lapply(dmms16, function(x){
    
    x<-x[,c("B15_6","B15_7",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_6","B15_7","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_6,B15_7),
                       factor_key=FALSE)
    
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    x1
  })
  marDF3<- as.data.frame(do.call("rbind", mar3)) #CATS 1,2,4,5,6
  
  marDF<-rbind(marDF,marDF2,marDF3)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Financing the move and the settlement in Switzerland",
                          "Finding accommodation",
                          "Finding childcare/school",
                          "Dealing with the administration",
                          "Speaking/understanding the local language",
                          "Feeling lonely and/or homesick",
                          "Feeling homesick")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  
  marDFf<-marDF[marDF$groupz=="Financing the move and the settlement in Switzerland",]
  
  marDFf1<-if(input$mag3=="Relative"){
    marDFf[,c(1,2,3,5)]
  } else {
    marDFf[,c(1,2,3,4)]}
  
  colnames(marDFf1)<-c("groupz","year","value", "prop")
  
  marDFf1<-as.data.frame(marDFf1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDFf1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDFf1[marDFf$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDFf1[marDFf$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDFf1[marDFf$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Financing the move and the settlement in Switzerland",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFf[marDFf$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFf[marDFf$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFf[marDFf$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
  
})   

output$B15_2_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B15_1","B15_2","B15_3","B15_4",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_1","B15_2","B15_3","B15_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_1, B15_2,B15_3,B15_4),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz,value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    #x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  dmms1620 <-  dmms[c(1,3)]
  mar2 <- lapply(dmms1620, function(x){
    
    x<-x[,c("B15_5",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_5","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_5),
                       factor_key=FALSE)
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      # filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    
    #    x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  #dmms1820 <-  dmms[2:3]
  dmms16 <-  dmms[1]
  mar3 <- lapply(dmms16, function(x){
    
    x<-x[,c("B15_6","B15_7",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_6","B15_7","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_6,B15_7),
                       factor_key=FALSE)
    
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      # filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    x1
  })
  marDF3<- as.data.frame(do.call("rbind", mar3)) #CATS 1,2,4,5,6
  
  marDF<-rbind(marDF,marDF2,marDF3)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Financing the move and the settlement in Switzerland",
                          "Finding accommodation",
                          "Finding childcare/school",
                          "Dealing with the administration",
                          "Speaking/understanding the local language",
                          "Feeling lonely and/or homesick",
                          "Feeling homesick")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  marDFf<-marDF[marDF$groupz=="Finding accommodation",]
  
  marDFf1<-if(input$mag3=="Relative"){
    marDFf[,c(1,2,3,5)]
  } else {
    marDFf[,c(1,2,3,4)]}
  
  colnames(marDFf1)<-c("groupz","year","value", "prop")
  
  marDFf1<-as.data.frame(marDFf1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDFf1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDFf1[marDFf$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDFf1[marDFf$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDFf1[marDFf$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Finding accommodation",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFf[marDFf$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFf[marDFf$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFf[marDFf$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank    
  
})

output$B15_3_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B15_1","B15_2","B15_3","B15_4",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_1","B15_2","B15_3","B15_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_1, B15_2,B15_3,B15_4),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz,value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    #x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  dmms1620 <-  dmms[c(1,3)]
  mar2 <- lapply(dmms1620, function(x){
    
    x<-x[,c("B15_5",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_5","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_5),
                       factor_key=FALSE)
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      # filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    
    #    x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  #dmms1820 <-  dmms[2:3]
  dmms16 <-  dmms[1]
  mar3 <- lapply(dmms16, function(x){
    
    x<-x[,c("B15_6","B15_7",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_6","B15_7","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_6,B15_7),
                       factor_key=FALSE)
    
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    x1
  })
  marDF3<- as.data.frame(do.call("rbind", mar3)) #CATS 1,2,4,5,6
  
  marDF<-rbind(marDF,marDF2,marDF3)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Financing the move and the settlement in Switzerland",
                          "Finding accommodation",
                          "Finding childcare/school",
                          "Dealing with the administration",
                          "Speaking/understanding the local language",
                          "Feeling lonely and/or homesick",
                          "Feeling homesick")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  marDFf<-marDF[marDF$groupz=="Finding childcare/school",]
  
  marDFf1<-if(input$mag3=="Relative"){
    marDFf[,c(1,2,3,5)]
  } else {
    marDFf[,c(1,2,3,4)]}
  
  colnames(marDFf1)<-c("groupz","year","value", "prop")
  
  marDFf1<-as.data.frame(marDFf1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDFf1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDFf1[marDFf$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDFf1[marDFf$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDFf1[marDFf$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Finding childcare/school",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFf[marDFf$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFf[marDFf$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFf[marDFf$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank  
  
})

output$B15_4_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B15_1","B15_2","B15_3","B15_4",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_1","B15_2","B15_3","B15_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_1, B15_2,B15_3,B15_4),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz,value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    #x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  dmms1620 <-  dmms[c(1,3)]
  mar2 <- lapply(dmms1620, function(x){
    
    x<-x[,c("B15_5",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_5","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_5),
                       factor_key=FALSE)
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    
    #    x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  #dmms1820 <-  dmms[2:3]
  dmms16 <-  dmms[1]
  mar3 <- lapply(dmms16, function(x){
    
    x<-x[,c("B15_6","B15_7",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_6","B15_7","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_6,B15_7),
                       factor_key=FALSE)
    
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      # filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    x1
  })
  marDF3<- as.data.frame(do.call("rbind", mar3)) #CATS 1,2,4,5,6
  
  marDF<-rbind(marDF,marDF2,marDF3)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Financing the move and the settlement in Switzerland",
                          "Finding accommodation",
                          "Finding childcare/school",
                          "Dealing with the administration",
                          "Speaking/understanding the local language",
                          "Feeling lonely and/or homesick",
                          "Feeling homesick")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  marDFf<-marDF[marDF$groupz=="Dealing with the administration",]
  
  marDFf1<-if(input$mag3=="Relative"){
    marDFf[,c(1,2,3,5)]
  } else {
    marDFf[,c(1,2,3,4)]}
  
  colnames(marDFf1)<-c("groupz","year","value", "prop")
  
  marDFf1<-as.data.frame(marDFf1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDFf1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDFf1[marDFf$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDFf1[marDFf$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDFf1[marDFf$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Dealing with the administration",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFf[marDFf$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFf[marDFf$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFf[marDFf$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank  
  
  
})

output$B15_5_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B15_1","B15_2","B15_3","B15_4",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_1","B15_2","B15_3","B15_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_1, B15_2,B15_3,B15_4),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz,value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    #x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  dmms1620 <-  dmms[c(1,3)]
  mar2 <- lapply(dmms1620, function(x){
    
    x<-x[,c("B15_5",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_5","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_5),
                       factor_key=FALSE)
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    
    #    x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  #dmms1820 <-  dmms[2:3]
  dmms16 <-  dmms[1]
  mar3 <- lapply(dmms16, function(x){
    
    x<-x[,c("B15_6","B15_7",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_6","B15_7","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_6,B15_7),
                       factor_key=FALSE)
    
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #   filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    x1
  })
  marDF3<- as.data.frame(do.call("rbind", mar3)) #CATS 1,2,4,5,6
  
  marDF<-rbind(marDF,marDF2,marDF3)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Financing the move and the settlement in Switzerland",
                          "Finding accommodation",
                          "Finding childcare/school",
                          "Dealing with the administration",
                          "Speaking/understanding the local language",
                          "Feeling lonely and/or homesick",
                          "Feeling homesick")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  marDFf<-marDF[marDF$groupz=="Speaking/understanding the local language",]
  
  marDFf1<-if(input$mag3=="Relative"){
    marDFf[,c(1,2,3,5)]
  } else {
    marDFf[,c(1,2,3,4)]}
  
  colnames(marDFf1)<-c("groupz","year","value", "prop")
  
  marDFf1<-as.data.frame(marDFf1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDFf1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDFf1[marDFf$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDFf1[marDFf$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDFf1[marDFf$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Speaking/understanding the local language",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFf[marDFf$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFf[marDFf$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFf[marDFf$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank  
  
})

output$B15_6_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B15_1","B15_2","B15_3","B15_4",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_1","B15_2","B15_3","B15_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_1, B15_2,B15_3,B15_4),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz,value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    #x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  dmms1620 <-  dmms[c(1,3)]
  mar2 <- lapply(dmms1620, function(x){
    
    x<-x[,c("B15_5",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_5","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_5),
                       factor_key=FALSE)
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    
    #    x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  #dmms1820 <-  dmms[2:3]
  dmms16 <-  dmms[1]
  mar3 <- lapply(dmms16, function(x){
    
    x<-x[,c("B15_6","B15_7",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_6","B15_7","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_6,B15_7),
                       factor_key=FALSE)
    
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    x1
  })
  marDF3<- as.data.frame(do.call("rbind", mar3)) #CATS 1,2,4,5,6
  
  marDF<-rbind(marDF,marDF2,marDF3)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Financing the move and the settlement in Switzerland",
                          "Finding accommodation",
                          "Finding childcare/school",
                          "Dealing with the administration",
                          "Speaking/understanding the local language",
                          "Feeling lonely and/or homesick",
                          "Feeling homesick")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  marDFf<-marDF[marDF$groupz=="Feeling lonely and/or homesick",]
  
  marDFf1<-if(input$mag3=="Relative"){
    marDFf[,c(1,2,3,5)]
  } else {
    marDFf[,c(1,2,3,4)]}
  
  colnames(marDFf1)<-c("groupz","year","value", "prop")
  
  marDFf1<-as.data.frame(marDFf1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDFf1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDFf1[marDFf$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDFf1[marDFf$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDFf1[marDFf$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Feeling lonely and/or homesick",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFf[marDFf$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFf[marDFf$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFf[marDFf$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
  
})

output$B15_7_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B15_1","B15_2","B15_3","B15_4",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_1","B15_2","B15_3","B15_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_1, B15_2,B15_3,B15_4),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz,value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    #x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  dmms1620 <-  dmms[c(1,3)]
  mar2 <- lapply(dmms1620, function(x){
    
    x<-x[,c("B15_5",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_5","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_5),
                       factor_key=FALSE)
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    
    #    x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  #dmms1820 <-  dmms[2:3]
  dmms16 <-  dmms[1]
  mar3 <- lapply(dmms16, function(x){
    
    x<-x[,c("B15_6","B15_7",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_6","B15_7","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_6,B15_7),
                       factor_key=FALSE)
    
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    x1
  })
  marDF3<- as.data.frame(do.call("rbind", mar3)) #CATS 1,2,4,5,6
  
  marDF<-rbind(marDF,marDF2,marDF3)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Financing the move and the settlement in Switzerland",
                          "Finding accommodation",
                          "Finding childcare/school",
                          "Dealing with the administration",
                          "Speaking/understanding the local language",
                          "Feeling lonely and/or homesick",
                          "Feeling homesick")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  marDFf<-marDF[marDF$groupz=="Feeling homesick",]
  
  marDFf1<-if(input$mag3=="Relative"){
    marDFf[,c(1,2,3,5)]
  } else {
    marDFf[,c(1,2,3,4)]}
  
  colnames(marDFf1)<-c("groupz","year","value", "prop")
  
  marDFf1<-as.data.frame(marDFf1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDFf1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDFf1[marDFf$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDFf1[marDFf$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDFf1[marDFf$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Feeling homesick",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFf[marDFf$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFf[marDFf$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFf[marDFf$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
  
})

output$B15_8_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("B15_1","B15_2","B15_3","B15_4",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_1","B15_2","B15_3","B15_4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_1, B15_2,B15_3,B15_4),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz,value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    #x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  dmms1620 <-  dmms[c(1,3)]
  mar2 <- lapply(dmms1620, function(x){
    
    x<-x[,c("B15_5",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_5","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_5),
                       factor_key=FALSE)
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    
    #    x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF2<- as.data.frame(do.call("rbind", mar2))
  
  dmms1820 <-  dmms[c(2,3)]
  mar4 <- lapply(dmms1820, function(x){
    
    x<-x[,c("B15_8",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_8","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_8),
                       factor_key=FALSE)
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    
    #    x1$prop<-round(x1$pop/unique(x1$pop)*100,1)
    
    x1
  })
  marDF4<- as.data.frame(do.call("rbind", mar4))
  
  #dmms1820 <-  dmms[2:3]
  dmms16 <-  dmms[1]
  mar3 <- lapply(dmms16, function(x){
    
    x<-x[,c("B15_6","B15_7",
            input$mag1,"year")]#input$mag1,
    
    colnames(x)<-c("B15_6","B15_7","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(B15_6,B15_7),
                       factor_key=FALSE)
    
    
    xlong[is.na(xlong)] <- -8
    
    x1<-xlong %>%
      group_by(year,groupz, value) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #  filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,0))
    
    x1
  })
  marDF3<- as.data.frame(do.call("rbind", mar3)) #CATS 1,2,4,5,6
  
  marDF<-rbind(marDF,marDF2,marDF3,marDF4)
  #marDF1<-marDF[marDF$year==2016,]
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Financing the move and the settlement in Switzerland",
                          "Finding accommodation",
                          "Finding childcare/school",
                          "Dealing with the administration",
                          "Speaking/understanding the local language",
                          "Feeling lonely and/or homesick",
                          "Feeling homesick",
                          "Finding a job for the spouse/partner")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$mag1=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$mag3=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  marDFf<-marDF[marDF$groupz=="Finding a job for the spouse/partner",]
  
  marDFf1<-if(input$mag3=="Relative"){
    marDFf[,c(1,2,3,5)]
  } else {
    marDFf[,c(1,2,3,4)]}
  
  colnames(marDFf1)<-c("groupz","year","value", "prop")
  
  marDFf1<-as.data.frame(marDFf1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDFf1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDFf1[marDFf$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDFf1[marDFf$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDFf1[marDFf$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag3=="Absolute",0,0),
             max=ifelse(input$mag3=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Finding a job for the spouse/partner",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFf[marDFf$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFf[marDFf$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFf[marDFf$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank  
  
})

