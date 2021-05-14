#### E1 WHAT WAS YOUR LABOUR MARKET SITUATION ##### 

output$E1_1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("E1_1","E1_2","E1_3","E1_4",
            "E1_5","E1_6","E1_7","E1_8","E1_9",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E1_1","E1_2","E1_3","E1_4",
                   "E1_5","E1_6","E1_7","E1_8","E1_9","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E1_1,E1_2,E1_3,E1_4,
                                       E1_5,E1_6,E1_7,E1_8,E1_9),
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
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 5500,550000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  data<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(data)<-c("groupz","year","prop")
  
  data$year<-paste("Wave ",data$year, sep="")
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("E1_1",input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E1_1","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E1_1),
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Labour market situation before moving to Switzerland",align = 'left')  %>%
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

#### E30 Before moving to Switzerland in [A6], did you ever work abroad (Abroad means in another country than Switzerland)? ####

output$E30_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E30",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E30","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E30),
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
  
  levels(marDF$groupz)<-c("Yes, I already had a working experience abroad")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 1000, 100000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("E30",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E30","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E30),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      # filter(value!=2)%>%
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Before moving to Switzerland, did you ever work abroad?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(2,3)]))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### E31 Before moving to Switzerland in [A6], did you work as a cross-border worker in Switzerland? ####

output$E31_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E31",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E31","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E31),
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
  
  levels(marDF$groupz)<-c("Yes, I was a cross-border worker")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 500, 50000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("E31",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E30","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E30),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      # filter(value!=2)%>%
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,20))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Before moving to Switzerland, did you ever work abroad?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>% hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(2,3)]))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### E2 OCCUPATIONAL SITUATION ##### 

output$E2_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BE,input$magE,"year")]
    #x<-x[,c("E2","weight","year")]
    colnames(x)<-c("E2", "pop","year")
    x<-x %>%
      filter(E2!=-7)%>%
      filter(E2!=-9)%>%
      filter(E2!=-8)%>%
      group_by(year,E2) %>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((E2))
    
    x$E2<-as.factor(x$E2)
    x$E2<-fct_explicit_na(x$E2)
    
    x
  })
  
  levels(mar[["D16"]]$E2)<-c("Self-employed",
                             "A company owner",
                             "A relative employed in a family business",
                             "Employed as director or board member and/or with managerial",
                             "Employed without managerial responsibility",
                             "Employed in a protected workshop (except support staff)",
                             "An apprentice",
                             "A PhD student")
  
  levels(mar[["D18"]]$E2)<-c("Self-employed",
                             "A company owner",
                             "A relative employed in a family business",
                             "Employed as director or board member and/or with managerial",
                             "Employed without managerial responsibility",
                             "Employed in a protected workshop (except support staff)",
                             "An apprentice",
                             "A PhD student")
  
  levels(mar[["D20"]]$E2)<-c("Self-employed",
                             "A company owner",
                             "A relative employed in a family business",
                             "Employed as director or board member and/or with managerial",
                             "Employed without managerial responsibility",
                             "Employed in a protected workshop (except support staff)",
                             "An apprentice",
                             "A PhD student")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$E2<-as.factor(marDF$E2)
  
  data<- if(input$magE2=="Absolute"){ 
    marDF[,c("year","E2", "pop")]}else{
      marDF[,c("year","E2", "prop")]}
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 5000,500000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$E2), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What was your occupational status before moving to Switzerland?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 

#### E3 Once you arrived in Switzerland in [A6], what sector of business, or industry was your company or institution active in for the most part? ####

output$E3_16 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c(input$BE,input$magE,"year")]
  x[is.na(x)] <- -7
  colnames(x)<-c("E3","pop","year")
  x<-x %>%
    group_by(year,E3) %>% 
    filter(E3!=-7)%>%
    filter(E3!=-9)%>%
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round((pop/sum(pop)*100),1))
  
  x$E3<-as.factor(x$E3)
  x$E3<-fct_explicit_na(x$E3)
  x
  #})
  
  levels(x$E3)<-c(
    "Agriculture, forestry and fishing",
    "Manufacturing, mining and quarrying and other industry ",
    "Construction",
    "Wholesale and retail trade, transportation, hotels and restaurants",
    "Information and communication",
    "Financial and insurance activities",
    "Real estate activities",
    "Professional, scientific, technical, administration and support service activities",
    "Public administration, defense, education, human health and social action",
    "Other activities and services")
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 1000,80000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$E3), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,25))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "2016: Sector of business, or industry was your company",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    
    #hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[1]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 

output$E3_18 <-renderText({ 
  
  x<-"Question not included in 2018 and 2020"
})

#### E32 Just before moving to Switzerland in [A6], were you actively looking for a new job in the labor market, either in Switzerland or abroad? ####

output$E32_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E32",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E32","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E32),
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
  
  levels(marDF$groupz)<-c("Yes")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 3000, 325000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("E32",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E32","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E32),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #        filter(value!=2)%>%
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Just before moving to Switzerland, were you actively looking for a new job in the labor market, either in Switzerland or abroad?",align = 'left')  %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(2,3)]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})



#### E33 Please indicate the situation that best describes your job search just before moving to Switzerland in [A6 ##### 
output$E33_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BE,input$magE,"year")]
    #x<-x[,c("E33","weight","year")]
    colnames(x)<-c("E33", "pop","year")
    x<-x %>%
      filter(E33!=-7)%>%
      filter(E33!=-9)%>%
      filter(E33!=-8)%>%
      group_by(year,E33) %>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((E33))
    
    x$E33<-as.factor(x$E33)
    x$E33<-fct_explicit_na(x$E33)
    
    x
  })
  
  
  
  levels(mar[["D18"]]$E33)<-c("I was specifically looking for a job in Switzerland",
                              "I was looking for a job in Switzerland but also in other countries",
                              "I was looking for a job in another or other countries, Switzerland was not my priority")
  
  levels(mar[["D20"]]$E33)<-c("I was specifically looking for a job in Switzerland",
                              "I was looking for a job in Switzerland but also in other countries",
                              "I was looking for a job in another or other countries, Switzerland was not my priority")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$E33<-as.factor(marDF$E33)
  
  data<- if(input$magE2=="Absolute"){ 
    marDF[,c("year","E33", "pop")]}else{
      marDF[,c("year","E33", "prop")]}
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 2000,200000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$E33), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Job search just before moving to Switzerland",
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

#### E34 What were the main reasons explaining that you were looking for a job in Switzerland?  ####

output$E34_1_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E34_1","E34_2","E34_3",
            "E34_4","E34_5","E34_6",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E34_1","E34_2","E34_3",
                   "E34_4","E34_5","E34_6",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E34_1,E34_2,E34_3,
                                       E34_4,E34_5,E34_6),
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
  
  levels(marDF$groupz)<-c("Family reasons",
                          "The high wages in Switzerland",
                          "The overall working conditions in Switzerland",
                          "I was looking for a professional experience abroad, not specifically in Switzerland",
                          "The quality of life in Switzerland",
                          "Other reasons")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 1000, 125000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("E34_1",input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E34_1","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E34_1),
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What were the main reasons explaining that you were looking for a job in Switzerland?",align = 'left')  %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDFtotales[marDFtotales$year==2016,"pop"])),sep=": "),
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

#### E4 WDid you have a job contract or a job offer in Switzerland at the time you immigrated to Switzerland in [A6]?  ####

output$E4_16 <-renderHighchart({ 
  #dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("E4",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E4",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E4),
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
  
  levels(marDF$groupz)<-c("Yes")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 4000, 400000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("E4",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E4","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E4),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #        filter(value!=2)%>%
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Did you have a job or a job offer in Switzerland before you immigrated to Switzerland?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### E5 It was a transfer within the same company ##### 

output$E5_16 <-renderHighchart({ 
  #dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("E5",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E5",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E5),
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
  
  levels(marDF$groupz)<-c("Yes")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 1250, 100000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("E5",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E5","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E5),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #        filter(value!=2)%>%
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "It was a transfer within the same company",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})


### E43 Under what type of work contract did you work when arriving in Switzerland?#####
output$E43_20 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D20"]][,c(input$BE,input$magE,"year")]
  x[is.na(x)] <- -7
  colnames(x)<-c("E43","pop","year")
  x<-x %>%
    group_by(year,E43) %>% 
    filter(E43!=-7)%>%
    filter(E43!=-9)%>%
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round((pop/sum(pop))*100,1))
  
  x$E43<-as.factor(x$E43)
  x$E43<-fct_explicit_na(x$E43)
  x
  #})
  
  levels(x$E43)<-c(
    "I retained my work contract with my company",
    "I have an additional assignment contract for the duration of my stay in Switzerland",
    "I have an employment contract with the Swiss branch of my company for the time of my stay in Switzerland",
    "I don't know")
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 300,30000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$E43), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,60))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2020", data = data) %>%
    hc_title(text = "Under what type of work contract did you work when arriving in Switzerland?",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    
    #hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[3]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 
### E44 If you had been given the choice, which type of work contract would you have preferred when coming to Switzerland? ####
output$E44_20 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D20"]][,c(input$BE,input$magE,"year")]
  x[is.na(x)] <- -7
  colnames(x)<-c("E44","pop","year")
  x<-x %>%
    group_by(year,E44) %>% 
    filter(E44!=-7)%>%
    filter(E44!=-9)%>%
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round((pop/sum(pop))*100,1))
  
  x$E44<-as.factor(x$E44)
  x$E44<-fct_explicit_na(x$E44)
  x
  #})
  
  levels(x$E44)<-c(
    "A contract with the company in the country I came from",
    "A contract with the Swiss branch of my company",
    "I don't know")
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 500,30000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$E44), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2020", data = data) %>%
    hc_title(text = "Under what type of work contract did you work when arriving in Switzerland?",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    
    #hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[3]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 
#### E6 Before moving to Switzerland in [A6], in which countries did you look for a job?  ##### 

output$E6_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E6_1","E6_2","E6_3",
            "E6_4","E6_5","E6_6",
            input$magE,"year")]#iinput$magE,
    
    colnames(x)<-c("E6_1","E6_2","E6_3",
                   "E6_4","E6_5","E6_6",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E6_1,E6_2,E6_3,
                                       E6_4,E6_5,E6_6),
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
  
  levels(marDF$groupz)<-c("Switzerland",
                          "Your home country of origin",
                          "Your country of birth",
                          "The last country you lived in prior to coming to Switzerland",
                          "Other countries",
                          "You were not looking for a job")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 3000, 300000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("E6_1",
            input$magE,"year")]#iinput$magE,
    
    colnames(x)<-c("E6_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E6_1),
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Before moving to Switzerland, in which countries did you look for a job?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      #paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      #paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### E7 How did you go about looking for a job in Switzerland? ####
output$E7_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E7_1","E7_2","E7_3",
            "E7_4","E7_5","E7_6",
            "E7_7","E7_8","E7_9","E7_10",
            input$magE,"year")]#iinput$magE,
    
    colnames(x)<-c("E7_1","E7_2","E7_3",
                   "E7_4","E7_5","E7_6",
                   "E7_7","E7_8","E7_9","E7_10",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E7_1,E7_2,E7_3,
                                       E7_4,E7_5,E7_6,
                                       E7_7,E7_8,E7_9,E7_10),
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
  
  levels(marDF$groupz)<-c("Contact potential employer(s) directly",
                          "Talk to friends or relatives",
                          "Place or answer newspaper, internet or other media ad(s)",
                          "Contact an employment agency",
                          "Ask for referrals from another employer",
                          "Contact a school, community college, university",
                          "Contact government agencies",
                          "Contact ethnic/cultural group or association",
                          "Participate in networking events",
                          "Other")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 1000, 100000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("E6_1",
            input$magE,"year")]#iinput$magE,
    
    colnames(x)<-c("E6_1","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E6_1),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      filter(value!=2)%>%
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "How did you go about looking for a job in Switzerland?",align = 'left')  %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### E8 Have you had any of the following problems or difficulties when looking for a job in Switzerland? ####
output$E8_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E8_1","E8_2","E8_3",
            "E8_4","E8_5","E8_6",
            "E8_7","E8_8","E8_9",
            input$magE,"year")]#iinput$magE,
    
    colnames(x)<-c("E8_1","E8_2","E8_3",
                   "E8_4","E8_5","E8_6",
                   "E8_7","E8_8","E8_9",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E8_1,E8_2,E8_3,
                                       E8_4,E8_5,E8_6,
                                       E8_7,E8_8,E8_9),
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
  
  levels(marDF$groupz)<-c("Language problems",
                          "Qualifications and job experience from outside Switzerland not accepted",
                          "Problems with the administration or need for a permit in order to work",
                          "No family or friends who could help",
                          "No connections in the job market",
                          "Employers only offered you unsatisfactory job contracts",
                          "Personal or financial constraints (time, costs, family, other responsibilities)",
                          "Discrimination",
                          "Other problems or difficulties")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 750, 50000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Have you had any of the following problems or difficulties when looking for a job in Switzerland?",align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(tem[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})


#### e8 tables ####

output$TE8_16<- renderText({
  
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E8_1","E8_2","E8_3",
            "E8_4","E8_5","E8_6",
            "E8_7","E8_8","E8_9",
            input$magE,"year")]#iinput$magE,
    
    colnames(x)<-c("E8_1","E8_2","E8_3",
                   "E8_4","E8_5","E8_6",
                   "E8_7","E8_8","E8_9",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E8_1,E8_2,E8_3,
                                       E8_4,E8_5,E8_6,
                                       E8_7,E8_8,E8_9),
                       factor_key=FALSE)
    
    xlong[is.na(xlong)] <- -7
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
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  
  levels(marDF$groupz)<-c("Language problems",
                          "Qualifications and job experience from outside Switzerland not accepted",
                          "Problems with the administration or need for a permit in order to work",
                          "No family or friends who could help",
                          "No connections in the job market",
                          "Employers only offered you unsatisfactory job contracts",
                          "Personal or financial constraints (time, costs, family, other responsibilities)",
                          "Discrimination",
                          "Other problems or difficulties")
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

#### E9 Sometimes people receive help when looking for job. From whom did you receive assistance? Was it from ####

output$E9_1_16 <-renderHighchart({ 
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E9_1","E9_2","E9_3",
            "E9_4","E9_5","E9_6",
            "E9_7","E9_8",
            input$magE,"year")]#iinput$magE,
    
    colnames(x)<-c("E9_1","E9_2","E9_3",
                   "E9_4","E9_5","E9_6",
                   "E9_7","E9_8",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E9_1,E9_2,E9_3,
                                       E9_4,E9_5,E9_6,
                                       E9_7,E9_8),
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
  
  levels(marDF$groupz)<-c("Friends or relatives",
                          "Former business relations/colleagues",
                          "Your spouse's/partner's employer",
                          "A private institution",
                          "A public institution",
                          "Users of an online social media",
                          "Other persons or institutions",
                          "I did not receive any support")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 750, 75000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("E9_1",
            input$magE,"year")]#iinput$magE,
    
    colnames(x)<-c("E9_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E9_1),
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "From whom did you receive assistance?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      #  paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### E10 Once you arrived in Switzerland in [A6], how long did you spend looking for a job before finding one? ####
output$E10_16 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c(input$BE,input$magE,"year")]
  x[is.na(x)] <- -7
  colnames(x)<-c("E10","pop","year")
  x<-x %>%
    group_by(year,E10) %>% 
    filter(E10!=-7)%>%
    filter(E10!=-9)%>%
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round((pop/sum(pop))*100,1))
  
  x$E10<-as.factor(x$E10)
  x$E10<-fct_explicit_na(x$E10)
  x
  #})
  
  levels(x$E10)<-c(
    "Less than one month",
    "Up to 6 months",
    "6 to 12 months",
    "More than 12 months")
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 300,30000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$E10), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,60))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "How long did you spend looking for a job before finding one",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    
    #hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 



#### e11 What was your labor market situation once you arrived in Switzerland in [A6]? Were you… ##### 

output$E11_1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("E11_1","E11_2","E11_3","E11_4",
            "E11_5","E11_6","E11_7","E11_8","E11_9",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E11_1","E11_2","E11_3","E11_4",
                   "E11_5","E11_6","E11_7","E11_8","E11_9","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E11_1,E11_2,E11_3,E11_4,
                                       E11_5,E11_6,E11_7,E11_8,E11_9),
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
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 5500,550000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("E11_1",input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E11_1","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E11_1),
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Labour market situation once you arrived to Switzerland",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFtotales[marDFtotales$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFtotales[marDFtotales$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFtotales[marDFtotales$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### E12 What was your occupational status once you arrived in Switzerland in  ##### 

output$E12_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BE,input$magE,"year")]
    #x<-x[,c("E2","weight","year")]
    colnames(x)<-c("E12", "pop","year")
    x<-x %>%
      filter(E12!=-7)%>%
      filter(E12!=-9)%>%
      filter(E12!=-8)%>%
      group_by(year,E12) %>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((E12))
    
    x$E12<-as.factor(x$E12)
    x$E12<-fct_explicit_na(x$E12)
    
    x
  })
  
  levels(mar[["D16"]]$E12)<-c("Self-employed",
                              "A company owner",
                              "A relative employed in a family business",
                              "Employed as director or board member and/or with managerial",
                              "Employed without managerial responsibility",
                              "Employed in a protected workshop (except support staff)",
                              "An apprentice",
                              "A PhD student")
  
  levels(mar[["D18"]]$E12)<-c("Self-employed",
                              "A company owner",
                              "A relative employed in a family business",
                              "Employed as director or board member and/or with managerial",
                              "Employed without managerial responsibility",
                              "Employed in a protected workshop (except support staff)",
                              "An apprentice",
                              "A PhD student")
  
  levels(mar[["D20"]]$E12)<-c("Self-employed",
                              "A company owner",
                              "A relative employed in a family business",
                              "Employed as director or board member and/or with managerial",
                              "Employed without managerial responsibility",
                              "Employed in a protected workshop (except support staff)",
                              "An apprentice",
                              "A PhD student")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$E12<-as.factor(marDF$E12)
  
  data<- if(input$magE2=="Absolute"){ 
    marDF[,c("year","E12", "pop")]}else{
      marDF[,c("year","E12", "prop")]}
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 5000,500000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$E12), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What was your occupational status once you arrived in Switzerland?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 



#### E13 Once you arrived in Switzerland in [A6], how long did you spend looking for a job before finding one? ####
output$E13_16 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c(input$BE,input$magE,"year")]
  x[is.na(x)] <- -7
  colnames(x)<-c("E13","pop","year")
  x<-x %>%
    group_by(year,E13) %>% 
    filter(E13!=-7)%>%
    filter(E13!=-9)%>%
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round((pop/sum(pop))*100,1))
  
  x$E13<-as.factor(x$E13)
  x$E13<-fct_explicit_na(x$E13)
  x
  #})
  
  levels(x$E13)<-c("Agriculture, forestry and fishing",
                   "Manufacturing, mining and quarrying and other industry ",
                   "Construction",
                   "Wholesale and retail trade, transportation, hotels and restaurants",
                   "Information and communication",
                   "Financial and insurance activities",
                   "Real estate activities",
                   "Professional, scientific, technical, administration and support service activities",
                   "Public administration, defense, education, human health and social action",
                   "Other activities and services")
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magE=="n_nw",  800,80000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$E13), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "Once you arrived in Switzerland, what sector of business, or industry was your company or institution active in for the most part?",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    
    #hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 


#### E14 In total, how many years have you been in paid work in your whole life? ####

output$E14_16 <-renderHighchart({ 
  
  #nco <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c(input$BE,input$magE,"year")]
  
  x[is.na(x)] <- -7
  x<-x%>%
    filter(E14!=-7)%>%
    filter(E14!=0)
  
  x$B5CAT<- with(x, ifelse(E14<5,"Less than five year",
                           ifelse((E14>=5 & E14<10),"Between 5 and 9 years",
                                  ifelse((E14>=10 & E14<20),"Between 10 and 19 years",
                                         ifelse((E14>=20 & E14<30),"Between 19 and 29 years",
                                                ifelse(E14 >=30,"30 or more years",0))))))
  
  
  x$B5CAT<-factor(x$B5CAT, levels=c("Less than five year","Between 5 and 9 years",
                                    "Between 10 and 19 years","Between 19 and 29 years",
                                    "30 or more years"))
  
  colnames(x)<-c("B5", "pop", "year","B5CAT")
  
  x<-x %>%
    group_by(year,B5CAT) %>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange(desc(B5CAT))
  x
  #})
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 2500,200000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$B5CAT), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "Number of years in paid work",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### E15 Before moving to Switzerland in [A6], did you ever work abroad (Abroad means in another country than Switzerland)? ####

output$E15_16 <-renderHighchart({ 
  #dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("E15",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E15","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E15),
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
  
  levels(marDF$groupz)<-c("Yes")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 2250, 200000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("E15",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E15","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E15),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #        filter(value!=2)%>%
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Do you still have the same job and position as when you arrived in Switzerland?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%     
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})



#### E16 WHAT WAS YOUR LABOUR MARKET SITUATION ##### 

output$E16_1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("E16_1","E16_2","E16_3","E16_4",
            "E16_5","E16_6","E16_7","E16_8","E16_9",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E16_1","E16_2","E16_3","E16_4",
                   "E16_5","E16_6","E16_7","E16_8",
                   "E16_9","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E16_1,E16_2,E16_3,E16_4,
                                       E16_5,E16_6,E16_7,E16_8,E16_9),
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
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 5500,550000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("E16_1",input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E16_1","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E16_1),
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What is your current labor market situation?",align = 'left')  %>%
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


#### E36 Once you arrived in Switzerland in [A6], how long did you spend looking for a job before finding one? ####

output$E36_18 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c(input$BE,input$magE,"year")]
  x[is.na(x)] <- -7
  colnames(x)<-c("E36","pop","year")
  x<-x %>%
    group_by(year,E36) %>% 
    filter(E36!=-7)%>%
    filter(E36!=-9)%>%
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round((pop/sum(pop))*100,1))
  
  x$E36<-as.factor(x$E36)
  x$E36<-fct_explicit_na(x$E36)
  x
  #})
  
  levels(x$E36)<-c(
    "Less than three months",
    "3 months to 1 year",
    "More than 1 year")
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 300,25000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$E36), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2018", data = data) %>%
    hc_title(text = "Time looking for a job in Switzerland",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%  
    hc_colors(c(gg_color_hue(3)[2]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 


#### E37 Among the following statements, which best describes your situation regarding your job search in Switzerland? ####


output$E37_18 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c(input$BE,input$magE,"year")]
  x[is.na(x)] <- -7
  colnames(x)<-c("E37","pop","year")
  x<-x %>%
    group_by(year,E37) %>% 
    filter(E37!=-7)%>%
    filter(E37!=-9)%>%
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round((pop/sum(pop))*100,1))
  
  x$E37<-as.factor(x$E37)
  x$E37<-fct_explicit_na(x$E37)
  x
  #})
  
  levels(x$E37)<-c(
    "I recently received a job contract or a job offer",
    "I will quickly find a job in Switzerland",
    "I will probably find a job in Switzerland but it will take some time",
    "It will be difficult for me to find a job in Switzerland",
    "I will probably not find a job in Switzerland",
    "I am so discouraged that I do not look for a job anymore")
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 400,25000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$E37), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2018", data = data) %>%
    hc_title(text = "Job search in Switzerland",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%  
    hc_colors(c(gg_color_hue(3)[2]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 

#### E17 In what year did you start your current job? #####
output$E17_16 <-renderHighchart({ 
  
  x<-dmms[["D16"]][,c(input$BE,input$magE,"year")]
  
  colnames(x)<-c("E17","pop","year")
  x[is.na(x)] <- -7
  x<-x %>%
    group_by(year,E17) %>% 
    filter(E17!=-7)%>%
    filter(E17!=-9)%>%
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange(desc(E17))
  
  x$E17<-as.factor(x$E17)
  x$E17<-fct_explicit_na(x$E17)
  x
  #})
  
  levels(x$E17)<-c(2016:2006,"Before 2006")
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 750,60000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  
  rank <- highchart() %>%
    hc_chart(type = 'column',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$E17), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,25))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "Year starting current job",align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%  
    hc_colors(c(gg_color_hue(3)[1]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank
  
}) 



#### E18 What is your current occupational status? ##### 

output$E18_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BE,input$magE,"year")]
    #x<-x[,c("E2","weight","year")]
    colnames(x)<-c("E18", "pop","year")
    x<-x %>%
      filter(E18!=-7)%>%
      filter(E18!=-9)%>%
      filter(E18!=-8)%>%
      group_by(year,E18) %>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((E18))
    
    x$E18<-as.factor(x$E18)
    x$E18<-fct_explicit_na(x$E18)
    
    x
  })
  
  levels(mar[["D16"]]$E18)<-c("Self-employed",
                              "A company owner",
                              "A relative employed in a family business",
                              "Employed as director or board member and/or with managerial",
                              "Employed without managerial responsibility",
                              "Employed in a protected workshop (except support staff)",
                              "An apprentice",
                              "A PhD student")
  
  levels(mar[["D18"]]$E18)<-c("Self-employed",
                              "A company owner",
                              "A relative employed in a family business",
                              "Employed as director or board member and/or with managerial",
                              "Employed without managerial responsibility",
                              "Employed in a protected workshop (except support staff)",
                              "An apprentice",
                              "A PhD student")
  
  levels(mar[["D20"]]$E18)<-c("Self-employed",
                              "A company owner",
                              "A relative employed in a family business",
                              "Employed as director or board member and/or with managerial",
                              "Employed without managerial responsibility",
                              "Employed in a protected workshop (except support staff)",
                              "An apprentice",
                              "A PhD student")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$E18<-as.factor(marDF$E18)
  
  data<- if(input$magE2=="Absolute"){ 
    marDF[,c("year","E18", "pop")]}else{
      marDF[,c("year","E18", "prop")]}
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 5000,500000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$E18), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "What is your current occupational status?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 



#### E19 OWhat sector of business, or industry is your company or institution active in for the most part? ? ####
output$E19_16 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c(input$BE,input$magE,"year")]
  x[is.na(x)] <- -7
  colnames(x)<-c("E19","pop","year")
  x<-x %>%
    group_by(year,E19) %>% 
    filter(E19!=-7)%>%
    filter(E19!=-9)%>%
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round((pop/sum(pop))*100,1))
  
  x$E19<-as.factor(x$E19)
  x$E19<-fct_explicit_na(x$E19)
  x
  #})
  
  levels(x$E19)<-c("Agriculture, forestry and fishing",
                   "Manufacturing, mining and quarrying and other industry ",
                   "Construction",
                   "Wholesale and retail trade, transportation, hotels and restaurants",
                   "Information and communication",
                   "Financial and insurance activities",
                   "Real estate activities",
                   "Professional, scientific, technical, administration and support service activities",
                   "Public administration, defense, education, human health and social action",
                   "Other activities and services")
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magE=="n_nw",  800,60000)
  
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.character(x$E19), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "What sector of business, or industry is your company or institution active in for the most part?",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    
    #hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
}) 




#### E21 PDo you have a work contract of.... ##### 
output$E21_16 <-renderHighchart({ 
  # dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BE,input$magE,"year")]
    #x<-x[,c("E33","weight","year")]
    colnames(x)<-c("E21", "pop","year")
    x<-x %>%
      filter(E21!=-7)%>%
      filter(E21!=-9)%>%
      filter(E21!=-8)%>%
      group_by(year,E21) %>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((E21))
    
    x$E21<-as.factor(x$E21)
    x$E21<-fct_explicit_na(x$E21)
    
    x
  })
  
  
  levels(mar[["D16"]]$E21)<-c("unlimited duration",
                              "limited duration",
                              " You don't have a contract")
  
  levels(mar[["D18"]]$E21)<-c("unlimited duration",
                              "limited duration",
                              " You don't have a contract")
  
  levels(mar[["D20"]]$E21)<-c("unlimited duration",
                              "limited duration",
                              " You don't have a contract")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$E21<-as.factor(marDF$E21)
  
  data<- if(input$magE2=="Absolute"){ 
    marDF[,c("year","E21", "pop")]}else{
      marDF[,c("year","E21", "prop")]}
  
  hc_yAxis<-ifelse(input$magE=="n_nw",  4500,500000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$E21), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Do you have a work contract of....",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})  


#### E22 In which locality is the company that you work for?#####

output$E22_16 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c(input$BE,input$magE,"year")]
  
  colnames(x)<-c("E22", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(E22) %>% 
    filter(E22!=-9)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((E22))
  
  x$E22<-as.factor(x$E22)
  x$E22<-fct_explicit_na(x$E22)
  
  x
  # })
  
  levels(x$E22)<-c("In the same commune as your residence",
                   "In the same canton but not the same commune as your residence",
                   "In a different canton to your residence",
                   "Abroad")
  
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 2500,200000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$E22), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "Locality of the company that you work for",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank
})

#### E38 Do you think that your current level of education (that is [D1]) is appropriate for your current job?##### 
output$E38_18 <-renderHighchart({ 
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c(input$BE,input$magE,"year")]
    #x<-x[,c("E33","weight","year")]
    colnames(x)<-c("E38", "pop","year")
    x<-x %>%
      filter(E38!=-7)%>%
      filter(E38!=-9)%>%
      filter(E38!=-8)%>%
      group_by(year,E38) %>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((E38))
    
    x$E38<-as.factor(x$E38)
    x$E38<-fct_explicit_na(x$E38)
    
    x
  })
  
  
  
  
  levels(mar[["D18"]]$E38)<-c("Yes, fully appropriate",
                              "Yes, rather appropriate",
                              "No, not really appropriate",
                              "No, absolutely not appropriate")
  
  levels(mar[["D20"]]$E38)<-c("Yes, fully appropriate",
                              "Yes, rather appropriate",
                              "No, not really appropriate",
                              "No, absolutely not appropriate")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$E38<-as.factor(marDF$E38)
  
  data<- if(input$magE2=="Absolute"){ 
    marDF[,c("year","E38", "pop")]}else{
      marDF[,c("year","E38", "prop")]}
  
  hc_yAxis<-ifelse(input$magE=="n_nw",  4000,400000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$E38), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Do you think that your current level of education is appropriate for your current job?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
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


#### E23 LEVEL OF EDUCATION FOR JOB #####

output$E23_16 <-renderHighchart({ 
  dmms1618<-  dmms[1:2]
  mar <- lapply(dmms1618, function(x){
    
    x<-x[,c(input$BE,input$magE,"year")]
    
    colnames(x)<-c("E23", "pop","year")
    x[is.na(x)] <- -9
    
    x<-x %>%
      group_by(year, E23) %>% 
      filter(E23!=-9)%>% 
      filter(E23!=-7)%>%
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange(desc(E23))
    
    x$E23<-as.factor(x$E23)
    x$E23<-fct_explicit_na(x$E23)
    
    x
  })
  
  levels(mar[["D16"]]$E23)<-c("No formal educational qualification",
                              "Compulsory education",
                              "Higher secondary education not giving access to universities",
                              "Vocational education and/or training",
                              "High school-leaving certificate giving access to universities",
                              "Advanced technical and professional training",
                              "Bachelor or equivalent",
                              "Master or equivalent",
                              "Phd Doctoral or equivalent")
  
  levels(mar[["D18"]]$E23)<-c("No formal educational qualification",
                              "Compulsory education",
                              "Higher secondary education not giving access to universities",
                              "Vocational education and/or training",
                              "High school-leaving certificate giving access to universities",
                              "Advanced technical and professional training",
                              "Bachelor or equivalent",
                              "Master or equivalent",
                              "Phd Doctoral or equivalent")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$E23<-as.factor(marDF$E23)
  
  data<- if(input$magE2=="Absolute"){ 
    marDF[,c("year","E23", "pop")]}else{
      marDF[,c("year","E23", "prop")]}
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 2000,150000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$E23), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What type of education do you feel is most appropriate for your current job?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      #  paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(1:2)]))%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank
  
})




#### E24 What are the reasons for you currently occupying a job that does not correspond to your educational level? ##### 

output$E24_1_16 <-renderHighchart({ 
  dmms16<-  dmms[1]
  mar <- lapply(dmms16, function(x){
    
    x<-x[,c("E24_1","E24_2","E24_3","E24_4",
            "E24_5","E24_6","E24_7","E24_8","E24_9",
            "E24_10","E24_11","E24_12","E24_13",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E24_1","E24_2","E24_3","E24_4",
                   "E24_5","E24_6","E24_7","E24_8","E24_9",
                   "E24_10","E24_11","E24_12","E24_13","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E24_1,E24_2,E24_3,E24_4,
                                       E24_5,E24_6,E24_7,E24_8,E24_9,
                                       E24_10,E24_11,E24_12,E24_13),
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
  
  levels(marDF$groupz)<-c("Inadequate knowledge of one of the national languages",
                          "Qualifications obtained abroad are not recognized in Switzerland",
                          "A change of career",
                          "Lack of jobs with corresponding qualifications",
                          "Future salary improvements and promotional opportunities",
                          "To be able to study at the same time",
                          "To avoid unemployment",
                          "Origin, religion or social background",
                          "Family obligations",
                          "Health reasons",
                          "No interest in changing jobs",
                          "Other obstacle",
                          "No particular obstacles")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 750,50000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms16, function(x){
    
    x<-x[,c("E24_1",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E24_1","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E24_1),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      # filter(value!=2)%>%
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
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What are the reasons for you currently occupying a job that does not correspond to your educational level?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      # paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      #  paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})


#### e25 FROM 0 TO 7 #####

#input<-data.frame(mag1="weight",mag3="Relative")

output$E25_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("E25",
            input$magE,"year")]#input$mag1,
    
    colnames(x)<-c("E25","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E25),
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
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Knowledge and overall skills utilized in your current work")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 3000,300000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magE2=="Relative"){
    marDF[,c(1,2,3,5)]
  } else {
    marDF[,c(1,2,3,4)]}
  
  colnames(marDF1)<-c("groupz","year","value", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Knowledge and overall skills utilized in your current work",align = 'left')  %>%
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

#### E26 What are the reasons for you currently occupying a job that does not correspond to your educational level? ##### 

output$E26_1_16 <-renderHighchart({ 
  dmms16<-  dmms[1]
  mar <- lapply(dmms16, function(x){
    
    x<-x[,c("E26_1","E26_2","E26_3","E26_4",
            "E26_5","E26_6","E26_7","E26_8","E26_9",
            "E26_10","E26_11","E26_12","E26_13",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E26_1","E26_2","E26_3","E26_4",
                   "E26_5","E26_6","E26_7","E26_8","E26_9",
                   "E26_10","E26_11","E26_12","E26_13","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E26_1,E26_2,E26_3,E26_4,
                                       E26_5,E26_6,E26_7,E26_8,
                                       E26_6,
                                       E26_10,E26_11,E26_12,E26_13),
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
  
  levels(marDF$groupz)<-c("Inadequate knowledge of one of the national languages",
                          "Qualifications obtained abroad are not recognized in Switzerland",
                          "A change of career",
                          "Lack of jobs with corresponding qualifications",
                          "Future salary improvements and promotional opportunities",
                          "To be able to study at the same time",
                          "To avoid unemployment",
                          "Origin, religion or social background",
                          "Family obligations",
                          "Health reasons",
                          "No interest in changing jobs",
                          "Other obstacle",
                          "No particular obstacles")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 750,50000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms16, function(x){
    
    x<-x[,c("E26_1",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E26_1","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E26_1),
                       factor_key=FALSE)
    
    
    x1<-xlong %>%
      group_by(year,groupz) %>% 
      filter(value!=-7)%>%
      filter(value!=-9)%>%
      filter(value!=-8)%>%
      #      filter(value!=2)%>%
      summarise(pop=round(sum(pop),0))
    
    x1
  })
  marDF1<- as.data.frame(do.call("rbind", mar1)) #
  
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What are the reasons for you currently occupying a job that does not utilize your knowledge and overall skills?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      # paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      #paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})



#### e39 FROM 0 TO 7 #####
#input<-data.frame(mag1="weight",mag3="Relative")

output$E39_16 <-renderHighchart({ 
  #aver<-dmms[[1]]
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("E39",
            input$magE,"year")]#input$mag1,
    
    colnames(x)<-c("E39","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E39),
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
  
  marDF<-marDF %>% complete(groupz, nesting(year))
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("Level of satisfaction with your current occupation")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 2500,250000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magE2=="Relative"){
    marDF[,c(1,2,3,5)]
  } else {
    marDF[,c(1,2,3,4)]}
  
  colnames(marDF1)<-c("groupz","year","value", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    # hc_add_series(name= "Wave 2016",data = marDF[marDF$year==2016,4])%>%
    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,4])%>%
    hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,4])%>%
    hc_chart(type = 'column',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Level of satisfaction with your current occupation",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
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


#### E27 when comparing your situation today with your situation before moving to Switzerland in ...? It has ...####

output$E27_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BE,input$magE,"year")]
    
    colnames(x)<-c("E27", "pop","year")
    x[is.na(x)] <- -9
    
    x<-x %>%
      group_by(year, E27) %>% 
      filter(E27!=-9)%>% 
      filter(E27!=-7)%>%
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange(desc(E27))
    
    x$E27<-as.factor(x$E27)
    x$E27<-fct_explicit_na(x$E27)
    
    x
  })
  
  levels(mar[["D16"]]$E27)<-c("Improved substantially",
                              "Improved slightly",
                              "Remained the same",
                              "Worsened slightly",
                              "Worsened substantially")
  
  levels(mar[["D18"]]$E27)<-c("Improved substantially",
                              "Improved slightly",
                              "Remained the same",
                              "Worsened slightly",
                              "Worsened substantially")
  
  levels(mar[["D20"]]$E27)<-c("Improved substantially",
                              "Improved slightly",
                              "Remained the same",
                              "Worsened slightly",
                              "Worsened substantially")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$E27<-as.factor(marDF$E27)
  
  data<- if(input$magE2=="Absolute"){ 
    marDF[,c("year","E27", "pop")]}else{
      marDF[,c("year","E27", "prop")]}
  
  hc_yAxis<-ifelse(input$magE=="n_nw",3500,350000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = rev(levels(data$E27)), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Improvement in professional situation",
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

#### E40 Have you had business activities in the past 12 months with people or institutions in countries other than Switzerland?####

output$E40_16 <-renderText({ 
  
  x<-"Question not included in 2016 and 2020"
})

output$E40_18 <-renderHighchart({ 
  
  
  #fst <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c("E40",input$magE,"year")]
  
  colnames(x)<-c("E40","pop","year")
  
  x[is.na(x)] <- -7
  
  x<-x %>%
    group_by(year,E40) %>% 
    filter(E40!=-7)%>%
    filter(E40!=-9)%>%
    filter(E40!=-8)%>%
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
    
    hc_title(text = "2018:Business activities in countries other than Switzerland", align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x[1,3][[1]],
                                            x[2,3][[1]]))), 
                              sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    #
    hc_tooltip(pointFormat = "N: {point.y}", enabled = TRUE) 
  pie_single})


#### e41 COUNTRY OF bussnies ####

output$E41_1_16 <-renderText({ 
  
  x<-"Question not included in 2016"
})

output$E41_1_18 <-renderHighchart({ 
  
  #hom <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c("E41_1",input$magE,"year")]
  colnames(x)<-c("E41_1","pop","year")
  x<-as.data.frame(x)
  x = data.frame(x, countries[match(x[,"E41_1"],
                                    countries[,"A3"]),c("official_title_en")])
  colnames(x)[length(names(x))]<-paste("E41_1","B",sep="")
  x$E41_1B<-as.factor(x$E41_1B)
  x$E41_1B<-fct_explicit_na(x$E41_1B)
  
  colnames(x)<-c("B1","pop","year","B1B")
  
  x<-x %>%
    group_by(year,B1B) %>% 
    filter(B1B!="(Missing)")%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange(desc(pop))%>% 
    head(20)
  x
  # })
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magD=="n_nw", 200,20000)
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = as.character(x$B1B), title = list(text = '')) %>%
    hc_yAxis(title = list(text = 'N')) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Value", 
                  data = data) %>%
    hc_title(text = "2018: Top 20 countries business activities (1st choice)",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,25))%>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)
  rank
})

output$E41_2_18 <-renderHighchart({ 
  
  #hom <- lapply(dmms, function(x){
  
  x<-dmms[["D18"]][,c("E41_2",input$magE,"year")]
  colnames(x)<-c("E41_2","pop","year")
  x<-as.data.frame(x)
  x = data.frame(x, countries[match(x[,"E41_2"],
                                    countries[,"A3"]),c("official_title_en")])
  colnames(x)[length(names(x))]<-paste("E41_2","B",sep="")
  x$E41_2B<-as.factor(x$E41_2B)
  x$E41_2B<-fct_explicit_na(x$E41_2B)
  
  colnames(x)<-c("B1","pop","year","B1B")
  
  x<-x %>%
    group_by(year,B1B) %>% 
    filter(B1B!="(Missing)")%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange(desc(pop))%>% 
    head(20)
  x
  # })
  
  data<- if(input$magE2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magD=="n_nw", 200,20000)
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = as.character(x$B1B), title = list(text = '')) %>%
    hc_yAxis(title = list(text = 'N')) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Value", 
                  data = data) %>%
    hc_title(text = "2018: Top 20 countries business activities (2nd choice)",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,25))%>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)
  rank
})



#### E42 What kinds of business activities have you had in the past 12 months with people and institutions in countries other than Switzerland? ##### 

output$E42_1_18 <-renderHighchart({ 
  dmms16<-  dmms[2]
  mar <- lapply(dmms16, function(x){
    
    x<-x[,c("E42_1","E42_2","E42_3","E42_4",
            "E42_5","E42_6",
            input$magE,"year")]#input$magC,
    
    colnames(x)<-c("E42_1","E42_2","E42_3","E42_4",
                   "E42_5","E42_6","pop","year")
    
    xlong<-x %>%gather(groupz, value,c(E42_1,E42_2,E42_3,E42_4,
                                       E42_5,E42_6),
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
  
  levels(marDF$groupz)<-c("I import goods produced abroad",
                          "I have people located abroad who do jobs for me (paid or unpaid)",
                          "I have financial support from abroad for my business activities",
                          "I sell goods / services abroad (myself and / or via internet)",
                          "I receive information, training, or counseling services from abroad, including via the internet",
                          "Other")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magE=="n_nw", 500,50000)
  
  formatter<- ifelse(input$magE2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magE2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  rank <-marDF %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magE2=="Absolute",0,0),
             max=ifelse(input$magE2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What kinds of business activities have you had in the past 12 months with people and institutions in countries other than Switzerland?",align = 'left')  %>%
    hc_subtitle(text =  paste("\n2018 N",a(sum(c(mar[[1]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})




