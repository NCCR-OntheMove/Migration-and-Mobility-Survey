#### H1 Is your stay in Switzerland limited in time?#####

output$H1_16 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("H1",input$magH,"year")]
  
  colnames(x)<-c("H1", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(H1) %>% 
    filter(H1!=-9)%>% 
    filter(H1!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop= round((pop/sum(pop))*100,1))%>%
    arrange((H1))
  
  x$H1<-as.factor(x$H1)
  x$H1<-fct_explicit_na(x$H1)
  
  x
  # })
  
  levels(x$H1)<-c("Yes",
                  "No",
                  "I do not know yet")
  
  
  data<- if(input$magH2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 5000,400000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$H1), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "2016: Stay limited in time",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(1)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank})

#### H2 How many more years would like to stay in Switzerland? ####

output$H2_16 <-renderHighchart({
  
  x<-dmms[["D16"]][,c("H2",input$magH,"year")]
  
  colnames(x)<-c("H2","pop","year")
  x[is.na(x)] <- -7
  
  x<-x %>%
    filter(H2!=-7)
  
  x$H2CAT<- with(x, ifelse(H2==0,"Less than one",
                           ifelse(H2>=1 & H2 <=3,"1-3 years",
                                  ifelse(H2>=4 & H2 <=5,"4-5 years",
                                         ifelse(H2>=6 & H2 <=10,"6-10 years",
                                                ifelse(H2 >10,"More than 10 years",0))))))
  
  x$H2CAT<-as.factor(x$H2CAT)
  x$H2CAT<-factor(x$H2CAT, levels=c("Less than one",
                                    "1-3 years",
                                    "4-5 years",
                                    "6-10 years",
                                    "More than 10 years"))
  x<-x %>%
    group_by(year,H2CAT) %>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((H2CAT))
  x
  
  data<- if(input$magH2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 200,20000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$H2CAT), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,40))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "2016: How many more years would like to stay in Switzerland",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(1)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  
  rank})






#####H3  How often have you considered emigrating from Switzerland in the last three months? #####

output$H3_16 <-renderHighchart({ 
  
  dmms1820 <-  dmms[1:2]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H3",input$magH,"year")]#input$BF,input$magH,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("H3", "pop","year")
    
    
    x<-x %>%
      group_by(year,H3) %>% 
      filter(H3!="(Missing)")%>%
      filter(H3!=-7)%>%
      filter(H3!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((H3))
    
    x$H3<-as.factor(x$H3)
    x$H3<-fct_explicit_na(x$H3)
    
    x
  })
  
  
  levels(mar[["D16"]]$H3)<-c("Very often",
                             "Often",
                             "From time to time",
                             "Never")
  
  levels(mar[["D18"]]$H3)<-c("Very often",
                             "Often",
                             "From time to time",
                             "Never")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$H3<-as.factor(marDF$H3)
  
  data<- if(input$magH2=="Absolute"){ 
    marDF[,c("year","H3", "pop")]}else{
      marDF[,c("year","H3", "prop")]}
  
  hc_yAxis<-ifelse(input$magH=="n_nw",  6000,600000)
  
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$H3), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    #hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      #paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "How often have you considered emigrating from Switzerland in the last three months?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(1:2)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#### H4  Do you plan to emigrate from Switzerland within the next 12 months?#### 

output$H4_16 <-renderHighchart({ 
  
  
  # fst <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("H4",input$magH,"year")]
  
  colnames(x)<-c("H4","pop","year")
  
  x[is.na(x)] <- -7
  
  x<-x %>%
    group_by(year,H4) %>% 
    filter(H4!=-7)%>%
    filter(H4!=-9)%>%
    filter(H4!=-8)%>%
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
    
    hc_title(text = "2016: Plan to emigrate from Switzerland", align = 'left')  %>%
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

output$H4_18 <-renderText({ 
  
  x<-"Question not included in 2018 and 2020"
})

#### H5 COUNTRY FOR EMIGRATION ####

output$H5_16 <-renderHighchart({ 
  #input<-data.frame(magH="weight",magH2="Relative")
  hom <- lapply(dmms[1], function(x){
    
    x<-x[,c("H5",input$magH,"year")]
    colnames(x)<-c("H5","pop","year")
    x<-as.data.frame(x)
    x = data.frame(x, countries[match(x[,"H5"],
                                      countries[,"A3"]),c("official_title_en")])
    colnames(x)[length(names(x))]<-paste("H5","B",sep="")
    x$H5B<-as.factor(x$H5B)
    x$H5B<-fct_explicit_na(x$H5B)
    
    colnames(x)<-c("B1","pop","year","B1B")
    
    x[is.na(x)] <- -9
    x<-x %>%
      filter(B1!=-9)%>%
      filter(B1!=-7)%>%
      filter(B1B!="(Missing)")%>%
      group_by(year,B1B) %>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange(desc(pop))%>% 
      head(20)
    x
  })
  
  data<- if(input$magH2=="Absolute"){ 
    c(hom[["D16"]]$pop)}else{
      c(hom[["D16"]]$prop)}
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 400,40000)
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = as.character(hom[["D16"]]$B1B), title = list(text = '')) %>%
    hc_yAxis(title = list(text = 'N')) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Value", 
                  data = data) %>%
    hc_title(text = "2016: Top 20 countries",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(hom[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,25))%>%
    hc_exporting(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(1)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)
  rank
})

output$H5_18 <-renderHighchart({ 
  
  hom <- lapply(dmms[2], function(x){
    
    x<-x[,c("H5",input$magH,"year")]
    colnames(x)<-c("H5","pop","year")
    x<-as.data.frame(x)
    x = data.frame(x, countries[match(x[,"H5"],
                                      countries[,"A3"]),c("official_title_en")])
    colnames(x)[length(names(x))]<-paste("H5","B",sep="")
    x$H5B<-as.factor(x$H5B)
    x$H5B<-fct_explicit_na(x$H5B)
    
    colnames(x)<-c("B1","pop","year","B1B")
    
    x[is.na(x)] <- -9
    x<-x %>%
      filter(B1!=-9)%>%
      filter(B1!=-7)%>%
      filter(B1B!="(Missing)")%>%
      group_by(year,B1B) %>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange(desc(pop))%>% 
      head(20)
    x
  })
  
  data<- if(input$magH2=="Absolute"){ 
    c(hom[["D18"]]$pop)}else{
      c(hom[["D18"]]$prop)}
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 400,40000)
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = as.character(hom[["D18"]]$B1B), title = list(text = '')) %>%
    hc_yAxis(title = list(text = 'N')) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Value", 
                  data = data) %>%
    hc_title(text = "2018: Top 20 countries",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(hom[["D18"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,25))%>%
    hc_exporting(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)
  rank
})

#### H6  FROM 0 TO 10 #####

output$H6_1_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("H6_1",input$magH,"year")]
    colnames(x)<-c("H6_1","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,H6_1) %>% 
      filter(H6_1!=-7)%>%
      filter(H6_1!=-9)%>%
      filter(H6_1!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H6_1, nesting(year))
  marDF$H6_1<-as.factor(marDF$H6_1)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H6_1)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H6_1),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 2500,250000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magH2=="Relative"){
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
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,35))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "With life in general",align = 'left')  %>%
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

output$H6_2_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H6_2",input$magH,"year")]
    colnames(x)<-c("H6_2","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,H6_2) %>% 
      filter(H6_2!=-7)%>%
      filter(H6_2!=-9)%>%
      filter(H6_2!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H6_2, nesting(year))
  marDF$H6_2<-as.factor(marDF$H6_2)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H6_2)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H6_2),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 2500,250000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magH2=="Relative"){
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
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,35))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "With your job in general",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      #paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(1:2)]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

output$H6_3_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  dmms1820 <-  dmms[1]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H6_3",input$magH,"year")]
    colnames(x)<-c("H6_3","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,H6_3) %>% 
      filter(H6_3!=-7)%>%
      filter(H6_3!=-9)%>%
      filter(H6_3!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H6_3, nesting(year))
  marDF$H6_3<-as.factor(marDF$H6_3)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H6_3)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H6_3),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 2500,250000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magH2=="Relative"){
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
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,35))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "With the studies you are currently engaged in",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      #paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      #paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[c(1:2)]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank   
})

output$H6_4_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("H6_4",input$magH,"year")]
    colnames(x)<-c("H6_4","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,H6_4) %>% 
      filter(H6_4!=-7)%>%
      filter(H6_4!=-9)%>%
      filter(H6_4!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H6_4, nesting(year))
  marDF$H6_4<-as.factor(marDF$H6_4)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H6_4)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H6_4),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 2500,250000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magH2=="Relative"){
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
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,35))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "With your personal, social and family relationships",align = 'left')  %>%
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

output$H6_5_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("H6_5",input$magH,"year")]
    colnames(x)<-c("H6_5","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,H6_5) %>% 
      filter(H6_5!=-7)%>%
      filter(H6_5!=-9)%>%
      filter(H6_5!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H6_5, nesting(year))
  marDF$H6_5<-as.factor(marDF$H6_5)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H6_5)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H6_5),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 2500,250000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magH2=="Relative"){
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
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "With your decision to move to Switzerland",align = 'left')  %>%
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

output$H6_6_18 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H6_6",input$magH,"year")]
    colnames(x)<-c("H6_6","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,H6_6) %>% 
      filter(H6_6!=-7)%>%
      filter(H6_6!=-9)%>%
      filter(H6_6!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H6_6, nesting(year))
  marDF$H6_6<-as.factor(marDF$H6_6)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H6_6)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H6_6),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 2500,250000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magH2=="Relative"){
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
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,35))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "With your housing conditions",align = 'left')  %>%
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

output$H6_7_18 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H6_7",input$magH,"year")]
    colnames(x)<-c("H6_7","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,H6_7) %>% 
      filter(H6_7!=-7)%>%
      filter(H6_7!=-9)%>%
      filter(H6_7!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H6_7, nesting(year))
  marDF$H6_7<-as.factor(marDF$H6_7)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H6_7)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H6_7),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 2500,250000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magH2=="Relative"){
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
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,35))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "With the quality of life in your neighborhood",align = 'left')  %>%
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


#### H7 Discrimination means that a person is treated less favorably than other people because of different characteristics. Have you experienced situations of prejudice or discrimination in Switzerland in the last 24 months?#####

output$H7_16 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("H7",input$magH,"year")]
  
  colnames(x)<-c("H7", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(H7) %>% 
    filter(H7!=-9)%>% 
    filter(H7!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((H7))
  
  x$H7<-as.factor(x$H7)
  x$H7<-fct_explicit_na(x$H7)
  
  x
  # })
  
  levels(x$H7)<-c("Yes",
                  "No")
  
  
  data<- if(input$magH2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 5000,400000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$H7), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "2016: Experienced situations of prejudice or discrimination",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(1)]))%>% 
    hc_tooltip(enabled = TRUE)
  
  rank})

output$H7_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    x<-x[,c("H7",input$magH,"year")]
    
    colnames(x)<-c("H7", "pop","year")
    x[is.na(x)] <- -9
    
    x<-x %>%
      group_by(year, H7) %>% 
      filter(H7!=-9)%>% 
      filter(H7!=-7)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((H7))
    
    x$H7<-as.factor(x$H7)
    x$H7<-fct_explicit_na(x$H7)
    levels(x$H7)<-c("Never",
                    "Rarely",
                    "Sometimes",
                    "Frequently")
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H7, nesting(year))
  marDF$H7<-as.factor(marDF$H7)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H7)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H7),])
  
  
  
  marDF1<-if(input$magH2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF1)<-c("value","year", "prop")
  
  marDF1<-as.data.frame(marDF1)
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 5000,400000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    
    hc_xAxis(categories = levels(as.factor(marDF1$value)), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = marDF1[marDF1$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = marDF1[marDF1$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = marDF1[marDF1$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy')%>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Experienced situations of prejudice or discrimination",align = 'left')  %>%
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

#### H8 In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place? Because of. ##### 

output$H8_1_16 <-renderHighchart({ 
  # dmms1820 <-  dmms[1]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("H8_1","H8_2","H8_3",
            "H8_4","H8_5","H8_6",
            "H8_7","H8_8",
            input$magH,"year")]#iinput$magH,
    
    colnames(x)<-c("H8_1","H8_2","H8_3",
                   "H8_4","H8_5","H8_6",
                   "H8_7","H8_8",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(H8_1,H8_2,H8_3,
                                       H8_4,H8_5,H8_6,
                                       H8_7,H8_8),
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
  
  levels(marDF$groupz)<-c("Racist reasons",
                          "Your immigrant background, origin, nationality",
                          "Your religion",
                          "Your gendre",
                          "Your disability, impairment or chronic disease",
                          "Your age",
                          "Your sexual orientation",
                          "Other")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 2000, 200000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magH2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms, function(x){
    
    x<-x[,c("H8_1",
            input$magH,"year")]#iinput$magH,
    
    colnames(x)<-c("H8_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(H8_1),
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
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "In your opinion, on the basis of what actual or assigned characteristic(s) has this discrimination taken place?",align = 'left')  %>%
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

##### H9Where did you experience this discrimination? Was it #####

output$H9_1_16 <-renderHighchart({ 
  
  #input<-data.frame(magH="weight",magH2="Relative")
  # dmms1820 <-  dmms[1]
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("H9_1","H9_2","H9_3",
            "H9_4","H9_6",
            input$magH,"year")]#iinput$magH,
    
    colnames(x)<-c("H9_1","H9_2","H9_3",
                   "H9_4","H9_6",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(H9_1,H9_2,H9_3,
                                       H9_4,H9_6),
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
  
  marDF$groupz<-as.factor(marDF$groupz)
  #levels( marDF$groupz)
  #marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                           "B27_4","B27_5","B27_6"))
  
  levels(marDF$groupz)<-c("During education and work",
                          "In shops, in public and/or during leisure activities",
                          "In healthcare and care",
                          "By public authorities",
                          # "On the Internet",
                          "In another situation or by other persons")
  marDF[is.na(marDF)] <- 0
  
  
  mar1 <- lapply(dmms[1], function(x){
    
    x<-x[,c("H9_5",
            input$magH,"year")]#iinput$magH,
    
    colnames(x)<-c("H9_5",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(H9_5),
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
  marDF1<- as.data.frame(do.call("rbind", mar1)) #CATS 1,2,4,5,6
  
  marDF1$groupz<-as.factor(marDF1$groupz)
  #levels( marDF$groupz)
  #marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                           "B27_4","B27_5","B27_6"))
  
  levels(marDF1$groupz)<-c("On the Internet")
  marDF1[is.na(marDF1)] <- 0
  
  
  mar1820 <- lapply(dmms[2:3], function(x){
    
    x<-x[,c("H9_7","H9_8",
            input$magH,"year")]#iinput$magH,
    
    colnames(x)<-c("H9_7","H9_8",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(H9_7,H9_8),
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
  marDF1820<- as.data.frame(do.call("rbind", mar1820)) #CATS 1,2,4,5,6
  
  marDF1820$groupz<-as.factor(marDF1820$groupz)
  #levels( marDF$groupz)
  #marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                           "B27_4","B27_5","B27_6"))
  
  levels(marDF1820$groupz)<-c("During the search for a job",
                              "When applying for an apartment")
  marDF1820[is.na(marDF1820)] <- 0
  
  
  
  marfinal<-rbind(marDF,marDF1,marDF1820)
  
  #marfinal<-marfinal %>% complete(marfinal, nesting(year))
  
  marfinal<-with(marfinal, marfinal[order(groupz),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 2000, 200000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marfinal<-if(input$magH2=="Relative"){
    marfinal[,c(1,2,4)]
  } else {
    marfinal[,c(1,2,3)]}
  
  colnames(marfinal)<-c("year","groupz","prop")
  
  marfinal$year<-paste("Wave ",marfinal$year, sep="")
  
  mar2 <- lapply(dmms, function(x){
    
    x<-x[,c("H9_1",
            input$magH,"year")]#iinput$magH,
    
    colnames(x)<-c("H9_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(H9_1),
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
  marDFs<- as.data.frame(do.call("rbind", mar2))
  
  rank <-marfinal %>% 
    hchart('bar', hcaes(x = 'groupz', y = 'prop', group = 'year')) %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(title = list(text = '')) %>%
    hc_xAxis(title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Where did you experience this discrimination?",align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDFs[marDFs$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDFs[marDFs$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDFs[marDFs$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
})

#### H12 To deal with the discrimination of foreigners in the labour market, some people slightly modify their curriculum vitae to hide their origin. Did you use one of the following strategies when writing your curriculum vitae in order to increase the possibility of success, when looking for a job in Switzerland?#### 

output$H12_1_18 <-renderHighchart({  
  # input<-data.frame(magH="weight",magH2="Relative")
  dmms1820 <-  dmms[2]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H12_1","H12_2","H12_3",
            "H12_4","H12_5","H12_6",
            "H12_7",
            input$magH,"year")]#iinput$magG,
    
    colnames(x)<-c("H12_1","H12_2","H12_3",
                   "H12_4","H12_5","H12_6",
                   "H12_7",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(H12_1,H12_2,H12_3,
                                       H12_4,H12_5,H12_6,
                                       H12_7),
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
  
  levels(marDF$groupz)<-c("Removing information or photos referring to your place of origin",
                          "Modifying your name or surname in order to hide your origin",
                          "Adding some diploma equivalences or professional experiences in Switzerland",
                          "Mentioning social or volunteer activities specifically in Switzerland",
                          "Mentioning pastimes or hobbies related to Switzerland",
                          "Providing an address (of a friend of relative) in Switzerland, even if you were living abroad",
                          "None of those strategies")
  marDF[is.na(marDF)] <- 0
  
  marDF<-with(marDF, marDF[order(groupz),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 7000, 725000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF<-if(input$magH2=="Relative"){
    marDF[,c(1,2,4)]
  } else {
    marDF[,c(1,2,3)]}
  
  colnames(marDF)<-c("groupz","year","prop")
  
  marDF$year<-paste("Wave ",marDF$year, sep="")
  
  mar1 <- lapply(dmms1820, function(x){
    
    x<-x[,c("H12_1",
            input$magH,"year")]#iinput$magG,
    
    colnames(x)<-c("H12_1",
                   "pop","year")
    
    xlong<-x %>%gather(groupz, value,c(H12_1),
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
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Did you use one of the following strategies when writing your curriculum vitae in order to increase the possibility of success, when looking for a job in Switzerland?",align = 'left')  %>%
    hc_subtitle(text =paste(
      #paste("\n2016 N",a(sum(marDF1[marDF1$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF1[marDF1$year==2018,"pop"])),sep=": "),
      #paste("\n2020 N",a(sum(marDF1[marDF1$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE) %>%
    hc_colors(c(gg_color_hue(3)[2]))%>%  
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank  
  
  
})

#### H10 Considering all of your income and your expenses over a year, would you say you currently.#####

output$H10_16 <-renderHighchart({ 
  
  #  mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c("H10",input$magH,"year")]
  
  colnames(x)<-c("H10", "pop","year")
  x[is.na(x)] <- -9
  
  x<-x %>%
    group_by(H10) %>% 
    filter(H10!=-9)%>% 
    filter(H10!=-7)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange((H10))
  
  x$H10<-as.factor(x$H10)
  x$H10<-fct_explicit_na(x$H10)
  
  x
  # })
  
  levels(x$H10)<-c("Put money aside",
                   "Spend what you earn",
                   "Consume your savings or your heritage, your reservations",
                   "Go into debt")
  
  
  data<- if(input$magH2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 5000,400000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$H10), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magG2=="Absolute",0,0),
             max=ifelse(input$magG2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "Considering all of your income and your expenses over a year, would you say you currently.",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[1]))%>%  
    hc_tooltip(enabled = TRUE)
  
  rank})


#### H11  FROM 0 TO 7 #####

output$H11_1_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("H11_1",input$magH,"year")]
    colnames(x)<-c("H11_1","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,H11_1) %>% 
      filter(H11_1!=-7)%>%
      filter(H11_1!=-9)%>%
      filter(H11_1!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H11_1, nesting(year))
  marDF$H11_1<-as.factor(marDF$H11_1)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H11_1)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H11_1),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw",2500,250000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magH2=="Relative"){
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
    hc_title(text = "To what extent do you have a feeling of attachment to Switzerland?",align = 'left')  %>%
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

output$H11_2_16 <-renderHighchart({ 
  
  # input<-data.frame(magG="weight",magG2="Relative")
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c("H11_2",input$magH,"year")]
    colnames(x)<-c("H11_2","pop","year")
    x[is.na(x)] <- -9
    
    
    x<-x %>%
      group_by(year,H11_2) %>% 
      filter(H11_2!=-7)%>%
      filter(H11_2!=-9)%>%
      filter(H11_2!=-8)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round((pop/sum(pop))*100,1))
    x
  })
  
  marDF<- as.data.frame(do.call("rbind", mar)) #CATS 1,2,4,5,6
  
  marDF<-marDF %>% complete(H11_2, nesting(year))
  marDF$H11_2<-as.factor(marDF$H11_2)
  #levels( marDF$groupz)
  #   marDF$groupz<-factor(marDF$groupz, levels=c("B27_1","B27_2","B27_3",
  #                                              "B27_4","B27_5","B27_6"))
  
  #levels(marDF$H11_2)<-c("in Switzerland")
  marDF[is.na(marDF)] <- 0
  
  
  marDF<-with(marDF, marDF[order(H11_2),])
  
  hc_yAxis<-ifelse(input$magH=="n_nw",2500,250000)
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  # marDF<-as.data.frame(marDF)
  #marDF<-ifelse(input$mag3=="Relative","prop","pop")
  marDF1<-if(input$magH2=="Relative"){
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
    hc_title(text = "To what extent do you have a feeling of attachment to your home country of origin?",align = 'left')  %>%
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

#####H13  How often have you positive contacts with Swiss people? #####

output$H13_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H13",input$magH,"year")]#input$BF,input$magH,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("H13", "pop","year")
    
    
    x<-x %>%
      group_by(year,H13) %>% 
      filter(H13!="(Missing)")%>%
      filter(H13!=-7)%>%
      filter(H13!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((H13))
    
    x$H13<-as.factor(x$H13)
    x$H13<-fct_explicit_na(x$H13)
    
    x
  })
  
  
  levels(mar[["D18"]]$H13)<-c("Never",
                              "From time to time",
                              "Often",
                              "Very often")
  
  levels(mar[["D20"]]$H13)<-c("Never",
                              "From time to time",
                              "Often",
                              "Very often")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$H13<-as.factor(marDF$H13)
  
  data<- if(input$magH2=="Absolute"){ 
    marDF[,c("year","H13", "pop")]}else{
      marDF[,c("year","H13", "prop")]}
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 5000,400000)
  
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$H13), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "How often have you positive contacts with Swiss people?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#####H14  How often have you positive contacts with Swiss people? #####

output$H14_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H14",input$magH,"year")]#input$BF,input$magH,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("H14", "pop","year")
    
    
    x<-x %>%
      group_by(year,H14) %>% 
      filter(H14!="(Missing)")%>%
      filter(H14!=-7)%>%
      filter(H14!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((H14))
    
    x$H14<-as.factor(x$H14)
    x$H14<-fct_explicit_na(x$H14)
    
    x
  })
  
  
  levels(mar[["D18"]]$H14)<-c("Never",
                              "From time to time",
                              "Often",
                              "Very often")
  
  levels(mar[["D20"]]$H14)<-c("Never",
                              "From time to time",
                              "Often",
                              "Very often")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$H14<-as.factor(marDF$H14)
  
  data<- if(input$magH2=="Absolute"){ 
    marDF[,c("year","H14", "pop")]}else{
      marDF[,c("year","H14", "prop")]}
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 5000,400000)
  
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$H14), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      #  paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "How often have you negative contacts with Swiss people?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

##### H15 When arriving in Switzerland, did you have any plans regarding the duration of your stay?#####

output$H15_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H15",input$magH,"year")]#input$BF,input$magH,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("H15", "pop","year")
    
    
    x<-x %>%
      group_by(year,H15) %>% 
      filter(H15!="(Missing)")%>%
      filter(H15!=-7)%>%
      filter(H15!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((H15))
    
    x$H15<-as.factor(x$H15)
    x$H15<-fct_explicit_na(x$H15)
    
    x
  })
  
  
  levels(mar[["D18"]]$H15)<-c("Yes, I wanted to stay in Switzerland for a limited duration (less than 1 year)",
                              "Yes, I wanted to stay one or more years in Switzerland, but ultimately leave the country",
                              "Yes, I wanted to stay all my life in Switzerland",
                              "No, I did not have a well defined plan")
  
  levels(mar[["D20"]]$H15)<-c("Yes, I wanted to stay in Switzerland for a limited duration (less than 1 year)",
                              "Yes, I wanted to stay one or more years in Switzerland, but ultimately leave the country",
                              "Yes, I wanted to stay all my life in Switzerland",
                              "No, I did not have a well defined plan")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$H15<-as.factor(marDF$H15)
  
  data<- if(input$magH2=="Absolute"){ 
    marDF[,c("year","H15", "pop")]}else{
      marDF[,c("year","H15", "prop")]}
  
  hc_yAxis<-ifelse(input$magH=="n_nw", 5000,400000)
  
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$H15), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "When arriving in Switzerland, did you have any plans regarding the duration of your stay?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})

#### H16 And now, what are your plans regarding the duration of your stay in Switzerland?#####

output$H16_18 <-renderHighchart({ 
  
  dmms1820 <-  dmms[2:3]
  mar <- lapply(dmms1820, function(x){
    
    x<-x[,c("H16",input$magH,"year")]#input$BF,input$magH,
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("H16", "pop","year")
    
    
    x<-x %>%
      group_by(year,H16) %>% 
      filter(H16!="(Missing)")%>%
      filter(H16!=-7)%>%
      filter(H16!=-9)%>%
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((H16))
    
    x$H16<-as.factor(x$H16)
    x$H16<-fct_explicit_na(x$H16)
    
    x
  })
  
  
  levels(mar[["D18"]]$H16)<-c("I will stay in Switzerland for a limited period (less than 1 year)",
                              "I will stay one or more years in Switzerland, but ultimately leave the country",
                              "I will stay all my life in Switzerland",
                              "I do not have a well defined plan")
  
  levels(mar[["D20"]]$H16)<-c("I will stay in Switzerland for a limited period (less than 1 year)",
                              "I will stay one or more years in Switzerland, but ultimately leave the country",
                              "I will stay all my life in Switzerland",
                              "I do not have a well defined plan")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$H16<-as.factor(marDF$H16)
  
  data<- if(input$magH2=="Absolute"){ 
    marDF[,c("year","H16", "pop")]}else{
      marDF[,c("year","H16", "prop")]}
  
  hc_yAxis<-ifelse(input$magH=="n_nw",5000,400000)
  
  
  formatter<- ifelse(input$magH2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$H16), title = list(text = '')) %>%
    #hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magH2=="Absolute",0,0),
             max=ifelse(input$magH2=="Absolute",hc_yAxis,100))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      # paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "And now, what are your plans regarding the duration of your stay in Switzerland?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[c(2:3)]))%>% 
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
})



