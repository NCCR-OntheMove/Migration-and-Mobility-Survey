### D1 What is the highest level of education you have successfully completed? ####

output$D1_16 <-renderHighchart({ 
  
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BD,input$magD,"year")]
    #x<-x[,c("A8","weight","year")]
    colnames(x)<-c("D1", "pop","year")
    x<-x %>%
      group_by(year,D1) %>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange((D1))
    
    x$D1<-as.factor(x$D1)
    x$D1<-fct_explicit_na(x$D1)
    
    x
  })
  
  levels(mar[["D16"]]$D1)<-c("No formal educational qualification",
                             "Compulsory education",
                             "Higher secondary education not giving access to universities (or similar)",
                             "Vocational education and/or training",
                             "High school-leaving certificate giving access to universities (or similar)",
                             "Advanced technical and professional training",
                             "Bachelor or equivalent",
                             "Master or equivalent",
                             "Phd Doctoral or equivalent")
  
  levels(mar[["D18"]]$D1)<-c("No formal educational qualification",
                             "Compulsory education",
                             "Higher secondary education not giving access to universities (or similar)",
                             "Vocational education and/or training",
                             "High school-leaving certificate giving access to universities (or similar)",
                             "Advanced technical and professional training",
                             "Bachelor or equivalent",
                             "Master or equivalent",
                             "Phd Doctoral or equivalent")
  
  levels(mar[["D20"]]$D1)<-c("No formal educational qualification",
                             "Compulsory education",
                             "Higher secondary education not giving access to universities (or similar)",
                             "Vocational education and/or training",
                             "High school-leaving certificate giving access to universities (or similar)",
                             "Advanced technical and professional training",
                             "Bachelor or equivalent",
                             "Master or equivalent",
                             "Phd Doctoral or equivalent")
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$D1<-as.factor(marDF$D1)
  
  data<- if(input$magD2=="Absolute"){ 
    marDF[,c("year","D1", "pop")]}else{
      marDF[,c("year","D1", "prop")]}
  
  hc_yAxis<-ifelse(input$magD=="n_nw", 6000,475000)
  
  formatter<- ifelse(input$magD2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(marDF$D1), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$mag2=="Absolute",0,0),
             max=ifelse(input$mag2=="Absolute",hc_yAxis,70))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_title(text = "What is the highest level of education you have successfully completed?",
             align = 'left')  %>%
    #hc_subtitle(text =  paste("\nN",a(sum(c(mar[["D16"]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank
  
  
}) 


#### D2 In which country did you attain your highest educational qualification?  ###### 

output$D2_16 <-renderHighchart({ 
  
  nac <- lapply(dmms, function(x){
    
    x<-x[,c(input$BD,input$magD,"year")]#input$BD,input$magD
    colnames(x)<-c("D2","pop","year")
    x<-as.data.frame(x)
    x = data.frame(x, countries[match(x[,"D2"],
                                      countries[,"A3"]),c("official_title_en")])
    colnames(x)[length(names(x))]<-paste("D2","B",sep="")
    x$D2B<-as.factor(x$D2B)
    x$D2B<-fct_explicit_na(x$D2B)
    
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
  
  nacdf<-nacdf[nacdf$year==input$yearD,]
  
  data<- if(input$magD2=="Absolute"){ 
    nacdf[,c("year","A3B","pop")]}else{
      nacdf[,c("year","A3B", "prop")]}
  
  formatter<- ifelse(input$magD2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  hc_yAxis<-ifelse(input$magD=="n_nw", 700,175000)
  
  color<-ifelse(input$yearD==2016,1,
                ifelse(input$yearD==2018,2,3))
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = as.character(data$A3B), title = list(text = '')) %>%
    #hc_yAxis(title = list(text = 'N')) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(data = data[,3])%>%
    hc_title(text = paste("Top 20 countries of highest education:",input$yearD,sep=" "),
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(nac[[
      
      if(input$yearD==2016){ 
        1}else{
          if(input$yearD==2018){   
            2}else{3}}
      
    ]]$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magD2=="Absolute",0,0),
             max=ifelse(input$magD2=="Absolute",hc_yAxis,35))%>%
    hc_exporting(enabled = TRUE)%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[color]))
  rank
}) 



#### D3 HIGHER LEVEL OF EDUCATION IN HOME COUNTRY #####

output$D3_16 <-renderHighchart({ 
  #input<-data.frame(BD="D3", magD="weight",magD2="Relative")
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BD,input$magD, "year")]#input$BD,input$magD,
    
    colnames(x)<-c("D3", "pop","year")
    x[is.na(x)] <- -9
    
    x<-x %>%
      group_by(year,D3) %>% 
      filter(D3!=-9)%>% 
      filter(D3!=-7)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange(desc(D3))
    
    x$D3<-as.factor(x$D3)
    x$D3<-fct_explicit_na(x$D3)
    
    x
  })
  
  levels(mar[["D16"]]$D3)<-c("Compulsory education",
                             "Higher secondary education not giving access to universities (or similar)",
                             "Vocational education and/or training",
                             "High school-leaving certificate giving access to universities (or similar)",
                             "Advanced technical and professional training",
                             "Bachelor or equivalent",
                             "Master or equivalent",
                             "Phd Doctoral or equivalent",
                             "I have never been in school in my home country of origin [B1]")
  
  levels(mar[["D18"]]$D3)<-c("Compulsory education",
                             "Higher secondary education not giving access to universities (or similar)",
                             "Vocational education and/or training",
                             "High school-leaving certificate giving access to universities (or similar)",
                             "Advanced technical and professional training",
                             "Bachelor or equivalent",
                             "Master or equivalent",
                             "Phd Doctoral or equivalent",
                             "I have never been in school in my home country of origin [B1]")
  
  levels(mar[["D20"]]$D3)<-c("Compulsory education",
                             "Higher secondary education not giving access to universities (or similar)",
                             "Vocational education and/or training",
                             "High school-leaving certificate giving access to universities (or similar)",
                             "Advanced technical and professional training",
                             "Bachelor or equivalent",
                             "Master or equivalent",
                             "Phd Doctoral or equivalent",
                             "I have never been in school in my home country of origin [B1]")
  
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$D3<-as.factor(marDF$D3)
  
  data<- if(input$magD2=="Absolute"){ 
    marDF[,c("year","D3", "pop")]}else{
      marDF[,c("year","D3", "prop")]}
  
  data<-with(data, data[order(D3),])
  hc_yAxis<-ifelse(input$magD=="n_nw", 750,50000)
  
  formatter<- ifelse(input$magD2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$D3), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magD2=="Absolute",0,0),
             max=ifelse(input$magD2=="Absolute",hc_yAxis,50))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "What is the highest level of education that you have completed in your home country of origin?",
             align = 'left')  %>%
    hc_subtitle(text =paste(
      paste("\n2016 N",a(sum(marDF[marDF$year==2016,"pop"])),sep=": "),
      paste("\n2018 N",a(sum(marDF[marDF$year==2018,"pop"])),sep=": "),
      paste("\n2020 N",a(sum(marDF[marDF$year==2020,"pop"])),sep=": "),
      sep=" | "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)))%>%     hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")     
  
  rank})

#### D4 Did you make an official request in Switzerland in order to obtain a certificate of equivalence for your educational qualifications? #####

output$D4_16 <-renderHighchart({ 
  #input<-data.frame(BD="D3", magD="weight",magD2="Relative")
  mar <- lapply(dmms, function(x){
    
    x<-x[,c(input$BD,input$magD, "year")]#input$BD,input$magD,
    
    colnames(x)<-c("D3", "pop","year")
    x[is.na(x)] <- -9
    
    x<-x %>%
      group_by(year,D3) %>% 
      filter(D3!=-9)%>% 
      filter(D3!=-7)%>% 
      #replace_with_na(replace = list(A6 = -9))%>% 
      summarise(pop=round(sum(pop),0))%>%
      mutate(prop=round(pop/sum(pop)*100,1))%>%
      arrange(desc(D3))
    
    x$D3<-as.factor(x$D3)
    x$D3<-fct_explicit_na(x$D3)
    
    x
  })
  
  levels(mar[["D16"]]$D3)<-c("Yes, the certificate was obtained",
                             "Yes, but the certificate was not obtained",
                             "Yes, but the procedure is not yet complete",
                             "No, it was not necessary",
                             "No, other reasons")
  
  levels(mar[["D18"]]$D3)<-c("Yes, the certificate was obtained",
                             "Yes, but the certificate was not obtained",
                             "Yes, but the procedure is not yet complete",
                             "No, it was not necessary",
                             "No, other reasons")
  
  levels(mar[["D20"]]$D3)<-c("Yes, the certificate was obtained",
                             "Yes, but the certificate was not obtained",
                             "Yes, but the procedure is not yet complete",
                             "No, it was not necessary",
                             "No, other reasons")
  
  
  marDF<- as.data.frame(do.call("rbind", mar))
  marDF$D3<-as.factor(marDF$D3)
  
  data<- if(input$magD2=="Absolute"){ 
    marDF[,c("year","D3", "pop")]}else{
      marDF[,c("year","D3", "prop")]}
  
  data<-with(data, data[order(D3),])
  hc_yAxis<-ifelse(input$magD=="n_nw", 5000,500000)
  
  formatter<- ifelse(input$magD2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>% 
    #hc_chart(type = "bar") %>%
    hc_xAxis(categories = levels(data$D3), title = list(text = '')) %>%
    hc_add_series(name= "Wave 2016",data = data[data$year==2016,3])%>%
    hc_add_series(name= "Wave 2018",data = data[data$year==2018,3])%>%
    hc_add_series(name= "Wave 2020",data = data[data$year==2020,3])%>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled =TRUE ) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magD2=="Absolute",0,0),
             max=ifelse(input$magD2=="Absolute",hc_yAxis,75))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    #hc_add_series(name = "Value", data = data) %>%
    hc_title(text = "Did you make an official request in Switzerland in order to obtain a certificate of equivalence for your educational qualifications?",
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
  
  rank})

#### D7 Please enter the formal education in which you are currently engaged #####

output$D7_16 <-renderHighchart({ 
  
  #mar <- lapply(dmms, function(x){
  
  x<-dmms[["D16"]][,c(input$BD,input$magD,"year")]
  
  colnames(x)<-c("D7", "pop","year")
  x[is.na(x)] <- 99
  
  x<-x %>%
    group_by(D7) %>% 
    filter(D7!=99)%>% 
    #replace_with_na(replace = list(A6 = -9))%>% 
    summarise(pop=round(sum(pop),0))%>%
    mutate(prop=round(pop/sum(pop)*100,1))%>%
    arrange(desc(D7))
  
  x$D7<-as.factor(x$D7)
  x$D7<-fct_explicit_na(x$D7)
  
  x
  # })
  
  levels(x$D7)<-c("Compulsory education",
                  "Higher secondary education not giving access to universities (or similar)",
                  "Vocational education and/or training",
                  "High school-leaving certificate giving access to universities (or similar)",
                  "Advanced technical and professional training",
                  "Bachelor or equivalent",
                  "Master or equivalent",
                  "Phd Doctoral or equivalent")
  
  
  data<- if(input$magD2=="Absolute"){ 
    c(x$pop)}else{
      c(x$prop)}
  
  hc_yAxis<-ifelse(input$magD=="n_nw", 200,20000)
  
  formatter<- ifelse(input$magD2=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                     "function(){ return Math.abs(this.value); }")
  
  rank <- highchart() %>%
    hc_chart(type = 'bar',zoomType= 'xy') %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(categories = as.factor(x$D7), title = list(text = '')) %>%
    hc_yAxis(labels = list(formatter = JS(formatter)),
             min=ifelse(input$magD2=="Absolute",0,0),
             max=ifelse(input$magD2=="Absolute",hc_yAxis,30))%>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = "Wave 2016", data = data) %>%
    hc_title(text = "2016: Formal education in which you are currently engaged",
             align = 'left')  %>%
    hc_subtitle(text =  paste("\nN",a(sum(c(x$pop))),sep=": "))%>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")%>%
    hc_exporting(enabled = TRUE)%>%
    hc_tooltip(enabled = TRUE)%>%
    hc_colors(c(gg_color_hue(3)[1]))%>%
    hc_credits(enabled = TRUE, text = "NCCR ON THE MOVE",href = "https://nccr-onthemove.ch/")
  
  
  rank})



output$D7_18 <-renderText({ 
  
  x<-"Question not included in 2018 and 2020"
})






