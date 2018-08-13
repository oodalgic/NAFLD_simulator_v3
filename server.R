# Libraries
library(shiny)
library(ggplot2)
#library(ggthemes)
#library(viridis)
library(reshape2)
library(rmarkdown)
library(shinythemes)
library(rsconnect)
library(grid)
library(cowplot)
library(RColorBrewer)
library(shinyjs)
library(ReporteRs)

#data=read.csv("data/All_results_combined_NN_removed.csv",stringsAsFactors = T)#Data file, subject to changes
data=read.csv("data/concatenated_input_data.csv",stringsAsFactors = T,row.names = 1)#Data file, subject to changes
data_15=data[data$Year<16,]#This is to limit the analyses to the first 15 years
data_15[,6:13]=data_15[,6:13]*100
measure=colnames(data_15)[5:ncol(data_15)]#this gets the colnames (outcomes) from the csv header 
measure_mort=measure[c(7,8,9)]
measure_ao=measure[c(2,3)]
measure_inc=measure[c(5,6)]
#this gets the name of the conditions (NASh, NAFL,..) and fiborsis stages (F0,..,F4)- sorted in the prefer#3c4d68 order (sorting is manual and hard coded)data$
stages=unique(data$Stage)
stages=stages[sort.list(stages, method = "shell")]
stages=stages[c(6,1:5)]#sorts the stages
stageNames<-c("NAFL NAFL (No NASH)","Stage 0","Stage 1","Stage 2", "Stage 3", "Stage 4")
stages<-setNames(as.character(stages),stageNames)
# names(stages)<-stageNames
my_colors=c("#375E97","#FB6542","#FFBB00","#3F681C","#003B46","#F4CC70")

#This is where the shiny app starts


library(scales)
my_colors2 <- c("#A4243B","#D8C99B","#D8973C", "#BD632F","#95A0A5", "#273E47")#brewer.pal(9,"Blues")[4:10]#"#4E6168"
my_colors3<-c('#FFFFFF','#E6EEF0','#C3D1DA','#A5BACB','#7E98A9','#476A7E','#274456')
mortality_colors=c("#68829E","#505160","#79832E")
# Define server to print the input and output panels
shinyServer(function(input, output,session) {
  
  
  
  
  
  
  
  
  


  #updateCheckboxInput(session, "stage2", value = input$stage)
  #Text outputs for all tabs 
  observe({
    if("NAFL" %in% input$stage2){
      runjs("
            
            document.getElementById('NAFL').style.display='block';
            document.getElementById('NAFL2').style.display='block';
            document.getElementById('NAFL3').style.display='block';
            document.getElementById('NAFL4').style.display='block';
            ")
      
    }
    else{
      runjs("
            document.getElementById('NAFL').style.display='none';
            document.getElementById('NAFL2').style.display='none';
            document.getElementById('NAFL3').style.display='none';
            document.getElementById('NAFL4').style.display='none';
            ")
    }
    
    if("F0" %in% input$stage2){
      runjs("
            
            document.getElementById('F0').style.display='block';
            document.getElementById('F02').style.display='block';
            document.getElementById('F03').style.display='block';
            document.getElementById('F04').style.display='block';
            ")
      
    }
    else{
      runjs("
            document.getElementById('F0').style.display='none';
            document.getElementById('F02').style.display='none';
            document.getElementById('F03').style.display='none';
            document.getElementById('F04').style.display='none';
            ")
    }
    
    if("F1" %in% input$stage2){
      runjs("
            
            document.getElementById('F1').style.display='block';
            document.getElementById('F12').style.display='block';
            document.getElementById('F13').style.display='block';
            document.getElementById('F14').style.display='block';
            ")
      
    }
    else{
      runjs("
             document.getElementById('F1').style.display='none';
            document.getElementById('F12').style.display='none';
            document.getElementById('F13').style.display='none';
            document.getElementById('F14').style.display='none';
            ")
    }
    
    if("F2" %in% input$stage2){
      runjs("
            
            document.getElementById('F2').style.display='block';
            document.getElementById('F22').style.display='block';
            document.getElementById('F23').style.display='block';
            document.getElementById('F24').style.display='block';
            ")
      
    }
    else{
      runjs("
            document.getElementById('F2').style.display='none';
            document.getElementById('F22').style.display='none';
            document.getElementById('F23').style.display='none';
            document.getElementById('F24').style.display='none';
            ")
    }
    
    if("F3" %in% input$stage2){
      runjs("
            
            document.getElementById('F3').style.display='block';
            document.getElementById('F32').style.display='block';
            document.getElementById('F33').style.display='block';
            document.getElementById('F34').style.display='block';
            ")
      
    }
    else{
      runjs("
            document.getElementById('F3').style.display='none';
            document.getElementById('F32').style.display='none';
            document.getElementById('F33').style.display='none';
            document.getElementById('F34').style.display='none';
            ")
    }
    
    if("F4" %in% input$stage2){
      runjs("
            
            document.getElementById('F4').style.display='block';
            document.getElementById('F42').style.display='block';
            document.getElementById('F43').style.display='block';
            document.getElementById('F44').style.display='block';
            ")
      
    }
    else{
      runjs("
            document.getElementById('F4').style.display='none';
            document.getElementById('F42').style.display='none';
            document.getElementById('F43').style.display='none';
            document.getElementById('F44').style.display='none';
            ")
    }
    if(length(input$stage2)==0){
      runjs("
            document.getElementById('report').style.pointerEvents= 'none';
            document.getElementById('report').style.opacity=0.5;
            ")
      
    }
    else{
      runjs("
            document.getElementById('report').style.pointerEvents= 'auto';
            document.getElementById('report').style.opacity=1;
            ")
      
    }
    
    
    
    
  })
  output$DP_text<-renderText({"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur diam mi, vestibulum at porta sit amet, ornare a tellus. Curabitur blandit finibus sapien, at facilisis risus sodales sed. Sed dignissim justo purus, et fermentum augue tincidunt at. Nam rutrum dolor nec lectus semper, id congue erat porta. Pellentesque ligula eros, molestie at diam sed, dapibus congue enim. Nam ac dignissim ante. Mauris interdum, risus in pellentesque vulputate, felis nunc porta tellus, sit amet pulvinar urna elit nec risus. Sed at lorem velit. Proin nec elit euismod, volutpat ex vel, eleifend libero. Vestibulum venenatis quam mauris, ut accumsan risus pulvinar nec. Nullam semper non erat sed aliquam. Vivamus eu suscipit tortor. Vivamus molestie quam quis nibh convallis euismod."})#Tab1 text
  #output$EI_verbatim<-renderText({"Nulla eleifend augue blandit, suscipit quam a, pharetra dolor. Praesent convallis purus in nunc bibendum tempus. Donec id mauris ut ligula ultricies vulputate id ac arcu. Fusce accumsan est turpis, sed commodo elit molestie ac. Praesent feugiat felis eget ex tempor venenatis. Suspendisse potenti. Nunc ante arcu, laoreet ac pharetra id, lobortis vehicula sapien. Ut diam justo, dignissim vitae rutrum vehicula, vestibulum ut eros. Sed fringilla consequat viverra. Maecenas sed augue ut ex mattis mollis id nec nisl. Duis mattis ex augue, non consectetur tellus venenatis vel. Vivamus non velit auctor ante hendrerit pulvinar."})#Tab2 verbatim
  dp_text_start="This analysis compares the survival, mortality and advanced outcomes across two more stages
                of your choice."

  observe({
    dp_text_var="Please select at least two stages from the left side panel."
    dp_text_end=""
    if(length(input$stage2)>=2){
      if(length(input$stage2)==2){
        dp_text_var=paste("You are currently comparing: " ,paste(paste("<span style='color:red;font-weight:bold;'>",input$stage2,'</span>',sep=""),collapse = " and "),".", sep="")
        # dp_text_end=paste("<br> 10-year outcomes <br>", 
        #                   "Overall Survival: ",
        #                   
        #                   paste(paste(input$stage2,":",round(data_15[data_15$Stage%in%input$stage2&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,"Overall_Survival"],0),"%"),collapse = " &#8594 "))
        }
      if(length(input$stage2)>2){
        dp_text_var=paste("You are currently comparing: " ,paste(paste("<span style='color:red;font-weight:bold;'>",input$stage2[2:length(input$stage2)-1],'</span>'),collapse = ", "), ", and ","<span style='color:red;font-weight:bold;'>",input$stage2[length(input$stage2)],"</span>",".", sep="")
        
      }
      dp_text_end=paste("<br> 10-year outcomes <br>", 
                        "Overall Survival: ",
                        
                        paste(paste(paste("<span style='color:red;font-weight:bold;'>",input$stage2,'</span>',sep=""),":",round(data_15[data_15$Stage%in%input$stage2&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,"Overall_Survival"],0),"%"),collapse = " &#8594 "))
      
      
    }
    
    dp_text=paste(dp_text_start,dp_text_var,dp_text_end,sep=" ")
    output$CA_text<-renderUI({HTML(dp_text)})
  })
  #Tab2 text
  #output$M_verbatim<-renderText({"Nulla eleifend augue blandit, suscipit quam a, pharetra dolor. Praesent convallis purus in nunc bibendum tempus. Donec id mauris ut ligula ultricies vulputate id ac arcu. Fusce accumsan est turpis, sed commodo elit molestie ac. Praesent feugiat felis eget ex tempor venenatis. Suspendisse potenti. Nunc ante arcu, laoreet ac pharetra id, lobortis vehicula sapien. Ut diam justo, dignissim vitae rutrum vehicula, vestibulum ut eros. Sed fringilla consequat viverra. Maecenas sed augue ut ex mattis mollis id nec nisl. Duis mattis ex augue, non consectetur tellus venenatis vel. Vivamus non velit auctor ante hendrerit pulvinar."})#Tab2 verbatim
  
  # output$AO_text<-renderText({"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur diam mi, vestibulum at porta sit amet, ornare a tellus. Curabitur blandit finibus sapien, at facilisis risus sodales sed. Sed dignissim justo purus, et fermentum augue tincidunt at. Nam rutrum dolor nec lectus semper, id congue erat porta. Pellentesque ligula eros, molestie at diam sed, dapibus congue enim. Nam ac dignissim ante. Mauris interdum, risus in pellentesque vulputate, felis nunc porta tellus, sit amet pulvinar urna elit nec risus. Sed at lorem velit. Proin nec elit euismod, volutpat ex vel, eleifend libero. Vestibulum venenatis quam mauris, ut accumsan risus pulvinar nec. Nullam semper non erat sed aliquam. Vivamus eu suscipit tortor. Vivamus molestie quam quis nibh convallis euismod."})#Tab3 text
  # output$AO_verbatim<-renderText({"Nulla eleifend augue blandit, suscipit quam a, pharetra dolor. Praesent convallis purus in nunc bibendum tempus. Donec id mauris ut ligula ultricies vulputate id ac arcu. Fusce accumsan est turpis, sed commodo elit molestie ac. Praesent feugiat felis eget ex tempor venenatis. Suspendisse potenti. Nunc ante arcu, laoreet ac pharetra id, lobortis vehicula sapien. Ut diam justo, dignissim vitae rutrum vehicula, vestibulum ut eros. Sed fringilla consequat viverra. Maecenas sed augue ut ex mattis mollis id nec nisl. Duis mattis ex augue, non consectetur tellus venenatis vel. Vivamus non velit auctor ante hendrerit pulvinar."})#Tab3 verbatim
  # 
  
  
  
  output$OS10y_NAFL<-renderText({paste("NAFL ",round(data_15[data_15$Stage=="NAFL"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,"Overall_Survival"]),"%",sep = "")}) 
  output$OS10y_F0<-renderText({paste("F0 ",round(data_15[data_15$Stage=="F0"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,"Overall_Survival"]),"%",sep = "")}) 
  output$OS10y_F1<-renderText({paste("F1 ",round(data_15[data_15$Stage=="F1"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,"Overall_Survival"]),"%",sep = "")}) 
  output$OS10y_F2<-renderText({paste("F2 ",round(data_15[data_15$Stage=="F2"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,"Overall_Survival"]),"%",sep = "")}) 
  output$OS10y_F3<-renderText({paste("F3 ",round(data_15[data_15$Stage=="F3"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,"Overall_Survival"]),"%",sep = "")}) 
  output$OS10y_F4<-renderText({paste("F4 ",round(data_15[data_15$Stage=="F4"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,"Overall_Survival"]),"%",sep = "")}) 
  
  
  output$DC10y_NAFL<-renderText({paste("NAFL ",round(data_15[data_15$Stage=="NAFL"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[1]]),"%",sep = "")}) 
  output$DC10y_F0<-renderText({paste("F0 ",round(data_15[data_15$Stage=="F0"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[1]]),"%",sep = "")}) 
  output$DC10y_F1<-renderText({paste("F1 ",round(data_15[data_15$Stage=="F1"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[1]]),"%",sep = "")}) 
  output$DC10y_F2<-renderText({paste("F2 ",round(data_15[data_15$Stage=="F2"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[1]]),"%",sep = "")}) 
  output$DC10y_F3<-renderText({paste("F3 ",round(data_15[data_15$Stage=="F3"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[1]]),"%",sep = "")}) 
  output$DC10y_F4<-renderText({paste("F4 ",round(data_15[data_15$Stage=="F4"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[1]]),"%",sep = "")}) 
  
  
  output$HCC10y_NAFL<-renderText({paste("NAFL ",round(data_15[data_15$Stage=="NAFL"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[2]]),"%",sep = "")}) 
  output$HCC10y_F0<-renderText({paste("F0 ",round(data_15[data_15$Stage=="F0"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[2]]),"%",sep = "")}) 
  output$HCC10y_F1<-renderText({paste("F1 ",round(data_15[data_15$Stage=="F1"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[2]]),"%",sep = "")}) 
  output$HCC10y_F2<-renderText({paste("F2 ",round(data_15[data_15$Stage=="F2"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[2]]),"%",sep = "")}) 
  output$HCC10y_F3<-renderText({paste("F3 ",round(data_15[data_15$Stage=="F3"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[2]]),"%",sep = "")}) 
  output$HCC10y_F4<-renderText({paste("F4 ",round(data_15[data_15$Stage=="F4"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_ao[2]]),"%",sep = "")}) 
  
  
  output$LRD10y_NAFL<-renderText({paste(round(data_15[data_15$Stage=="NAFL"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[1]]),"%",sep = "")}) 
  output$LRD10y_F0<-renderText({paste(round(data_15[data_15$Stage=="F0"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[1]]),"%",sep = "")}) 
  output$LRD10y_F1<-renderText({paste(round(data_15[data_15$Stage=="F1"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[1]]),"%",sep = "")}) 
  output$LRD10y_F2<-renderText({paste(round(data_15[data_15$Stage=="F2"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[1]]),"%",sep = "")}) 
  output$LRD10y_F3<-renderText({paste(round(data_15[data_15$Stage=="F3"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[1]]),"%",sep = "")}) 
  output$LRD10y_F4<-renderText({paste(round(data_15[data_15$Stage=="F4"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[1]]),"%",sep = "")}) 
  
  output$NLRD10y_NAFL<-renderText({paste(round(data_15[data_15$Stage=="NAFL"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[2]]),"%",sep = "")}) 
  output$NLRD10y_F0<-renderText({paste(round(data_15[data_15$Stage=="F0"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[2]]),"%",sep = "")}) 
  output$NLRD10y_F1<-renderText({paste(round(data_15[data_15$Stage=="F1"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[2]]),"%",sep = "")}) 
  output$NLRD10y_F2<-renderText({paste(round(data_15[data_15$Stage=="F2"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[2]]),"%",sep = "")}) 
  output$NLRD10y_F3<-renderText({paste(round(data_15[data_15$Stage=="F3"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[2]]),"%",sep = "")}) 
  output$NLRD10y_F4<-renderText({paste(round(data_15[data_15$Stage=="F4"&data_15$Year==10&data_15$Age==input$age&data_15$Gender==input$gender,measure_mort[2]]),"%",sep = "")}) 
  
  #Output and render the plot| OS
  output$inc_rateOS <- renderPlot({
    #filter the data to plot
    plot_data=data_15[data_15$Stage%in%input$stage2&data_15$Age==input$age&data_15$Gender==input$gender,]
    data_15$Stage<-factor(data_15$Stage, stages)
    plot_data$Stage <- factor(plot_data$Stage, stages)
    names(my_colors2)<-levels(factor(data_15$Stage))
    
    ggplot(plot_data,aes_string(x = "Year", y =gsub(" ","_",measure_inc[1]), colour="Stage"))+
      ggtitle("Survival")+
      theme(
        # Theme definition,
        # change legend border color,
        # legend position, legend font size, panel bg color,
        # axis title font size, axis thick marks font size,
        # remove x-axis gridlines (maj and min),
        # change y-axis gridlines  (maj and min), remove plot bg
        plot.title = element_text(hjust = 0.5,size = 17,face="bold"),
        legend.key = element_rect(fill = alpha("blue",0.0)),
        legend.position = 'bottom',
        #legend.direction="horizontal",
        legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("blue",0.0)),
        legend.title = element_blank(),
        panel.background=element_rect(fill="white",colour=NA),
        axis.title = element_text(size=14),
        axis.text= element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4, linetype = 'dashed',
                                          colour = alpha("black",0.2)),
        panel.grid.minor.y = element_line(size = 0.15, linetype = 'dashed',
                                          colour = alpha("black",0.1)),
        plot.background =  element_rect(fill = NA,colour = NA, size = 0.5)
      )+
      theme(axis.line.x = element_line(color="black", size = .21),
            axis.line.y = element_line(color="black", size = .21))+
      geom_line(size=1,alpha=1.0)+#line thickness and alpha
      xlab("Year")+ylab("Survival rate (%)")+#Labels of x and y axes
      scale_x_continuous(expand=c(0,0),limits=c(0,15))+#set x-axis limits to 0-15
      scale_y_continuous(expand=c(0,0),limits=c(0,100))+
      scale_color_manual(values = my_colors2)+
      guides(color=guide_legend(nrow=1))+
      labs(colour="Stage: ")#legend title | legend is based on different colors
  })
  
  
  
  #Output and render the plot| DC
  output$inc_rateDC <- renderPlot({
    #filter the data to plot
    maxY=max(data_15$Decompensated_Cirrhosis[data_15$Age==input$age&data_15$Gender==input$gender])
    plot_data=data_15[data_15$Stage%in%input$stage2&data_15$Age==input$age&data_15$Gender==input$gender,]
    data_15$Stage<-factor(data_15$Stage, stages)
    plot_data$Stage <- factor(plot_data$Stage, stages)
    names(my_colors2)<-levels(factor(data_15$Stage))
    #plot_data <- melt(plot_data[,c("Year","Stage",input$out_inc)], id.vars = c("Year","Stage"))
    #plot_data$variable=gsub("_", " ", plot_data$variable)
    ggplot(plot_data,aes_string(x = "Year", y =gsub(" ","_",measure_ao[1]), colour="Stage"))+
      ggtitle(gsub("_"," ",measure_ao[1]))+
      theme(
        # Theme definition,
        # change legend border color,
        # legend position, legend font size, panel bg color,
        # axis title font size, axis thick marks font size,
        # remove x-axis gridlines (maj and min),
        # change y-axis gridlines  (maj and min), remove plot bg
        legend.key = element_rect(fill = alpha("blue",0.0)),
        #legend.position=c(0,1),legend.justification = c(0,1),
        legend.position = 'bottom',
        legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("blue",0.0)),
        #legend.box.background=element_rect(fill = alpha("blue",0.0)),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 17,face="bold"),
        panel.background=element_rect(fill="white",colour=NA),
        axis.title = element_text(size=14),
        axis.text= element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4, linetype = 'dashed',
                                          colour = alpha("black",0.2)),
        panel.grid.minor.y = element_line(size = 0.15, linetype = 'dashed',
                                          colour = alpha("black",0.1)),
        plot.background =  element_rect(fill = NA,colour = NA, size = 0.5)
      )+
      theme(axis.line.x = element_line(color="black", size = .21),
            axis.line.y = element_line(color="black", size = .21))+
      geom_line(size=1,alpha=1.0)+#line thickness and alpha
      xlab("Year")+ylab("Cumulative rate (%)")+#Labels of x and y axes
      #geom_point(size=5)+#add points (not necessary)
      scale_x_continuous(expand = c(0, 0),limits=c(0,15))+
      #xlim(0,15)+#set x-axis limits to 0-15
      scale_y_continuous(expand = c(0, 0),limits=c(0,maxY*1.10))+
      scale_color_manual(values = my_colors2)+
      guides(color=guide_legend(nrow=1))+
      #scale_color_viridis(option = "A",discrete = T)+ #use viridis color scale
      labs(colour="Stage: ")#legend title | legend is based on different colors
  })
  
  
  #Output and render the plot| HCC
  output$inc_rateHCC <- renderPlot({
    
    #filter the data to plot
    maxY=max(data_15$Hepatocellular_Carcinoma[data_15$Age==input$age&data_15$Gender==input$gender])
    plot_data=data_15[data_15$Stage%in%input$stage2&data_15$Age==input$age&data_15$Gender==input$gender,]
    data_15$Stage<-factor(data_15$Stage, stages)
    plot_data$Stage <- factor(plot_data$Stage, stages)
    names(my_colors2)<-levels(factor(data_15$Stage))
    
    #plot_data <- melt(plot_data[,c("Year","Stage",input$out_inc)], id.vars = c("Year","Stage"))
    #plot_data$variable=gsub("_", " ", plot_data$variable)
    ggplot(plot_data,aes_string(x = "Year", y =gsub(" ","_",measure_ao[2]), colour="Stage"))+
      ggtitle(gsub("_"," ",measure_ao[2]))+
      theme(
        # Theme definition,
        # change legend border color,
        # legend position, legend font size, panel bg color,
        # axis title font size, axis thick marks font size,
        # remove x-axis gridlines (maj and min),
        # change y-axis gridlines  (maj and min), remove plot bg
        legend.key = element_rect(fill = alpha("blue",0.0)),
        #legend.position=c(0,1),legend.justification = c(0,1),
        legend.position = 'bottom',
        
        legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("blue",0.0)),
        plot.title = element_text(hjust = 0.5,size = 17,face="bold"),
        #legend.box.background=element_rect(fill = alpha("blue",0.0)),
        legend.title = element_blank(),
        panel.background=element_rect(fill="white",colour=NA),
        axis.title = element_text(size=14),
        axis.text= element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4, linetype = 'dashed',
                                          colour = alpha("black",0.2)),
        panel.grid.minor.y = element_line(size = 0.15, linetype = 'dashed',
                                          colour = alpha("black",0.1)),
        plot.background =  element_rect(fill = NA,colour = NA, size = 0.5)
      )+
      theme(axis.line.x = element_line(color="black", size = .21),
            axis.line.y = element_line(color="black", size = .21))+
      geom_line(size=1,alpha=1.0)+#line thickness and alpha
      xlab("Year")+ylab("Cumulative rate (%)")+#Labels of x and y axes
      
      #geom_point(size=5)+#add points (not necessary)
      scale_x_continuous(expand = c(0, 0),limits=c(0,15))+
      #xlim(0,15)+#set x-axis limits to 0-15
      scale_y_continuous(expand = c(0, 0),limits=c(0,maxY*1.10))+
      scale_color_manual(values = my_colors2)+
      guides(color=guide_legend(nrow=1))+
      #scale_color_viridis(option = "A",discrete = T)+ #use viridis color scale
      labs(colour="Stage: ")#legend title | legend is based on different colors
  })
  
  #Output and render the plot| Mortality
  output$inc_mortality <- renderPlot({
    
    #filter the data to plot
    plot_data=data_15[data_15$Stage%in%input$stage2&data_15$Age==input$age&data_15$Gender==input$gender,]
    data_15$Stage<-factor(data_15$Stage, stages)
    plot_data$Stage <- factor(plot_data$Stage, stages)
    names(my_colors2)<-levels(factor(data_15$Stage))
    plot_data <- melt(plot_data[,c("Year","Stage",input$mort_out)], id.vars = c("Year","Stage"))
    plot_data<-plot_data[plot_data$Year== 10,]
    names(mortality_colors)<-levels(factor(measure_mort))
    # if(length(input$stage2)>1){
    #   for(i in 1:length(input$stage2)){
    #     plot_data$Year[plot_data$Stage==input$stage2[i]]=plot_data$Year[plot_data$Stage==input$stage2[i]]+0.7*(i-length(input$stage2)/2-1)
    #   }
    # }
    # 
    labels_mort=gsub("Liver ","Liver-",gsub("Nonliver","Non-Liver",gsub("Death", "Mortality",gsub("_"," ",input$mort_out))))
    if(length(input$mort_out)>0){
      ggplot()+
        geom_bar(aes(y = value,x=Stage, fill = variable), data = plot_data,
                 stat="identity",width = 0.6)+ scale_fill_manual(labels=labels_mort, values =mortality_colors )+
        #scale_x_discrete(aes(x))+
        # scale_color_manual(values = rep("black",18))+
        guides(fill=FALSE)+
        ggtitle("10-Year Mortality")+
        theme(
          # Theme definition,
          # change legend border color,
          # legend position, legend font size, panel bg color,
          # axis title font size, axis thick marks font size,
          # remove x-axis gridlines (maj and min),
          # change y-axis gridlines  (maj and min), remove plot bg
          legend.key = element_blank(),#rect(fill = alpha("blue",0.0)),
          #legend.position=c(0,1),legend.justification = c(0,1),
          legend.position = 'bottom',
          legend.direction = 'vertical',
          legend.text=element_text(size=14),
          legend.background=element_blank(),#rect(fill = alpha("blue",0.0)),
          plot.title = element_text(hjust = 0.5,size = 17,face="bold"),
          legend.title = element_blank(),
          panel.background=element_rect(fill="white",colour=NA),
          axis.title = element_text(size=14),
          axis.text= element_text(size=14),
          axis.text.x= element_text(angle=0,size=14,vjust = 0.3,hjust = 0.3),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.4, linetype = 'dashed',
                                            colour = alpha("black",0.2)),
          panel.grid.minor.y = element_line(size = 0.15, linetype = 'dashed',
                                            colour = alpha("black",0.1)),
          plot.background =  element_rect(fill = NA,colour = NA, size = 0.5)
        )+
        theme(axis.line.x = element_line(color="black", size = .21),
              axis.line.y = element_line(color="black", size = .21))+
        geom_line(size=1,alpha=1.0)+#line thickness and alpha
        xlab("Stage")+ylab("Cumulative rate (%)")+#Labels of x and y axes
        scale_y_continuous(expand = c(0, 0),limits=c(0,max(rowSums(data_15[,measure_mort]))*1.10))+
        #scale_color_viridis(option = "A",discrete = T)+ #use viridis color scale
        labs(colour="Stage: ")#legend title | legend is based on different colors
    }
    else{
      ggplot()+
        geom_blank()+#(aes(y = value,x=Stage, fill = variable), data = plot_data,
                 #stat="identity",width = 0.6)+ scale_fill_manual(labels=labels_mort, values =mortality_colors )+
        #scale_x_discrete(aes(x))+
        # scale_color_manual(values = rep("black",18))+
        guides(fill=FALSE)+
        ggtitle("10-Year Mortality")+
        theme(
          # Theme definition,
          # change legend border color,
          # legend position, legend font size, panel bg color,
          # axis title font size, axis thick marks font size,
          # remove x-axis gridlines (maj and min),
          # change y-axis gridlines  (maj and min), remove plot bg
          legend.key = element_blank(),#rect(fill = alpha("blue",0.0)),
          #legend.position=c(0,1),legend.justification = c(0,1),
          legend.position = 'bottom',
          legend.direction = 'vertical',
          legend.text=element_text(size=14),
          legend.background=element_blank(),#rect(fill = alpha("blue",0.0)),
          plot.title = element_text(hjust = 0.5,size = 17,face="bold"),
          legend.title = element_blank(),
          panel.background=element_rect(fill="white",colour=NA),
          axis.title = element_text(size=14),
          axis.text= element_text(size=14),
          axis.text.x= element_text(angle=45,size=14,vjust = 0.3,hjust = 0.3),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.4, linetype = 'dashed',
                                            colour = alpha("black",0.2)),
          panel.grid.minor.y = element_line(size = 0.15, linetype = 'dashed',
                                            colour = alpha("black",0.1)),
          plot.background =  element_rect(fill = NA,colour = NA, size = 0.5)
        )+
        theme(axis.line.x = element_line(color="black", size = .21),
              axis.line.y = element_line(color="black", size = .21))+
        geom_line(size=1,alpha=1.0)+#line thickness and alpha
        xlab("Stage")+ylab("Cumulative rate (%)")+#Labels of x and y axes
        scale_y_continuous(expand = c(0, 0),limits=c(0,max(rowSums(data_15[,measure_mort]))*1.10))+
        #scale_color_viridis(option = "A",discrete = T)+ #use viridis color scale
        labs(colour="Stage: ")#legend title | legend is based on different colors
    }
    
      
  })
  
  
  
  
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      tempReport <- file.path("report.Rmd")
      # Copy file 'report.Rmd', this file defines the report format.
      # In order to generate a report file, there must be one under the working directory.
      #tempsty <- file.path("reportstyle.sty")
      #file.copy("report.Rmd", tempReport, overwrite = TRUE)
      #file.copy("reportstyle.sty", tempsty, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      # these should include all data to generate the plot
      params <- list(stage=input$stage2,age=input$age,gender=input$gender)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}
)



