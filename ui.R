# #this sets the directory to that of the source file. Very useful!!
# path <- rstudioapi::getActiveDocumentContext()$path
# Encoding(path) <- "UTF-8"
# setwd(dirname(path))
# 
# #Install new packages if necessary, if there is a library introduced it should also ve included in the list "list.of.packages.". Very useful!
# list.of.packages <- c("shiny", "ggplot2","ggthemes","viridis","reshape2","rmarkdown","shinythemes")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)


# Libraries
library(shiny)
library(ggplot2)
#library(ggthemes)
#library(viridis)
library(reshape2)
library(rmarkdown)
library(shinythemes)
library(rsconnect)
#library(cowplot)
library(tinytex)
library(shinyjs)
library(scales)
library(RColorBrewer)
library(ReporteRs)
#deployApp() 


#read data file
data=read.csv("data/concatenated_input_data.csv",stringsAsFactors = T,row.names = 1)#Data file, subject to changes
#This is to limit the analyses to the first 15 years
data_15=data[data$Year<16,]

measure=colnames(data_15)[5:ncol(data_15)]#this gets the colnames (outcomes) from the csv header 
measure_mort=measure[c(7,8,9)]#mortality measures
measure_ao=measure[c(2,3)]#advanced outcome measures
measure_inc=measure[c(5,6)]#survival measures

#this gets the name of the conditions (NASH, NAFL,..) and fiborsis stages (F0,..,F4)- sorted in the prefer#3c4d68 order (sorting is manual and hard coded)
stages=unique(data$Stage)
stages=stages[sort.list(stages, method = "shell")]#sorts the stages
stages=stages[c(6,1:5)]#sorts the stages
stageNames<-c("NAFL","Stage 0","Stage 1","Stage 2", "Stage 3", "Stage 4")
stages<-setNames(as.character(stages),stageNames)
mort_names=c("Liver-Realted-Mortality","Non-Liver-Related Mortality","Background Morality")

#measure_mort<-setNames(as.character(measure_mort),mort_names)
# names(stages)<-stageNames

my_colors=c("#375E97","#FB6542","#FFBB00","#3F681C","#003B46","#F4CC70")
my_colors2 <- c("#A4243B","#D8C99B","#D8973C", "#BD632F","#95A0A5", "#273E47")#brewer.pal(9,"Blues")[4:10]
sty="border-radius:5px;width:40px;height:40px;margin-left:5px;padding:5px;padding-top:5px; background-color:"
sty2top="border-radius:5px 5px 0px 0px;width:40px;height:20px;margin:0px;padding:5px; background-color:"
sty2bot="border-radius:0px 0px 5px 5px;width:40px;height:20px;margin:0px;padding:5px; background-color:"
sty3="border-radius:5px;width:40px;height:20px;margin:0px;padding:5px; background-color:white;color:black;"
sty4="border-radius:5px;width:40px;height:60px;margin-left:5px;margin-bottom:5px;padding:0px;background-color:white"
my_colors3<-c('#FFFFFF','#E6EEF0','#C3D1DA','#A5BACB','#7E98A9','#476A7E','#274456')
mortality_colors=c("#68829E","#505160","#79832E")
# 
# border: 2px solid #dadada;
# background-color:rgba(132, 178, 206,0.3);
# border-radius: 7px;
# outline: none;
# border-color: #9ecaed;
#   box-shadow: 0 0 10px #9ecaed;
# This creates the user-iterface, everything should be inside the UI
shinyUI(
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML(".tabbable > .nav > li[class=active]    > a {background-color: white;border:white;}

                     
                      .speech-bubble {
	                       width:60%;
                          position:absolute;
                          bottom:450px;
                        	background: rgba(7, 7, 7,0.7);
                          border-radius: .4em;
                          font-weight:bold;
                          font-size:16pt;
                          color:white;
                          padding:5px;
                          z-index:1000;
                      }
                      
                      .speech-bubble:after {
                      content: '';
                      position: absolute;
                      bottom: 0;
                      left: 50%;
                      width: 0;
                      height: 0;
                      border: 90px solid transparent;
                      border-top-color: rgba(7, 7, 7,0.7);
                      border-bottom: 0;
                      border-left: 0;
                      margin-left: -45px;
                      margin-bottom: -90px;
                      }
                      .side-panel-fixed{
                      
                      }
                                      
                      ")
      )
    ),
    # input[type='checkbox']:checked+span{ 
    #   
    #   background-color:rgba(132, 178, 206,0.3);;
    #   display:inline-block;
    #   min-width:30px;
    #   align:center;
    #   text-align:center;
    #   border-radius: 5px;
    #   padding:5px;
    #   
    # }
    theme = shinytheme("spacelab"),#choose the theme, check https://rstudio.github.io/shinythemes/ for other themes 
    style="background-color:white",#this sets sthe style of the background (body in HTML)
    div(
      style="margin: 0 auto;", 
      titlePanel("",windowTitle = "NASH Simulator"),
      #h3('NASH Simulator',style="text-align: center;position:sticky;"),
      sidebarLayout(#Sidebar layout declaration
        fluid=FALSE,
        sidebarPanel(class="side-panel-fixed",
          width = 3,#Parameter panel declaration (left side of the app) 
          selectInput("age", "Age",selected = "50",c(4:18*5)),#Input declaration for the age
          radioButtons("gender", "Gender",c("Female","Male")),#Input declaration for the gender 
          conditionalPanel(
            condition="input.conditionedPanels==1",#Conditional panel declaration, shows the below input(s) when Tab1 is active
            radioButtons("stage", "NAFLD stage:",selected = "F3",stages)#input declaration to choose the fibrosis stage| Tab1
            ),
          conditionalPanel(
            condition="input.conditionedPanels==2",#Conditional panel declaration, shows the below input(s) when Tab2 is active
            # span(id="alert",color="red",
            #      "Select two or more stages to compare",
            #      class="speech-bubble"
            # ),
            h5(id="title",style="font-weight: bold; padding:5px;","NAFLD stage:"
               ),
            div(id="animation",
                checkboxGroupInput("stage2", NULL, stages,selected = stages[4])#input declaration to choose the fibrosis stage| Tab2    
                )
            
            ),
          downloadButton(#Download button definition
            "report",
            class="btn-block input-block-level",
            style="margin-bottom:4px;white-space: normal;",
            "Generate report"
            ),
          downloadButton(#Download button definition
            "ppt",
            class="btn-block input-block-level",
            style="margin-bottom:4px;white-space: normal;",
            "Download graphs"
          ),
          br()
          ),
        # Main panel declaration
        mainPanel(
          style="border-radius: 10px;box-shadow: inset 0 1px 1px rgba(0,0,0,.1),padding-bottom: 5cm;",
          tabsetPanel(#Tab style panel layout declaration
            type = "tabs",
            id = "conditionedPanels",
            tabPanel(
              " ",
              style="",
              #div(htmlOutput("CA_text"),style="padding:10px;border-radius:5px;background-color:#f5f5f5;"),
              div(style="height:0px;"),
              div(class="col-centered",align="center",
                div(
                  class="col-xs-12",align="center",
                  style="margin-top:25px;",
                  div(class="col-xs-7", style="padding-left:0px;float:left;",
                      plotOutput(
                    outputId = "inc_rateOS",
                    #width="350px",
                    height = "250px"
                    )
                  ),
                  div(class="col-xs-5",style="width:170px; padding-left:5px;",
                    h5(class="bg-info",style="background-color:#7c826d;padding:5px;width:140px;border-radius:5px;color:white;","10-Year Survival"),
                    h5(id="NAFL",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[1],";",sep = ""),textOutput("OS10y_NAFL")),
                    h5(id="F0",class="col-xs-2 ",style=paste(sty,my_colors2[2],";",sep = ""),textOutput("OS10y_F0")),
                    h5(id="F1",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[3],";",sep = ""),textOutput("OS10y_F1")),
                    h5(id="F2",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[4],";",sep = ""),textOutput("OS10y_F2")),
                    h5(id="F3",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[5],";",sep = ""),textOutput("OS10y_F3")),
                    h5(id="F4",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[6],";",sep = ""),textOutput("OS10y_F4"))
                    )
                  ),
                
                div(hr(),
                    class="col-xs-12",
                    style="margin-top:25px;",
                    div(class="col-xs-7", style="padding-left:0px;float:left;",
                        plotOutput(
                        outputId = "inc_mortality",
                        #width="350px",
                        height = "230px"
                      ),
                      
                  
                  div(class="col-xs-3", style="padding-left:0px;float:left;",
                      
                      checkboxGroupInput("mort_out", 
                                         label=NULL,
                                         choiceNames = list(
                                           tags$span(measure_mort[1], style = paste("min-width:200px;border-radius:2px;font-size:10pt;padding:2px;padding-left:10px;padding-right:10px;color:white;background-color: ",mortality_colors[2],";",sep="")),
                                           tags$span(measure_mort[2], style =  paste("width:200px;border-radius:2px;font-size:10pt;padding:2px;padding-left:10px;padding-right:10px;color:white;background-color:",mortality_colors[3],";",sep="")), 
                                           tags$span(measure_mort[3], style =  paste("width:200px;border-radius:2px;font-size:10pt;padding:2px;padding-left:10px;padding-right:10px;color:white;background-color:",mortality_colors[1],";",sep=""))
                                         ),
                                         choiceValues = measure_mort,selected = measure_mort)
                      )
                  ),
                  div(class="col-xs-5",style="width:170px; padding-left:5px;",
                      h5(class="bg-info",style="background-color:#7c826d;padding:5px;width:140px;border-radius:5px;color:white;","10-Year Liver- and Non-Liver-Related Mortality"),
                      div(id="NAFL4",class="col-xs-2 bg-primary",style=sty4,
                          h5(class="col-xs-2 bg-primary",style=sty3,"NAFL"),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2top,mortality_colors[2],";",sep = ""),textOutput("LRD10y_NAFL")),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2bot,mortality_colors[3],";",sep = ""),textOutput("NLRD10y_NAFL"))
                          ),
                      div(id="F04",class="col-xs-2 bg-primary",style=sty4,
                          h5(class="col-xs-2 bg-primary",style=sty3,"F0"),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2top,mortality_colors[2],";",sep = ""),textOutput("LRD10y_F0")),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2bot,mortality_colors[3],";",sep = ""),textOutput("NLRD10y_F0"))
                      ),
                      
                      div(id="F14",class="col-xs-2 bg-primary",style=sty4,
                          h5(class="col-xs-2 bg-primary",style=sty3,"F1"),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2top,mortality_colors[2],";",sep = ""),textOutput("LRD10y_F1")),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2bot,mortality_colors[3],";",sep = ""),textOutput("NLRD10y_F1"))
                      ),
                      div(id="F24",class="col-xs-2 bg-primary",style=sty4,
                          h5(class="col-xs-2 bg-primary",style=sty3,"F2"),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2top,mortality_colors[2],";",sep = ""),textOutput("LRD10y_F2")),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2bot,mortality_colors[3],";",sep = ""),textOutput("NLRD10y_F2"))
                      ),
                      div(id="F34",class="col-xs-2 bg-primary",style=sty4,
                          h5(class="col-xs-2 bg-primary",style=sty3,"F3"),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2top,mortality_colors[2],";",sep = ""),textOutput("LRD10y_F3")),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2bot,mortality_colors[3],";",sep = ""),textOutput("NLRD10y_F3"))
                      ),
                      div(id="F44",class="col-xs-2 bg-primary",style=sty4,
                          h5(class="col-xs-2 bg-primary",style=sty3,"F4"),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2top,mortality_colors[2],";",sep = ""),textOutput("LRD10y_F4")),
                          h5(class="col-xs-2 bg-primary",style=paste(sty2bot,mortality_colors[3],";",sep = ""),textOutput("NLRD10y_F4"))
                      ),
                      img(src="mort_legend.png")
                      # h4(class="col-xs-2 bg-primary",style=paste("border-radius:5px;width:60px;height:60px;margin-left:5px;padding:10px;padding-top:9px; background-color:",my_colors2[1],";",sep = ""),textOutput("DC10y_NAFL")),
                      # h4(class="col-xs-2 bg-primary",style=paste("border-radius:5px;width:60px;height:60px;margin-left:5px;padding:10px;padding-top:9px; background-color:",my_colors2[2],";",sep = ""),textOutput("DC10y_F0")),
                      # h4(class="col-xs-2 bg-primary",style=paste("border-radius:5px;width:60px;height:60px;margin-left:5px;padding:10px;padding-top:9px; background-color:",my_colors2[3],";",sep = ""),textOutput("DC10y_F1")),
                      # h4(class="col-xs-2 bg-primary",style=paste("border-radius:5px;width:60px;height:60px;margin-left:5px;padding:10px;padding-top:9px; background-color:",my_colors2[4],";",sep = ""),textOutput("DC10y_F2")),
                      # h4(class="col-xs-2 bg-primary",style=paste("border-radius:5px;width:60px;height:60px;margin-left:5px;padding:10px;padding-top:9px; background-color:",my_colors2[5],";",sep = ""),textOutput("DC10y_F3")),
                      # h4(class="col-xs-2 bg-primary",style=paste("border-radius:5px;width:60px;height:60px;margin-left:5px;padding:10px;padding-top:9px; background-color:",my_colors2[6],";",sep = ""),textOutput("DC10y_F4"))
                  )
                ),
                
                div(hr(),
                    class="col-xs-12",align="center",
                    style="margin-top:25px;",
                    div(class="col-xs-7", style="padding-left:0px;float:left;",
                        plotOutput(
                        outputId = "inc_rateDC",
                        #width="350px",
                        height = "250px"
                      )
                  ),
                  div(class="col-xs-5",style="width:170px; padding-left:5px;",
                      h5(class="bg-info",style="background-color:#7c826d;padding:5px;width:140px;border-radius:5px;color:white;","10-Year Decompensated Cirrhosis Risk"),
                      h5(id="NAFL2",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[1],";",sep = ""),textOutput("DC10y_NAFL")),
                      h5(id="F02",class="col-xs-2",style=paste(sty,my_colors2[2],";",sep = ""),textOutput("DC10y_F0")),
                      h5(id="F12",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[3],";",sep = ""),textOutput("DC10y_F1")),
                      h5(id="F22",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[4],";",sep = ""),textOutput("DC10y_F2")),
                      h5(id="F32",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[5],";",sep = ""),textOutput("DC10y_F3")),
                      h5(id="F42",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[6],";",sep = ""),textOutput("DC10y_F4"))
                  )
                ),
                
                div(hr(),
                    class="col-xs-12",align="center",
                    style="margin-top:25px;",
                    div(class="col-xs-7", style="padding-left:0px;float:left;",
                        plotOutput(
                        outputId = "inc_rateHCC",
                        #width="350px",
                        height = "250px"
                      )
                  ),
                  div(class="col-xs-5",style="width:170px; padding-left:5px;",
                      h5(class="bg-info",style="background-color:#7c826d;padding:5px;width:140px;border-radius:5px;color:white;","10-Year Hepatocellular Carcinoma Risk"),
                      h5(id="NAFL3",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[1],";",sep = ""),textOutput("HCC10y_NAFL")),
                      h5(id="F03",class="col-xs-2 ",style=paste(sty,my_colors2[2],";",sep = ""),textOutput("HCC10y_F0")),
                      h5(id="F13",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[3],";",sep = ""),textOutput("HCC10y_F1")),
                      h5(id="F23",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[4],";",sep = ""),textOutput("HCC10y_F2")),
                      h5(id="F33",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[5],";",sep = ""),textOutput("HCC10y_F3")),
                      h5(id="F43",class="col-xs-2 bg-primary",style=paste(sty,my_colors2[6],";",sep = ""),textOutput("HCC10y_F4"))
                  )
                )
                
                
                
                
                
                ),
              value = 2)
            )
          )
      ),
      hr(),
      div(
        class="row",
        style="margin-right: 60px;",
        div(class="col-xs-4",
            align="left",
            "Contact: Jagpreet Chhatwal, PhD",
            br(),
            "MGH Inst. for Tech. Assessment",
            br(),
            "Harvard Medical School",
            br(),
            "101 Merrimac St. STE 1010",
            br(),
            "Boston, MA 02114 United States",
            br(),
            a(href="mailto:hepccalculator@mgh-ita.org", "hepccalculator@mgh-ita.org")
        ),
        div(
          class="col-xs-4",
          img(
            src="mgh-logo.png",
            style="width:177px;height:36px;",
            alt="Massachusetts General Hospital Logo",
            width=177,
            height=36
          ),
          br(),
          br(),
          img(
            src="hms-logo.png",
            style="width:137px;height:35px;",
            alt="Harvard Medical School Logo", 
            width=177,
            height="36"
          )
        ),
        div(
          class="col-xs-4",
          tags$img(
            src="aasld-logo.png",
            style="width:87px;height:35px; float:left; margin: 0px 10px 0px 0px;", 
            alt="AASLD Logo",
            width="60", 
            height="60"
          ),
          "We acknowledge the support of the American Association for the Study of Liver Diseases in the development of the NAFLD Natural History Simulator",
          br(),
          br(),
          HTML('&copy;'),
          " MGH Inst. for Tech. Assessment 2017-2018                                           "
        )
      )
    )
  )
)
