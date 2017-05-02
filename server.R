if (!"shiny"%in%installed.packages()[,1]) {install.packages("shiny")} else {library(shiny)}
if (!"magrittr"%in%installed.packages()[,1]) {install.packages("magrittr")} else {library(magrittr)}
if (!"reshape2"%in%installed.packages()[,1]) {install.packages("reshape2")} else {library(reshape2)}
if (!"dplyr"%in%installed.packages()[,1]) {install.packages("dplyr")} else {library(dplyr)}
if (!"ggplot2"%in%installed.packages()[,1]) {install.packages("ggplot2")} else {library(ggplot2)}
sino<-read.csv("sino.csv")

#Define Factorize Function that Factorize Batch and ColorChart
Factorize<-function(data.load)
{
  data.load$Batch<-as.factor(data.load$Batch)
  data.load$ColorChart<-as.factor(data.load$ColorChart)
  return(data.load)
}

#Define Function for Per-minute Data for ggplot2.
Merge1<-function(dataout)
{
  cn<-colnames(select(dataout$df,Date:ColorChart,X_0:X_840))[-c(1:4)]
  cn<-gsub("X_","",cn)%>%as.numeric()%>%as.data.frame()%>%t()
  colnames(cn)<-paste("min",as.character(cn/60),sep=".")
  d_reg<-data.frame(dataout$df,cn)
  id_names<-colnames(d_reg)[c(1:4)]
  dm_reg<-melt(d_reg,id.vars=id_names)
  dm_reg_temp<-filter(dm_reg,variable%in%levels(dm_reg$variable)[c(13:39)])
  dm_reg_time<-filter(dm_reg,variable%in%levels(dm_reg$variable)[-c(13:39)])
  dm_reg_merge<-cbind(dm_reg_time[,-5],dm_reg_temp[,6])
  colnames(dm_reg_merge)<-c(id_names,"Time","Temperature")
  return(dm_reg_merge)
}

#Define Function for Per-event Data for ggplot2.
Merge2<-function(dataout)
{
  d_sta<-select(dataout$df,Date:EndTemp)
  id_names<-colnames(d_sta)[c(1:4)]
  dm_sta<-melt(d_sta,id.vars=id_names)
  dm_sta_time<-filter(dm_sta,variable%in%levels(dm_sta$variable)[1:12])
  dm_sta_temp<-filter(dm_sta,variable%in%levels(dm_sta$variable)[-c(1:12)])
  dm_sta_merge<-cbind(dm_sta_time,dm_sta_temp$value)
  colnames(dm_sta_merge)<-c(id_names,"Status","Time","Temperature")
  return(dm_sta_merge)
}

#Define Ploting Function
roastplot<-function(dataout,dr,ds)
{
  z<-interaction(dataout$df$Date,dataout$df$Bean,dataout$df$Batch,dataout$df$ColorChart)%>%as.character()
  cl<-sapply(z,function(x){
    ctemp<-unlist(strsplit(x,"[.]"))
    paste(paste(sino[1,],ctemp[2],"\n",sep=""),
          paste(sino[2,],ctemp[1],sep=""),
          paste(sino[3,],ctemp[3],sino[4,],sep=""),
          paste(sino[5,],ctemp[4],"\n",sep=""),
          sep=" ")
  })%>%'names<-'(NULL)
  ggplot(dr$df,aes(x=Time,y=Temperature,
                            color=as.character(interaction(Date,Bean,Batch,ColorChart))))+
    geom_line(aes(group=as.character(interaction(Date,Bean,Batch,ColorChart))))+
    geom_point(data=ds$df,size=5,aes(shape=Status))+
    labs(x=sino[6,],y=sino[7,],color=sino[8,],shape=sino[9,],
         title=paste(sino[10,],format(Sys.Date(),"%Y/%m/%d"),sep=""))+
    ylim(50,230)+
    scale_color_discrete(labels=cl,breaks=z)+
    scale_shape_manual(
      values=1:12,
      labels=sino[11:22,])+
    guides(shape=guide_legend(ncol=2))+
    theme(legend.key.size = unit(15, 'mm'),
          legend.key = element_rect(size = 1),
          legend.text = element_text(size=15),
          legend.title = element_text(size = 15),
          plot.title = element_text(size=22,hjust=0.5),
          axis.title = element_text(size=22),
          axis.text = element_text(size=15))
}

shinyServer(
  
  function(input, output,session) {
#For R-portable only.
#Define Quit Button
#    observeEvent(input$quit,q())

#User Upload Data
    data.load<-reactive({
                    inFile <- input$file1
                    if (is.null(inFile))
                    return(NULL)
                    read.csv(inFile$datapath, header=T, sep=",")
                  })
#Factorize Loaded Data and View upon Pressing "View" botton.
    data<-reactive(Factorize(data.load()))
    observeEvent(input$view,{output$origin<-renderTable(data(),align="c")})

#Generate Checkboxes from Loaded Data, with the Select/Unselect All botton.    
    output$Date<-renderUI({checkboxGroupInput("date","Date",choices=levels(data()$Date))})
    observe({
      if (input$alldate%%2==0) {
        updateCheckboxGroupInput(session,"date","Date",choices=levels(data()$Date),selected=levels(data()$Date))
      } else {
        updateCheckboxGroupInput(session,"date","Date",choices=levels(data()$Date))
        }
    })
    
    output$Bean<-renderUI({checkboxGroupInput("bean","Bean",choices=levels(data()$Bean),
                                              selected=levels(data()$Bean))})
    observe({
      if (input$allbean%%2!=0) {
        updateCheckboxGroupInput(session,"bean","Bean",choices=levels(data()$Bean),selected=levels(data()$Bean))
      } else {
        updateCheckboxGroupInput(session,"bean","Bean",choices=levels(data()$Bean))
      }
    })
    
    output$Batch<-renderUI({checkboxGroupInput("batch","Batch",choices=levels(data()$Batch),
                                               selected=levels(data()$Batch))})
    observe({
      if (input$allbatch%%2!=0) {
        updateCheckboxGroupInput(session,"batch","Batch",choices=levels(data()$Batch),
                                 selected=levels(data()$Batch))
      } else {
        updateCheckboxGroupInput(session,"batch","Batch",choices=levels(data()$Batch))
      }
    })
    
    output$ColorChart<-renderUI({checkboxGroupInput("colorchart","ColorChart",choices=levels(data()$ColorChart),
                                                    selected=levels(data()$ColorChart))})
    observe({
      if (input$allcolor%%2!=0) {
        updateCheckboxGroupInput(session,"colorchart","ColorChart",choices=levels(data()$ColorChart),
                                 selected=levels(data()$ColorChart))
      } else {
        updateCheckboxGroupInput(session,"colorchart","ColorChart",choices=levels(data()$ColorChart))
      }
    })

#Filter Data by User Checkbox Inputs and then Display the Selected Keys on Screen.
    dataout<-reactiveValues(df=NULL)
    observeEvent(input$reset,{dataout$df<-NULL})
    observeEvent(input$go,{
      temp<-as.data.frame(data())%>%
        filter(Date%in%input$date,Bean%in%input$bean,Batch%in%input$batch,ColorChart%in%input$colorchart)
      dataout$df<-rbind(dataout$df,temp)%>%unique()
    })
    output$d<-renderTable(dataout$df[,1:4])
        
    dr<-reactiveValues(df=NULL)
    ds<-reactiveValues(df=NULL)
    observeEvent(input$reset,{
      dr$df<-NULL
      ds$df<-NULL})
    observeEvent(input$confirm,{
    #Note: if we use reactive(Merge1(dataout)) instead, the program can run but when rendering anything out
    #from here we get an error...
    #But if we remove reactive() then we are fine with it. This might be related to how reactiveValues() works.
      if (!is.null(dataout$df)) {
      dr$df<-Merge1(dataout)%>%na.omit()
      ds$df<-Merge2(dataout)
      } else return(NULL)
    })

    output$plot<-renderPlot(NULL)
    observeEvent(input$reset,{output$plot<-renderPlot(NULL)})
    observeEvent(input$confirm,{
      if (!is.null(dataout$df)) {
      output$plot<-renderPlot({
        roastplot(dataout,dr,ds)
        },height=960,width=1280)
      } else return(NULL)
    })

}

)
