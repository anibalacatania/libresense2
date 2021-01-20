

 
 library(shiny)
library(FactoMineR)
library(SensoMineR)
library(ggplot2)
 library(ggradar)
#cambiar el path
 P<-"responses"
 
 
 
 outputDir <-P
 files <- list.files(outputDir, full.names = TRUE)
 data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
 data<- do.call(rbind, data)
 cols <- sapply(data, is.logical)
 data[,cols] <- lapply(data[,cols], as.numeric)


titlePanel("Descriptivo")




shinyUI(fluidPage(
  
  sidebarLayout(
  sidebarPanel( 
    
    
    
    
    img(src = "index.png", height = 100, width = 100),
    h4("LibreSense"),
    em(h4("e-Tasting")),
    h5("Centro de Estudios de Enologia"),
    h5("INTA EEA Mendoza"),
    width=2),
                       
                       

        
        
        
        
        
        
       
    
        

        
        
        
         
      
      
      
      
      
      
  
    mainPanel(
      tabsetPanel(
        tabPanel("Datos", tableOutput("table")),
        tabPanel("ACP", plotOutput("distPlot1")),
        tabPanel("Cajas" , plotOutput("distPlot2"),selectInput('xcol', 'Boxplot', choices=colnames(data))),
        
        tabPanel("Anova" , plotOutput("distPlot3"),selectInput('ycol', 'Anova', choices=colnames(data))),

        tabPanel("Radar", plotOutput("distPlot4"))
     

    
      )
    )
  )))
