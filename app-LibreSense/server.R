

library(magrittr)
library(tidyr)
library(dplyr)
library(shiny)
library(FactoMineR)
library(SensoMineR)
library(ggplot2)
library(ggradar)
library(ggrepel)
library(wordcloud)


# cambiar el path de los datos
P<-"responses"



outputDir <-P
files <- list.files(outputDir, full.names = TRUE)
data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
data<- do.call(rbind, data)
cols <- sapply(data, is.logical)
data[,cols] <- lapply(data[,cols], as.numeric)




shinyServer(function(input, output) {
  
  
  autoInvalidate <- reactiveTimer(2000)
#------------------------------------------------------------------------------------------------------------
  
  

  output$table<- renderTable({
    autoInvalidate()
    outputDir <- P
 
 
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 

    data<- do.call(rbind, data)
    cols <- sapply(data, is.logical)
    data[,cols] <- lapply(data[,cols], as.numeric)
   data
      
  })

  
#-------------------------------------------------------------------------------------------------------------  

#-------------------------------------------------------------------------------------------------------------  
  output$distPlot1 <- renderPlot({
    outputDir <- P
    
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    data<- do.call(rbind, data)
    cols <- sapply(data, is.logical)
    data[,cols] <- lapply(data[,cols], as.numeric)
  
    
    
    
    
    par(mfrow=c(1,2))
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    data<- do.call(rbind, data)
    cols <- sapply(data, is.logical)
    data[,cols] <- lapply(data[,cols], as.numeric)
    data$copa<-as.factor(data$copa)
    
    
    
    
    par(mfrow=c(1,2))
    res.decat <- decat(data,formul="~copa+nombre",firstvar=3, lastvar=ncol(data),graph=FALSE)
  
    res.pca <- PCA(res.decat$adjmean,scale.unit=FALSE)
  
        res.pca
        
  })
  
  
##########
  output$distPlot2 <- renderPlot({
    outputDir <- P
    
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    # Concatenate all data together into one data.frame
    data<- do.call(rbind, data)
    

      ggplot(data, aes(x=copa, y=data[,input$xcol],group=copa))+
        
        
        geom_boxplot() +geom_text_repel(aes(label=nombre,color="red"),size=6)
      

  
  })
 ################################### 
  
  
  output$distPlot3 <- renderPlot({
    outputDir <- P
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    # Concatenate all data together into one data.frame
    data<- do.call(rbind, data)
    
    
    options(contrasts=c("contr.sum","contr.sum"))
    model <- aov(data[,input$ycol]~copa,data=data)
    library(agricolae)
    res.LSD <- LSD.test(model,"copa", p.adj="bonferroni")
    
    names(res.LSD)
    bar.group(res.LSD$groups,ylim=c(0,10),density=4,border="black",cex.names=0.7)
    
    
  })
  
  #####################
  #####################
  

  
  
  
  
  
  
  
  
  
  
  
  
  #####################
  
  
  output$distPlot4 <- renderPlot({
    outputDir <- P
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    # Concatenate all data together into one data.frame
    data<- do.call(rbind, data)
  
  library(scales)
  library(extrafont) 
  
  data8<-group_by(data, copa)
  data8$nombre<-NULL
  
  data9<-summarise_each(data8,funs(mean))  
  
  data9 %>%
    
    mutate_each(funs(rescale), -copa)  -> mtcars_radar
  
  j<-(ggradar(mtcars_radar,axis.labels = colnames(mtcars_radar)[-1],axis.label.size = 3,grid.label.size = 12,group.point.size = 4,legend.title = "",grid.min = 0,
              grid.mid = 0.5, grid.max = 1,plot.extent.x.sf = 1, plot.extent.y.sf = 1.2,
              x.centre.range = 0.02 * (1), label.centre.y = FALSE,
              grid.line.width = 0.5) )
  j
  
  })
  
})


 