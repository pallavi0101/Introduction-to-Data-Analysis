getwd()
##### This File is made for the functions which are repeatedly  used in Main programs

#### Finding Descriptive Statistics
descStats <- function(stats) {
  library(moments) # required for skewness
  newMatrix <- matrix (1:7, nrow = 1)
  colnames(newMatrix) <- c("Mean", "Median", "Variance", "Minimum", "Maximum", "Skewness","Sum")
  rownames(newMatrix) <- "Descriptive Statistics"
  newMatrix[1,] <- c(mean(stats),median(stats),var(stats),min(stats),max(stats),skewness(stats),sum(stats))
  return(newMatrix)
}

#### Finding Highest and Lowest Performing Countries
findingStats<-function(x,y){
  newMatrix <- matrix (1:2, nrow = 1)
  colnames(newMatrix) <- c("High Performing", "Low Performing")
  rownames(newMatrix) <- "GDP Countries"
  a<-x[which.max(as.numeric(y))]
  b<-x[which.min(as.numeric(y))]
  r1<-paste(a)
  r2<-paste(b)
  newMatrix[1,]<- c(r1,r2)
  return(newMatrix)
}





#### F1#####Function to calculate ggplot2(barplot)
ggplotFunc<-function(a1,a2,a3,a4){
      ggplot(a1, aes(x=a2,y=a3,fill =a2))+geom_bar(stat = "identity",width=.5,fill = "steelblue",alpha = 1,colour="black")+ 
      theme(axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"),axis.text.x = element_text(angle = 45, vjust =0 ,hjust = 0))+ 
      xlab("Countries") + scale_y_continuous(name = "Million (USD)",
                                             labels = function(y) y / 1000000)+ ggtitle(a4)+guides(fill=guide_legend(title=""))

}


##################F2###### Top 5 GDP####

#### Function to calculate ggplot2(barplot) for Passengers




ggplotFuncFlights<-function(a1,a2,a3,a4,a5,a6){
  
  ggplot(a1, aes(x=a2,y=a3,fill =a3))+geom_bar(stat = "identity",width=0.7,fill = factor(x),alpha = 1,colour="black")+ 
    theme(axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"),
          axis.text.x = element_text(angle = 90, vjust =0.5 ,hjust =1))+ 
    xlab(a5) + ylab(a6) +ggtitle(a4)+scale_x_discrete(labels=c("2013","2014","2015","2016","2017","2018","2019"))
}



#### Function to calculate ggplot2 ScatterPlot
ggplotFuncScatter<-function(a1,a2,a3,a4){
         ggplot(data = a1, mapping = aes(x = a2, y = a3, color = a3)) +
            layer(geom = "a4")+ scale_color_gradient()
}
  ######################

ggplotFuncGDP<-function(a1,a2,a3,a4,a5,a6,a7,a8){
  ggplot(a1, aes(x=a2,y=a3,fill=factor(a4)))+geom_bar(stat = "identity",width=.8,alpha = 1,position =position_dodge(width=0.8))+ 
    theme(axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"),
          axis.text.x = element_text(angle = 90, vjust =0 ,hjust = 0))+ 
    xlab(a5) + scale_y_continuous(name = "Million (USD)",
                                  labels = function(y) y / 1000000)+
  ggtitle(a7)+ guides(fill=guide_legend(title=a8))+
    theme(axis.title.y = element_text(color = "darkblue", size=13),
    axis.title.y.right = element_text( size=13))
   
  #xlim(0,NA)
    #scale_y_discrete(limit = c("5", "10", "15","20"))
  
}


  ############# Finding five top GDP 
  
  topTenGDP<-function(GDPyear,GDPValue){
    GDPyear <- GDPyear[order(-GDPValue),] 
    newFile<-GDPyear[1:10,]
    return(newFile)
    
  }
  
  ############# Writting a csv file for List of countries with highest GDP
  
  
  writeToFile <- function(topFiveCountry, outputFilename) {
    write.table(topFiveCountry, file=outputFilename, sep=",",append=F)
  }
  
  
  
  ###########################   Crops Project Functions #################

