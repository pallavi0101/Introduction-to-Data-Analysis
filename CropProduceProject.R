getwd()
install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("gapminder")
library(gapminder)
install.packages("gganimate") #For animated Plots
library(gganimate)
install.packages("plotly")
library(plotly)
install.packages(reshape2) #Used to melt Datframe instead pf melt package
library(reshape2)
install.packages("data.table")
library(data.table)
install.packages("gapminder") #animated plots
library(gapminder)
install.packages("gganimate") #animated plots
library(gganimate)
install.packages("plotly")
library(plotly)
install.packages("melt")
library(melt)
install.packages("dplyr")
library(dplyr)
install.packages("patchwork")
library(patchwork) # To display 2 charts together
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("impute")
library(impute)
install.packages("plotrix")
library(plotrix )## To Clip abline
install.packages("cvequality") ### To read Coefficient of Variation
library(cvequality)
library("ggpubr")

source('~/functionFile.R') ##Sourcing Function File into Main file

############################################

# This Project is done with objective to study rainfall patterns in India and it's affect on Crop Production

# Through study we will try to findout if there is a correlation between amount of crop production and variability in rainfall

## Studies on Crops which needs more water to grow 

### Data for Crops was sourced at http://www.fao.org/faostat/en/#data/PP

### Data for Rainfall was sourced at  https://climateknowledgeportal.worldbank.org/download-data 

# Rainfall study was referenced through https://www.researchgate.net/publication/270585608_Influences_of_rainfall_on_crop_production_and_suggestions_for_adaptation 

### Correlation Model was referenced at http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

#################################################### 

##### reading types of crops and there production in Hectare and Production in Tonnes between Year 1996 till 2016                
produceDescrip<- read.csv(file="ProductionCropsAllData.csv", head=TRUE, sep=",")
str(produceDescrip)
nrow(produceDescrip) #### number of rows =1048575
complete.cases(produceDescrip)
# Checking for NA
produceDescrip[!complete.cases(produceDescrip),] 

### There are incomplete cases however this will be dealt if required data is not there in selected model
produceDescrip<-subset(produceDescrip, Year >= 1996 & Year <= 2016 & Area == "India" & Element =="Production" )
produceDescrip     

###Subsetting Crops which requires more water
produceDescripSub<-produceDescrip[produceDescrip$Item %in% c('Wheat','Maize','Cotton lint','Bananas'),]
produceDescripSub

##Checking for incomplete rows
produceDescripSub[!complete.cases(produceDescripSub),] 
### There are no incomplete cases in selected model hence no need to further clean data


##Descriptive Statistics
## Aggregate of Produce Finding mean Production each year
agg_Crop_MeanYear<-aggregate(produceDescripSub$Value, by=list(produceDescripSub$Item,produceDescripSub$Year), FUN=mean)
agg_Crop_MeanYear

agg_Crop_Mean<-aggregate(produceDescripSub$Value, by=list(produceDescripSub$Item), FUN=mean)
agg_Crop_Mean

## Aggregate of Produce Finding min Production in selected year
agg_Crop_Min<-aggregate(produceDescripSub$Value, by=list(produceDescripSub$Item), FUN=min)
agg_Crop_Min

agg_Crop_Max<-aggregate(produceDescripSub$Value, by=list(produceDescripSub$Item), FUN=max)
agg_Crop_Max



######################################################################################################


##### reading Rainfall in each country
rainfallData<- read.csv(file="RainfallCountries.csv", head=TRUE, sep=",")
rainfallData

rainfallData <- subset(rainfallData, Country==" India" & Year >= 1996 & Year <= 2016)
str(rainfallData)


nrow(rainfallData) #### number of rows =387875
complete.cases(rainfallData)
# Checking for NA
rainfallData[!complete.cases(rainfallData),]
### There are no incomplete cases or NAs in the data
summary(rainfallData)
str(rainfallData)
nrow(rainfallData)

############# Histogram of Rainfall in 10 years
jpeg("rainfallplot.jpg")
hist(rainfallData$Rainfall.MM.,120,col="red",pch=19,main="Rainfall Trends",xlab="Years", ylab="Rainfall in MM.", xaxt='n')
dev.off()


########### Pltting scatter plot for Rainfall
jpeg("scatterplotrainfall.jpg")
plot(rainfallData$Rainfall.MM.,col="Blue",main="Rainfall Data",xlab="Years", ylab="Rainfall in MM.", xaxt='n',pch=19)
dev.off()



##AnnualRainfall per Year
sumMean1995_2016<-sum(tapply(rainfallData$Rainfall.MM.,rainfallData$Year,mean))
sumMean1995_2016
sumStdDev1995_2016<-sum(tapply(rainfallData$Rainfall.MM.,rainfallData$Year,sd))
sumStdDev1995_2016
sumMin1995_2016<-sum(tapply(rainfallData$Rainfall.MM.,rainfallData$Year,min))
sumMin1995_2016
sumMax1995_2016<-sum(tapply(rainfallData$Rainfall.MM.,rainfallData$Year,max))
sumMax1995_2016
sumCv1995_2016<-sum(tapply(rainfallData$Rainfall.MM.,rainfallData$Year,cv))
sumCv1995_2016



##### 

###Seasonal Rainfall during the month from June till October
SeasonalRains<-subset(rainfallData,  Statistics==" Jun Average" |Statistics== " Jul Average" | 
                         Statistics==" Aug Average"| Statistics==" Sep Average" | Statistics==" Oct Average")
SeasonalRains


## Aggregate of Rainfall For each Seasonal Month Groupby Year
agg_Rainfall<-aggregate(SeasonalRains$Rainfall.MM., by=list(SeasonalRains$Year), FUN=mean)
agg_Rainfall
d<-agg_Rainfall
n<-4
rainfallDF<-do.call("rbind", replicate(n, d, simplify = FALSE))
rainfallDF
rainfallDF<-rainfallDF[order(rainfallDF$Group.1),]
rainfallDF




#Seasonal Rainfall  Plot between 1995-2013 month from June till October
jpeg("Rainfall Pattern during Seasonal Rains(June-October).jpg")
valueColor <- "Black"
xColor <- rgb(0.2, 0.6, 0.9, 1)
ggplot(agg_Rainfall,aes(x=Group.1,y=x))+geom_line(colour="Blue")+geom_point()+ 
geom_point(size=1, shape=18,color="Blue")+
  theme_ipsum() +theme(
    axis.title.y = element_text(color = valueColor, size=13),
    axis.title.y.right = element_text(color = xColor, size=13),
    axis.text.x = element_text(angle = 0, vjust =0.5 ,hjust = 0)
  ) +xlab("Years") + ylab("Rainfall in MM ") +ggtitle("Average Seasonal Rainfall (June-October)")
dev.off()


###Combining Mean Crop Production and Rainfall Data

combinedModelCropRain<- cbind(agg_Crop_MeanYear,rainfallDF) 
combinedModelCropRain
##Changing Column names of dataframe
colnames(combinedModelCropRain) <- c("Country", "Year", "Production(HA.)","Year","Rainfall Average (MM)")
combinedModelCropRain

#### Aggregate of Year and Country
combinedModelCropRainAgg<-aggregate(combinedModelCropRain$`Production(HA.)`,by=list(combinedModelCropRain$Country,combinedModelCropRain$Year,combinedModelCropRain$`Rainfall Average (MM)`), FUN=sum)
combinedModelCropRainAgg<-combinedModelCropRainAgg[order(combinedModelCropRainAgg$Group.2),]
combinedModelCropRainAgg

####Converting Tonnes to Mega Tonnes
combinedModelCropRainAgg$MegaTonnes<-combinedModelCropRainAgg$x/100000
combinedModelCropRainAgg

## Writing the input in CSV File
writeToFile (combinedModelCropRainAgg, "CombinedModelCrop&Rain.csv") 
writeToFile <- function(topFiveCountry, outputFilename) {
  write.table(topFiveCountry, file=outputFilename, sep=",",append=F)
}


##### Plotting Crop Production and Rainfall Pattern during Seasonal Rains(June-October)

jpeg("Crop Production and Rainfall Pattern during Seasonal Rains(June-October).jpg")
coeff3 <-10
ggplot(combinedModelCropRainAgg)+geom_bar( aes(x=Group.2,y=MegaTonnes,fill= Group.1),stat="identity",size=.5, alpha=.6,position =position_dodge(width=0.8),colour="black") + 
geom_line(aes(x=Group.2,y=Group.3*coeff3,),size=1, color=xColor)+geom_point(aes(x=Group.2,y=Group.3*coeff3),size=3, shape=18,color="Blue")+
scale_x_continuous(name = "Years ") +scale_y_continuous(name = "Crop Production Mega Tonnes",  # Add a second axis and specify its features
sec.axis = sec_axis(~./coeff3, name="Rainfall in MM"))+theme_ipsum() +theme(axis.title.y = element_text(color = "darkblue", size=13),
axis.title.y.right = element_text( size=13)) +ggtitle("Crop production and Rainfall")+ guides(fill=guide_legend(title=""))
dev.off()





###################################################   Correlation Between Crop and Rainfall #######################

### Correlation Model was referenced at http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
# Shapiro-Wilk normality test for Crops

shapiro.test(combinedModelCropRainAgg$MegaTonnes) # => p = 2.809e-09

# Shapiro-Wilk normality test for Data Rainfall
shapiro.test(combinedModelCropRainAgg$Group.3) # => p = 0.04603

###From the output, the two p-values are less than the significance level 0.05 implying that the distribution of the 
#data  for Crops are  significantly different from normal distribution. In other words, we can't assume the normality.


###Checking normality of datasets
# MegaTonnes
jpeg("NormalityProduction.jpg")
ggqqplot(combinedModelCropRainAgg$MegaTonnes, ylab = "PRoduction (Mega Tonnes)",title = "Shapiro-Wilk normality test  Crop Produce")
dev.off()

# Rainfall
jpeg("NormalityRainFall.jpg")
ggqqplot(combinedModelCropRainAgg$Group.3, ylab = "GDP (Rainfall in MM.)",title = "Shapiro-Wilk normality test  Rainfall")
dev.off()

#The plots shows that the data may not be  normally distributed.


###Correlation Test for Crop Data
### Correlation Model was referenced at http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
res1 <- cor.test(combinedModelCropRainAgg$MegaTonnes,combinedModelCropRainAgg$Group.3, 
                method = "pearson")
res1
##Measure of correlation between crops and 
r2 <-res1*res1    # calculate r squared
r2
#The p-value of the test is 0.5174, which is more than the significance level alpha = 0.05. 
#We can conclude that production and rainfall are not significantly correlated with a correlation coefficient of 0.07161444  and p-value of 0.5174 .


jpeg("Correlation between Crops and Rainfall.jpg")
ggscatter(combinedModelCropRainAgg, x = "MegaTonnes", y = "Group.3",add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",xlab = "Production in MegaTonnes", ylab = "Rainfall in MM.",title="Correlation between Crop Production and Rainfall")
dev.off()


