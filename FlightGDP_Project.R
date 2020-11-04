getwd()
install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("plotly")
library(plotly)
install.packages(reshape2)
library(reshape2)             #to transpose Dataframe plots
install.packages("data.table")
library(data.table)
install.packages("gapminder") #animated plots
library(gapminder)
install.packages("gganimate") #animated plots
library(gganimate)
install.packages("plotly")
library(plotly)
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
install.packages("tidyverse") ## To reOrder grouped bars on X-axis Plots
install.packages("ggpubr")
library(ggpubr) ## To do scatter plot of Corelation
install.packages("tidyverse")
library(tidyverse) #tidyverse for easy data manipulation and visualization
install.packages("broom")
library(broom) #broom: creates a tidy data frame from statistical test results
install.packages("melt")
library(melt) 

source('~/functionFile.R') ##Sourcing Function File into Main file

############################################

# This Project is done with objective to study relationship of GDP and increase in flight arrivals in Ireland

# The study try to findout that correlation between increase in GDP and number of Flights

# Data for Flights arrival  was sourced at  https://statbank.cso.ie/px/pxeirestat/Statire/SelectVarVal/saveselections.asp 

### Data for GDP was sourced at https://data.oecd.org/gdp/gross-domestic-product-gdp.htm#indicator-chart 

### Data for ISO 3 Country codes was sourced at https://github.com/datasets/country-codes

### Correlation Model was referenced at http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
# https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html

#################################################### 
##############################  Reading file GDP Yearly CSV file     ########################

gDPYearly<- read.csv(file="GDPYearly.csv", head=TRUE, sep=",")
gDPYearly
nrow(gDPYearly) #### number of rows =4441

gDPYearly<-subset(gDPYearly,ï..LOCATION!="EA19")
gDPYearly
nrow(gDPYearly)

# Checking for NA
gDPYearly[!complete.cases(gDPYearly),]
### There are no incomplete cases or NAs in the data
summary(gDPYearly)
str(gDPYearly)  #Structure of CSV file

###########################
#Creating a subset of 2013 till 2019 GDP data as we only have flights arrival data from this date
gDPYearly_2013_2019<-subset(gDPYearly, TIME >= 2013 & TIME <= 2019  & MEASURE =="MLN_USD" & ï..LOCATION!= "OECD" & ï..LOCATION!="OECDE")
gDPYearly_2013_2019

#### Aggregate of GDP groupbyYEar and Location
gDPYearly_2013_2019Agg<-aggregate(gDPYearly_2013_2019$Value,by=list(gDPYearly_2013_2019$TIME,gDPYearly_2013_2019$ï..LOCATION), FUN=sum)
gDPYearly_2013_2019Agg


#### Aggregate of GDP Values groupbyYear 
gDPYearly_2013_2019_Year<-aggregate(gDPYearly_2013_2019$Value,by=list(gDPYearly_2013_2019$TIME), FUN=sum)
gDPYearly_2013_2019_Year


############################ Subsets of each Year ########################
####### Creating a Subset of Year for study 2013 and measure as MLN_USD WHILE TAKING OUT OECD and OECDE as it is total shown in the dataset
gDPYearly_2013<-subset(gDPYearly, TIME == 2013 & MEASURE =="MLN_USD" & ï..LOCATION!= "OECD" & ï..LOCATION!="OECDE")
gDPYearly_2013
nrow(gDPYearly_2013)

##### Basic Statistics
## Calling Function descStats sourcing it from DifferentFile
descStats(gDPYearly_2013$Value)

## Using Function findingStats to lovcate which Countries are highest gDP and lowest perfomeing GDPs ##########
findingStats(gDPYearly_2013$ï..LOCATION,gDPYearly_2013$Value) ## EU28 and MLT are High and low performing Economies

# Aggregating GDP by Year and location
agg_gDP2013<-aggregate(gDPYearly_2013$Value, by=list(gDPYearly_2013$TIME,gDPYearly_2013$ï..LOCATION), FUN=sum)
agg_gDP2013


###### Calling Function ggplot2 from source File to plot gDP in 2013#####

jpeg("OECD2013.jpg")
ggplotFunc(agg_gDP2013,agg_gDP2013$Group.2,agg_gDP2013$x,"GDP of OECD Group of countries for 2013")
dev.off()

###### Calling function topTenGDP and  Finding top 10 performing countries for 2013
topTen2013<-topTenGDP(gDPYearly_2013,gDPYearly_2013$Value)
topTen2013

## Plot for Top Ten Countries
jpeg("TopTenGDP2013.jpg")
ggplotFunc(topTen2013,topTen2013$ï..LOCATION,topTen2013$Value,"Top Five GDP performing countries for 2013")
dev.off()

### Assigning the result to a variable so that further into analysis we can cbind to from a model
topTenEachYear<- topTen2013
topTenEachYear


######################################    2014 ###############################
####### Creating a Subset of Year for study 2014 and measure as MLN_USD WHILE TAKING OUT OECD and OECDE as it is total shown in the dataset
gDPYearly_2014<-subset(gDPYearly, TIME == 2014 & MEASURE =="MLN_USD" & ï..LOCATION!= "OECD" & ï..LOCATION!="OECDE")
gDPYearly_2014
nrow(gDPYearly_2014)

##### Basic Statistics
## Calling Function descStats sourcing it from Different File
descStats(gDPYearly_2014$Value)

## Using Function findingStats to lovcate which Countries are highest GDP and lowest perfomeing GDPs ##########
findingStats(gDPYearly_2014$ï..LOCATION,gDPYearly_2014$Value)

## Aggregate GDP by group of Countries
agg_gDP2014<-aggregate(gDPYearly_2014$Value, by=list(gDPYearly_2014$TIME,gDPYearly_2014$ï..LOCATION), FUN=sum)
agg_gDP2014

##### 

jpeg("ToTenGDP2014.jpg")
ggplotFunc(agg_gDP2014,agg_gDP2014$Group.2,agg_gDP2014$x, "GDP of OECD Group of countries for 2014")
dev.off()

###### Calling function and  Finding top 10 performing conutries for 2014
topTen2014<-topTenGDP(gDPYearly_2014,gDPYearly_2014$Value)
topTen2014

#############################

gDPYearly_2015<-subset(gDPYearly, TIME == 2015 & MEASURE =="MLN_USD" & ï..LOCATION!= "OECD" & ï..LOCATION!="OECDE")
gDPYearly_2015
str(gDPYearly_2015)
nrow(gDPYearly_2015)


## Calling Function descStats sourcing it from Different File
descStats(gDPYearly_2015$Value)
###### Calling function and  Finding top 5 performing conutries for 2015
topTen2015<-topTenGDP(gDPYearly_2015,gDPYearly_2015$Value)
topTen2015
## Using Function findingStats to lovcate which Countries are highest GDP and lowest perfomeing GDPs ##########
findingStats(gDPYearly_2015$ï..LOCATION,gDPYearly_2015$Value)

##Aggregate by country
agg_gDP2015<-aggregate(gDPYearly_2015$Value, by=list(gDPYearly_2015$TIME,gDPYearly_2015$ï..LOCATION), FUN=sum)
agg_gDP2015


#########################################2016

gDPYearly_2016<-subset(gDPYearly, TIME == 2016 & MEASURE =="MLN_USD" & ï..LOCATION!= "OECD" & ï..LOCATION!="OECDE")
gDPYearly_2016
nrow(gDPYearly_2016)



## Calling Function descStats sourcing it from Different File
descStats(gDPYearly_2016$Value)

## Using Function findingStats to lovcate which Countries are highest gDP and lowest perfomeing gDPs ##########
findingStats(gDPYearly_2016$ï..LOCATION,gDPYearly_2016$Value)

agg_gDP2016<-aggregate(gDPYearly_2016$Value, by=list(gDPYearly_2016$TIME,gDPYearly_2016$ï..LOCATION), FUN=sum)
agg_gDP2016

###### Calling function and  Finding top 10 performing conutries for 2016
topTen2016<-topTenGDP(gDPYearly_2016,gDPYearly_2016$Value)
topTen2016

############################################

gDPYearly_2017<-subset(gDPYearly, TIME == 2017 & MEASURE =="MLN_USD" & ï..LOCATION!= "OECD" & ï..LOCATION!="OECDE")
gDPYearly_2017
nrow(gDPYearly_2017)


## Calling Function descStats sourcing it from Different File
descStats(gDPYearly_2017$Value)

## Using Function findingStats to lovcate which Countries are highest gDP and lowest perfomeing GDPs ##########
findingStats(gDPYearly_2017$ï..LOCATION,gDPYearly_2017$Value)

agg_gDP2017<-aggregate(gDPYearly_2017$Value, by=list(gDPYearly_2017$TIME,gDPYearly_2017$ï..LOCATION), FUN=sum)
agg_gDP2017

###### Calling function and  Finding top 5 performing conutries for 2017
topTen2017<-topTenGDP(gDPYearly_2017,gDPYearly_2017$Value)
topTen2017




#####################################################

gDPYearly_2018<-subset(gDPYearly, TIME == 2018 & MEASURE =="MLN_USD" & ï..LOCATION!= "OECD" & ï..LOCATION!="OECDE")
gDPYearly_2018
nrow(gDPYearly_2018)



## Calling Function descStats sourcing it from Different File
descStats(gDPYearly_2018$Value)

## Using Function findingStats to lovcate which Countries are highest GDP and lowest perfomeing GDPs ##########
findingStats(gDPYearly_2018$ï..LOCATION,gDPYearly_2018$Value)

agg_gDP2018<-aggregate(gDPYearly_2018$Value, by=list(gDPYearly_2018$TIME,gDPYearly_2018$ï..LOCATION), FUN=sum)
agg_gDP2018

###### Calling function and  Finding top 10 performing conutries for 2018
topTen2018<-topTenGDP(gDPYearly_2018,gDPYearly_2018$Value)
topTen2018

#####################################################

gDPYearly_2019<-subset(gDPYearly, TIME == 2019 & MEASURE =="MLN_USD" & ï..LOCATION!= "OECD" & ï..LOCATION!="OECDE"  &ï..LOCATION!="EA19")
gDPYearly_2019
nrow(gDPYearly_2019)


## Calling Function descStats sourcing it from Different File
descStats(gDPYearly_2019$Value)

## Using Function findingStats to lovcate which Countries are highest gDP and lowest perfomeing GDPs ##########
findingStats(gDPYearly_2019$ï..LOCATION,gDPYearly_2019$Value)

agg_gDP2019<-aggregate(gDPYearly_2019$Value, by=list(gDPYearly_2019$TIME,gDPYearly_2019$ï..LOCATION), FUN=sum)
agg_gDP2019

### GDP 2019
jpeg("TopTenGDP2019.jpg")
ggplotFunc(agg_gDP2019,agg_gDP2019$Group.2,agg_gDP2019$x, "GDP of OECD Group of countries for 2019")
dev.off()

###### Calling function and  Finding top 10 performing conutries for 2019
topTen2019<-topTenGDP(gDPYearly_2019,gDPYearly_2019$Value)
topTen2019

############
combinedtopTenYearly <- rbind(topTen2013,topTen2014,topTen2015,topTen2016,topTen2017,topTen2018,topTen2019) 
combinedtopTenYearly


#### Aggregate of GDP groupbyYEar and Location
combinedtopTenYearlyAgg<-aggregate(combinedtopTenYearly$Value,by=list(combinedtopTenYearly$TIME,combinedtopTenYearly$ï..LOCATION), FUN=sum)
combinedtopTenYearlyAgg


#####################     Plotting Descriptive Functions     ############

### Qplot showing GDP over the years each dot represent one year


### GDP 2019
jpeg("TopTenGDP2013_2019.jpg")
ggplot(gDPYearly_2013_2019Agg,aes(Group.2,x,color=Group.2))+geom_point()+
  theme(axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"),
        axis.text.x = element_text(angle =90, vjust =0 ,hjust = 0))+ 
  xlab("Countries") + ylab("Million USD") +
  scale_color_discrete(name = "Countries")+ guides(fill=guide_legend(title=""))
dev.off()


####Calling function from Source File to plot 
jpeg("OECDGDP2013_2019.jpg")
ggplotFuncGDP(gDPYearly_2013_2019Agg,gDPYearly_2013_2019Agg$Group.2,gDPYearly_2013_2019Agg$x,
              gDPYearly_2013_2019Agg$Group.1,"Countries","Million-USD","Countries with rising GDP over the years","Years")
dev.off()


################################################
####Calling function from Source File to plot 
jpeg("OECDTenGDP2013_2019.jpg")
ggplotFuncGDP(combinedtopTenYearlyAgg,combinedtopTenYearlyAgg$Group.2,combinedtopTenYearlyAgg$x,
              combinedtopTenYearlyAgg$Group.1,"Countries","Million-USD","Top Ten Best Performing GDP Countries","Year")
dev.off()



########################################
      ######################################################

                #########################################################################

##### custom Functions sourced from functionFile
source('~/04_function.R')

########################    Arrivals ########################
yearlyArrivalsFlights<- read.csv(file="YearlyArrivalsFlights.csv", head=TRUE, sep=",")
yearlyArrivalsFlights
class(yearlyArrivalsFlights)
nrow(yearlyArrivalsFlights)

# Checking for NA
yearlyArrivalsFlights[!complete.cases(yearlyArrivalsFlights),]
yearlyArrivalsFlights<-yearlyArrivalsFlights[complete.cases(yearlyArrivalsFlights),]

#### strucrure of File
str(yearlyArrivalsFlights) 


#### Aggregating Years by category and arrival Airport
yearlyArrivalsFlightsCity<-aggregate(yearlyArrivalsFlights[,4:10],by=list(yearlyArrivalsFlights$Category,yearlyArrivalsFlights$DestinationCity), FUN=sum)
yearlyArrivalsFlightsCity
### Using melt function to melt years
yAFNew<- melt(yearlyArrivalsFlightsCity)
yAFNew


### QPlot for Airports and Passengers
jpeg("Flights and PassengersAirport.jpg")
ggplot(yAFNew,aes(Group.2,value,color=Group.1))+geom_point()+
  theme(axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"),
        axis.text.x = element_text(angle =90, vjust =0.5 ,hjust = 1))+ 
  xlab("Countries") + ylab("Passengers Thousand") +
  scale_color_discrete(name = "Countries")
dev.off()


###################################################################################


#### Aggregating Years by Destination Countries  and arrival Airport
yearlyArrivalsFlightsCountry<-aggregate(yearlyArrivalsFlights[,4:10],by=list(yearlyArrivalsFlights$DestinationCity,yearlyArrivalsFlights$SourceCountries), FUN=sum)
yearlyArrivalsFlightsCountry


#####################  Flights #############################
#### Subseting Arrivals as per Flights
yearlyArrivalFlightsComm<-subset(yearlyArrivalsFlights, Category == "Commercial Flights (Thousand)")
yearlyArrivalFlightsComm


############# Total Commercial Flights in Ireland each year #############

totalFlightsComm<-aggregate(yearlyArrivalFlightsComm[,4:10],by=list(yearlyArrivalFlightsComm$Category), FUN=sum)
totalFlightsComm

### Using melt function to melt years
totalFlightsCommNew<- melt(totalFlightsComm)
totalFlightsCommNew

####################### Creating Model################
## Removing Character from the Model
totalFlightsCommNew$variable<- as.integer(gsub('[a-zA-Z]', '', totalFlightsCommNew$variable))
totalFlightsCommNew

gDPYearly_2013_2019_Collated<-subset(gDPYearly_2013_2019_Year ,select=-Group.1)
gDPYearly_2013_2019_Collated
#####
combinedValuesGDP_Flights <- cbind(totalFlightsCommNew,gDPYearly_2013_2019_Collated) 
combinedValuesGDP_Flights


##################### Line Plots to Show GDP and Number of Flights ############

######################### Below Code is inspired from 
########################## https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html


jpeg("FligtVsGDP.jpg")
coeff <- 100000
valueColor <- "red"
xColor <- rgb(0.2, 0.6, 0.9, 1)
ggplot(combinedValuesGDP_Flights, aes(x=variable)) +
  geom_line( aes(y=value),size=1, color=valueColor) + 
  geom_line( aes(y=x/coeff),size=1, color=xColor) + # Divide by 10 to get the same range than the temperature
  scale_x_continuous(
   name = "Years (2013-2019)               "
  ) +#geom_point(aes(y)size=4, shape=21)
  scale_y_continuous(
    name = "Flights in Thousand",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="GDP Million USD")
  )+
  theme_ipsum() +theme(
    axis.title.y = element_text(color = valueColor, size=13),
    axis.title.y.right = element_text(color = xColor, size=13)
  ) +ggtitle("Increase in GDP and Flights over the years")
dev.off()


##Aggregating All the passengers from All years
totalFlightsGroupCountry<-aggregate(yearlyArrivalFlightsComm[,4:10],by=list(yearlyArrivalFlightsComm$SourceCountries,yearlyArrivalFlightsComm$DestinationCity), FUN=sum)
totalFlightsGroupCountry

#############################
### Callling ggplot Function from source file

jpeg("TotalFlightsIreland.jpg")

ggplot(totalFlightsCommNew, aes(x=variable,y=value))+geom_bar(stat = "identity",width=0.7,alpha = 1,colour="#CC79A7",fill="blue")+ 
  theme(axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"),
        axis.text.x = element_text(angle = 90, vjust =0.5 ,hjust =1))+ 
  xlab("Years") + ylab("Number of Flights in Thousands") +ggtitle("Flights to Ireland between 2013 and 2019")+guides(fill=guide_legend(title=""))

dev.off()

#### Summing
totalFlightsAirport<-aggregate(yearlyArrivalFlightsComm[,4:10],by=list(yearlyArrivalFlightsComm$SourceCountries,yearlyArrivalFlightsComm$DestinationCity), FUN=sum)
totalFlightsAirport

##############################################################################
# Subseting Arrivals as per passengers coming in Ireland
yearlyArrivalPassenger<-subset(yearlyArrivalsFlights, Category == "Passengers (Thousand)")
YearlyArrivalPassenger

totalPassengPerAirport<-aggregate(yearlyArrivalPassenger[,4:10],by=list(yearlyArrivalPassenger$Category,yearlyArrivalPassenger$SourceCountries), FUN=sum)
totalPassengPerAirport

### Using melt function to melt years
tPA_New<- melt(totalPassengPerAirport)
tPA_New

### Plotting passengers per Airport
jpeg("TotalPassengersArrivedIreland.jpg")

ggplot(tPA_New, aes(x=Group.2,y=value,fill=variable))+
  geom_bar(stat = "identity",width=0.7,alpha = 1,colour="blue")+ 
  theme(axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"),
        axis.text.x = element_text(angle = 90, vjust =0.5 ,hjust =1))

dev.off()


##################################################################################################################

## GDP dataset contain only ISO3 country names whereas Flight contain country name inorder to read combined model 
#For Loop was created which checked ISO 3 country codes with Corresponding Country name and added a new Column in dataframe
countryCodes<- read.csv(file="country-codes.csv", head=TRUE, sep=",")
countryCodes

### Grouping GDP by Countries
combinedtopTenGDPYearlyAgg<-aggregate(combinedtopTenYearly$Value,by=list(combinedtopTenYearly$TIME,combinedtopTenYearly$ï..LOCATION), FUN=sum)
as.data.frame(combinedtopTenGDPYearlyAgg)


##### We have ISO3 values for Countries to get Full name  compared it to file CountryCodes to get full Country name
# Created new Variable Country in the file assigning it a NA
combinedtopTenGDPYearlyAgg$Country=NA
combinedtopTenGDPYearlyAgg

#Assign index1 to number of rows
indexes1 <- nrow(combinedtopTenGDPYearlyAgg)
indexes1
indexes2<-nrow(countryCodes) #Index2 to row of country codes
indexes2
#options(warn=-1)
options(warn=0)
# Iterate over each row of 
for(index1 in 1:indexes1) {
  # Iterate over each row of countrycode
  for(index2 in 1:indexes2) {
    if(combinedtopTenGDPYearlyAgg[index1,"Group.2"] == countryCodes[index2,"ISO3166.1.Alpha.3"]){
      
      combinedtopTenGDPYearlyAgg[index1,"Country"]<-as.character(countryCodes[index2,"official_name_en"])
      break
    }
  }
}
combinedtopTenGDPYearlyAgg
#combinedtopTenGDPYearlyPlot<-select(combinedtopTenYearlyAgg,-c(2))
#combinedtopTenGDPYearlyPlot

##### Creating a model from Top ten GDP and Flights from Countries
##Sourced from above code contains Flight information
totalFlightsGroupCountry

#### Transposed Data to Bind the Two different Datasets
toFlightsGroupCountryTransposed<-melt(totalFlightsGroupCountry)
toFlightsGroupCountryTransposed

### Here the Variable which is Year is a Factor as it reads x2013 and so on.
## Removing character values from Year as this is how File is read from CSV files
toFlightsGroupCountryTransposed$variable <- as.integer(gsub('[a-zA-Z]', '', toFlightsGroupCountryTransposed$variable))
toFlightsGroupCountryTransposed
#Aggregating Flights by year and Country
toFlightsGroupCountryTransposedAgg<-aggregate(toFlightsGroupCountryTransposed$value,by=list(toFlightsGroupCountryTransposed$variable,toFlightsGroupCountryTransposed$Group.1), FUN=sum)
toFlightsGroupCountryTransposedAgg


### Combined Model Creating a loop 

combinedModel<-combinedtopTenGDPYearlyAgg ## created using For loop assigning it to new variable
combinedModel

combinedModel$numberOfFlights=NA #Creating a new Column 
combinedModel

indexes1 <- nrow(combinedModel)
indexes1
indexes2<-nrow(toFlightsGroupCountryTransposed)
indexes2
#options(warn=-1)
options(warn=0)
# Iterate over each row of 
for(index1 in 1:indexes1) {
  # Iterate over each row of countrycode
  for(index2 in 1:indexes2) {
    if(combinedModel[index1,"Country"] == toFlightsGroupCountryTransposed[index2,"Group.1"] & 
       combinedModel[index1,"Group.1"] == toFlightsGroupCountryTransposed[index2,"variable"]){
      
      combinedModel[index1,"numberOfFlights"]<-as.numeric(toFlightsGroupCountryTransposed[index2,"value"])
      break
    }
  }
}
combinedModel #Model Contains all the Top ten countries GDP and Flight over 2013-19

### Selecting Countries whose Flight and GDP data is available
combinedModelGDPFlight<-combinedModel[complete.cases(combinedModel), ]  #Selecting only those who have gdp and flights to Ireland
combinedModelGDPFlight

######SELECTING COUNTRIES with Available GDP and Flight info
combinedModelGDPFlight<-combinedModelGDPFlight[c(1:14,(18:24),(33:39)),]
combinedModelGDPFlight
### Changing GDP to miilion
combinedModelGDPFlight$GDPMillion<-combinedModelGDPFlight$x/1000000 
combinedModelGDPFlight

### Flights to Ireland By Country of Residence

jpeg("TopFourGDPCountry.jpg")

ggplot(combinedModelGDPFlight,aes(x=as.numeric(Group.1),y=numberOfFlights,fill=Country))+
  geom_bar(stat='identity', position='dodge2')+theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(
    title = 'Flights to Ireland by Country of residence 2013-2019',
    x = '') 
dev.off()

jpeg("TopFourFlightCountry.jpg")
ggplot(combinedModelGDPFlight)+geom_bar(aes(reorder(Group.1,-GDPMillion),GDPMillion,fill=Country),
                                        stat='identity', position='dodge2')+theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(
    title = 'GDP of residence Countries 2013-2019', x = '') 
dev.off()

####Subsetting USA for Linear Regression
combinedModelGDPFlight_USA<-combinedModelGDPFlight[22:28,]
combinedModelGDPFlight_USA

###### Germany Subset
combinedModelGDPFlight_Germany<-combinedModelGDPFlight[1:7,]
combinedModelGDPFlight_Germany

####EU28 Subset
combinedModelGDPFlight_EU28<-combinedModelGDPFlight[7:14,]
combinedModelGDPFlight_EU28

###Summary of GDP and Number of Flights by Countries
summaryOfCountries<-by(combinedModelGDPFlight, combinedModelGDPFlight$Country, summary) 
summaryOfCountries

###Mean of GDP by Country###########
tapply(combinedModelGDPFlight$GDPMillion,combinedModelGDPFlight$Country,mean)

###Mean of Numberof flights by Country
tapply(combinedModelGDPFlight$numberOfFlights,combinedModelGDPFlight$Country,mean)

#################### Correlation Model ##################

# Shapiro-Wilk normality test for numberOfFlights
shapiro.test(combinedModelGDPFlight_USA$numberOfFlights) # => p = 0.6481

# Shapiro-Wilk normality test for USA$GDPMillion
shapiro.test(combinedModelGDPFlight_USA$GDPMillion) # => p = 0.9338

###From the output, the two p-values are greater than the significance level 0.05 implying that the distribution of the 
#data are not significantly different from normal distribution. In other words, we can assume the normality.


###Checking normality of datasets
# GDPMillion
jpeg("NormalityNoFlights.jpg")
ggqqplot(combinedModelGDPFlight_USA$GDPMillion, ylab = "Number of Flights(Thousand)")
dev.off()

# GDPMillion
jpeg("NormalityGDP.jpg")
ggqqplot(combinedModelGDPFlight_USA$GDPMillion, ylab = "GDP (Million USD)")
dev.off()

#The plots shows that the Flight data may be  normal distributions.

###Correlation Test for USA Data

res <- cor.test(combinedModelGDPFlight_USA$numberOfFlights, combinedModelGDPFlight_USA$GDPMillion, 
                method = "pearson")
res

jpeg("CorrelationModel.jpg")
ggscatter(combinedModelGDPFlight_USA, x = "numberOfFlights", y = "GDPMillion", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of Flights(Thousand)", ylab = "GDP (Million USD)")
dev.off()

### Measure correlation (r) between All selected Contries Flights and GDP
r <- by(combinedModelGDPFlight,combinedModelGDPFlight$Country, FUN = function(X) cor(X$numberOfFlights, X$GDPMillion, method = "pearson"))
r 
r2 <- r*r    # calculate r squared
r2




########################To forecast Flights we now do a linear regression Model

##### Creating a Linear Regression Model for each Country using by function .This will provide y=a+bx for each country
models <- by(combinedModelGDPFlight, combinedModelGDPFlight$Country, function(df) lm(numberOfFlights~GDPMillion, data=df)) 
models

### Confidence Interval of Each Country
lapply(models, confint) 


### European Union 28 Intercept and Slope
#y1=a+bx
y1=-4.645+5.117*3.6 # x(independent variable = 3.6 assumed)
y1

### Germany Intercept and Slope
#y2=a+bx
y2=-7.549+3.595 *3.6 # x(independent variable = 3.6 assumed)
y2

### United Kingdom Intercept and Slope
y3=17.96+12.69 *3.6 # x(independent variable = 3.6 assumed)
y3


###United States of America
y4=-10.8004+0.9505 *3.6 # x(independent variable = 3.6 assumed)
y4


####Linear Regression for USA  using  ggplot2

jpeg("LinearRegressionModel.jpg")
plot(combinedModelGDPFlight_USA$GDPMillion, combinedModelGDPFlight_USA$numberOfFlights, pch = 16, cex = 1.3, col = "blue", 
     main = "GDP vs Number of Flights", xlab = "GDP USA (Million)", ylab = "Number of Flights (Thousand)")
ablineclip(lm(combinedModelGDPFlight_USA$numberOfFlights ~ combinedModelGDPFlight_USA$GDPMillion), col="orange",x17=1,x2=25) # show regression line
dev.off()

## Predicted values for seven years in the dataset
predict(lm(combinedModelGDPFlight_USA$numberOfFlights ~ combinedModelGDPFlight_USA$GDPMillion))

#predicted values for Flights from USA if GDP falls because of Coronavirus Pandemic
flightsInIreland <- data.frame(GDP_Thousand<- c(3.5,4.8,5.5,6.2,6.8,7.2,7.8))
flightsInIreland 
predict(lm(combinedModelGDPFlight_USA$numberOfFlights ~ combinedModelGDPFlight_USA$GDPMillion), flightsInIreland)

############## BarPlot and LinePlot  ############
######################### Below Code is inspired from 
########################## https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html

jpeg("USALMModel.jpg")
valueColor <- "#69b3a2"
coeff <- 10000
xColor <- "blue"
ggplot(combinedModelGDPFlight_USA)+
geom_bar( aes(x=Group.1,y=numberOfFlights),stat="identity", fill=valueColor,size=.5, color="black", alpha=.6) + 
geom_line(aes(x=Group.1,y=GDPMillion),size=1, color=xColor)+
  scale_x_continuous(
    name = "Years (2013-2019)"
  ) +
  scale_y_continuous(
    name = "Flights in Thousand",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="GDP Million USD")
  )+theme_ipsum() +theme(
    axis.title.y = element_text(color = valueColor, size=13),
    axis.title.y.right = element_text(color = xColor, size=13)
  ) +ggtitle("USA Model -Increase in GDP and Flights")
dev.off()

#############################

jpeg("GermanyLMModel.jpg")
valueColor <- "#ffb653"
coeff <- 10000
xColor <- "blue"
ggplot(combinedModelGDPFlight_Germany)+
  geom_bar( aes(x=Group.1,y=numberOfFlights),stat="identity", fill=valueColor,size=.5, color="black", alpha=.6) + 
  geom_line(aes(x=Group.1,y=GDPMillion),size=1, color=xColor)+
  scale_x_continuous(
    name = "Years (2013-2019)"
  ) +
  scale_y_continuous(
    name = "Flights in Thousand",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="GDP Million USD")
  )+theme_ipsum() +theme(
    axis.title.y = element_text(color = valueColor, size=13),
    axis.title.y.right = element_text(color = xColor, size=13)
  ) +ggtitle("Increase in GDP and Flights over the years")
dev.off()

#############################






