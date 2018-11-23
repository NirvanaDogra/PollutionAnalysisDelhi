#RH ->relaive humidiy
#SR ->solar radition
#vWS ->vertile wind speed
#ws->wind speed
#PM10 ->patulicae matter
#AT ->air temp
(RawData)
summary(RawData)

#coverting the character to  integer
RawData$AT<-as.numeric(RawData$AT)


#checing the values of temp all are null
RawData$Temp
max(RawData$Temp)
table(RawData$Temp)


RawData$WS #is a character type need conversion
RawData$WS<-as.numeric(RawData$WS)
summary(RawData$WS)

#comverting the data to numberic
RawData$VWS<-as.numeric(RawData$VWS)
RawData$NH3<-as.numeric(RawData$NH3)
RawData$NO<-as.numeric(RawData$NO)
RawData$NO2<-as.numeric(RawData$NO2)
RawData$NOx<-as.numeric(RawData$NOx)
RawData$Ozone<-as.numeric(RawData$Ozone)
RawData$SO2<-as.numeric(RawData$SO2)
RawData$CO<-as.numeric(RawData$CO)


table(RawData$From)

#install.packages("stringr")
packages(stringr)

#creating a new data set for making making bigger changes
dataset<-subset(RawData, !(is.na(RawData$From)))

#getting time of the day
dataset$From=str_sub(dataset$From, start=12, end=16)

#convering the data form canonical to numerical
uniNewForm=unique(dataset$From)
uniNewForm
listValue=c();
for(i in 1:96){
  listValue[i]<-i
}
listValue

dataset$From<- factor(dataset$From, uniNewForm, listValue)


#doing the same for the column 2
for(i in 2:96){
  listValue[i-1]<-i
}
listValue[96]<-1
listValue[97]<-3  #handling the 04:27 value that does not fall 
#into any category by binning it with 04:30 category
listValue

dataset$To=str_sub(RawData$To, start=12, end=16)
uniNewTo<-unique(dataset$To)
uniNewTo
dataset$To<- factor(dataset$To, uniNewTo, listValue)

#finally adding the data column to the dataset
dataset$date<-str_sub(RawData$From, start =0, end=11)
val = as.Date(dataset$date, "%d-%m-%y")

#futher processing################################################33
#visiuizaton of dataset

summary(dataset)

#visualization function
basicObservationOF<- function(x){
    summary(x)
  hist(x,freq=FALSE)
  lines(density(x, na.rm=TRUE), col="red", lwd=2)
}

#plotting the trend in accordance of of date VS attribute
library("ggplot2")
dateVSAttribute<- function(x){
  qplot(dataset$date, x)
}

###############################################################################
summary(dataset$AT)
basicObservationOF(dataset$AT)
#left skewed
dateVSAttribute(dataset$AT)
#the general trend is decreasing wih date
################################################################################


################################################################################
#the processing for BP
summary(dataset$BP)
basicObservationOF(dataset$BP)
dateVSAttribute(dataset$BP)
#the general trensd increases with date
#################################################################################



#################################################################################
#the processing of PM10
summary(dataset$PM10)
basicObservationOF(dataset$PM10)
#observation 1.The graph is right skewed and requires normalization 2.THe most frequent values are 100-400

dateVSAttribute(dataset$PM10)
#no obervation can be derived but we can say there are sudden changes in values through the day
#################################################################################


#################################################################################
#the processing of PM2.5
summary(dataset$PM2.5)
basicObservationOF(dataset$PM2.5)
#1.values are right skewed snd requires transformation 2.THe values are in 50-100 range. 3. There are some values at 600 and is an outlier
dateVSAttribute(dataset$PM2.5)
#no observation can be derived is usually low below 120 but in some case there are spikes
################################################################################


################################################################################
#the process ofRH
summary(dataset$RH)
basicObservationOF(dataset$RH)
#1. the graph as similar mean and


dateVSAttribute(dataset$RH)
#no observations remains consistant through out the month
#no outliers
#################################################################################


################################################################################
summary(dataset$SR)
basicObservationOF(dataset$SR)
#1.Right skewed 2.major values lie in 0-50 

dateVSAttribute(dataset$SR)
#the general trend tell us a decrease in the radition with passsing of days
#################################################################################



#################################################################################
summary(dataset$WD)
basicObservationOF(dataset$WD)
#1.bimodal graph 2.the values major lies between 20-60 and 160-180 ie North west and south-west 
dateVSAttribute(dataset$WD)
#South - 180 degrees West - 270 degrees North - 360 degrees 
#the winds usually flows in the direction between 0-200 i.e North(Muzzarfnagar)---east(Gurugram)---south(Noida)----south-west(Dwarka) 
#and rarely in North-west
###################################################################################


##################################################################################
summary(dataset$WS)
basicObservationOF(dataset$WS)
#1. Right skewed


dateVSAttribute(dataset$WS)
#the genaral speed 1.5m/s
##################################################################################


##################################################################################
summary(dataset$VWS)
basicObservationOF(dataset$VWS)


dateVSAttribute(dataset$VWS)
##################################################################################

##################################################################################
summary(dataset$Benzene)
basicObservationOF(dataset$Benzene)
#right skewed
dateVSAttribute(dataset$Benzene)
#no observation
##################################################################################


##################################################################################
summary(dataset$Toluene)
basicObservationOF(dataset$Toluene)
#right skewed

dateVSAttribute(dataset$Toluene)
#no observation
##################################################################################



##################################################################################
summary(dataset$NH3)
basicObservationOF(dataset$NH3)
#a little right skewed
dateVSAttribute(dataset$NH3)
#genarally increasing with time
##################################################################################


##################################################################################
summary(dataset$NO)
basicObservationOF(dataset$NO)
#right skewed
dateVSAttribute(dataset$NO)
#no observation
##################################################################################


##################################################################################
summary(dataset$NO2)
basicObservationOF(dataset$NO2)

dateVSAttribute(dataset$NO2)
#no observation values between 30-150
##################################################################################


##################################################################################
summary(dataset$NOx)
basicObservationOF(dataset$NOx)
#right skewed

dateVSAttribute(dataset$NOx)
#no observation values between 10-200
###################################################################################



##################################################################################

summary(dataset$Ozone)
basicObservationOF(dataset$Ozone)
#right skewed

dateVSAttribute(dataset$Ozone)
#no observation
##################################################################################



################################################################################
summary(dataset$SO2)
basicObservationOF(dataset$SO2)
#right skewed

dateVSAttribute(dataset$SO2)
#the values between 20-40
###################################################################################



#################################################################################
summary(dataset$CO)
basicObservationOF(dataset$CO)
#right skewed

dateVSAttribute(dataset$CO)
#################################################################################




install.packages("GGally")


library("GGally")
library("ggplot2")
ggcorr(dataset)

correlationBetween<-function(x,y){
  plot(x,y)
  cor(x,use="na.or.complete", y)
  
}



#results shouwed the follows -


###########################positive correleation###################################################################
#1. Nox & CO (no realtion what to ever and will be consdered as erroneous, but both are emmited by vehicles maybe this might be the cause of the rellation
#2. NO & NOx (we found out that NOx the measure of NO and NO2 in air)
#3. Benzene & Toluene (no conversion as such emmited together)
#4. NO and CO (same as 1.)
#5. Toluene and CO
#6. Benzene and CO

#############################negative correnlation###################################################################

#AT & RH 
correlationBetween(dataset$AT, dataset$RH)

#RH & SR
#(https://www.sciencedirect.com/science/article/pii/S0378377415000281)
correlationBetween(dataset$RH, dataset$SR)

#RH & Ozone (In general, warm dry weather is more conducive to ozone formation than cool wet weather. but can reduce the concentration of ozone)
#hecnce also expalins the relation of ws and other attributes. The wind disperse the pollutants(https://cfpub.epa.gov/roe/indicator_pdf.cfm?i=8)
correlationBetween(dataset$RH, dataset$Ozone)


#Nox and ozone (https://en.wikipedia.org/wiki/Ozone_depletion)(Ozone can be destroyed by a number of free radical catalysts; the most important are the hydroxyl radical (OH·), nitric oxide radical (NO·),)
correlationBetween(dataset$NO, dataset$Ozone) 
correlationBetween(dataset$NO2, dataset$Ozone)#the most important negative corr is between NO2 and ozone

#AT and CO (CO is converts to CO2)
correlationBetween(dataset$AT, dataset$CO) 













regressor = lm(formula = AT ~ PM10,
               data = dataset)
summary(regressor)
plot(regressor)


