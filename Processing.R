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
  hist(x,freq=FALSE, xlab ="Attribute")
  lines(density(x, na.rm=TRUE), col="red", lwd=2)
}

#plotting the trend in accordance of of date VS attribute
library("ggplot2")
dateVSAttribute<- function(x){
  qplot(dataset$date, x, xlab = "Date", ylab = "Range")
}

library("ggplot2")
TimeVSAttribute<- function(x){
  qplot(dataset$From, x, xlab= "Time", ylab = "Range")
}

###############################################################################
summary(dataset$AT)
basicObservationOF(dataset$AT)
#left skewed
dateVSAttribute(dataset$AT)
#the general trend is decreasing wih date
TimeVSAttribute(dataset$AT)
#this makes sense as the value inc form 32-70 that is the day time

################################################################################


################################################################################
#the processing for BP
summary(dataset$BP)
basicObservationOF(dataset$BP)
dateVSAttribute(dataset$BP)
#the general trensd increases with date
TimeVSAttribute(dataset$BP)
#################################################################################



#################################################################################
#the processing of PM10
summary(dataset$PM10)
basicObservationOF(dataset$PM10)
#observation 1.The graph is right skewed and requires normalization 2.THe most frequent values are 100-400

dateVSAttribute(dataset$PM10)
#no obervation can be derived but we can say there are sudden changes in values through the day

TimeVSAttribute(dataset$PM10)
#stagnent
#################################################################################


#################################################################################
#the processing of PM2.5
summary(dataset$PM2.5)
basicObservationOF(dataset$PM2.5)
#1.values are right skewed snd requires transformation 2.THe values are in 50-100 range. 3. There are some values at 600 and is an outlier
dateVSAttribute(dataset$PM2.5)
#no observation can be derived is usually low below 120 but in some case there are spikes

TimeVSAttribute(dataset$PM2.5)
#stagnent
################################################################################


################################################################################
#the process ofRH
summary(dataset$RH)
basicObservationOF(dataset$RH)
#1. the graph as similar mean and


dateVSAttribute(dataset$RH)
#no observations remains consistant through out the month
#no outliers

TimeVSAttribute(dataset$RH)
#drop during the after noon at 12ish
#################################################################################


################################################################################
summary(dataset$SR)
basicObservationOF(dataset$SR)
#1.Right skewed 2.major values lie in 0-50 

dateVSAttribute(dataset$SR)
#the general trend tell us a decrease in the radition with passsing of days

TimeVSAttribute(dataset$SR)
#peaking during the noon
#################################################################################



#################################################################################
summary(dataset$WD)
basicObservationOF(dataset$WD)
#1.bimodal graph 2.the values major lies between 20-60 and 160-180 ie North west and south-west 
dateVSAttribute(dataset$WD)
#South - 180 degrees West - 270 degrees North - 360 degrees 
#the winds usually flows in the direction between 0-200 i.e North(Muzzarfnagar)---east(Gurugram)---south(Noida)----south-west(Dwarka) 
#and rarely in North-west

TimeVSAttribute(dataset$WD)
#stagnet
###################################################################################


##################################################################################
summary(dataset$WS)
basicObservationOF(dataset$WS)
#1. Right skewed


dateVSAttribute(dataset$WS)
#the genaral speed 1.5m/s

TimeVSAttribute(dataset$WS)
#some what inc in noon
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

TimeVSAttribute(dataset$Benzene)
#during the evening time the range is less maybe because industris dont work at night
##################################################################################


##################################################################################
summary(dataset$Toluene)
basicObservationOF(dataset$Toluene)
#right skewed

dateVSAttribute(dataset$Toluene)
#no observation
TimeVSAttribute(dataset$Toluene)
#during the evening time the range is less maybe because industris dont work at night
##################################################################################



##################################################################################
summary(dataset$NH3)
basicObservationOF(dataset$NH3)
#a little right skewed
dateVSAttribute(dataset$NH3)
#genarally increasing with time
TimeVSAttribute(dataset$NH3)
#stagnent
##################################################################################


##################################################################################
summary(dataset$NO)
basicObservationOF(dataset$NO)
#right skewed
dateVSAttribute(dataset$NO)
#no observation

TimeVSAttribute(dataset$NO)

##################################################################################


##################################################################################
summary(dataset$NO2)
basicObservationOF(dataset$NO2)

dateVSAttribute(dataset$NO2)
#no observation values between 30-150

TimeVSAttribute(dataset$NO2)
#random
##################################################################################


##################################################################################
summary(dataset$NOx)
basicObservationOF(dataset$NOx)
#right skewed

dateVSAttribute(dataset$NOx)
#no observation values between 10-200

TimeVSAttribute(dataset$NOx)
###################################################################################



##################################################################################

summary(dataset$Ozone)
basicObservationOF(dataset$Ozone)
#right skewed

dateVSAttribute(dataset$Ozone)
#no observation

TimeVSAttribute(dataset$Ozone)
#onzone frmation inc
##################################################################################



################################################################################
summary(dataset$SO2)
basicObservationOF(dataset$SO2)
#right skewed

dateVSAttribute(dataset$SO2)
#the values between 20-40

TimeVSAttribute(dataset$SO2)
###################################################################################



#################################################################################
summary(dataset$CO)
basicObservationOF(dataset$CO)
#right skewed

dateVSAttribute(dataset$CO)

TimeVSAttribute(dataset$CO)
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






####################################################Final conclusion##################################################
# Remove VWS
# 
#
#
#
#
#######################################################################################################################


#transformation ---

transformedDataObservations<-function(x){
  par(mfrow=c(4,1))
  boxplot(x, log10(x), sqrt(x), col = c("red", "blue", "green"), names = c("Original", "Log10(X)", "sqrt"))
  
  hist(x, xlab = "x", freq = FALSE)
  lines(density(x, na.rm=TRUE), col="red", lwd=2)
  
  hist(log(x), xlab = "log(x)", freq = FALSE)
  lines(density(log(x), na.rm=TRUE), col="red", lwd=2)
  
  hist(sqrt(x), xlab = "sqrt(x)", freq = FALSE)
  lines(density(sqrt(x), na.rm=TRUE), col="red", lwd=2)
  
  print("Summary of Attribute");
  print(summary(x))
  
  print("Summary of Log10(X)");
  print(summary(log10(x)))
  
  print("Summary of Sqrt")
  print(summary(sqrt(x)))
}


newdata<-dataset[ -c(4,9,10,11,12,13,14,16,17) ]
summary(newdata)

newdata<-na.omit(newdata)
length(newdata$AT)

library("GGally")
library("ggplot2")
ggcorr(newdata)



transformedDataObservations(newdata$AT)
#the log transformationlook to be the best option in this case
#treating outliers for AT
par(mfrow= c(1,1))
plot(newdata$AT)
boxplot.stats(newdata$AT)$out
boxplot(newdata$AT)

newdata$AT[newdata$AT<12]<-mean(newdata$AT)
transformedDataObservations(newdata$AT)
boxplot.stats(log(newdata$AT))$out




transformedDataObservations(newdata$PM10)
#transformation using log
boxplot.stats((newdata$PM10))$out
boxplot(log(newdata$PM10))
min(newdata$PM10)




transformedDataObservations(newdata$PM2.5)
boxplot.stats(log(newdata$PM2.5))$out
#transformation using log

transformedDataObservations(newdata$NH3)
#transforation usng log
boxplot.stats(log(newdata$NH3))$out

transformedDataObservations(newdata$NOx)
#transfroamtrion using log
boxplot.stats(log(newdata$NOx))$out


transformedDataObservations(newdata$Ozone)
#transforarion using log
boxplot.stats(log(newdata$Ozone))$out

transformedDataObservations(newdata$SO2)
#transforatiom using log
boxplot.stats(log(newdata$SO2))$out

transformedDataObservations(newdata$CO)
boxplot.stats((newdata$CO))$out
par(mfrow=c(1,1))


library(caTools)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(newdata,SplitRatio = 0.75) 
train =subset(newdata,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(newdata, sample==FALSE)

class(newdata$date)
plot(train$date, train$PM2.5)


lct <- Sys.getlocale("LC_TIME"); 
Sys.setlocale("LC_TIME", "C")
val<-as.Date(train$date,"%d-%m-%y")
val
class(val)
plot(val, train$AT)
