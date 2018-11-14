#RH ->relaive humidiy
#SR ->solar radition
#vWS ->vertile wind speed
#ws->wind speed
#PM10 ->patulicae matter
#AT ->air pollution
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

#futher processing################################################33
#visiuizaton of dataset

summary(dataset)
summary(dataset$PM10)
hist(dataset$PM10)
mean(dataset$PM10)
seq
#dnorm(dataset$PM10, mean = (dataset$PM10), sd(dataset$PM10))
line(dataset$PM10, )
hist(dataset$AT)

regressor = lm(formula = AT ~ BP,
               data = dataset)
summary(regressor)
plot(regressor)


