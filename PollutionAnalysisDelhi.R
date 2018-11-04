#gentration of workabl data
data1=subset(data,, c(1,2))

data1$AT<-AT
data1$BP<-as.numeric(data$BP)
data1$PM10<-as.numeric(data$PM10)
data1$PM2.5<-as.numeric(data$PM2.5)
data1$RH<-as.numeric(data$RH)

View(data1)