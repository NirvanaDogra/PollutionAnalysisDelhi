View(RawData)
summary(RawData)


#converting the data into integer values ie is a more workable format
write.csv(RawData, file="Data.csv")
Data=read.csv("Data.csv")
summary(Data)
