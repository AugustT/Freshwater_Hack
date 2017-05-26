CollData<-read.csv(file.choose())
EngDataTime<-read.csv(file.choose())
EngDataSum<-read.csv(file.choose())
summary(EngDataSum)
summary(EngDataTime)
#################################

library(ggplot2)
library(scales)
library(corrplot)

dframe3<-read.csv(file.choose())

M<-cor(dframe3)
head(round(M,2))

corrplot(M, method="circle")


####################################################

EngDataSum<-read.csv(file.choose())

model1<-glm(Sample_bi~Attendees+Paid+TranTrai+Upload+access, family="binomial", data=EngDataSum)
summary(model1)

