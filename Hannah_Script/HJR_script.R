CollData<-read.csv(file.choose())
EngDataTime<-read.csv(file.choose())
EngDataSum<-read.csv(file.choose())
summary(EngDataSum)
summary(EngDataTime)
summary(CollData)

#################################

model1<-glm(Sample_bi~Online+Paid+TranTrai+Upload+Attendees, family="binomial", data=EngDataSum)
summary(model1)

library(boot)
boot::inv.logit() #::saves calling library

library(lme4)
predict.plot(model1)

plot(EngDataSum$Sample_bi~EngDataSum$Online)

model2<-glm(Super_user~PCA1_Difficulty+Upload+Team+People,family="binomial",data=EngDataSum)
summary(model2)


############################################################################################

#playing around with visualising models#
library(visreg)
library(ggplot2)
library(reshape2)

visreg(model1,xvar="Attendees")
visreg(model1,xvar="Paid")
visreg(model1,xvar="TranTrai")
visreg(model1,xvar="Upload")

Paid<-table(EngDataSum$Sample_bi,EngDataSum$Paid)
Online<-table(EngDataSum$Sample_bi,EngDataSum$Online)
TranTrai<-table(EngDataSum$Sample_bi,EngDataSum$TranTrai)
Upload<-table(EngDataSum$Sample_bi,EngDataSum$Upload)
Attendees<-table(EngDataSum$Sample_bi,EngDataSum$Attendees)


x<- melt(Paid)
names(x) <- c("Paid","Stay","value")

ggplot(x, aes(Paid, Stay)) +
  geom_point(aes(size = value), alpha=0.8, color="darkgreen", show_guide=FALSE) +
  geom_text(aes(label = value), color="white") +
  scale_size(range = c(15,50)) +
  theme_bw()+xlim(-0.5,1.5)+ylim(-0.5,1.5)



