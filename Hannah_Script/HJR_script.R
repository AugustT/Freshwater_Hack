CollData<-read.csv(file.choose())
EngDataTime<-read.csv(file.choose())
EngDataSum<-read.csv(file.choose())
summary(EngDataSum)
summary(EngDataTime)
summary(CollData)

#################################

model1<-glm(Sample_bi~Paid+TranTrai+Upload+Attendees, family="binomial", data=EngDataSum)
summary(model1)

model2<-glm(Super_user~PCA1_Difficulty+Upload+Team+People,family="binomial",data=EngDataSum)
summary(model2)


############################################################################################

#playing around with visualising models#
library(visreg)

visreg(model1,xvar="Attendees")
visreg(model1,xvar="Paid")
visreg(model1,xvar="TranTrai")
visreg(model1,xvar="Upload")


x<-table(EngDataSum$Sample_bi,EngDataSum$Paid)
mosaicplot(x,data=EngDataSum,color=2:4,legend=TRUE,las = 1)


x<-read.csv(file.choose())


library(reshape2)
x <- melt(x)
names(x) <- c("Paid","Stay","value")

library(ggplot2)
ggplot(x, aes(Paid, Stay)) +
  geom_point(aes(size = value), alpha=0.8, color="darkgreen", show_guide=FALSE) +
  geom_text(aes(label = value), color="white") +
  scale_size(range = c(15,50)) +
  theme_bw()+xlim(-0.5,1.5)+ylim(-0.5,1.5)
