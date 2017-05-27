CollData<-read.csv(file.choose())
EngDataTime<-read.csv(file.choose())
EngDataSum<-read.csv('Hannah_Script/HACK_bi_without_staff.csv')
summary(EngDataSum)
summary(EngDataTime)
summary(CollData)

#################################

model1<-glm(Sample_bi~Online+Paid+TranTrai+Upload+Attendees, family="binomial", data=EngDataSum)
summary(model1)

library(boot)
#boot::inv.logit() #::saves calling library

library(lme4)
#predict.plot(model1)

#plot(EngDataSum$Sample_bi~EngDataSum$Online)

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

plotTable <- function(table, variable){
  
  puploaded <- table
  puploaded[1,] <- puploaded[1,]/sum(puploaded[1,])
  puploaded[2,] <- puploaded[2,]/sum(puploaded[2,])
  
  muploaded <- melt(puploaded)
  colnames(muploaded)[3] <- 'Proportion_of_users'
  colnames(muploaded)[1] <- 'variable'
  colnames(muploaded)[2] <- 'Stay'
  muploaded$Stay[muploaded$Stay == 1] <- 'Stay'
  muploaded$Stay[muploaded$Stay == 0] <- 'Leave'
  
  gg<-ggplot(aes(x = as.factor(variable),
             y = Proportion_of_users,
             fill = as.factor(variable)),
         data = muploaded[muploaded$Stay == 'Stay',]) + 
    geom_bar(stat = 'identity',
             position=position_dodge()) +
    ggtitle(paste('Effect of', variable)) +
    xlab(variable) + 
    ylab('Proportion of users') +
    guides(fill=FALSE)

  plot(gg)
  
}

# plot and save the images
png(filename = 'Hannah_Script/figures/Paid.png', width = 4, height = 5, units = 'in', res = 300)
  plotTable(Paid, variable = 'Paid')
dev.off()

png(filename = 'Hannah_Script/figures/Online.png', width = 4, height = 6, units = 'in', res = 300)
  plotTable(Online, variable = 'Online training only')
dev.off()

png(filename = 'Hannah_Script/figures/TraningTransport.png', width = 4, height = 6, units = 'in', res = 300)
  plotTable(TranTrai, variable = 'Transport arranged to training')
dev.off()

png(filename = 'Hannah_Script/figures/Upload.png', width = 4, height = 6, units = 'in', res = 300)
  plotTable(Upload, variable = 'Uploaded data during training')
dev.off()

png(filename = 'Hannah_Script/figures/Attendees.png', width = 6, height = 4, units = 'in', res = 300)
  visreg(model1,
       xvar="Attendees", main = 'Effect of increasing number of people at training event')
dev.off()

Attendees<-table(EngDataSum$Sample_bi,EngDataSum$Attendees)

x<- melt(Paid)
names(x) <- c("Paid","Stay","value")

ggplot(x, aes(Paid, Stay)) +
  geom_point(aes(size = value), alpha=0.8, color="darkgreen", show_guide=FALSE) +
  geom_text(aes(label = value), color="white") +
  scale_size(range = c(15,50)) +
  theme_bw()+xlim(-0.5,1.5)+ylim(-0.5,1.5)



