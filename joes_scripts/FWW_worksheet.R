rm(list=ls())
library(reshape);library(vegan); library(plyr);library(ggplot2);library(lme4);library(nlme);
# library(lsmeans);library(doBy);
# library(lmerTest)

 dat <- read.csv("FWW_EngagementDAta_FinalSum.csv")

par(mfrow=c(2,1))
hist(dat$Period, breaks = 30)
str(dat)
## remove staff & 0 samples

NStaff <- subset(dat, !Staff == 1 & !NoSample == 0)

# hist(NStaff$Period, breaks = 30)
# ### remove users who have submitted less than 5 samples
# ### of those who submitted more than 5 no difference in number between paid and not paid
# NStaff2 <- subset(NStaff, NoSample >= 5) #splits into more than half again
# hist(NStaff2$NoSample, breaks = 30)
# count(NStaff2$Paid) ### no difference in paid vs not paid 76 and 77
#
#
# #### removing NAs
# NStaff3 <- NStaff2[!is.na(NStaff2$Paid),] #(only 153 left)
# count(NStaff3$Paid)
# hist(NStaff3$NoSample, breaks = 30)
# summary(NStaff3$NoSample)
#
# NStaff3 <- subset(NStaff3, NoSample < 600) #(only 153 left)
# #NStaff4 <- subset(NStaff3, NoSample > 20)
# count(NStaff3$Paid)
# hist(NStaff3$NoSample, breaks = 30)
# count(NStaff3$NoSample) # superuser has done 637
# summary(NStaff3$NoSample)
# #### you would expect those who are engaged longer to submit more, not entirely true
# Paid<- subset(NStaff3, Paid ==1)
# NotPaid<- subset(NStaff3, Paid ==0)


############################################################################################### LOOK AT THE NA PAID INCLUDED #################
#########################################################
########## our super users, 24
# NStaff4 <- subset(NStaff2, NoSample >= 20)
# hist(NStaff4$NoSample, breaks = 30)
# ggplot(NStaff4, aes(x=Period, y=NoSample, color=Paid)) + geom_point(shape=1) # Use a slightly darker palette than normal
# count(NStaff4$NoSample)
# nrow(NStaff4)

# NStaff4 <- subset(NStaff2, Period >= 200)
# NStaff4 <- subset(NStaff, NoSample >= 2)
top10 <- NStaff[order(NStaff$NoSample, decreasing = TRUE),][1:floor(nrow(NStaff)/10),]
top<- sum(top10$NoSample)#11313
all<-sum(NStaff$NoSample)#18056

top/all ## top 10% make up 62.7% of samples

names(top10)
str(top10$uid)

SU <- as.character(top10$uid)
write.csv(SU, file = "SuperUsers.csv")

date <- read.csv('dates.csv')

SS <- date[date$uid %in% SU[[1]], ]

# names(SS)

SS1 <- ddply(SS, .(uid, Year, Sample), summarise, no = sum(Count))
names(SS1)

# pd <- position_dodge(width=0.3)
# ggplot(SS1, aes(x= Year, y= no, colour= uid, group= uid))+
#   geom_line(position= pd)
#
# +
#   geom_point(position= pd, size=2.5)+
#   xlab("Survey Date")+
#   ylab(bquote('Log Abundance'~m^{2}))+
#   scale_color_manual(values=c("#9900CC", "#CC0000"))+
#   geom_vline(xintercept = 2.5, colour="red", linetype= "longdash")

# get the actual active period
str(date)
#convert date to date
date$Sample <- as.Date(as.character(date$Sample),format = '%d/%m/%Y')

periods <- lapply(unique(date$uid), FUN = function(x){

  temp <- date[date$uid == x,]
  period <- as.numeric(max(temp$Sample) - min(temp$Sample))
  data.frame(uid = x,
             period = period)

})

periods <- do.call(rbind, periods)

write.csv(periods, 'SamplingActivePeriod.csv')
