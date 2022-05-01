







setwd("C:/Users/Mikaela/Documents/NonMastersStuff")



library(tidyverse)
library(ggplot2)
library("ggpubr")

### PULL DATA FROM EXCEL .CSV FILE ###

LE<-read.csv("PTMC 2022 Leachate.csv", header = TRUE)

EM <- read.csv("PTMC Emergence Sheet.csv", header = TRUE)

ALL<-read.csv("PTMC 2022 Data.csv", header = TRUE)

### FIXING SOME STUFF ###

# THIS MAKES PLOT TITLES CENTER #
PT1 = theme(plot.title = element_text(hjust = 0.5))

# THE DATA SHEET WAS MESSED UP & THIS WAS EASIEST FIX #
Algae<-ALL[ALL$Inoculum =="Algae",]
Biocrust<-ALL[ALL$Inoculum =="Biocrust",]
None<-ALL[ALL$Inoculum =="None",]

ALL0<-rbind(Algae,Biocrust,None)

# FUNCTION TO FIX THE ORDER OF INOCULUM ON GRAPHS #
MicOrd<- function(x){factor(x$Inoculum,levels=c("None","Algae","Biocrust"))}

ALL0$H5<-as.numeric(ALL0$H5)
ALL0$L5<-as.numeric(ALL0$L5)

ALL0$Inoculum<-MicOrd(ALL0)

EM$P.Days<-as.numeric(EM$P.Days)
EM$E.Days<-as.numeric(EM$E.Days)

EMe<-EM %>% filter(!is.na(EM$E.Days))
EMp<-EM %>% filter(!is.na(EM$P.Days))

### PH PLOTS ###

PH1<-ggplot(ALL0, aes(x=Inoculum, y=pH1,col=Inoculum)) +scale_y_continuous(limits = c(4.5,9.5))+ 
  geom_boxplot() + geom_point(size=3,alpha=.9) +scale_color_manual(values=c("#547980","#C5E90B","#45ADA8"))+stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("pH Week 1")+ ylab("pH")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

PH2<-ggplot(ALL0, aes(x=Inoculum, y=pH2,col=Inoculum)) +scale_y_continuous(limits = c(4.5,9.5))+ 
  geom_boxplot() + geom_point(size=3,alpha=.9) +scale_color_manual(values=c("#547980","#C5E90B","#45ADA8"))+stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("pH Week 6")+ ylab("pH")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

PH3<-ggplot(ALL0, aes(x=Inoculum, y=pH.3,col=Inoculum)) +scale_y_continuous(limits = c(4.5,9.5))+ 
  geom_boxplot() + geom_point(size=3,alpha=.9) +scale_color_manual(values=c("#547980","#C5E90B","#45ADA8"))+stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("pH Week 10")+ ylab("pH")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

ggarrange(PH1,PH2,PH3, ncol=1, nrow=3)
dev.copy(png,"PTMCphALL.png",width=30,height=20,units="cm",res=300)
dev.off()

# ALTERNITIVE BLUE/GREENS #5dd39e, #348AA7 #39F1077 #

PH1b<-ggplot(ALL0, aes(x=Ammendment, y=pH1,col=Ammendment)) + scale_y_continuous(limits = c(4.5,9.5))+
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("pH Week 1")+ ylab("pH")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

PH2b<-ggplot(ALL0, aes(x=Ammendment, y=pH2,col=Ammendment)) + scale_y_continuous(limits = c(4.5,9.5))+
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("pH Week 6")+ ylab("pH")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

PH3b<-ggplot(ALL0, aes(x=Ammendment, y=pH.3,col=Ammendment)) + scale_y_continuous(limits = c(4.5,9.5))+
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("pH Week 10")+ ylab("pH")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

ggarrange(PH1b,PH2b,PH3b, ncol=1, nrow=3)
dev.copy(png,"PTMCph.bALL.png",width=30,height=20,units="cm",res=300)
dev.off()

library(tidyverse)
library(readxl)
library(readr)
library(rlist)

QYpp<-PosPos %>% dplyr::select(Site.Group,Site,Type,Rep,SlopeM)
ALL0$pH1
P1<-ALL0 %>% dplyr::select(Treatment, pH1)
P2<-ALL0 %>% dplyr::select(Treatment, pH2)
P3<-ALL0 %>% dplyr::select(Treatment, pH.3)
colnames(P1) = c("Treatment","pH")
colnames(P2) = c("Treatment","pH")
colnames(P3) = c("Treatment","pH")
P1$Week<-rep("One",length(P1$pH))
P2$Week<-rep("Two",length(P2$pH))
P3$Week<-rep("Three",length(P3$pH))

PHall<-rbind(P1,P2,P3)

an <- aov(ALL0$Bean.Weight ~ ALL0$Treatment)
summary(an)
TukeyHSD(an)

an <- aov(ALL0$Dry.Wt.Prop ~ ALL0$Treatment)

summary(an)

TukeyHSD(an)

par(mfrow=c(1,2))
qqnorm(an$residuals)
qqline(an$residuals)
plot(an$fitted.values, an$residuals,main="Residuals vs Fitted",xlab="Fitted values",ylab="Residuals")


an <- aov(ALL0$pH1 ~ ALL0$Treatment)
summary(an)

TukeyHSD(an)

par(mfrow=c(1,2))
qqnorm(an$residuals)
qqline(an$residuals)
plot(an$fitted.values, an$residuals,main="Residuals vs Fitted",xlab="Fitted values",ylab="Residuals")


an <- aov(ALL0$H5 ~ ALL0$Treatment)
summary(an)
TukeyHSD(an)

View(CountIN)
par(mfrow=c(1,2))
qqnorm(an$residuals)
qqline(an$residuals)
plot(an$fitted.values, an$residuals,main="Residuals vs Fitted",xlab="Fitted values",ylab="Residuals")

CountIN<-ALL0 %>% group_by(Treatment,H5) %>% tally()

### EMERGENCE TALLIES ###

CountIN<-EM %>% group_by(Treatment,Em.Count) %>% tally()

EXo<-CountIN[CountIN$n == "15",]
EMo<-CountIN[CountIN$Em.Count == "1",]

EMooo<-rbind(EMo,EXo)
EMooo = EMooo [,-2]
EMooo<-na.omit(EMooo) 

Ammendment<-cbind(c("Ctrl","R","R","RC","RC","RC","R","RS","RV","RV","RVC","RVC","RVC","RV","RS","RS"))
Inoculum<-cbind(c("Ctrl","A","B","A","B","N","N","A","A","B","A","B","N","N","B","N"))

EMooo$Amendment<-Ammendment
EMooo$Inoculum<-Inoculum
EMooo$n<-as.numeric(EMooo$n)
EMooo$n[EMooo$n > 14] <- "0"
EMooo$n<-as.numeric(EMooo$n)
EMooo$Inoculum[EMooo$Inoculum == "A"] <- "Algae"
EMooo$Inoculum[EMooo$Inoculum == "B"] <- "Biocrust"
EMooo$Inoculum[EMooo$Inoculum == "N"] <- "None"
MicOrd2<- function(x){factor(x$Inoculum,levels=c("Ctrl","None","Algae","Biocrust"))}
EMooo$Inoculum<-MicOrd2(EMooo)


EMooo$Avg<-EMooo$n/15
EMooo$Avg[1]=0.66666667
EMooo$Per<-EMooo$Avg*100

ggplot(EMooo, aes(x=Treatment, y=n,col=Treatment)) +scale_y_continuous(limits = c(0,15))+
  geom_point(size=3,alpha=.9) +
  ggtitle("Emergence")+ ylab("Number of Emergent Plants")+ xlab("Treatment")+  theme_bw()+PT1

dev.copy(png,"PTMC2021EM1.png",width=20,height=15,units="cm",res=300)
dev.off()

EMMo <- EMooo[EMooo$Inoculum != "Ctrl",]

EMM0<-ggplot(EMooo, aes(x=Inoculum, y=Per,col=Inoculum)) +scale_y_continuous(limits = c(0,100))+
  geom_boxplot() + geom_point(size=3,alpha=.9) +scale_color_manual(values=c("#000000","#547980","#C5E90B","#45ADA8"))+stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Emergence")+ ylab("Percentage of Emergent Plants (%)")+ xlab("Inoculum")+  theme_bw()+PT1

EMMM0<-ggplot(EMooo, aes(x=Amendment, y=Per,col=Amendment)) +scale_y_continuous(limits = c(0,100))+
  geom_boxplot() + geom_point(size=3,alpha=.9) +scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969"))+stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Emergence")+ ylab("Percentage of Emergent Plants (%)")+ xlab("Amendment")+  theme_bw()+PT1

EMM0<-ggplot(EMMo, aes(x=Inoculum, y=n,col=Inoculum)) +scale_y_continuous(limits = c(0,15))+
  geom_boxplot() + geom_point(size=3,alpha=.9) +scale_color_manual(values=c("#000000","#547980","#C5E90B","#45ADA8"))+stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Emergence")+ ylab("Number of Emergent Plants")+ xlab("Inoculum")+  theme_bw()+PT1

EMMM0<-ggplot(EMMo, aes(x=Amendment, y=n,col=Amendment)) +scale_y_continuous(limits = c(0,15))+
  geom_boxplot() + geom_point(size=3,alpha=.9) +scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969"))+stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Emergence")+ ylab("Number of Emergent Plants")+ xlab("Amendment")+  theme_bw()+PT1


EMM<-ggplot(EMooo, aes(x=Inoculum, y=n,col=Inoculum)) +scale_y_continuous(limits = c(0,15))+
  geom_boxplot() + geom_point(size=3,alpha=.9) +scale_color_manual(values=c("#000000","#547980","#C5E90B","#45ADA8"))+stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Emergence")+ ylab("Number of Emergent Plants")+ xlab("Inoculum")+  theme_bw()+PT1

EMMM<-ggplot(EMooo, aes(x=Amendment, y=n,col=Amendment)) +scale_y_continuous(limits = c(0,15))+
  geom_boxplot() + geom_point(size=3,alpha=.9) +scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969"))+stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Emergence")+ ylab("Number of Emergent Plants")+ xlab("Amendment")+  theme_bw()+PT1

ggarrange(EMM0,EMMM0, ncol=2, nrow=1)
dev.copy(png,"PTMC2021EM2o.png",width=20,height=10,units="cm",res=300)
dev.off()

ggarrange(EMM,EMMM, ncol=2, nrow=1)
dev.copy(png,"PTMC2021EM2.png",width=20,height=10,units="cm",res=300)
dev.off()
 
# 4 x 9 in #

### LEAF BIOMASS COMPARISON ###
View(ALL0)
ALL0$lf.wt<-ALL0$Weight.lf-ALL0$Bean.Weight-ALL0$Weight.no.lf
ALL0$lf.p<-ALL0$lf.wt/ALL0$Weight.lf*100
ALL0$bean.p<-ALL0$Bean.Weight/ALL0$Weight.lf*100

ggplot(ALL0, aes(x=Ammendment, y=lf.wt,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Leaf Weight")+ ylab("Leaf Weight (g)")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

Lu<-ggplot(ALL0, aes(x=Inoculum, y=lf.wt,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Leaf Weight")+ ylab("Leaf Weight (g)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

Lu2<-ggplot(ALL0, aes(x=Inoculum, y=lf.p,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Percent Leaf Biomass")+ ylab("Percent Leaf Biomass (%)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

ggplot(ALL0, aes(x=Inoculum, y=bean.p,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Percent Bean Biomass")+ ylab("Percent Bean Biomass (%)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

ggarrange(Lu, Lu2, ncol=1, nrow=2)

### HEIGHT GRAPHS ###

H6<-ggplot(ALL0, aes(x=Ammendment, y=H5,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Height Week 6")+ ylab("Height (cm)")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

H6b<-ggplot(ALL0, aes(x=Inoculum, y=H5,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Height Week 6")+ ylab("Height (cm)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

ggarrange(H6,H6b, ncol=1, nrow=2)
dev.copy(png,"PTMC2021H6.png",width=40,height=20,units="cm",res=300)
dev.off()

H10<-ggplot(ALL0, aes(x=Ammendment, y=H10,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Height Week 10")+ ylab("Height (cm)")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

H10b<-ggplot(ALL0, aes(x=Inoculum, y=H10,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Height Week 10")+ ylab("Height (cm)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

ggarrange(H10,H10b, ncol=1, nrow=2)
dev.copy(png,"PTMC2021H10.png",width=30,height=20,units="cm",res=300)
dev.off()

ggarrange(Wlf6b,H10b,Ro2,BbB, ncol=1, nrow=4)
dev.copy(png,"PTMC2022biom.png",width=30,height=35,units="cm",res=300)
dev.off()

### WEIGHTS ###

III<-ALL0
names(III)[3]<-'Amendment'
III<- III[III$Pot != "CTRL4",]

View(III)

Ro1<-ggplot(III, aes(x=Amendment, y=Dry.Wt,col=Amendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Dry Root Biomass")+ ylab("Dry Root Biomass (g)")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

ggplot(III, aes(x=Inoculum, y=Dry.Wt,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Dry Root Biomass")+ ylab("Dry Root Biomass (g)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Amendment)

Ro2<-ggplot(ALL0, aes(x=Inoculum, y=Dry.Wt,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Dry Root Biomass")+ ylab("Dry Root Biomass (g)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

Ro3<-ggplot(ALL0, aes(x=Inoculum, y=Dry.Wt.Prop,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Dry Root Biomass")+ ylab("Dry Root Biomass (g)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

ggarrange(Ro2,Ro3, ncol=1, nrow=2)
dev.copy(png,"PTMCroot.png",width=30,height=20,units="cm",res=300)
dev.off()
View(III)
Wlf6<-ggplot(III, aes(x=Amendment, y=Weight.lf,col=Amendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Total Above Ground Biomass")+ ylab("Above Ground Biomass (g)")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

Wlf6b<-ggplot(III, aes(x=Inoculum, y=Weight.lf,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Total Above Ground Biomass")+ ylab("Above Ground Biomass (g)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Amendment)

ggarrange(Wlf6,Wlf6b, ncol=1, nrow=2)
dev.copy(png,"PTMCtotBiomass.png",width=30,height=20,units="cm",res=300)
dev.off()

Wnlf6<-ggplot(ALL0, aes(x=Ammendment, y=Weight.no.lf,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Stem Biomass")+ ylab("Biomass (g)")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

Wnlf6b<-ggplot(ALL0, aes(x=Inoculum, y=Weight.no.lf,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Stem Biomass")+ ylab("Biomass (g)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

ggarrange(Wnlf6,Wnlf6b, ncol=1, nrow=2)
dev.copy(png,"PTMCstemBiomass.png",width=30,height=20,units="cm",res=300)
dev.off()

### BEANS ###
ALL0$Bean.Count

Bb<-ggplot(ALL0, aes(x=Ammendment, y=Bean.Weight,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Bean Biomass")+ ylab("Biomass (g)")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

BbB<-ggplot(III, aes(x=Inoculum, y=Bean.Weight,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Bean Biomass")+ ylab("Biomass (g)")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Amendment)

# 3 X 10 IN #

ggarrange(Bb,BbB, ncol=1, nrow=2)
dev.copy(png,"PTMCbeanBiomass.png",width=30,height=20,units="cm",res=300)
dev.off()

Bc<-ggplot(ALL0, aes(x=Ammendment, y=Bean.Count,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Bean Count")+ ylab("Number of Beans")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

BcB<-ggplot(ALL0, aes(x=Inoculum, y=Bean.Count,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Bean Count")+ ylab("Number of Beans")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

ggarrange(Bc,BcB, ncol=1, nrow=2)
dev.copy(png,"PTMCbeanCount.png",width=30,height=20,units="cm",res=300)
dev.off()

### LEAF COUNT GRAPHS ###

ALL0$BeanPer<-100-(100*((ALL0$Weight.lf-ALL0$Bean.Weight)/ALL0$Weight.lf))

ggplot(ALL0, aes(x=Ammendment, y=BeanPer,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Leaf COunt Week 6")+ ylab("Leaf Number")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

ggplot(ALL0, aes(x=Inoculum, y=BeanPer,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Height Week 6")+ ylab("Leaf Number")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

L6<-ggplot(ALL0, aes(x=Ammendment, y=L5,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Leaf COunt Week 6")+ ylab("Leaf Number")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

L6b<-ggplot(ALL0, aes(x=Inoculum, y=L5,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Height Week 6")+ ylab("Leaf Number")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

ggarrange(L6,L6b, ncol=1, nrow=2)
dev.copy(png,"PTMC2021L6.png",width=30,height=20,units="cm",res=300)
dev.off()

### EMERGENCE GRAPHS ###

E<-ggplot(EMe, aes(x=Inoculum, y=E.Days,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Days to Emergence")+ ylab("Days")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

Eb<-ggplot(EMe, aes(x=Ammendment, y=E.Days,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Days to Emergence")+ ylab("Days")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

ggarrange(E,Eb, ncol=1, nrow=2)
dev.copy(png,"PTMC2021E.png",width=40,height=20,units="cm",res=300)
dev.off()

P<-ggplot(EMp, aes(x=Inoculum, y=E.Days,col=Inoculum)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#547980","#C5E90B","#45ADA8")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Days to Pre-Emergence")+ ylab("Days")+ xlab("Inoculum")+  theme_bw()+PT1+facet_grid(~Ammendment)

Pb<-ggplot(EMp, aes(x=Ammendment, y=E.Days,col=Ammendment)) +
  geom_boxplot() + geom_point(size=3,alpha=.9)+scale_color_manual(values=c("#000000","#FA9203","#E14F08","#B31A15","#6D0E4E","#462969")) +stat_summary(fun=mean, geom="point", shape=23, size=3)+
  ggtitle("Days to Pre-Emergence")+ ylab("Days")+ xlab("Ammendment")+  theme_bw()+PT1+facet_grid(~Inoculum)

ggarrange(P,Pb, ncol=1, nrow=2)
dev.copy(png,"PTMC2021P.png",width=40,height=20,units="cm",res=300)
dev.off()

### RHIZOBIA ###

Cou0<-ALL0 %>% group_by(Ammendment,Presence.Rhizobia) %>% tally()
Cou<-ALL0 %>% group_by(Ammendment,Inoculum,Presence.Rhizobia) %>% tally()
Cou2<-ALL0 %>% group_by(Treatment,Presence.Rhizobia) %>% tally()

ggplot(data=Cou,aes(x=Presence.Rhizobia,y=n,fill=Presence.Rhizobia)) + geom_bar(stat="identity") +
  scale_y_continuous(limits = c(0,15)) + xlab("Root Status") + ylab("Number of Pots") +
  scale_fill_manual(values=c("#000044","#660066","#FF9966","#FFCC99"))+facet_grid(~Ammendment) +theme_bw()
dev.copy(png,"PTMCrhi1.png",width=50,height=15,units="cm",res=300)
dev.off()

ggplot(data=Cou,aes(x=Presence.Rhizobia,y=n,fill=Presence.Rhizobia)) + geom_bar(stat="identity") +
  scale_y_continuous(limits = c(0,20)) + xlab("Root Status") + ylab("Count") +
  scale_fill_manual(values=c("#000044","#660066","#FF9966","#FFCC99"))+theme_bw()+ facet_grid(~Inoculum) 
dev.copy(png,"PTMCrhi2.png",width=30,height=15,units="cm",res=300)
dev.off()

ggplot(data=Cou2,aes(x=Presence.Rhizobia,y=n,fill=Presence.Rhizobia)) + geom_bar(stat="identity") +
  scale_y_continuous(limits = c(0,6)) + xlab("Root Status") + ylab("Count") +
  scale_fill_manual(values=c("#000044","#660066","#FF9966","#FFCC99"))+facet_grid(~Treatment) +theme_bw()
dev.copy(png,"PTMCrhi3.png",width=120,height=15,units="cm",res=300)
dev.off()

### ALGAL COVER ### 

Aou0<-ALL0 %>% group_by(Ammendment,Algal.Cover) %>% tally()
Aou<-ALL0 %>% group_by(Ammendment,Inoculum,Algal.Cover) %>% tally()
Aou2<-ALL0 %>% group_by(Treatment,Algal.Cover) %>% tally()

Aou$Algal.Cover[Aou$Algal.Cover == ""] <- "Missing"
Aou0$Algal.Cover[Aou0$Algal.Cover == ""] <- "Missing"
Aou2$Algal.Cover[Aou2$Algal.Cover == ""] <- "Missing"

ggplot(data=Aou2,aes(x=Algal.Cover,y=n,fill=Algal.Cover)) + geom_bar(stat="identity") +
  scale_y_continuous(limits = c(0,15)) + xlab("Algal Cover") + ylab("Number of Pots") +
  scale_fill_manual(values=c("#000044","#660066","#FF9966","#FFCC99"))+facet_grid(~Treatment) +theme_bw()

ggplot(data=Aou0,aes(x=Algal.Cover,y=n,fill=Algal.Cover)) + geom_bar(stat="identity") +
  scale_y_continuous(limits = c(0,15)) + xlab("Algal Cover") + ylab("Number of Pots") +
  scale_fill_manual(values=c("#000044","#660066","#FF9966","#FFCC99"))+facet_grid(~Ammendment) +theme_bw()

ggplot(data=Aou,aes(x=Algal.Cover,y=n,fill=Algal.Cover)) + geom_bar(stat="identity") +
  scale_y_continuous(limits = c(0,18)) + xlab("Algal Cover") + ylab("Number of Pots") +
  scale_fill_manual(values=c("#000044","#660066","#FF9966","#FFCC99"))+facet_grid(~Inoculum) +theme_bw()
dev.copy(png,"PTMCalcoI.png",width=30,height=10,units="cm",res=300)
dev.off()

### LEACHATE DATA: UNFINISHED ###
View(LE0)
LE0 = LE[,-3]
LE0 = LE0[,-4]
LE0 = LE0[,-4]
LE0 = LE0[,-4]
LE0 = LE0[,-37]
LE0 = LE0[,-37]

LE0$Week<-as.character(LE0$Week)

LE1<-LE0[LE0$Week =="1",]
LE3<-LE0[LE0$Week =="3",]

LEgoods <-LE0[,5:36]
LElables <-LE0[,1:4]

LEgoods0 = LEgoods[,-18]
LEgoods0 = LEgoods0[,-18]
View(LEgoods)

LEE<-LEgoods0 %>% dplyr::select(pH,Chloride,NO3.N,Co,Al,Cr,Fe,Ni,Zn,Si,Mn,V,Be,As,Mo,Li,P,Pb )


X <-LEgoods0[,1:4]
XX <-LEgoods0[,5:30]

LEEx <- princomp(LEE,cor=TRUE,score=TRUE)
XXx <- princomp(XX,cor=TRUE,score=TRUE)

L.pca1 <- princomp(LEgoods0,cor=TRUE,score=TRUE)
summary(L.pca1)

L.pcsCH<-cbind(LEgoods,L.pca1$scores[,1:2])
cor(LEgoods,L.pca1$scores[,1:2])

par(mfrow=c(1,1))
plot(L.pca1,main="Eigenvalues for PCA on ENV",col="bisque",las=2)
abline(h=mean(L.pca1),col="blue")
legend("topright","Average Eigenvale",lwd=1,col=2,bty="n")

### THIS IS THE GOOD PLOT ###
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

P1<-ggbiplot::ggbiplot(L.pca1,obs.scale=1,var.scale=1,elipse=TRUE,groups=LElables$Innoculum,center=TRUE,ellipse = TRUE, labels = LElables$Amendment,alpha=1) + ggtitle("PCA Plot")+ theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#62D26F","#208EA3","#000000")) + PT1 

ggbiplot::ggbiplot(Xx,obs.scale=1,var.scale=1,elipse=TRUE,groups=LElables$Innoculum,center=TRUE,ellipse = TRUE, labels = LElables$Amendment,circle = TRUE,alpha=1) + ggtitle("PCA Plot")+ theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#62D26F","#208EA3","#000000")) + PT1 
ggbiplot::ggbiplot(XXx,obs.scale=1,var.scale=1,elipse=TRUE,groups=LElables$Innoculum,center=TRUE,ellipse = TRUE, labels = LElables$Amendment,circle = TRUE,alpha=1) + ggtitle("PCA Plot")+ theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#62D26F","#208EA3","#000000")) + PT1 

# 10 x 10 in #

# ggbiplot::ggbiplot(L.pca1,obs.scale=1,var.scale=1,elipse=TRUE,groups=LElables$Innoculum,center=TRUE,ellipse = TRUE, circle = TRUE,alpha=.8)+geom_point(aes(shape = factor(LElables$Amendment))) + ggtitle("PCA Plot")+ theme_bw() + theme(legend.position="bottom") + PT1 

P2<-ggbiplot::ggbiplot(L.pca1,obs.scale=1,var.scale=1,elipse=TRUE,groups=LElables$Amendment,center=TRUE,ellipse = TRUE, labels = LElables$Innoculum,alpha=.8) + ggtitle("PCA Plot")+ theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#A7226E","#62D26F","#F26B38","#F9D423","#6F63BB","#363636","#17BECF","#355C7D","#AA4499")) +PT1 

P3<-ggbiplot::ggbiplot(L.pca1,obs.scale=1,var.scale=1,elipse=TRUE,groups=LElables$Week,center=TRUE,ellipse = TRUE, labels = LElables$Treatment,alpha=.8) + ggtitle("PCA Plot")+ theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#208EA3","#62D26F","#F9D423","#6F63BB")) + PT1 
ggbiplot::ggbiplot(LEEx,obs.scale=1,var.scale=1,elipse=TRUE,groups=LElables$Week,center=TRUE,ellipse = TRUE, labels = LElables$Treatment,alpha=.8) + ggtitle("PCA Plot")+ theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#208EA3","#62D26F","#F9D423","#6F63BB")) + PT1 

ggarrange(P1,P2,P3, ncol=3, nrow=1)

dev.copy(png,"PTMCpcaW.png",width=20,height=20,units="cm",res=300)
dev.off()


# ggbiplot::ggbiplot(L.pca1,obs.scale=1,var.scale=1,elipse=TRUE,groups=LElables$Innoculum,center=TRUE,ellipse = TRUE, labels = LElables$Amendment,circle = TRUE,alpha=.8) + ggtitle("PCA Plot")+ theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1 +  expand_limits(y = c(-5, 4))+  expand_limits(x = c(-6, 8))


LE1<-LE0[LE0$Week =="1",]
LE3<-LE0[LE0$Week =="3",]

LEgoods1 <-LE1[,5:36]
LElables1 <-LE1[,1:4]
LEgoods3 <-LE3[,5:36]
LElables3 <-LE3[,1:4]

LEgoods01 = LEgoods1[,-18]
LEgoods01 = LEgoods01[,-18]
LEgoods03 = LEgoods3[,-18]
LEgoods03 = LEgoods03[,-18]

L.pca01 <- princomp(LEgoods01,cor=TRUE,score=TRUE)
summary(L.pca01)
L.pca03 <- princomp(LEgoods03,cor=TRUE,score=TRUE)
summary(L.pca03)

L.pcsCH1<-cbind(LEgoods01,L.pca01$scores[,1:2])
cor(LEgoods01,L.pca01$scores[,1:2])

par(mfrow=c(1,1))
plot(L.pca01,main="Eigenvalues for PCA on ENV",col="bisque",las=2)
abline(h=mean(L.pca01),col="blue")
legend("topright","Average Eigenvale",lwd=1,col=2,bty="n")



