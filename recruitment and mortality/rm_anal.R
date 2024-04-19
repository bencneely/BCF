#--------------------------------------------------------------
#Ben Neely
#11/14/2022
#Assess recruitment and mortality
#--------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
library(lubridate)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=22,color="black",face="bold"),
        axis.text=element_text(size=18,color="black"),
        legend.position=c(0,1),
        legend.justification=c("left","top"),
        legend.title=element_blank(),
        legend.text=element_text(size=22),
        legend.background=element_rect(fill="transparent")
        )
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/recruitment and mortality/")

## Read in aged and unaged fish with import
dat=import("allaged_out.csv")

################################################################################
################################################################################
## Recruitment

################################################################################
## Milford
## Get data frame with number of fish observed at each age plus one
milr=filter(dat,impd=="MILR")%>%
  group_by(impd,age)%>%
  summarize(ct=n()+1)%>%
  ungroup()%>%
  complete(age=min(age):max(age),
           fill=list(impd="MILR",ct=1))

## Fit weighted catch curve from age-2 to max age and extract values
milr_cc=catchCurve(ct~age,data=milr,ages2use=2:max(milr$age),weighted=T)
plot(milr_cc)
milr_cc_dat=bind_cols(impd="MILR",
                      age=milr_cc$age.e,
                      year=2018-milr_cc$age.e,
                      catch=milr_cc$catch.e,
                      log_catch=milr_cc$log.catch.e,
                      weights=milr_cc$weights.e,
                      ycs=rstudent(milr_cc$lm))

## Fit linear model to get R2 for RCD value
milr_ccmod=lm(log_catch~age,weight=weights,data=milr_cc_dat)

## Extract Z, A, and RCD
milr_z=sprintf("%0.2f",summary(milr_cc)[1])
milr_a=sprintf("%0.2f",summary(milr_cc)[2])
milr_rcd=sprintf("%0.2f",summary(milr_ccmod)$r.squared)

## Get 20th and 80th percentile year class strength from the t-distribution
milr_20=qt(0.2,df=length(milr_cc_dat$ycs)-1)
milr_80=qt(0.8,df=length(milr_cc_dat$ycs)-1)

## Create recruitment strength plot
## Consider all ycs values > abs(3) to be outliers
milr_cc_dat1=milr_cc_dat%>%
  mutate(ycs=case_when(abs(ycs)>=3 ~ NaN,
                       TRUE ~ ycs))

milr_rec=ggplot(milr_cc_dat)+
  geom_bar(aes(x=year,y=ycs),stat="identity")+
  geom_hline(mapping=aes(yintercept=0),linetype="solid",color="black")+
  geom_hline(mapping=aes(yintercept=milr_20),linetype="dashed",color="gray")+
  geom_hline(mapping=aes(yintercept=milr_80),linetype="dashed",color="gray")+
  scale_y_continuous(breaks=seq(-2,2,1),
                     name="Year class strength")+
  scale_x_continuous(limits=c(1999.5,2016.5),
                     seq(2000,2016,1),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=2000,y=2.5,label="Milford",hjust=0,vjust=1,size=8)+
  annotate("text",x=2000,y=2.0,label=paste("Z=",milr_z),hjust=0,vjust=1,size=5)+
  annotate("text",x=2000,y=1.7,label=paste("A=",milr_a),hjust=0,vjust=1,size=5)+
  annotate("text",x=2000,y=1.4,label=paste("RCD=",milr_rcd),hjust=0,vjust=1,size=5)+
  annotate("text",x=2013,y=-2.45,label="-5.37",angle=90,hjust=0,vjust=0.5,size=5,color="white")+
  coord_cartesian(ylim=c(-2.55,2.55),expand=F)+
  pubtheme+
  theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5))

################################################################################
## El Dorado
## Get data frame with number of fish observed at each age plus one
eldr=filter(dat,impd=="ELDR")%>%
  group_by(impd,age)%>%
  summarize(ct=n()+1)%>%
  ungroup()%>%
  complete(age=min(age):max(age),
           fill=list(impd="ELDR",ct=1))

## Fit weighted catch curve from age-2 to max age and extract values
eldr_cc=catchCurve(ct~age,data=eldr,ages2use=2:max(eldr$age),weighted=T)
plot(eldr_cc)
eldr_cc_dat=bind_cols(impd="ELDR",
                      age=eldr_cc$age.e,
                      year=2020-eldr_cc$age.e,
                      catch=eldr_cc$catch.e,
                      log_catch=eldr_cc$log.catch.e,
                      weights=eldr_cc$weights.e,
                      ycs=rstudent(eldr_cc$lm))

## Fit linear model to get R2 for RCD value
eldr_ccmod=lm(log_catch~age,weight=weights,data=eldr_cc_dat)

## Extract Z, A, and RCD
eldr_z=sprintf("%0.2f",summary(eldr_cc)[1])
eldr_a=sprintf("%0.2f",summary(eldr_cc)[2])
eldr_rcd=sprintf("%0.2f",summary(eldr_ccmod)$r.squared)

## Get 20th and 80th percentile year class strength from the t-distribution
eldr_20=qt(0.2,df=length(eldr_cc_dat$ycs)-1)
eldr_80=qt(0.8,df=length(eldr_cc_dat$ycs)-1)

## Create recruitment strength plot
## Consider all ycs values > abs(3) to be outliers
eldr_cc_dat1=eldr_cc_dat%>%
  mutate(ycs=case_when(abs(ycs)>=3 ~ NaN,
                       TRUE ~ ycs))

eldr_rec=ggplot(eldr_cc_dat)+
  geom_bar(aes(x=year,y=ycs),stat="identity")+
  geom_hline(mapping=aes(yintercept=0),linetype="solid",color="black")+
  geom_hline(mapping=aes(yintercept=eldr_20),linetype="dashed",color="gray")+
  geom_hline(mapping=aes(yintercept=eldr_80),linetype="dashed",color="gray")+
  scale_y_continuous(breaks=seq(-2,2,1),
                     name="")+
  scale_x_continuous(limits=c(2006.5,2018.5),
                     seq(2007,2018,1),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=2007,y=2.5,label="El Dorado",hjust=0,vjust=1,size=8)+
  annotate("text",x=2007,y=2.0,label=paste("Z=",eldr_z),hjust=0,vjust=1,size=5)+
  annotate("text",x=2007,y=1.7,label=paste("A=",eldr_a),hjust=0,vjust=1,size=5)+
  annotate("text",x=2007,y=1.4,label=paste("RCD=",eldr_rcd),hjust=0,vjust=1,size=5)+
  coord_cartesian(ylim=c(-2.55,2.55),expand=F)+
  pubtheme+
  theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5))

################################################################################
## Tuttle Creek
## Get data frame with number of fish observed at each age plus one
tcrr=filter(dat,impd=="TCRR")%>%
  group_by(impd,age)%>%
  summarize(ct=n()+1)%>%
  ungroup()%>%
  complete(age=min(age):max(age),
           fill=list(impd="TCRR",ct=1))

## Fit weighted catch curve from age-2 to max age and extract values
tcrr_cc=catchCurve(ct~age,data=tcrr,ages2use=2:max(tcrr$age),weighted=T)
plot(tcrr_cc)
tcrr_cc_dat=bind_cols(impd="TCRR",
                      age=tcrr_cc$age.e,
                      year=2021-tcrr_cc$age.e,
                      catch=tcrr_cc$catch.e,
                      log_catch=tcrr_cc$log.catch.e,
                      weights=tcrr_cc$weights.e,
                      ycs=rstudent(tcrr_cc$lm))

## Fit linear model to get R2 for RCD value
tcrr_ccmod=lm(log_catch~age,weight=weights,data=tcrr_cc_dat)

## Extract Z, A, and RCD
tcrr_z=sprintf("%0.2f",summary(tcrr_cc)[1])
tcrr_a=sprintf("%0.2f",summary(tcrr_cc)[2])
tcrr_rcd=sprintf("%0.2f",summary(tcrr_ccmod)$r.squared)

## Get 20th and 80th percentile year class strength from the t-distribution
tcrr_20=qt(0.2,df=length(tcrr_cc_dat$ycs)-1)
tcrr_80=qt(0.8,df=length(tcrr_cc_dat$ycs)-1)

## Create recruitment strength plot
## Consider all ycs values > abs(3) to be outliers
tcrr_cc_dat1=tcrr_cc_dat%>%
  mutate(ycs=case_when(abs(ycs)>=3 ~ NaN,
                       TRUE ~ ycs))

tcrr_rec=ggplot(tcrr_cc_dat)+
  geom_bar(aes(x=year,y=ycs),stat="identity")+
  geom_hline(mapping=aes(yintercept=0),linetype="solid",color="black")+
  geom_hline(mapping=aes(yintercept=tcrr_20),linetype="dashed",color="gray")+
  geom_hline(mapping=aes(yintercept=tcrr_80),linetype="dashed",color="gray")+
  scale_y_continuous(breaks=seq(-2,2,1),
                     name="Year class strength")+
  scale_x_continuous(limits=c(2010.5,2019.5),
                     seq(2011,2019,1),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=2011,y=2.5,label="Tuttle Creek",hjust=0,vjust=1,size=8)+
  annotate("text",x=2011,y=2.0,label=paste("Z=",tcrr_z),hjust=0,vjust=1,size=5)+
  annotate("text",x=2011,y=1.7,label=paste("A=",tcrr_a),hjust=0,vjust=1,size=5)+
  annotate("text",x=2011,y=1.4,label=paste("RCD=",tcrr_rcd),hjust=0,vjust=1,size=5)+
  coord_cartesian(ylim=c(-2.55,2.55),expand=F)+
  pubtheme+
  theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5))

################################################################################
## Wolf Creek
## Get data frame with number of fish observed at each age plus one
wlfc=filter(dat,impd=="WLFC")%>%
  group_by(impd,age)%>%
  summarize(ct=n()+1)%>%
  ungroup()%>%
  complete(age=min(age):max(age),
           fill=list(impd="WLFC",ct=1))

## Fit weighted catch curve from age-2 to max age and extract values
wlfc_cc=catchCurve(ct~age,data=wlfc,ages2use=2:max(wlfc$age),weighted=T)
plot(wlfc_cc)
wlfc_cc_dat=bind_cols(impd="WLFC",
                      age=wlfc_cc$age.e,
                      year=2022-wlfc_cc$age.e,
                      catch=wlfc_cc$catch.e,
                      log_catch=wlfc_cc$log.catch.e,
                      weights=wlfc_cc$weights.e,
                      ycs=rstudent(wlfc_cc$lm))

## Fit linear model to get R2 for RCD value
wlfc_ccmod=lm(log_catch~age,weight=weights,data=wlfc_cc_dat)

## Extract Z, A, and RCD
wlfc_z=sprintf("%0.2f",summary(wlfc_cc)[1])
wlfc_a=sprintf("%0.2f",summary(wlfc_cc)[2])
wlfc_rcd=sprintf("%0.2f",summary(wlfc_ccmod)$r.squared)

## Get 20th and 80th percentile year class strength from the t-distribution
wlfc_20=qt(0.2,df=length(wlfc_cc_dat$ycs)-1)
wlfc_80=qt(0.8,df=length(wlfc_cc_dat$ycs)-1)

## Create recruitment strength plot
## Consider all ycs values > abs(3) to be outliers
wlfc_cc_dat1=wlfc_cc_dat%>%
  mutate(ycs=case_when(abs(ycs)>=3 ~ NaN,
                       TRUE ~ ycs))

wlfc_rec=ggplot(wlfc_cc_dat)+
  geom_bar(aes(x=year,y=ycs),stat="identity")+
  geom_hline(mapping=aes(yintercept=0),linetype="solid",color="black")+
  geom_hline(mapping=aes(yintercept=wlfc_20),linetype="dashed",color="gray")+
  geom_hline(mapping=aes(yintercept=wlfc_80),linetype="dashed",color="gray")+
  scale_y_continuous(breaks=seq(-2,2,1),
                     name="")+
  scale_x_continuous(limits=c(2006.5,2020.5),
                     seq(2007,2020,1),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=2007,y=2.5,label="Wolf Creek",hjust=0,vjust=1,size=8)+
  annotate("text",x=2007,y=2.0,label=paste("Z=",wlfc_z),hjust=0,vjust=1,size=5)+
  annotate("text",x=2007,y=1.7,label=paste("A=",wlfc_a),hjust=0,vjust=1,size=5)+
  annotate("text",x=2007,y=1.4,label=paste("RCD=",wlfc_rcd),hjust=0,vjust=1,size=5)+
  coord_cartesian(ylim=c(-2.55,2.55),expand=F)+
  pubtheme+
  theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5))

rec_out=(milr_rec|eldr_rec)/(tcrr_rec|wlfc_rec)
ggsave(plot=rec_out,"rec_mor.png",height=8,width=12,units="in",bg="transparent")
