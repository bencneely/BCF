#---------------------------------------------------------------------------------------
#Ben Neely
#09/13/2023
#Fit Gompertz growth models to BCF populations
#---------------------------------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Set seed for reproducability
set.seed(913)

## http://derekogle.com/IFAR/supplements/growth/OtherGrowthFuns.html

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

if("nlstools" %in% rownames(installed.packages()) == FALSE) {install.packages("nlstools")}
library(nlstools)

if("car" %in% rownames(installed.packages()) == FALSE) {install.packages("car")}
library(car)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/age and growth/")

## Read in data with import
dat=import("allaged_out.csv")%>%
  filter(valid==1)

## Set up plotting theme
ADAStheme=theme_classic()+
  theme(panel.border=element_rect(color="black",fill=NA),
        axis.title=element_text(size=22,color="black",face="bold"),
        axis.text=element_text(size=18,color="black"),
        axis.text.x=element_text(angle=0,vjust=0.5),
        legend.position="none",
        plot.margin=margin(0,0,0,0,"cm"))

## Set up functions to estimate length up to max observed age
milr_ages=seq(0,max(subset(dat,impd=="MILR")$age),by=0.05)
milr_vbpred=function(x) predict(x,data.frame(age=milr_ages))

eldr_ages=seq(0,max(subset(dat,impd=="ELDR")$age),by=0.05)
eldr_vbpred=function(x) predict(x,data.frame(age=eldr_ages))

tcrr_ages=seq(0,max(subset(dat,impd=="TCRR")$age),by=0.05)
tcrr_vbpred=function(x) predict(x,data.frame(age=tcrr_ages))

wlfc_ages=seq(0,max(subset(dat,impd=="WLFC")$age),by=0.05)
wlfc_vbpred=function(x) predict(x,data.frame(age=wlfc_ages))

## Define growth model function and set starting values
vb=vbFuns()
vb_sv=list(k=0.3,t0=-1)

## Find Linf for each population
milr_linf=max(subset(dat,impd=="MILR")$cmgrp)
eldr_linf=max(subset(dat,impd=="ELDR")$cmgrp)
tcrr_linf=max(subset(dat,impd=="TCRR")$cmgrp)
wlfc_linf=max(subset(dat,impd=="WLFC")$cmgrp)

############################################################################################
############################################################################################
## MILR von Bertalanffy growth model with fixed Linf
## Fit the model and calculate confidence intervals
milr_vb_fit=nls(cmgrp~vb(age,milr_linf,k,t0),data=subset(dat,impd=="MILR"),start=vb_sv)
milr_vb_boot=nlsBoot(milr_vb_fit,niter=400)
(milr_vb_parms=cbind(ests=coef(milr_vb_fit),confint(milr_vb_boot))%>%
    rbind(Linf=milr_linf))

## Use model to predict length at age for plotting
milr_predboot=Boot(milr_vb_fit,f=milr_vbpred)
milr_preds=data.frame(milr_ages,
                      milr_vbpred(milr_vb_fit),
                      confint(milr_predboot))
names(milr_preds)=c("age","tl","lci95","uci95")

## Equation for plot label
milr_lab=paste0("TL==",1170,"~group('(',1-e^{-",0.062,"~(age","+",1.521,")},')')")
nrow(subset(dat,impd=="MILR"))

############################################################################################
############################################################################################
## ELDR von Bertalanffy growth model with fixed Linf
## Fit the model and calculate confidence intervals
eldr_vb_fit=nls(cmgrp~vb(age,eldr_linf,k,t0),data=subset(dat,impd=="ELDR"),start=vb_sv)
eldr_vb_boot=nlsBoot(eldr_vb_fit,niter=400)
(eldr_vb_parms=cbind(ests=coef(eldr_vb_fit),confint(eldr_vb_boot))%>%
    rbind(Linf=eldr_linf))

## Use model to predict length at age for plotting
eldr_predboot=Boot(eldr_vb_fit,f=eldr_vbpred)
eldr_preds=data.frame(eldr_ages,
                      eldr_vbpred(eldr_vb_fit),
                      confint(eldr_predboot))
names(eldr_preds)=c("age","tl","lci95","uci95")

## Equation for plot label
eldr_lab=paste0("TL==",720,"~group('(',1-e^{-",0.119,"~(age","+",0.955,")},')')")
nrow(subset(dat,impd=="ELDR"))

############################################################################################
############################################################################################
## TCRR von Bertalanffy growth model with fixed Linf
## Fit the model and calculate confidence intervals
tcrr_vb_fit=nls(cmgrp~vb(age,tcrr_linf,k,t0),data=subset(dat,impd=="TCRR"),start=vb_sv)
tcrr_vb_boot=nlsBoot(tcrr_vb_fit,niter=400)
(tcrr_vb_parms=cbind(ests=coef(tcrr_vb_fit),confint(tcrr_vb_boot))%>%
    rbind(Linf=tcrr_linf))

## Use model to predict length at age for plotting
tcrr_predboot=Boot(tcrr_vb_fit,f=tcrr_vbpred)
tcrr_preds=data.frame(tcrr_ages,
                      tcrr_vbpred(tcrr_vb_fit),
                      confint(tcrr_predboot))
names(tcrr_preds)=c("age","tl","lci95","uci95")

## Equation for plot label
tcrr_lab=paste0("TL==",950,"~group('(',1-e^{-",0.245,"~(age","-",0.539,")},')')")
nrow(subset(dat,impd=="TCRR"))

############################################################################################
############################################################################################
## WLFC von Bertalanffy growth model with fixed Linf
## Fit the model and calculate confidence intervals
wlfc_vb_fit=nls(cmgrp~vb(age,wlfc_linf,k,t0),data=subset(dat,impd=="WLFC"),start=vb_sv)
wlfc_vb_boot=nlsBoot(wlfc_vb_fit,niter=400)
(wlfc_vb_parms=cbind(ests=coef(wlfc_vb_fit),confint(wlfc_vb_boot))%>%
    rbind(Linf=wlfc_linf))

## Use model to predict length at age for plotting
wlfc_predboot=Boot(wlfc_vb_fit,f=wlfc_vbpred)
wlfc_preds=data.frame(wlfc_ages,
                      wlfc_vbpred(wlfc_vb_fit),
                      confint(wlfc_predboot))
names(wlfc_preds)=c("age","tl","lci95","uci95")

## Equation for plot label
wlfc_lab=paste0("TL==",1070,"~group('(',1-e^{-",0.105,"~(age","+",0.981,")},')')")
nrow(subset(dat,impd=="WLFC"))

############################################################################################
############################################################################################
############################################################################################
############################################################################################
## Organize data to plot all fish on vB models
milr=filter(dat,impd=="MILR")
eldr=filter(dat,impd=="ELDR")
tcrr=filter(dat,impd=="TCRR")
wlfc=filter(dat,impd=="WLFC")

## Plot von Bertalanffy growth models
## MILR
milr_plot=ggplot()+
  geom_ribbon(milr_preds,mapping=aes(x=age,ymin=lci95,ymax=uci95),fill="gray70")+
  geom_jitter(milr,mapping=aes(x=age,y=cmgrp),
             size=1,alpha=0.25,
             width=0.5)+
  scale_x_continuous(limits=c(0,18.5),
                     breaks=seq(0,18,2),
                     labels=seq(0,18,2),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,1220),
                     breaks=seq(0,1200,100),
                     labels=seq(0,1200,100),
                     expand=c(0,0),
                     name="Total length (mm)")+
  annotate("text",x=0.2,y=1200,label="Milford",hjust=0,vjust=1,size=8)+
  annotate("text",x=0.2,y=1120,label="N = 2369",hjust=0,vjust=1,size=6)+
  annotate("text",x=18,y=100,label=milr_lab,hjust=1,vjust=1,size=6,parse=T)+
  ADAStheme

## ELDR
eldr_plot=ggplot()+
  geom_ribbon(eldr_preds,mapping=aes(x=age,ymin=lci95,ymax=uci95),fill="gray70")+
  geom_jitter(eldr,mapping=aes(x=age,y=cmgrp),
              size=1,alpha=0.25,
              width=0.5)+
  scale_x_continuous(limits=c(0,18.5),
                     breaks=seq(0,18,2),
                     labels=seq(0,18,2),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,1220),
                     breaks=seq(0,1200,100),
                     labels=seq(0,1200,100),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=0.2,y=1200,label="El Dorado",hjust=0,vjust=1,size=8)+
  annotate("text",x=0.2,y=1120,label="N = 1137",hjust=0,vjust=1,size=6)+
  annotate("text",x=18,y=100,label=eldr_lab,hjust=1,vjust=1,size=6,parse=T)+
  ADAStheme

## TCRR
tcrr_plot=ggplot()+
  geom_ribbon(tcrr_preds,mapping=aes(x=age,ymin=lci95,ymax=uci95),fill="gray70")+
  geom_jitter(tcrr,mapping=aes(x=age,y=cmgrp),
              size=1,alpha=0.25,
              width=0.5)+
  scale_x_continuous(limits=c(0,18.5),
                     breaks=seq(0,18,2),
                     labels=seq(0,18,2),
                     expand=c(0,0),
                     name="Age")+
  scale_y_continuous(limits=c(0,1220),
                     breaks=seq(0,1200,100),
                     labels=seq(0,1200,100),
                     expand=c(0,0),
                     name="Total length (mm)")+
  annotate("text",x=0.2,y=1200,label="Tuttle Creek",hjust=0,vjust=1,size=8)+
  annotate("text",x=0.2,y=1120,label="N = 750",hjust=0,vjust=1,size=6)+
  annotate("text",x=18,y=100,label=tcrr_lab,hjust=1,vjust=1,size=6,parse=T)+
  ADAStheme

## WLFC
wlfc_plot=ggplot()+
  geom_ribbon(wlfc_preds,mapping=aes(x=age,ymin=lci95,ymax=uci95),fill="gray70")+
  geom_jitter(wlfc,mapping=aes(x=age,y=cmgrp),
              size=1,alpha=0.25,
              width=0.5)+
  scale_x_continuous(limits=c(0,18.5),
                     breaks=seq(0,18,2),
                     labels=seq(0,18,2),
                     expand=c(0,0),
                     name="Age")+
  scale_y_continuous(limits=c(0,1220),
                     breaks=seq(0,1200,100),
                     labels=seq(0,1200,100),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=0.2,y=1200,label="Wolf Creek",hjust=0,vjust=1,size=8)+
  annotate("text",x=0.2,y=1120,label="N = 572",hjust=0,vjust=1,size=6)+
  annotate("text",x=18,y=100,label=wlfc_lab,hjust=1,vjust=1,size=6,parse=T)+
  ADAStheme

vb_out=(milr_plot|eldr_plot)/(tcrr_plot|wlfc_plot)
vb_out

ggsave("von Bertalanffy growth curves.png",plot=vb_out,height=10,width=12,units="in")


############################################################################################
############################################################################################

## MILR
## Fit vb from mean length at age
milr_mla=dat%>%
  filter(impd=="MILR")%>%
  group_by(age)%>%
  summarize(mn=mean(cmgrp),
            sd=sd(cmgrp))%>%
  ungroup()

## MILR von Bertalanffy growth model with fixed Linf
## Fit the model and calculate confidence intervals
milr_vb_fit1=nls(mn~vb(age,milr_linf,k,t0),data=milr_mla,start=vb_sv)
milr_vb_boot1=nlsBoot(milr_vb_fit1,niter=400)
(milr_vb_parms1=cbind(ests=coef(milr_vb_fit1),confint(milr_vb_boot1))%>%
    rbind(Linf=milr_linf))

## Use model to predict length at age for plotting
milr_predboot1=Boot(milr_vb_fit1,f=milr_vbpred)
milr_preds1=data.frame(milr_ages,
                      milr_vbpred(milr_vb_fit1),
                      confint(milr_predboot1))
names(milr_preds1)=c("age","tl","lci95","uci95")

## Plot von Bertalanffy growth model
milr_plot1=ggplot()+
  geom_ribbon(milr_preds1,mapping=aes(x=age,ymin=lci95,ymax=uci95),fill="gray70")+
  geom_pointrange(milr_mla,mapping=aes(x=age,y=mn,ymin=mn-sd,ymax=mn+sd))+
  scale_x_continuous(limits=c(0,18.5),
                     breaks=seq(0,18,2),
                     labels=seq(0,18,2),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,1220),
                     breaks=seq(0,1200,100),
                     labels=seq(0,1200,100),
                     expand=c(0,0),
                     name="Total length (mm)")+
  annotate("text",x=0.2,y=1200,label="Milford",hjust=0,vjust=1,size=8)+
  annotate("text",x=0.2,y=1120,label="N = 2369",hjust=0,vjust=1,size=6)+
  annotate("text",x=18,y=100,label=milr_lab,hjust=1,vjust=1,size=6,parse=T)+
  ADAStheme
