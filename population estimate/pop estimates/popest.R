#--------------------------------------------------------------
#Ben Neely
#10/18/2022
#Blue Catfish population estimates
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
        legend.position="none")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/pop est")

## Read in data for each lake
## These data contain M (number of tagged fish in each PSD group)
## n (number of fish encountered during recapture sample)
## m (number of tagged fish encountered during recapture sample)
milr=import("pop est data prep/popestdat.xlsx",which="milr")
eldr=import("pop est data prep/popestdat.xlsx",which="eldr")
tcrr=import("pop est data prep/popestdat.xlsx",which="tcrr")
wlfc=import("pop est data prep/popestdat.xlsx",which="wlfc")

################################################################################
################################################################################
## Population estimates
## Chapman modification of the Petersen estimator
## 95% confidence intervals calculated using Krebs 1989 suggestion
## https://rdrr.io/cran/FSA/man/mrClosed.html

################################################################################
## Milford
milr_st=mrClosed(M=milr[1,2],n=milr[1,3],m=milr[1,4],
                 method="Chapman")
milr_st_ci=confint(milr_st,conf.level=0.95,type="suggested",verbose=T)

milr_qu=mrClosed(M=milr[2,2],n=milr[2,3],m=milr[2,4],
                 method="Chapman")
milr_qu_ci=confint(milr_qu,conf.level=0.95,type="suggested",verbose=T)

milr_pr=mrClosed(M=milr[3,2],n=milr[3,3],m=milr[3,4],
                 method="Chapman")
milr_pr_ci=confint(milr_pr,conf.level=0.95,type="suggested",verbose=T)

milr_me=mrClosed(M=milr[4,2],n=milr[4,3],m=milr[4,4],
                 method="Chapman")
milr_me_ci=confint(milr_me,conf.level=0.95,type="suggested",verbose=T)

milr_tr=mrClosed(M=milr[5,2],n=milr[5,3],m=milr[5,4],
                 method="Chapman")
milr_tr_ci=confint(milr_tr,conf.level=0.95,type="suggested",verbose=T)

## Combine
milr_ests=bind_rows(c(impd="milr",psd="st",est=summary(milr_st),lci95=milr_st_ci[1],uci95=milr_st_ci[2]),
                    c(impd="milr",psd="qu",est=summary(milr_qu),lci95=milr_qu_ci[1],uci95=milr_qu_ci[2]),
                    c(impd="milr",psd="pr",est=summary(milr_pr),lci95=milr_pr_ci[1],uci95=milr_pr_ci[2]),
                    c(impd="milr",psd="me",est=summary(milr_me),lci95=milr_me_ci[1],uci95=milr_me_ci[2]),
                    c(impd="milr",psd="tr",est=summary(milr_tr),lci95=milr_tr_ci[1],uci95=milr_tr_ci[2]))%>%
  mutate(impd=factor(impd),
         psd=factor(psd,levels=c("st","qu","pr","me","tr")),
         est=as.numeric(est),lci95=as.numeric(lci95),uci95=as.numeric(uci95))%>%
  mutate(est_ac=est/16020,
         lci95_ac=lci95/16020,
         uci95_ac=uci95/16020)

################################################################################
## El Dorado
eldr_st=mrClosed(M=eldr[1,2],n=eldr[1,3],m=eldr[1,4],
                 method="Chapman")
eldr_st_ci=confint(eldr_st,conf.level=0.95,type="suggested",verbose=T)

eldr_qu=mrClosed(M=eldr[2,2],n=eldr[2,3],m=eldr[2,4],
                 method="Chapman")
eldr_qu_ci=confint(eldr_qu,conf.level=0.95,type="suggested",verbose=T)

eldr_pr=mrClosed(M=eldr[3,2],n=eldr[3,3],m=eldr[3,4],
                 method="Chapman")
eldr_pr_ci=confint(eldr_pr,conf.level=0.95,type="suggested",verbose=T)

## Combine
eldr_ests=bind_rows(c(impd="eldr",psd="st",est=summary(eldr_st),lci95=eldr_st_ci[1],uci95=eldr_st_ci[2]),
                    c(impd="eldr",psd="qu",est=summary(eldr_qu),lci95=eldr_qu_ci[1],uci95=eldr_qu_ci[2]),
                    c(impd="eldr",psd="pr",est=summary(eldr_pr),lci95=eldr_pr_ci[1],uci95=eldr_pr_ci[2]))%>%
  mutate(impd=factor(impd),
         psd=factor(psd,levels=c("st","qu","pr")),
         est=as.numeric(est),lci95=as.numeric(lci95),uci95=as.numeric(uci95))%>%
  mutate(est_ac=est/8000,
         lci95_ac=lci95/8000,
         uci95_ac=uci95/8000)
eldr_ests

################################################################################
## Tuttle Creek
tcrr_st=mrClosed(M=tcrr[1,2],n=tcrr[1,3],m=tcrr[1,4],
                 method="Chapman")
tcrr_st_ci=confint(tcrr_st,conf.level=0.95,type="suggested",verbose=T)

tcrr_qu=mrClosed(M=tcrr[2,2],n=tcrr[2,3],m=tcrr[2,4],
                 method="Chapman")
tcrr_qu_ci=confint(tcrr_qu,conf.level=0.95,type="suggested",verbose=T)

tcrr_pr=mrClosed(M=tcrr[3,2],n=tcrr[3,3],m=tcrr[3,4],
                 method="Chapman")
tcrr_pr_ci=confint(tcrr_pr,conf.level=0.95,type="suggested",verbose=T)

tcrr_me=mrClosed(M=tcrr[4,2],n=tcrr[4,3],m=tcrr[4,4],
                 method="Chapman")
tcrr_me_ci=confint(tcrr_me,conf.level=0.95,type="suggested",verbose=T)

## Combine
tcrr_ests=bind_rows(c(impd="tcrr",psd="st",est=summary(tcrr_st),lci95=tcrr_st_ci[1],uci95=tcrr_st_ci[2]),
                    c(impd="tcrr",psd="qu",est=summary(tcrr_qu),lci95=tcrr_qu_ci[1],uci95=tcrr_qu_ci[2]),
                    c(impd="tcrr",psd="pr",est=summary(tcrr_pr),lci95=tcrr_pr_ci[1],uci95=tcrr_pr_ci[2]),
                    c(impd="tcrr",psd="me",est=summary(tcrr_me),lci95=tcrr_me_ci[1],uci95=tcrr_me_ci[2]))%>%
  mutate(impd=factor(impd),
         psd=factor(psd,levels=c("st","qu","pr","me")),
         est=as.numeric(est),lci95=as.numeric(lci95),uci95=as.numeric(uci95))%>%
  mutate(est_ac=est/15800,
         lci95_ac=lci95/15800,
         uci95_ac=uci95/15800)

################################################################################
## Wolf Creek
wlfc_st=mrClosed(M=wlfc[1,2],n=wlfc[1,3],m=wlfc[1,4],
                 method="Chapman")
wlfc_st_ci=confint(wlfc_st,conf.level=0.95,type="suggested",verbose=T)

wlfc_qu=mrClosed(M=wlfc[2,2],n=wlfc[2,3],m=wlfc[2,4],
                 method="Chapman")
wlfc_qu_ci=confint(wlfc_qu,conf.level=0.95,type="suggested",verbose=T)

wlfc_pr=mrClosed(M=wlfc[3,2],n=wlfc[3,3],m=wlfc[3,4],
                 method="Chapman")
wlfc_pr_ci=confint(wlfc_pr,conf.level=0.95,type="suggested",verbose=T)

wlfc_me=mrClosed(M=wlfc[4,2],n=wlfc[4,3],m=wlfc[4,4],
                 method="Chapman")
wlfc_me_ci=confint(wlfc_me,conf.level=0.95,type="suggested",verbose=T)

## Combine
wlfc_ests=bind_rows(c(impd="wlfc",psd="st",est=summary(wlfc_st),lci95=wlfc_st_ci[1],uci95=wlfc_st_ci[2]),
                    c(impd="wlfc",psd="qu",est=summary(wlfc_qu),lci95=wlfc_qu_ci[1],uci95=wlfc_qu_ci[2]),
                    c(impd="wlfc",psd="pr",est=summary(wlfc_pr),lci95=wlfc_pr_ci[1],uci95=wlfc_pr_ci[2]),
                    c(impd="wlfc",psd="me",est=summary(wlfc_me),lci95=wlfc_me_ci[1],uci95=wlfc_me_ci[2]))%>%
  mutate(impd=factor(impd),
         psd=factor(psd,levels=c("st","qu","pr","me")),
         est=as.numeric(est),lci95=as.numeric(lci95),uci95=as.numeric(uci95))%>%
  mutate(est_ac=est/5100,
         lci95_ac=lci95/5100,
         uci95_ac=uci95/5100)

################################################################################
################################################################################
## Combine population estimates for output
popest_out=bind_rows(milr_ests,eldr_ests,tcrr_ests,wlfc_ests)%>%
  complete(impd,psd,
           fill=list(est=0,lci95=0,uci95=0,est_ac=0,lci95_ac=0,uci95_ac=0))
export(popest_out,"popests.csv")
