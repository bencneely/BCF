#--------------------------------------------------------------
#Ben Neely
#09/19/2023
#Create ALKs to assign ages to unaged fish from all samples
#Creating ALK from aged fish captured during recap event
#Applying ALK to all fish captured during recap event
#This will be assigning an ALK age to individuals that were
#empirically aged due to data mismanagement
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

if("nnet" %in% rownames(installed.packages()) == FALSE) {install.packages("nnet")}
library(nnet)

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
        legend.position="none")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/age and growth")

## Read in aged and unaged fish with import
aged=import("ALK/aged.csv")%>%
  mutate(cmgrp=lencat(tl,10))%>%
  filter(cmgrp>=100)%>%
  select(impd,cmgrp,age)

unaged=import("ALK/unaged.csv")%>%
  expandCounts(~count)%>%
  mutate(cmgrp=lencat(tl,10),
         age=as.numeric(NA))%>%
  filter(cmgrp>=100)%>%
  select(impd,cmgrp,age)

## Set seed for reproducible results
set.seed(919)

##############################################################
##############################################################
## Milford
## Create age-length key from aged fish data using multinomial logistic regression model
milr_mlr=multinom(age~cmgrp,data=subset(aged,impd=="MILR"))

## Predict probability that a fish in a given cm group is a certain age
lens=seq(100,1200,10)
milr_alk=predict(milr_mlr,data.frame(cmgrp=lens),type="probs")
row.names(milr_alk)=lens

## Apply ALK to unaged data set and combine with aged fish
milr_out=alkIndivAge(milr_alk,age~cmgrp,data=subset(unaged,impd=="MILR"))%>%
  select(impd,age,cmgrp)

##############################################################
##############################################################
## El Dorado
## Create age-length key from aged fish data using multinomial logistic regression model
eldr_mlr=multinom(age~cmgrp,data=subset(aged,impd=="ELDR"))

## Predict probability that a fish in a given cm group is a certain age
lens=seq(100,1200,10)
eldr_alk=predict(eldr_mlr,data.frame(cmgrp=lens),type="probs")
row.names(eldr_alk)=lens

## Apply ALK to unaged data set and combine with aged fish
eldr_out=alkIndivAge(eldr_alk,age~cmgrp,data=subset(unaged,impd=="ELDR"))%>%
  select(impd,age,cmgrp)

##############################################################
##############################################################
## Tuttle Creek
## Create age-length key from aged fish data using multinomial logistic regression model
tcrr_mlr=multinom(age~cmgrp,data=subset(aged,impd=="TCRR"))

## Predict probability that a fish in a given cm group is a certain age
lens=seq(100,1200,10)
tcrr_alk=predict(tcrr_mlr,data.frame(cmgrp=lens),type="probs")
row.names(tcrr_alk)=lens

## Apply ALK to unaged data set and combine with aged fish
tcrr_out=alkIndivAge(tcrr_alk,age~cmgrp,data=subset(unaged,impd=="TCRR"))%>%
  select(impd,age,cmgrp)

##############################################################
##############################################################
## Wolf Creek
## Create age-length key from aged fish data using multinomial logistic regression model
wlfc_mlr=multinom(age~cmgrp,data=subset(aged,impd=="WLFC"))

## Predict probability that a fish in a given cm group is a certain age
lens=seq(100,1200,10)
wlfc_alk=predict(wlfc_mlr,data.frame(cmgrp=lens),type="probs")
row.names(wlfc_alk)=lens

## Apply ALK to unaged data set and combine with aged fish
wlfc_out=alkIndivAge(wlfc_alk,age~cmgrp,data=subset(unaged,impd=="WLFC"))%>%
  select(impd,age,cmgrp)

##############################################################
##############################################################
## Combine
allaged_out=bind_rows(milr_out,eldr_out,tcrr_out,wlfc_out)%>%
  mutate(valid=case_when(impd=="ELDR" & cmgrp>800 ~ 0,
                         TRUE~1))
export(allaged_out,"allaged_out.csv")
