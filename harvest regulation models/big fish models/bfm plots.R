#--------------------------------------------------------------
#Ben Neely
#11/15/2022
#Plot YPR for different regulations
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
        axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5),
        legend.key.size=unit(3,"line"),
        legend.text=element_text(size=16),
        legend.position="top")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/pop models")

## Read in asm data with import
milr=import("asmdat.xlsx",which="milr")
eldr=import("asmdat.xlsx",which="eldr")
tcrr=import("asmdat.xlsx",which="tcrr")

##############################################################
##############################################################
## Graph YPR plots with three different regulations

##############################################################
## Milford
milr_ypr=ggplot(subset(milr,metric=="yield" & u>0))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("762-mm maximum","381-mm minimum","635-mm to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0.02,0.38,0.02),
                     name="")+
  scale_y_continuous(breaks=seq(0,1.1,0.1),
                     name="Yield per recruit (kg)")+
  coord_cartesian(xlim=c(0.01,0.39),
                  ylim=c(0,1.11),
                  expand=F)+
  annotate("text",x=0.02,y=1.1,label="Milford",hjust=0,vjust=1,size=9)+
  pubtheme
  
##############################################################
## El Dorado
eldr_ypr=ggplot(subset(eldr,metric=="yield" & u>0))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("762-mm maximum","381-mm minimum","635-mm to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0.02,0.38,0.02),
                     name="")+
  scale_y_continuous(breaks=seq(0,1.1,0.1),
                     name="Yield per recruit (kg)")+
  coord_cartesian(xlim=c(0.01,0.39),
                  ylim=c(0,1.11),
                  expand=F)+
  annotate("text",x=0.02,y=1.1,label="El Dorado",hjust=0,vjust=1,size=9)+
  pubtheme+
  theme(legend.position="none")

##############################################################
## Tuttle Creek
tcrr_ypr=ggplot(subset(tcrr,metric=="yield" & u>0))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("762-mm maximum","381-mm minimum","635-mm to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0.02,0.38,0.02),
                     name="Angling mortality rate")+
  scale_y_continuous(breaks=seq(0,1.1,0.1),
                     name="Yield per recruit (kg)")+
  coord_cartesian(xlim=c(0.01,0.39),
                  ylim=c(0,1.11),
                  expand=F)+
  annotate("text",x=0.02,y=1.1,label="Tuttle Creek",hjust=0,vjust=1,size=9)+
  pubtheme+
  theme(legend.position="none")

##############################################################
## Wolf Creek

##############################################################
##############################################################
## Combine YPR plots
yprout=milr_ypr/eldr_ypr/tcrr_ypr/tcrr_ypr
ggsave(plot=yprout,"ypr.png",width=10,height=12,units="in",bg="transparent")
