#--------------------------------------------------------------
#Ben Neely
#09/25/2023
#Model population response for different regulations
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
        legend.position="none")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/pop models")

## Read in population model data with import and assign reg type to a factor
dat=import("popmoddat.csv")%>%
  mutate(reg=factor(reg,levels=c("no_length","maximum","protected_slot")))%>%
  filter(u>0)

################################################################################
################################################################################
################################################################################
## Graph YPR, SPR, and big fish remaining plots for each population

################################################################################
################################################################################
## YPR plots
yprdat=filter(dat,metric=="yield")

################################################################################
## YPR
## Milford
milr_ypr=ggplot(subset(yprdat,impd=="MILR"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.2),
                     name="Yield per recruit (kg)")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,1.11),
                  expand=F)+
  annotate("text",x=0.02,y=1.1,label="Milford",hjust=0,vjust=1,size=7.7)+
  pubtheme+
  theme(#legend.key.size=unit(3,"line"),
        legend.text=element_text(size=14),
        legend.background=element_rect(fill="transparent"),
        legend.position=c(1.01,1.1),
        legend.justification=c(1,1),
        legend.spacing=unit(0.8,"cm"),
        legend.key.width=unit(1.2,"cm"))

## El Dorado
eldr_ypr=ggplot(subset(yprdat,impd=="ELDR"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.2),
                     name="")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,1.11),
                  expand=F)+
  annotate("text",x=0.02,y=1.1,label="El Dorado",hjust=0,vjust=1,size=7.7)+
  pubtheme

## Tuttle Creek
tcrr_ypr=ggplot(subset(yprdat,impd=="TCRR"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.2),
                     name="")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,1.11),
                  expand=F)+
  annotate("text",x=0.02,y=1.1,label="Tuttle Creek",hjust=0,vjust=1,size=7.7)+
  pubtheme

## Wolf Creek
wlfc_ypr=ggplot(subset(yprdat,impd=="WLFC"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.2),
                     name="")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,1.11),
                  expand=F)+
  annotate("text",x=0.02,y=1.1,label="Wolf Creek",hjust=0,vjust=1,size=7.7)+
  pubtheme

## Combine YPR plots for plotting via patchwork
yprout=milr_ypr|eldr_ypr|tcrr_ypr|wlfc_ypr

################################################################################
################################################################################
## SPR plots
sprdat=filter(dat,metric=="spr")

################################################################################
## SPR
## Milford
milr_spr=ggplot(subset(sprdat,impd=="MILR"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.2),
                     name="Spawning potential ratio")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,1.01),
                  expand=F)+
  geom_hline(yintercept=0.3,linetype="dashed",color="gray30")+
  annotate("text",x=0.02,y=1,label="Milford",hjust=0,vjust=1,size=7.7)+
  pubtheme

## El Dorado
eldr_spr=ggplot(subset(sprdat,impd=="ELDR"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.2),
                     name="")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,1.01),
                  expand=F)+
  geom_hline(yintercept=0.3,linetype="dashed",color="gray30")+
  annotate("text",x=0.02,y=1,label="El Dorado",hjust=0,vjust=1,size=7.7)+
  pubtheme

## Tuttle Creek
tcrr_spr=ggplot(subset(sprdat,impd=="TCRR"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.2),
                     name="")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,1.01),
                  expand=F)+
  geom_hline(yintercept=0.3,linetype="dashed",color="gray30")+
  annotate("text",x=0.02,y=1,label="Tuttle Creek",hjust=0,vjust=1,size=7.7)+
  pubtheme

## Wolf Creek
wlfc_spr=ggplot(subset(sprdat,impd=="WLFC"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.2),
                     name="")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,1.01),
                  expand=F)+
  geom_hline(yintercept=0.3,linetype="dashed",color="gray30")+
  annotate("text",x=0.02,y=1,label="Wolf Creek",hjust=0,vjust=1,size=7.7)+
  pubtheme

## Combine SPR plots for plotting via patchwork
sprout=milr_spr|eldr_spr|tcrr_spr|wlfc_spr

################################################################################
################################################################################
## Big fish model plots (How many recruits get to 762 mm)
bfmdat=filter(dat,metric=="bfm")

################################################################################
## BFM
## Milford
milr_bfm=ggplot(subset(bfmdat,impd=="MILR"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="Angling exploitation")+
  scale_y_continuous(breaks=seq(0,200,40),
                     name="Recruits to 760 mm")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,205),
                  expand=F)+
  annotate("text",x=0.38,y=200,label="Milford",hjust=1,vjust=1,size=7.7)+
  pubtheme

## El Dorado
eldr_bfm=ggplot(subset(bfmdat,impd=="ELDR"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="Angling exploitation")+
  scale_y_continuous(breaks=seq(0,200,40),
                     name="")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,205),
                  expand=F)+
  annotate("text",x=0.38,y=200,label="El Dorado",hjust=1,vjust=1,size=7.7)+
  pubtheme

## Tuttle Creek
tcrr_bfm=ggplot(subset(bfmdat,impd=="TCRR"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="Angling exploitation")+
  scale_y_continuous(breaks=seq(0,200,40),
                     name="")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,205),
                  expand=F)+
  annotate("text",x=0.38,y=200,label="Tuttle Creek",hjust=1,vjust=1,size=7.7)+
  pubtheme

## Wolf Creek
wlfc_bfm=ggplot(subset(bfmdat,impd=="WLFC"))+
  geom_line(aes(x=u,y=value,linetype=reg),size=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c("381-mm minimum","762-mm maximum","635 to 889-mm protected"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     name="Angling exploitation")+
  scale_y_continuous(breaks=seq(0,200,40),
                     name="")+
  coord_cartesian(xlim=c(0,0.405),
                  ylim=c(0,205),
                  expand=F)+
  annotate("text",x=0.38,y=200,label="Wolf Creek",hjust=1,vjust=1,size=7.7)+
  pubtheme

## Combine BFM plots for plotting via patchwork
bfmout=milr_bfm|eldr_bfm|tcrr_bfm|wlfc_bfm

################################################################################
################################################################################
## combine all plots for a look
modplots=yprout/sprout/bfmout
ggsave(plot=modplots,"popmods.png",width=20,height=12,units="in",bg="white")
