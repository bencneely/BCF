#--------------------------------------------------------------
#Ben Neely
#09/11/2023
#Evaluate tag return data
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
        legend.position=c(0.999,0.8),
        legend.title=element_blank(),
        legend.text=element_text(size=16),
        legend.justification=c("right","top"),
        legend.background=element_blank())
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/tag returns")

## Read in data with import
returndat=import("one_year_returns.csv")%>%
  rename(impd=impd.tagged)%>%
  filter(tl>=300)
taggeddat=import("taggedfish.csv")%>%
  filter(tl>=300)

################################################################################
################################################################################
## Compare length distribution of tagged fish and angler caught fish
milrtagged=filter(taggeddat,impd=="MILR")
milrreturned=filter(returndat,impd=="MILR")
ks.test(milrtagged$tl,milrreturned$tl)

eldrtagged=filter(taggeddat,impd=="ELDR")
eldrreturned=filter(returndat,impd=="ELDR")
ks.test(eldrtagged$tl,eldrreturned$tl)

tcrrtagged=filter(taggeddat,impd=="TCRR")
tcrrreturned=filter(returndat,impd=="TCRR")
ks.test(tcrrtagged$tl,tcrrreturned$tl)

wlfctagged=filter(taggeddat,impd=="WLFC")
wlfcreturned=filter(returndat,impd=="WLFC")
ks.test(wlfctagged$tl,wlfcreturned$tl)

################################################################################
################################################################################
## Prep data for plotting and plot

################################################################################
## Milford
## Organize tagged fish data
milrtagged1=milrtagged%>%
  mutate(mgrp=lencat(tl,100))%>%
  group_by(mgrp)%>%
  summarize(count=n())%>%
  mutate(tot=sum(count))%>%
  ungroup()%>%
  complete(mgrp=seq(300,1100,100),
           fill=list(count=0))%>%
  mutate(prop=count/tot,
         type="Tagged")

## Organize returned fish data
milrreturned1=milrreturned%>%
  mutate(mgrp=lencat(tl,100))%>%
  group_by(mgrp)%>%
  summarize(count=n())%>%
  mutate(tot=sum(count))%>%
  ungroup()%>%
  complete(mgrp=seq(300,1100,100),
           fill=list(count=0))%>%
  mutate(prop=count/tot,
         type="Returned")

################################################################################
## El Dorado
## Organize tagged fish data
eldrtagged1=eldrtagged%>%
  mutate(mgrp=lencat(tl,100))%>%
  group_by(mgrp)%>%
  summarize(count=n())%>%
  mutate(tot=sum(count))%>%
  ungroup()%>%
  complete(mgrp=seq(300,1100,100),
           fill=list(count=0))%>%
  mutate(prop=count/tot,
         type="Tagged")

## Organize returned fish data
eldrreturned1=eldrreturned%>%
  mutate(mgrp=lencat(tl,100))%>%
  group_by(mgrp)%>%
  summarize(count=n())%>%
  mutate(tot=sum(count))%>%
  ungroup()%>%
  complete(mgrp=seq(300,1100,100),
           fill=list(count=0))%>%
  mutate(prop=count/tot,
         type="Returned")

## Combine for plotting
eldr=bind_rows(eldrtagged1,eldrreturned1)%>%
  mutate(impd="ELDR")
sum(subset(eldr,type=="Tagged")$count)
sum(subset(eldr,type=="Returned")$count)

################################################################################
## Tuttle Creek
## Organize tagged fish data
tcrrtagged1=tcrrtagged%>%
  mutate(mgrp=lencat(tl,100))%>%
  group_by(mgrp)%>%
  summarize(count=n())%>%
  mutate(tot=sum(count))%>%
  ungroup()%>%
  complete(mgrp=seq(300,1100,100),
           fill=list(count=0))%>%
  mutate(prop=count/tot,
         type="Tagged")

## Organize returned fish data
tcrrreturned1=tcrrreturned%>%
  mutate(mgrp=lencat(tl,100))%>%
  group_by(mgrp)%>%
  summarize(count=n())%>%
  mutate(tot=sum(count))%>%
  ungroup()%>%
  complete(mgrp=seq(300,1100,100),
           fill=list(count=0))%>%
  mutate(prop=count/tot,
         type="Returned")

################################################################################
## Wolf Creek
## Organize tagged fish data
wlfctagged1=wlfctagged%>%
  mutate(mgrp=lencat(tl,100))%>%
  group_by(mgrp)%>%
  summarize(count=n())%>%
  mutate(tot=sum(count))%>%
  ungroup()%>%
  complete(mgrp=seq(300,1100,100),
           fill=list(count=0))%>%
  mutate(prop=count/tot,
         type="Tagged")

## Organize returned fish data
wlfcreturned1=wlfcreturned%>%
  mutate(mgrp=lencat(tl,100))%>%
  group_by(mgrp)%>%
  summarize(count=n())%>%
  mutate(tot=sum(count))%>%
  ungroup()%>%
  complete(mgrp=seq(300,1100,100),
           fill=list(count=0))%>%
  mutate(prop=count/tot,
         type="Returned")

## Combine for plotting
wlfc=bind_rows(wlfctagged1,wlfcreturned1)%>%
  mutate(impd="WLFC")
sum(subset(wlfc,type=="Tagged")$count)
sum(subset(wlfc,type=="Returned")$count)

################################################################################
## Combine
out=milrout/eldrout/tcrrout/wlfcout
#ggsave(plot=out,"catchcomp.png",height=12,width=10,units="in",bg="white")

################################################################################
################################################################################
################################################################################
## Compare proportion tagged and caught for each meter group

## Set up data, export and manually manipulate to create x2dat because I'm dumb at R
dat=bind_rows(milr,eldr,tcrr,wlfc)
#export(dat,"tmp.csv")

## Z test for proportions
## https://online.stat.psu.edu/stat415/lesson/9/9.4
dat1=import("x2dat.csv")%>%
  mutate(prop_tagged=num_tagged/tot_tagged,
         prop_returned=num_returned/tot_returned,
         prop_total=(num_tagged+num_returned)/(tot_tagged+tot_returned),
         z_num=prop_tagged-prop_returned,
         z_denom=sqrt(prop_total*(1-prop_total)*(1/tot_tagged+1/tot_returned)),
         z=z_num/z_denom,
         p=round(2-2*pnorm(q=abs(z)),4),
         sig=case_when(p<=0.05 ~ 1,
                          TRUE ~ 0))%>%
  drop_na()%>%
  select(impd,mgrp,prop_tagged,prop_returned,z,p,sig)

## Replot
## MILR
milrout1=ggplot(milr)+
  geom_bar(aes(x=mgrp,y=prop,fill=type),stat="identity",position="dodge")+
  scale_fill_manual(values=c("gray","black"),
                    labels=c("Returned: N = 46","Tagged: N = 1442"))+
  scale_x_continuous(limits=c(249,1151),
                     breaks=seq(300,1100,100),
                     name="",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.65),
                     breaks=seq(0,0.6,0.1),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=1140,y=0.64,hjust=1,vjust=1,label="Milford",size=8)+
  annotate("text",x=400,y=0.46,label="***",size=14)+
  annotate("text",x=700,y=0.15,label="***",size=14)+
  annotate("text",x=800,y=0.15,label="***",size=14)+
  annotate("text",x=1100,y=0.05,label="***",size=14)+
  pubtheme+
  theme(legend.position=c(0.999,0.94))

## ELDR
eldrout1=ggplot(eldr)+
  geom_bar(aes(x=mgrp,y=prop,fill=type),stat="identity",position="dodge")+
  scale_fill_manual(values=c("gray","black"),
                    labels=c("Returned: N = 81","Tagged: N = 1987"))+
  scale_x_continuous(limits=c(249,1151),
                     breaks=seq(300,1100,100),
                     name="",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.65),
                     breaks=seq(0,0.6,0.1),
                     expand=c(0,0),
                     name="Proportion of tagged and returned fish")+
  annotate("text",x=1140,y=0.64,hjust=1,vjust=1,label="El Dorado",size=8)+
  annotate("text",x=400,y=0.46,label="***",size=14)+
  annotate("text",x=500,y=0.58,label="***",size=14)+
  annotate("text",x=600,y=0.18,label="***",size=14)+
  pubtheme+
  theme(axis.title.y=element_text(hjust=0.95),
        legend.position=c(0.999,0.94))

## TCRR
tcrrout1=ggplot(tcrr)+
  geom_bar(aes(x=mgrp,y=prop,fill=type),stat="identity",position="dodge")+
  scale_fill_manual(values=c("gray","black"),
                    labels=c("Returned: N = 43","Tagged: N = 2069"))+
  scale_x_continuous(limits=c(249,1151),
                     breaks=seq(300,1100,100),
                     name="100-mm group",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.65),
                     breaks=seq(0,0.6,0.1),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=1140,y=0.64,hjust=1,vjust=1,label="Tuttle Creek",size=8)+
  annotate("text",x=900,y=0.1,label="***",size=14)+
  annotate("text",x=1000,y=0.04,label="***",size=14)+
  pubtheme+
  theme(legend.position=c(0.999,0.94))

## WLFC
wlfcout1=ggplot(wlfc)+
  geom_bar(aes(x=mgrp,y=prop,fill=type),stat="identity",position="dodge")+
  scale_fill_manual(values=c("gray","black"),
                    labels=c("Returned: N = 17","Tagged: N = 919"))+
  scale_x_continuous(limits=c(249,1151),
                     breaks=seq(300,1100,100),
                     name="100-mm group",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.65),
                     breaks=seq(0,0.6,0.1),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=1140,y=0.64,hjust=1,vjust=1,label="Wolf Creek",size=8)+
  annotate("text",x=300,y=0.43,label="***",size=14)+
  annotate("text",x=700,y=0.43,label="***",size=14)+
  annotate("text",x=900,y=0.07,label="***",size=14)+
  pubtheme+
  theme(legend.position=c(0.999,0.94))

################################################################################
## Combine
out1=milrout1/eldrout1/tcrrout1/wlfcout1
ggsave(plot=out1,"catchcomp.png",height=12,width=10,units="in",bg="white")
