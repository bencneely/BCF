#--------------------------------------------------------------
#Ben Neely
#10/12/2022
#PSD of randomly sampled Blue Catfish
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
        legend.position=c(1,0),
        legend.justification=c("right","bottom"),
        legend.title=element_blank(),
        legend.text=element_text(size=22),
        legend.background=element_rect(fill="transparent")
        )
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Documents/Active manuscripts/BCF pop character paper/NAJFM second submission/pop characters/length frequency/")

## Read in fish and sample data from random sample
fish=import("tag_recap_dat.xlsx",which="recapfish")%>%
  expandCounts(~count)%>%
  select(impd,spp,grid,tl,w)%>%
  mutate(impd=factor(impd,levels=c("MILR","ELDR","TCRR","WLFC")),
         psd=psdAdd(tl,"Blue Catfish"))

################################################################################
## PSD values for each population
milr=filter(fish,impd=="MILR")
milr_psd=psdCalc(~tl,data=milr,species="Blue Catfish",digits=0,units=c("mm"),conf.level=0.95,what="traditional")%>%
  as_tibble(rownames=NA)%>%
  rownames_to_column("parm")%>%
  mutate(impd="MILR")

eldr=filter(fish,impd=="ELDR",
            tl<800)
eldr_psd=psdCalc(~tl,data=eldr,species="Blue Catfish",digits=0,units=c("mm"),conf.level=0.95,what="traditional")%>%
  as_tibble(rownames=NA)%>%
  rownames_to_column("parm")%>%
  mutate(impd="ELDR")

tcrr=filter(fish,impd=="TCRR",
            tl>=100)
tcrr_psd=psdCalc(~tl,data=tcrr,species="Blue Catfish",digits=0,units=c("mm"),conf.level=0.95,what="traditional")%>%
  as_tibble(rownames=NA)%>%
  rownames_to_column("parm")%>%
  mutate(impd="TCRR")

wlfc=filter(fish,impd=="WLFC")
wlfc_psd=psdCalc(~tl,data=wlfc,species="Blue Catfish",digits=0,units=c("mm"),conf.level=0.95,what="traditional")%>%
  as_tibble(rownames=NA)%>%
  rownames_to_column("parm")%>%
  mutate(impd="WLFC")

## Combine into single file
psdout=bind_rows(milr_psd,eldr_psd,tcrr_psd,wlfc_psd)

################################################################################
## Number of fish in each sample
nrow(milr)
nrow(eldr)
nrow(tcrr)
nrow(wlfc)

################################################################################
## Proportion of substock fish in each sample
nrow(subset(milr,psd=="substock"))/nrow(milr)
nrow(subset(eldr,psd=="substock"))/nrow(eldr)
nrow(subset(tcrr,psd=="substock"))/nrow(tcrr)
nrow(subset(wlfc,psd=="substock"))/nrow(wlfc)

################################################################################
## Proportion of memorable fish in each sample
nrow(subset(milr,psd=="memorable"))/nrow(milr)
nrow(subset(eldr,psd=="memorable"))/nrow(eldr)
nrow(subset(tcrr,psd=="memorable"))/nrow(tcrr)
nrow(subset(wlfc,psd=="memorable"))/nrow(wlfc)

################################################################################
## Length frequency with PSD
## Milford
milr1=milr%>%
  mutate(cmgrp=lencat(tl,10))%>%
  group_by(cmgrp)%>%
  summarize(ct=n())%>%
  ungroup()%>%
  mutate(prop=ct/nrow(milr))

milr_plot=ggplot(milr1)+
  geom_bar(aes(x=cmgrp,y=prop),stat="identity",width=10)+
  scale_x_continuous(limits=c(0,1250),
                     breaks=seq(0,1200,200),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,0.122),
                     breaks=seq(0,0.12,0.02),
                     expand=c(0,0),
                     name="")+
  geom_vline(xintercept=c(300,510,760,890),linetype="dashed",
             color="black",alpha=0.5)+
  annotate("text",x=1220,y=0.119,hjust=1,vjust=1,label="Milford",size=8)+
  annotate("text",x=1220,y=0.098,hjust=1,vjust=1,
           label="N = 2369\nPSD-Q = 19\nPSD-P = 4\nPSD-M = 3",size=4)+
  pubtheme

## El Dorado
eldr1=eldr%>%
  mutate(cmgrp=lencat(tl,10))%>%
  group_by(cmgrp)%>%
  summarize(ct=n())%>%
  ungroup()%>%
  mutate(prop=ct/nrow(eldr))

eldr_plot=ggplot(eldr1)+
  geom_bar(aes(x=cmgrp,y=prop),stat="identity",width=10)+
  scale_x_continuous(limits=c(0,1250),
                     breaks=seq(0,1200,200),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,0.122),
                     breaks=seq(0,0.12,0.02),
                     expand=c(0,0),
                     name="Proportion of catch")+
  geom_vline(xintercept=c(300,510,760,890),linetype="dashed",
             color="black",alpha=0.5)+
  annotate("text",x=1220,y=0.119,hjust=1,vjust=1,label="El Dorado",size=8)+
  annotate("text",x=1220,y=0.098,hjust=1,vjust=1,
           label="N = 1137\nPSD-Q = 42",size=4)+
  pubtheme+
  theme(axis.title.y=element_text(hjust=3))

## Tuttle Creek
tcrr1=tcrr%>%
  mutate(cmgrp=lencat(tl,10))%>%
  group_by(cmgrp)%>%
  summarize(ct=n())%>%
  ungroup()%>%
  mutate(prop=ct/nrow(tcrr))

tcrr_plot=ggplot(tcrr1)+
  geom_bar(aes(x=cmgrp,y=prop),stat="identity",width=10)+
  scale_x_continuous(limits=c(0,1250),
                     breaks=seq(0,1200,200),
                     expand=c(0,0),
                     name="")+
  scale_y_continuous(limits=c(0,0.122),
                     breaks=seq(0,0.12,0.02),
                     expand=c(0,0),
                     name="")+
  geom_vline(xintercept=c(300,510,760,890),linetype="dashed",
             color="black",alpha=0.5)+
  annotate("text",x=1220,y=0.119,hjust=1,vjust=1,label="Tuttle Creek",size=8)+
  annotate("text",x=1220,y=0.098,hjust=1,vjust=1,
           label="N = 750\nPSD-Q = 57\nPSD-P = 7\nPSD-M = 1",size=4)+
  pubtheme

## Wolf Creek
wlfc1=wlfc%>%
  mutate(cmgrp=lencat(tl,10))%>%
  group_by(cmgrp)%>%
  summarize(ct=n())%>%
  ungroup()%>%
  mutate(prop=ct/nrow(wlfc))

wlfc_plot=ggplot(wlfc1)+
  geom_bar(aes(x=cmgrp,y=prop),stat="identity",width=10)+
  scale_x_continuous(limits=c(0,1250),
                     breaks=seq(0,1200,200),
                     expand=c(0,0),
                     name="Total length (mm)")+
  scale_y_continuous(limits=c(0,0.122),
                     breaks=seq(0,0.12,0.02),
                     expand=c(0,0),
                     name="")+
  geom_vline(xintercept=c(300,510,760,890),linetype="dashed",
             color="black",alpha=0.5)+
  annotate("text",x=1220,y=0.119,hjust=1,vjust=1,label="Wolf Creek",size=8)+
  annotate("text",x=1220,y=0.098,hjust=1,vjust=1,
           label="N = 572\nPSD-Q = 32\nPSD-P = 9\nPSD-M = 2",size=4)+
  pubtheme

## Combine
lfout=milr_plot/eldr_plot/tcrr_plot/wlfc_plot
ggsave(plot=lfout,"lf.png",height=12,width=8,units="in",bg="white")