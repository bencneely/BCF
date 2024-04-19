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
        legend.position="none")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/pop est")

## Read in population estimates with 95% CI
dat=import("popests.csv")%>%
  mutate(est_ha=est_ac*2.47105,
         lci95_ha=lci95_ac*2.47105,
         uci95_ha=uci95_ac*2.47105,
         impd=factor(impd,levels=c("milr","eldr","tcrr","wlfc")))

###############################################################################
###############################################################################
## Plot population estimates by psd group

###############################################################################
## Stock (fish 300 mm and greater)
st=filter(dat,psd=="st")

stplot=ggplot(st)+
  geom_pointrange(aes(x=impd,y=est_ha,ymin=lci95_ha,ymax=uci95_ha))+
  scale_y_continuous(limits=c(0,47),
                     breaks=seq(0,45,5),
                     expand=c(0,0),
                     name="Stock/ha")+
  scale_x_discrete(labels=c("Milford","El Dorado","Tuttle Creek","Wolf Creek"),
                   name="")+
  pubtheme
  
###############################################################################
## Quality (fish 510 mm and greater)
qu=filter(dat,psd=="qu")

quplot=ggplot(qu)+
  geom_pointrange(aes(x=impd,y=est_ha,ymin=lci95_ha,ymax=uci95_ha))+
  scale_y_continuous(limits=c(0,14.1),
                     breaks=seq(0,14,2),
                     expand=c(0,0),
                     name="Quality/ha")+
  scale_x_discrete(labels=c("Milford","El Dorado","Tuttle Creek","Wolf Creek"),
                   name="")+
  pubtheme

###############################################################################
## Quality (fish 760 mm and greater)
pr=filter(dat,psd=="pr")

prplot=ggplot(pr)+
  geom_pointrange(aes(x=impd,y=est_ha,ymin=lci95_ha,ymax=uci95_ha))+
  scale_y_continuous(limits=c(0,1.01),
                     breaks=seq(0,1,0.2),
                     expand=c(0,0),
                     name="Preferred/ha")+
  scale_x_discrete(labels=c("Milford","El Dorado","Tuttle Creek","Wolf Creek"),
                   name="")+
  pubtheme

###############################################################################
## Memorable (fish 890 mm and greater)
me=filter(dat,psd=="me")

meplot=ggplot(me)+
  geom_pointrange(aes(x=impd,y=est_ha,ymin=lci95_ha,ymax=uci95_ha))+
  scale_y_continuous(limits=c(0,0.31),
                     breaks=seq(0,0.3,0.05),
                     expand=c(0,0),
                     name="Memorable/ha")+
  scale_x_discrete(labels=c("Milford","El Dorado","Tuttle Creek","Wolf Creek"),
                   name="")+
  pubtheme

###############################################################################
## Combine into single plot
popout=stplot/quplot/prplot/meplot
ggsave(plot=popout,"pop ests.png",height=12,width=8,units="in",bg="white")
