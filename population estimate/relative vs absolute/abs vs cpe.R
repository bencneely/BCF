#--------------------------------------------------------------
#Ben Neely
#04/17/2024
#Blue Catfish CPE vs population estimates
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
        legend.position="top",
        legend.title=element_blank(),
        legend.text=element_text(size=16),
        legend.key.width=unit(0.5,"in"))
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/pop est")

## Read in population estimates with 95% CI
dat=import("abs vs cpe.csv")%>%
  mutate(impd=factor(impd,levels=c("Milford","El Dorado","Tuttle Creek","Wolf Creek")))

################################################################################
################################################################################
## Compare estimated abundance/hectare with CPE by PSD group

################################################################################
## Stock
st=filter(dat,psd=="st")
st_lm=lm(cpe~est_ha,st)
summary(st_lm)

## plot
st_out=ggplot(st)+
  geom_pointrange(mapping=aes(x=est_ha,xmin=lci95_ha,xmax=uci95_ha,y=cpe,color=impd),linewidth=1.5)+
  geom_pointrange(mapping=aes(y=cpe,ymin=lci95_cpe,ymax=uci95_cpe,x=est_ha,color=impd),linewidth=1.5)+
  geom_point(mapping=aes(x=est_ha,y=cpe,fill=impd,color=impd,shape=impd),size=6)+
  scale_color_manual(values=c("#440154","#31688E","#35B779","#FDE725"))+
  scale_fill_manual(values=c("#440154","#31688E","#35B779","#FDE725"))+
  scale_shape_manual(values=c(21,22,23,24))+
  geom_smooth(aes(x=est_ha,y=cpe),method="lm",color="black",linetype="dashed")+
  scale_y_continuous(limits=c(0,161),
                     breaks=seq(0,160,20),
                     expand=c(0,0),
                     name="Stock CPE")+
  scale_x_continuous(limits=c(0,50.5),
                     breaks=seq(0,50,5),
                     expand=c(0,0),
                     name="Stock/ha")+
  annotate("text",label=expression(italic(P)==0.331),x=50,y=160,hjust=1,vjust=1,size=8)+
  pubtheme

################################################################################
## Quality
qu=filter(dat,psd=="qu")
qu_lm=lm(cpe~est_ha,qu)
summary(qu_lm)

## plot
qu_out=ggplot(qu)+
  geom_pointrange(mapping=aes(x=est_ha,xmin=lci95_ha,xmax=uci95_ha,y=cpe,color=impd),linewidth=1.5)+
  geom_pointrange(mapping=aes(y=cpe,ymin=lci95_cpe,ymax=uci95_cpe,x=est_ha,color=impd),linewidth=1.5)+
  geom_point(mapping=aes(x=est_ha,y=cpe,fill=impd,color=impd,shape=impd),size=6)+
  scale_color_manual(values=c("#440154","#31688E","#35B779","#FDE725"))+
  scale_fill_manual(values=c("#440154","#31688E","#35B779","#FDE725"))+
  scale_shape_manual(values=c(21,22,23,24))+
  geom_smooth(aes(x=est_ha,y=cpe),method="lm",color="black",linetype="dashed")+
  scale_y_continuous(limits=c(0,70.5),
                     breaks=seq(0,70,10),
                     expand=c(0,0),
                     name="Quality CPE")+
  scale_x_continuous(limits=c(0,15.2),
                     breaks=seq(0,15,3),
                     expand=c(0,0),
                     name="Quality/ha")+
  annotate("text",label=expression(italic(P)==0.983),x=15,y=70,hjust=1,vjust=1,size=8)+
  pubtheme+
  theme(legend.position="none")

################################################################################
## Preferred
pr=filter(dat,psd=="pr")
pr_lm=lm(cpe~est_ha,pr)
summary(pr_lm)

## plot
pr_out=ggplot(pr)+
  geom_pointrange(mapping=aes(x=est_ha,xmin=lci95_ha,xmax=uci95_ha,y=cpe,color=impd),linewidth=1.5)+
  geom_pointrange(mapping=aes(y=cpe,ymin=lci95_cpe,ymax=uci95_cpe,x=est_ha,color=impd),linewidth=1.5)+
  geom_point(mapping=aes(x=est_ha,y=cpe,fill=impd,color=impd,shape=impd),size=6)+
  scale_color_manual(values=c("#440154","#31688E","#35B779","#FDE725"))+
  scale_fill_manual(values=c("#440154","#31688E","#35B779","#FDE725"))+
  scale_shape_manual(values=c(21,22,23,24))+
  geom_smooth(aes(x=est_ha,y=cpe),method="lm",color="black",linetype="dashed")+
  scale_y_continuous(limits=c(0,6.1),
                     breaks=seq(0,6,1),
                     expand=c(0,0),
                     name="Preferred CPE")+
  scale_x_continuous(limits=c(0,1.01),
                     breaks=seq(0,1,0.1),
                     expand=c(0,0),
                     name="Preferred/ha")+
  annotate("text",label=expression(italic(P)==0.736),x=1,y=6,hjust=1,vjust=1,size=8)+
  pubtheme+
  theme(legend.position="none")

################################################################################
## Memorable
me=filter(dat,psd=="me")
me_lm=lm(cpe~est_ha,me)
summary(me_lm)

## plot
me_out=ggplot(me)+
  geom_pointrange(mapping=aes(x=est_ha,xmin=lci95_ha,xmax=uci95_ha,y=cpe,color=impd),linewidth=1.5)+
  geom_pointrange(mapping=aes(y=cpe,ymin=lci95_cpe,ymax=uci95_cpe,x=est_ha,color=impd),linewidth=1.5)+
  geom_point(mapping=aes(x=est_ha,y=cpe,fill=impd,color=impd,shape=impd),size=6)+
  scale_color_manual(values=c("#440154","#31688E","#35B779","#FDE725"))+
  scale_fill_manual(values=c("#440154","#31688E","#35B779","#FDE725"))+
  scale_shape_manual(values=c(21,22,23,24))+
  geom_smooth(aes(x=est_ha,y=cpe),method="lm",se=F,color="black",linetype="dashed")+
  scale_y_continuous(limits=c(0,5.1),
                     breaks=seq(0,5,1),
                     expand=c(0,0),
                     name="Memorable CPE")+
  scale_x_continuous(limits=c(0,0.305),
                     breaks=seq(0,0.3,0.05),
                     expand=c(0,0),
                     name="Memorable/ha")+
  annotate("text",label=expression(italic(P)==0.014),x=0.3,y=5,hjust=1,vjust=1,size=8)+
  pubtheme+
  theme(legend.position="none")

################################################################################
## Combine
out=st_out/qu_out/pr_out/me_out
ggsave(plot=out,"abs vs cpe.png",height=12,width=8,units="in",bg="transparent")
