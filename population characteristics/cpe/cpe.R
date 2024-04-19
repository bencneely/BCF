#--------------------------------------------------------------
#Ben Neely
#10/12/2022
#Population characteristics
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
        legend.position=c(1,0),
        legend.justification=c("right","bottom"),
        legend.title=element_blank(),
        legend.text=element_text(size=22),
        legend.background=element_rect(fill="transparent")
        )
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/NAJFM first submission/pop characters")

## Read in fish and sample data from random sample
fish=import("tag_recap_dat.xlsx",which="recapfish")%>%
  expandCounts(~count)%>%
  select(impd,grid,tl)%>%
  mutate(impd=factor(impd,levels=c("MILR","ELDR","TCRR","WLFC")))

samp=import("tag_recap_dat.xlsx",which="recapsamp")%>%
  select(impd,grid,eff)%>%
  mutate(impd=factor(impd,levels=c("MILR","ELDR","TCRR","WLFC")))
  
################################################################################
## Add PSD categories to sampled fish
fish1=fish%>%
  mutate(ss=case_when(tl<300 ~ 1, TRUE ~ 0),
         st=case_when(tl>=300 ~ 1, TRUE ~ 0),
         qu=case_when(tl>=510 ~ 1, TRUE ~ 0),
         pr=case_when(tl>=760 ~ 1, TRUE ~ 0),
         me=case_when(tl>=890 ~ 1, TRUE ~ 0),
         tr=case_when(tl>=1140 ~ 1, TRUE ~ 0))

################################################################################
## Milford
milrfish=filter(fish1,impd=="MILR")
milrsamp=filter(samp,impd=="MILR")

## Number of fish in each psd group per sample
milr1=milrfish%>%
  group_by(impd,grid)%>%
  summarize(ss_sum=sum(ss),
            st_sum=sum(st),
            qu_sum=sum(qu),
            pr_sum=sum(pr),
            me_sum=sum(me),
            tr_sum=sum(tr))%>%
  ungroup()%>%
  mutate(tot=ss_sum+st_sum)

## Join fish data with sample data
milr2=milrsamp%>%
  left_join(milr1,by=c("impd","grid"))%>%
  replace_na(list(ss_sum=0,st_sum=0,qu_sum=0,pr_sum=0,me_sum=0,tr_sum=0,tot=0))

## Calculate CPE each size group in each sample
milr3=milr2%>%
  mutate(tot_cpe=tot/eff,
         ss_cpe=ss_sum/eff,
         st_cpe=st_sum/eff,
         qu_cpe=qu_sum/eff,
         pr_cpe=pr_sum/eff,
         me_cpe=me_sum/eff,
         tr_cpe=tr_sum/eff)

## Calculate CPE and associated error for entire sample
## Total
milr_tot_cpe=mean(milr3$tot_cpe)
milr_tot_sd=sd(milr3$tot_cpe)
milr_tot_se=milr_tot_sd/sqrt(nrow(milr3))
milr_tot_rse=milr_tot_se/milr_tot_cpe

## Substock
milr_ss_cpe=mean(milr3$ss_cpe)
milr_ss_sd=sd(milr3$ss_cpe)
milr_ss_se=milr_ss_sd/sqrt(nrow(milr3))
milr_ss_rse=milr_ss_se/milr_ss_cpe

## Stock
milr_st_cpe=mean(milr3$st_cpe)
milr_st_sd=sd(milr3$st_cpe)
milr_st_se=milr_st_sd/sqrt(nrow(milr3))
milr_st_rse=milr_st_se/milr_st_cpe

## Quality
milr_qu_cpe=mean(milr3$qu_cpe)
milr_qu_sd=sd(milr3$qu_cpe)
milr_qu_se=milr_qu_sd/sqrt(nrow(milr3))
milr_qu_rse=milr_qu_se/milr_qu_cpe

## Preferred
milr_pr_cpe=mean(milr3$pr_cpe)
milr_pr_sd=sd(milr3$pr_cpe)
milr_pr_se=milr_pr_sd/sqrt(nrow(milr3))
milr_pr_rse=milr_pr_se/milr_pr_cpe

## Memorable
milr_me_cpe=mean(milr3$me_cpe)
milr_me_sd=sd(milr3$me_cpe)
milr_me_se=milr_me_sd/sqrt(nrow(milr3))
milr_me_rse=milr_me_se/milr_me_cpe

## Trophy
milr_tr_cpe=mean(milr3$tr_cpe)
milr_tr_sd=sd(milr3$tr_cpe)
milr_tr_se=milr_tr_sd/sqrt(nrow(milr3))
milr_tr_rse=milr_tr_se/milr_tr_cpe

## Combine
milrout=c("Milford",
          milr_tot_cpe,milr_tot_sd,milr_tot_se,milr_tot_rse,
          milr_ss_cpe,milr_ss_sd,milr_ss_se,milr_ss_rse,
          milr_st_cpe,milr_st_sd,milr_st_se,milr_st_rse,
          milr_qu_cpe,milr_qu_sd,milr_qu_se,milr_qu_rse,
          milr_pr_cpe,milr_pr_sd,milr_pr_se,milr_pr_rse,
          milr_me_cpe,milr_me_sd,milr_me_se,milr_me_rse,
          milr_tr_cpe,milr_tr_sd,milr_tr_se,milr_tr_rse)

################################################################################
## El Dorado
eldrfish=filter(fish1,impd=="ELDR")
eldrsamp=filter(samp,impd=="ELDR")

## Number of fish in each psd group per sample
eldr1=eldrfish%>%
  group_by(impd,grid)%>%
  summarize(ss_sum=sum(ss),
            st_sum=sum(st),
            qu_sum=sum(qu),
            pr_sum=sum(pr),
            me_sum=sum(me),
            tr_sum=sum(tr))%>%
  ungroup()%>%
  mutate(tot=ss_sum+st_sum)

## Join fish data with sample data
eldr2=eldrsamp%>%
  left_join(eldr1,by=c("impd","grid"))%>%
  replace_na(list(ss_sum=0,st_sum=0,qu_sum=0,pr_sum=0,me_sum=0,tr_sum=0,tot=0))

## Calculate CPE each size group in each sample
eldr3=eldr2%>%
  mutate(tot_cpe=tot/eff,
         ss_cpe=ss_sum/eff,
         st_cpe=st_sum/eff,
         qu_cpe=qu_sum/eff,
         pr_cpe=pr_sum/eff,
         me_cpe=me_sum/eff,
         tr_cpe=tr_sum/eff)

## Calculate CPE and associated error for entire sample
## Total
eldr_tot_cpe=mean(eldr3$tot_cpe)
eldr_tot_sd=sd(eldr3$tot_cpe)
eldr_tot_se=eldr_tot_sd/sqrt(nrow(eldr3))
eldr_tot_rse=eldr_tot_se/eldr_tot_cpe

## Substock
eldr_ss_cpe=mean(eldr3$ss_cpe)
eldr_ss_sd=sd(eldr3$ss_cpe)
eldr_ss_se=eldr_ss_sd/sqrt(nrow(eldr3))
eldr_ss_rse=eldr_ss_se/eldr_ss_cpe

## Stock
eldr_st_cpe=mean(eldr3$st_cpe)
eldr_st_sd=sd(eldr3$st_cpe)
eldr_st_se=eldr_st_sd/sqrt(nrow(eldr3))
eldr_st_rse=eldr_st_se/eldr_st_cpe

## Quality
eldr_qu_cpe=mean(eldr3$qu_cpe)
eldr_qu_sd=sd(eldr3$qu_cpe)
eldr_qu_se=eldr_qu_sd/sqrt(nrow(eldr3))
eldr_qu_rse=eldr_qu_se/eldr_qu_cpe

## Preferred
eldr_pr_cpe=mean(eldr3$pr_cpe)
eldr_pr_sd=sd(eldr3$pr_cpe)
eldr_pr_se=eldr_pr_sd/sqrt(nrow(eldr3))
eldr_pr_rse=eldr_pr_se/eldr_pr_cpe

## Memorable
eldr_me_cpe=mean(eldr3$me_cpe)
eldr_me_sd=sd(eldr3$me_cpe)
eldr_me_se=eldr_me_sd/sqrt(nrow(eldr3))
eldr_me_rse=eldr_me_se/eldr_me_cpe

## Trophy
eldr_tr_cpe=mean(eldr3$tr_cpe)
eldr_tr_sd=sd(eldr3$tr_cpe)
eldr_tr_se=eldr_tr_sd/sqrt(nrow(eldr3))
eldr_tr_rse=eldr_tr_se/eldr_tr_cpe

## Combine
eldrout=c("El Dorado",
          eldr_tot_cpe,eldr_tot_sd,eldr_tot_se,eldr_tot_rse,
          eldr_ss_cpe,eldr_ss_sd,eldr_ss_se,eldr_ss_rse,
          eldr_st_cpe,eldr_st_sd,eldr_st_se,eldr_st_rse,
          eldr_qu_cpe,eldr_qu_sd,eldr_qu_se,eldr_qu_rse,
          eldr_pr_cpe,eldr_pr_sd,eldr_pr_se,eldr_pr_rse,
          eldr_me_cpe,eldr_me_sd,eldr_me_se,eldr_me_rse,
          eldr_tr_cpe,eldr_tr_sd,eldr_tr_se,eldr_tr_rse)

################################################################################
## Tuttle Creek
tcrrfish=filter(fish1,impd=="TCRR")
tcrrsamp=filter(samp,impd=="TCRR")

## Number of fish in each psd group per sample
tcrr1=tcrrfish%>%
  group_by(impd,grid)%>%
  summarize(ss_sum=sum(ss),
            st_sum=sum(st),
            qu_sum=sum(qu),
            pr_sum=sum(pr),
            me_sum=sum(me),
            tr_sum=sum(tr))%>%
  ungroup()%>%
  mutate(tot=ss_sum+st_sum)

## Join fish data with sample data
tcrr2=tcrrsamp%>%
  left_join(tcrr1,by=c("impd","grid"))%>%
  replace_na(list(ss_sum=0,st_sum=0,qu_sum=0,pr_sum=0,me_sum=0,tr_sum=0,tot=0))

## Calculate CPE each size group in each sample
tcrr3=tcrr2%>%
  mutate(tot_cpe=tot/eff,
         ss_cpe=ss_sum/eff,
         st_cpe=st_sum/eff,
         qu_cpe=qu_sum/eff,
         pr_cpe=pr_sum/eff,
         me_cpe=me_sum/eff,
         tr_cpe=tr_sum/eff)

## Calculate CPE and associated error for entire sample
## Total
tcrr_tot_cpe=mean(tcrr3$tot_cpe)
tcrr_tot_sd=sd(tcrr3$tot_cpe)
tcrr_tot_se=tcrr_tot_sd/sqrt(nrow(tcrr3))
tcrr_tot_rse=tcrr_tot_se/tcrr_tot_cpe

## Substock
tcrr_ss_cpe=mean(tcrr3$ss_cpe)
tcrr_ss_sd=sd(tcrr3$ss_cpe)
tcrr_ss_se=tcrr_ss_sd/sqrt(nrow(tcrr3))
tcrr_ss_rse=tcrr_ss_se/tcrr_ss_cpe

## Stock
tcrr_st_cpe=mean(tcrr3$st_cpe)
tcrr_st_sd=sd(tcrr3$st_cpe)
tcrr_st_se=tcrr_st_sd/sqrt(nrow(tcrr3))
tcrr_st_rse=tcrr_st_se/tcrr_st_cpe

## Quality
tcrr_qu_cpe=mean(tcrr3$qu_cpe)
tcrr_qu_sd=sd(tcrr3$qu_cpe)
tcrr_qu_se=tcrr_qu_sd/sqrt(nrow(tcrr3))
tcrr_qu_rse=tcrr_qu_se/tcrr_qu_cpe

## Preferred
tcrr_pr_cpe=mean(tcrr3$pr_cpe)
tcrr_pr_sd=sd(tcrr3$pr_cpe)
tcrr_pr_se=tcrr_pr_sd/sqrt(nrow(tcrr3))
tcrr_pr_rse=tcrr_pr_se/tcrr_pr_cpe

## Memorable
tcrr_me_cpe=mean(tcrr3$me_cpe)
tcrr_me_sd=sd(tcrr3$me_cpe)
tcrr_me_se=tcrr_me_sd/sqrt(nrow(tcrr3))
tcrr_me_rse=tcrr_me_se/tcrr_me_cpe

## Trophy
tcrr_tr_cpe=mean(tcrr3$tr_cpe)
tcrr_tr_sd=sd(tcrr3$tr_cpe)
tcrr_tr_se=tcrr_tr_sd/sqrt(nrow(tcrr3))
tcrr_tr_rse=tcrr_tr_se/tcrr_tr_cpe

## Combine
tcrrout=c("Tuttle Creek",
          tcrr_tot_cpe,tcrr_tot_sd,tcrr_tot_se,tcrr_tot_rse,
          tcrr_ss_cpe,tcrr_ss_sd,tcrr_ss_se,tcrr_ss_rse,
          tcrr_st_cpe,tcrr_st_sd,tcrr_st_se,tcrr_st_rse,
          tcrr_qu_cpe,tcrr_qu_sd,tcrr_qu_se,tcrr_qu_rse,
          tcrr_pr_cpe,tcrr_pr_sd,tcrr_pr_se,tcrr_pr_rse,
          tcrr_me_cpe,tcrr_me_sd,tcrr_me_se,tcrr_me_rse,
          tcrr_tr_cpe,tcrr_tr_sd,tcrr_tr_se,tcrr_tr_rse)

################################################################################
## Wolf Creek
wlfcfish=filter(fish1,impd=="WLFC")
wlfcsamp=filter(samp,impd=="WLFC")

## Number of fish in each psd group per sample
wlfc1=wlfcfish%>%
  group_by(impd,grid)%>%
  summarize(ss_sum=sum(ss),
            st_sum=sum(st),
            qu_sum=sum(qu),
            pr_sum=sum(pr),
            me_sum=sum(me),
            tr_sum=sum(tr))%>%
  ungroup()%>%
  mutate(tot=ss_sum+st_sum)

## Join fish data with sample data
wlfc2=wlfcsamp%>%
  left_join(wlfc1,by=c("impd","grid"))%>%
  replace_na(list(ss_sum=0,st_sum=0,qu_sum=0,pr_sum=0,me_sum=0,tr_sum=0,tot=0))

## Calculate CPE each size group in each sample
wlfc3=wlfc2%>%
  mutate(tot_cpe=tot/eff,
         ss_cpe=ss_sum/eff,
         st_cpe=st_sum/eff,
         qu_cpe=qu_sum/eff,
         pr_cpe=pr_sum/eff,
         me_cpe=me_sum/eff,
         tr_cpe=tr_sum/eff)

## Calculate CPE and associated error for entire sample
## Total
wlfc_tot_cpe=mean(wlfc3$tot_cpe)
wlfc_tot_sd=sd(wlfc3$tot_cpe)
wlfc_tot_se=wlfc_tot_sd/sqrt(nrow(wlfc3))
wlfc_tot_rse=wlfc_tot_se/wlfc_tot_cpe

## Substock
wlfc_ss_cpe=mean(wlfc3$ss_cpe)
wlfc_ss_sd=sd(wlfc3$ss_cpe)
wlfc_ss_se=wlfc_ss_sd/sqrt(nrow(wlfc3))
wlfc_ss_rse=wlfc_ss_se/wlfc_ss_cpe

## Stock
wlfc_st_cpe=mean(wlfc3$st_cpe)
wlfc_st_sd=sd(wlfc3$st_cpe)
wlfc_st_se=wlfc_st_sd/sqrt(nrow(wlfc3))
wlfc_st_rse=wlfc_st_se/wlfc_st_cpe

## Quality
wlfc_qu_cpe=mean(wlfc3$qu_cpe)
wlfc_qu_sd=sd(wlfc3$qu_cpe)
wlfc_qu_se=wlfc_qu_sd/sqrt(nrow(wlfc3))
wlfc_qu_rse=wlfc_qu_se/wlfc_qu_cpe

## Preferred
wlfc_pr_cpe=mean(wlfc3$pr_cpe)
wlfc_pr_sd=sd(wlfc3$pr_cpe)
wlfc_pr_se=wlfc_pr_sd/sqrt(nrow(wlfc3))
wlfc_pr_rse=wlfc_pr_se/wlfc_pr_cpe

## Memorable
wlfc_me_cpe=mean(wlfc3$me_cpe)
wlfc_me_sd=sd(wlfc3$me_cpe)
wlfc_me_se=wlfc_me_sd/sqrt(nrow(wlfc3))
wlfc_me_rse=wlfc_me_se/wlfc_me_cpe

## Trophy
wlfc_tr_cpe=mean(wlfc3$tr_cpe)
wlfc_tr_sd=sd(wlfc3$tr_cpe)
wlfc_tr_se=wlfc_tr_sd/sqrt(nrow(wlfc3))
wlfc_tr_rse=wlfc_tr_se/wlfc_tr_cpe

## Combine
wlfcout=c("Wolf Creek",
          wlfc_tot_cpe,wlfc_tot_sd,wlfc_tot_se,wlfc_tot_rse,
          wlfc_ss_cpe,wlfc_ss_sd,wlfc_ss_se,wlfc_ss_rse,
          wlfc_st_cpe,wlfc_st_sd,wlfc_st_se,wlfc_st_rse,
          wlfc_qu_cpe,wlfc_qu_sd,wlfc_qu_se,wlfc_qu_rse,
          wlfc_pr_cpe,wlfc_pr_sd,wlfc_pr_se,wlfc_pr_rse,
          wlfc_me_cpe,wlfc_me_sd,wlfc_me_se,wlfc_me_rse,
          wlfc_tr_cpe,wlfc_tr_sd,wlfc_tr_se,wlfc_tr_rse)

################################################################################
################################################################################
## Combine all data for export and/or plotting
cpe=as_tibble(rbind(milrout,eldrout,tcrrout,wlfcout))%>%
  rename(impd=V1,
         tot_cpe=V2,tot_sd=V3,tot_se=V4,tot_rse=V5,
         ss_cpe=V6,ss_sd=V7,ss_se=V8,ss_rse=V9,
         st_cpe=V10,st_sd=V11,st_se=V12,st_rse=V13,
         qu_cpe=V14,qu_sd=V15,qu_se=V16,qu_rse=V17,
         pr_cpe=V18,pr_sd=V19,pr_se=V20,pr_rse=V21,
         me_cpe=V22,me_sd=V23,me_se=V24,me_rse=V25,
         tr_cpe=V26,tr_sd=V27,tr_se=V28,tr_rse=V29)

## Convert character columns to numeric
cpe[,2:29]=sapply(cpe[,2:29],as.numeric)

## Total effort and number of sites at each impoundment
milreff=sum(milr3$eff)
eldreff=sum(eldr3$eff)
tcrreff=sum(tcrr3$eff)
wlfceff=sum(wlfc3$eff)

milrsites=nrow(milr3)
eldrsites=nrow(eldr3)
tcrrsites=nrow(tcrr3)
wlfcsites=nrow(wlfc3)

## Output data for table
## Display 5 significant figures
options(pillar.sigfig=5)

## Only keep pertinent columns
cpeout=cpe%>%
  add_column(eff=c(milreff,eldreff,tcrreff,wlfceff))%>%
  add_column(sites=c(milrsites,eldrsites,tcrrsites,wlfcsites))%>%
  select(impd,eff,sites,
         tot_cpe,tot_rse,
         ss_cpe,ss_rse,
         st_cpe,st_rse,
         qu_cpe,qu_rse,
         pr_cpe,pr_rse,
         me_cpe,me_rse,
         tr_cpe,tr_rse)

## Export CPE data 
export(cpeout,"cpe.xlsx")