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
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/BCF pop character paper/pop est")

## Read in data with import and identify psd group of each fish
tag=import("pop est data prep/tag_recap_dat.xlsx",which="tagged")%>%
  mutate(stock=case_when(tl>=300 ~ 1, TRUE ~ 0),
         quality=case_when(tl>=510 ~ 1, TRUE ~ 0),
         preferred=case_when(tl>=760 ~ 1, TRUE ~ 0),
         memorable=case_when(tl>=890 ~ 1, TRUE ~ 0),
         trophy=case_when(tl>=1140 ~ 1, TRUE ~ 0))

recap=import("pop est data prep/tag_recap_dat.xlsx",which="recapfish")%>%
  expandCounts(~count)%>%
  mutate(stock=case_when(tl>=300 ~ 1, TRUE ~ 0),
         quality=case_when(tl>=510 ~ 1, TRUE ~ 0),
         preferred=case_when(tl>=760 ~ 1, TRUE ~ 0),
         memorable=case_when(tl>=890 ~ 1, TRUE ~ 0),
         trophy=case_when(tl>=1140 ~ 1, TRUE ~ 0))

################################################################################
################################################################################
## Population estimate data wrangling

################################################################################
## Milford
## Number of tagged fish within each psd length group
milr_st_M=tag%>%
  filter(impd=="MILR" & stock==1)%>%
  nrow()

milr_qu_M=tag%>%
  filter(impd=="MILR" & quality==1)%>%
  nrow()

milr_pr_M=tag%>%
  filter(impd=="MILR" & preferred==1)%>%
  nrow()

milr_me_M=tag%>%
  filter(impd=="MILR" & memorable==1)%>%
  nrow()

milr_tr_M=tag%>%
  filter(impd=="MILR" & trophy==1)%>%
  nrow()

milr_M=c(milr_st_M,milr_qu_M,milr_pr_M,milr_me_M,milr_tr_M)

## Number of fish encountered during recap event within each psd length group
milr_st_n=recap%>%
  filter(impd=="MILR" & stock==1)%>%
  nrow()

milr_qu_n=recap%>%
  filter(impd=="MILR" & quality==1)%>%
  nrow()

milr_pr_n=recap%>%
  filter(impd=="MILR" & preferred==1)%>%
  nrow()

milr_me_n=recap%>%
  filter(impd=="MILR" & memorable==1)%>%
  nrow()

milr_tr_n=recap%>%
  filter(impd=="MILR" & trophy==1)%>%
  nrow()

milr_n=c(milr_st_n,milr_qu_n,milr_pr_n,milr_me_n,milr_tr_n)

## Number of tagged fish encountered during recap event within each psd length group
milr_st_m=recap%>%
  filter(impd=="MILR" & stock==1 & tag>0)%>%
  nrow()

milr_qu_m=recap%>%
  filter(impd=="MILR" & quality==1 & tag>0)%>%
  nrow()

milr_pr_m=recap%>%
  filter(impd=="MILR" & preferred==1 & tag>0)%>%
  nrow()

milr_me_m=recap%>%
  filter(impd=="MILR" & memorable==1 & tag>0)%>%
  nrow()

milr_tr_m=recap%>%
  filter(impd=="MILR" & trophy==1 & tag>0)%>%
  nrow()

milr_m=c(milr_st_m,milr_qu_m,milr_pr_m,milr_me_m,milr_tr_m)

## Combine all data
milr_out=as_tibble(bind_cols(psd=c("stock","quality","preferred","memorable","trophy"),
                             milr_M=milr_M,
                             milr_n=milr_n,
                             milr_m=milr_m))

################################################################################
## El Dorado
## Number of tagged fish within each psd length group
eldr_st_M=tag%>%
  filter(impd=="ELDR" & stock==1)%>%
  nrow()

eldr_qu_M=tag%>%
  filter(impd=="ELDR" & quality==1)%>%
  nrow()

eldr_pr_M=tag%>%
  filter(impd=="ELDR" & preferred==1)%>%
  nrow()

eldr_me_M=tag%>%
  filter(impd=="ELDR" & memorable==1)%>%
  nrow()

eldr_tr_M=tag%>%
  filter(impd=="ELDR" & trophy==1)%>%
  nrow()

eldr_M=c(eldr_st_M,eldr_qu_M,eldr_pr_M,eldr_me_M,eldr_tr_M)

## Number of fish encountered during recap event within each psd length group
eldr_st_n=recap%>%
  filter(impd=="ELDR" & stock==1)%>%
  nrow()

eldr_qu_n=recap%>%
  filter(impd=="ELDR" & quality==1)%>%
  nrow()

eldr_pr_n=recap%>%
  filter(impd=="ELDR" & preferred==1)%>%
  nrow()

eldr_me_n=recap%>%
  filter(impd=="ELDR" & memorable==1)%>%
  nrow()

eldr_tr_n=recap%>%
  filter(impd=="ELDR" & trophy==1)%>%
  nrow()

eldr_n=c(eldr_st_n,eldr_qu_n,eldr_pr_n,eldr_me_n,eldr_tr_n)

## Number of tagged fish encountered during recap event within each psd length group
eldr_st_m=recap%>%
  filter(impd=="ELDR" & stock==1 & tag>0)%>%
  nrow()

eldr_qu_m=recap%>%
  filter(impd=="ELDR" & quality==1 & tag>0)%>%
  nrow()

eldr_pr_m=recap%>%
  filter(impd=="ELDR" & preferred==1 & tag>0)%>%
  nrow()

eldr_me_m=recap%>%
  filter(impd=="ELDR" & memorable==1 & tag>0)%>%
  nrow()

eldr_tr_m=recap%>%
  filter(impd=="ELDR" & trophy==1 & tag>0)%>%
  nrow()

eldr_m=c(eldr_st_m,eldr_qu_m,eldr_pr_m,eldr_me_m,eldr_tr_m)

## Combine all data
eldr_out=as_tibble(bind_cols(psd=c("stock","quality","preferred","memorable","trophy"),
                             eldr_M=eldr_M,
                             eldr_n=eldr_n,
                             eldr_m=eldr_m))

################################################################################
##Tuttle Creek
## Number of tagged fish within each psd length group
tcrr_st_M=tag%>%
  filter(impd=="TCRR" & stock==1)%>%
  nrow()

tcrr_qu_M=tag%>%
  filter(impd=="TCRR" & quality==1)%>%
  nrow()

tcrr_pr_M=tag%>%
  filter(impd=="TCRR" & preferred==1)%>%
  nrow()

tcrr_me_M=tag%>%
  filter(impd=="TCRR" & memorable==1)%>%
  nrow()

tcrr_tr_M=tag%>%
  filter(impd=="TCRR" & trophy==1)%>%
  nrow()

tcrr_M=c(tcrr_st_M,tcrr_qu_M,tcrr_pr_M,tcrr_me_M,tcrr_tr_M)

## Number of fish encountered during recap event within each psd length group
tcrr_st_n=recap%>%
  filter(impd=="TCRR" & stock==1)%>%
  nrow()

tcrr_qu_n=recap%>%
  filter(impd=="TCRR" & quality==1)%>%
  nrow()

tcrr_pr_n=recap%>%
  filter(impd=="TCRR" & preferred==1)%>%
  nrow()

tcrr_me_n=recap%>%
  filter(impd=="TCRR" & memorable==1)%>%
  nrow()

tcrr_tr_n=recap%>%
  filter(impd=="TCRR" & trophy==1)%>%
  nrow()

tcrr_n=c(tcrr_st_n,tcrr_qu_n,tcrr_pr_n,tcrr_me_n,tcrr_tr_n)

## Number of tagged fish encountered during recap event within each psd length group
tcrr_st_m=recap%>%
  filter(impd=="TCRR" & stock==1 & tag>0)%>%
  nrow()

tcrr_qu_m=recap%>%
  filter(impd=="TCRR" & quality==1 & tag>0)%>%
  nrow()

tcrr_pr_m=recap%>%
  filter(impd=="TCRR" & preferred==1 & tag>0)%>%
  nrow()

tcrr_me_m=recap%>%
  filter(impd=="TCRR" & memorable==1 & tag>0)%>%
  nrow()

tcrr_tr_m=recap%>%
  filter(impd=="TCRR" & trophy==1 & tag>0)%>%
  nrow()

tcrr_m=c(tcrr_st_m,tcrr_qu_m,tcrr_pr_m,tcrr_me_m,tcrr_tr_m)

## Combine all data
tcrr_out=as_tibble(bind_cols(psd=c("stock","quality","preferred","memorable","trophy"),
                             tcrr_M=tcrr_M,
                             tcrr_n=tcrr_n,
                             tcrr_m=tcrr_m))

################################################################################
## Wolf Creek
## Number of tagged fish within each psd length group
wlfc_st_M=tag%>%
  filter(impd=="WLFC" & stock==1)%>%
  nrow()

wlfc_qu_M=tag%>%
  filter(impd=="WLFC" & quality==1)%>%
  nrow()

wlfc_pr_M=tag%>%
  filter(impd=="WLFC" & preferred==1)%>%
  nrow()

wlfc_me_M=tag%>%
  filter(impd=="WLFC" & memorable==1)%>%
  nrow()

wlfc_tr_M=tag%>%
  filter(impd=="WLFC" & trophy==1)%>%
  nrow()

wlfc_M=c(wlfc_st_M,wlfc_qu_M,wlfc_pr_M,wlfc_me_M,wlfc_tr_M)

## Number of fish encountered during recap event within each psd length group
wlfc_st_n=recap%>%
  filter(impd=="WLFC" & stock==1)%>%
  nrow()

wlfc_qu_n=recap%>%
  filter(impd=="WLFC" & quality==1)%>%
  nrow()

wlfc_pr_n=recap%>%
  filter(impd=="WLFC" & preferred==1)%>%
  nrow()

wlfc_me_n=recap%>%
  filter(impd=="WLFC" & memorable==1)%>%
  nrow()

wlfc_tr_n=recap%>%
  filter(impd=="WLFC" & trophy==1)%>%
  nrow()

wlfc_n=c(wlfc_st_n,wlfc_qu_n,wlfc_pr_n,wlfc_me_n,wlfc_tr_n)

## Number of tagged fish encountered during recap event within each psd length group
wlfc_st_m=recap%>%
  filter(impd=="WLFC" & stock==1 & tag>0)%>%
  nrow()

wlfc_qu_m=recap%>%
  filter(impd=="WLFC" & quality==1 & tag>0)%>%
  nrow()

wlfc_pr_m=recap%>%
  filter(impd=="WLFC" & preferred==1 & tag>0)%>%
  nrow()

wlfc_me_m=recap%>%
  filter(impd=="WLFC" & memorable==1 & tag>0)%>%
  nrow()

wlfc_tr_m=recap%>%
  filter(impd=="WLFC" & trophy==1 & tag>0)%>%
  nrow()

wlfc_m=c(wlfc_st_m,wlfc_qu_m,wlfc_pr_m,wlfc_me_m,wlfc_tr_m)

## Combine all data
wlfc_out=as_tibble(bind_cols(psd=c("stock","quality","preferred","memorable","trophy"),
                             wlfc_M=wlfc_M,
                             wlfc_n=wlfc_n,
                             wlfc_m=wlfc_m))

################################################################################
################################################################################
## Combine them all and export
export(list(milr=milr_out,
            eldr=eldr_out,
            tcrr=tcrr_out,
            wlfc=wlfc_out),
       "popestdat.xlsx")