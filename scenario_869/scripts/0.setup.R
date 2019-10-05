#initial environment setup

#install.packages("devtools")
#install.packages("FLCore", repo = "http://flr-project.org/R")
#install.packages(c("ggplotFL"), repos="http://flr-project.org/R")
#library(devtools)
#install_github("ices-tools-prod/msy")

#pathR<-paste("C:/Program Files/R/R-",substr(R.Version()$version.string, start=11, stop=15), "/library", sep="")
#pathFLR<-paste("D:/pro/MI/stock_assessment/FLR-library/R-",substr(R.Version()$version.string, start=11, stop=15), sep="")
#.libPaths(c(pathR, pathFLR))



rm(list=ls())
gc()
try(dev.off(),silent=TRUE)

# To be updated
setwd("D:/pro/MI/stock_assessment/CSH/scenario_869")
MSE.Dir <- "D:/pro/MI/stock_assessment/CSH/scenario_869"


library(devtools)
#install_github("msy", "einarhjorleifsson", ref = "master")

library(FLCore)
library(msy)
library(tidyverse)
library(Hmisc)
library(xtable)
library(scales)
library(gplots)
library(Cairo)
library(reshape2)
library(stringr)

#globals

#reference points (as set 2018 WKPELA)
Blim <- 34000
Bpa <- 54000
BtrigMP <- 61000   #prev MP trigger
Fpa <- 0.27
Flim <- 0.45
Fmsy <- 0.26
Ftgt <- 0.23    #prev MP target F



#Drive <- "D:"
#Assessment.Dir <- file.path(Drive,"Stocks","her.27.irls","Assessment")
#MSE.Dir <- file.path(Drive,"Stocks","her.27.irls","MSE","Rebuilding2018","her.27.irls.MSE2018.SimpSim")

## Source SimpSIM functions
#source(file.path(MSE.Dir,"Source","SimpSIM_v3.R"))
source(file.path(MSE.Dir,"Source","SimpSIM_CSH.R"))   #CSH version
source(file.path(MSE.Dir,"Source","SimpSIM_additional_Funcs.r"))
source(file.path(MSE.Dir,"Source","MSE_StatPlot_Funcs_2016.r"))

#Assessment upon which to base the initialisation/SRR fits
#WG <- "HAWG2015"
#WG <- "HAWG2016"
#WG <- "HAWG2017"
#WG <- "WKPELA18" #benchmark of January 2018
#WG <- "HAWG2018"  #Herring WG of March 2018
WG <- "HAWG2019"  #Herring WG of March 2018

#stock name
stockName <- "her-irls"

#statistical periods for reporting
lStatPer <- list()
#annual stats
for (y in seq(1980,2047)){
  lStatPer[[ac(y)]]<-c(y,y)
}
#Short, Medium and Long Term
lStatPer[['ST']] <- c(2017,2021)
lStatPer[['MT']] <- c(2022,2026)
lStatPer[['LT']] <- c(2027,2046)

#iterations to run
nits <- 1000
#nits <- 10
#years to project
nyr <- 35
#long term
#nyr <- 200


