#Simulation graphical output


#plotting functions
source(file = file.path(MSE.Dir,"Source","MSE_StatPlot_Funcs_2016.r"))
source(file = file.path(MSE.Dir,"Source","MSE_StatTable_Funcs.R"))

#load(file = file.path(MSE.Dir,"Results",paste0(OM,"_HCR",HCR,"_SimpSIM_STATS.Rdata")))

OM <- "OM1"


#runNames to plot
rNames <- c("her-irls")

#plot names 
rNamesPlot <- c("her-irls")

runDAT <- list()

#year ranges for plots
#full range (inlcuding historical)
allYears <- seq(1980,2047)
#projection period
projYears <- seq(2018,2047)


#for (runName in rNames) {
runName <- rNames[1]

###~~~~~~~~~~~~~
### Plot settings
savePlots <- T
fileFormat <- "png"      # "eps", "wmf", "jpg"      (need png from transparency)
## Plot size
wth <- 7#3.14     #3.14 = 8cm = fit two across in word doc for report.           NEED TO CHANGE CEX FOR THIS TO WORK
hght <- 7#3.14

###~~~~~~~~~~~~~
# Store data
pBlim <- list(); pBpa <- list(); pBpaPerc <- list(); pDrop <- list()
tCat  <- list(); tCatH  <- list(); tCatL  <- list()
tSSB  <- list(); tSSBH  <- list(); tSSBL  <- list()
tFbar  <- list(); tFbarH  <- list(); tFbarL  <- list()
tRec  <- list(); tRecH  <- list(); tRecL  <- list()
tIAV  <- list(); tIAVH  <- list(); tIAVL  <- list()
recARs <- matrix(NA,nrow=1, ncol=10,dimnames=list("AR1",c(1:10)))

###~~~~~~~~~~~~~
## Load workspace
bp1 <- BP1s <- 34000
bp2 <- BP2s <- 54000

## Create results directory
#setwd(resPath); shell("md CSrev")
# output folder
#tmpresPath <- paste(resPath,"\\CSrev\\",sep="")

#bp <- paste("BPs",bp1,bp2,sep="_")  
# Iterations
#it <- paste("Its",nits,sep="_")
# Number of years 
#nY <- paste("Yrs",nyr,sep="_")
#runIden <- paste(bp,it,nY,sep="_")

#load the results
#load(file = file.path(MSE.Dir,"Results",paste0(OM,"_HCR",HCR,"_SimpSIM_STATS.Rdata",sep="")))

DATA <- out$stats

#debug test
#fTabulateStats(1,DATA)

#results tabulation
lapply(seq_along(DATA), fTabulateStats, data=DATA)

# for (yrR in c("2020","2025", "2018-2022","2023-2027","2028-2047")) {
#   # P(SSB<Blim)
#   #if(bp2==BP2s[1]) pBlim[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   if(is.null(pBlim[[yrR]][[ac(bp1)]])) pBlim[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) pBlim[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["pBlim"]][["pt"]][,yrR]
#   # P(SSB<Bpa)
#   #if(bp2==BP2s[1]) pBpa[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   if(is.null(pBpa[[yrR]][[ac(bp1)]])) pBpa[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) pBpa[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["pBpa"]][["pt"]][,yrR]
#   # P(SSB<Bpa) PERCEIVED
#   if(is.null(pBpaPerc[[yrR]][[ac(bp1)]])) pBpaPerc[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) pBpaPerc[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["pBpaPerc"]][["pt"]][,yrR]
#   # Catch
#   #Median
#   if(is.null(tCat[[yrR]][[ac(bp1)]])) tCat[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tCat[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["Catch"]][["bw"]]["50%",yrR]
#   #2.5%
#   if(is.null(tCatL[[yrR]][[ac(bp1)]])) tCatL[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tCatL[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["Catch"]][["bw"]]["5%",yrR]
#   #97.5%
#   if(is.null(tCatH[[yrR]][[ac(bp1)]])) tCatH[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tCatH[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["Catch"]][["bw"]]["95%",yrR]
#   # SSB
#   #Median
#   if(is.null(tSSB[[yrR]][[ac(bp1)]])) tSSB[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tSSB[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["SSB"]][["bw"]]["50%",yrR]
#   #2.5%
#   if(is.null(tSSBL[[yrR]][[ac(bp1)]])) tSSBL[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tSSBL[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["SSB"]][["bw"]]["5%",yrR]
#   #97.5%
#   if(is.null(tSSBH[[yrR]][[ac(bp1)]])) tSSBH[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tSSBH[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["SSB"]][["bw"]]["95%",yrR]
#   # Fbar
#   #Median
#   if(is.null(tFbar[[yrR]][[ac(bp1)]])) tFbar[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tFbar[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["Fbar"]][["bw"]]["50%",yrR]
#   #2.5%
#   if(is.null(tFbarL[[yrR]][[ac(bp1)]])) tFbarL[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tFbarL[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["Fbar"]][["bw"]]["5%",yrR]
#   #97.5%
#   if(is.null(tFbarH[[yrR]][[ac(bp1)]])) tFbarH[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tFbarH[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["Fbar"]][["bw"]]["95%",yrR]
#   # Rec
#   #Median
#   if(is.null(tRec[[yrR]][[ac(bp1)]])) tRec[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tRec[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["Rec"]][["bw"]]["50%",yrR]
#   #2.5%
#   if(is.null(tRecL[[yrR]][[ac(bp1)]])) tRecL[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tRecL[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["Rec"]][["bw"]]["5%",yrR]
#   #97.5%
#   if(is.null(tRecH[[yrR]][[ac(bp1)]])) tRecH[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tRecH[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["Rec"]][["bw"]]["95%",yrR]
#   # IAV_abs
#   #Median
#   if(is.null(tIAV[[yrR]][[ac(bp1)]])) tIAV[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tIAV[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["TACchange_abs"]][["bw"]]["50%",yrR]
#   #2.5%
#   if(is.null(tIAVL[[yrR]][[ac(bp1)]])) tIAVL[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tIAVL[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["TACchange_abs"]][["bw"]]["5%",yrR]
#   #97.5%
#   if(is.null(tIAVH[[yrR]][[ac(bp1)]])) tIAVH[[yrR]][[ac(bp1)]] <- matrix(NA,nrow=length(BP2s), ncol=length(Fvals), dimnames=list(BP2s,Fvals))
#   for (fv in Fvals) tIAVH[[yrR]][[ac(bp1)]][ac(bp2),ac(fv)] <- DATA[[ac(fv)]][["TACchange_abs"]][["bw"]]["95%",yrR]
# }

###----------------------------------------------------------
# Other plots data
## Create results directory
# setwd(resPath); shell(paste("md Rec_AR_",bp1,sep=""))
# # output folder
# tmpresPath <- paste(resPath,"Rec_AR_",bp1,"\\",sep="")
# Colours
cols <- rich.colors(10)


###~~~~~~~~~~~~~
### Simulation settings
# F values to scan over (can be a single value) = value for F above all breakpoints
#Fvals <- seq(0.1,0.4,len=31)
#Fvals <- c(0.01,0.10,0.17,0.23,0.26,0.75)
#Fvals <- seq(0.1,0.32,by=0.01)
Fvals <- as.numeric(names(DATA))

numFs <- length(Fvals)
# F values to plot
FVs <- Fvals
# years
#pltYears <- years <- 1981:2050


#### F value(s) to plot for
#if (!is.na(FVs)) for (fv in FVs) {

#for (fv in FVs) {
for (fv in names(DATA)) {
    
  #fv <- FVs[1]
  DAT <- runDAT[[runName]] <- DATA[[ac(fv)]]
  
  wM <- 1
  wM2 <- 2
  
  cat(fv,"\n")
  
  #replace dec point with underscore
  fv.rep <- stringr::str_replace(fv,"\\.","_")
  
  #True SSB
  #worms
  tmp <- DAT[["SSB"]][["worm"]]
  #stats
  tmp2 <- DAT[["SSB"]][["val"]]
  
  #ylims <- c(0,ceiling(max(tmp[,,,,,wM],tmp2["95%",])/1000000)*1000000)
  ylims <- c(0,2e5/1e3)
  if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0("SSB_",runName,"_",OM,"_",HCR,"_",fv.rep,".",fileFormat)), 
                       type=fileFormat,units="in", height=hght,width=2*wth,bg="white",dpi=96,pointsize=12)
  
  #setup the plot area
  plot(allYears,tmp[,ac(allYears),,,,wM], main = paste0(runName,", ",OM," HCR",HCR,", F=",fv),
       type="n", lwd=2, col=cols[1], ylab="SSB (kt)", xlab="Year", 
       ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  #10 worm lines
  for (wm in seq(1,10)) lines(allYears,tmp[,ac(allYears),,,,wm]/1e3, lwd=1, col="grey")
  #5th-90th percentile limits
  polygon(c(allYears,rev(allYears)),c(tmp2["5%",ac(allYears)]/1e3,rev(as.numeric(tmp2["95%",ac(allYears)]/1e3))), col=alpha(cols[length(cols)],0.5), border=NA)
  #lines(years,tmp[,ac(years),,,,wM2], lwd=2, col=cols[2])
  #lines(years,tmp[,ac(years),,,,wM], lwd=2, col=cols[1])
  #median
  #lines(years,tmp2["50%",ac(years)], lwd=2, col=cols[length(cols)], ylab="Recruitment", xlab="", ylim=ylims, lty=2)
  lines(allYears,tmp2["50%",ac(allYears)]/1e3, lwd=2, col=cols[length(cols)], lty=1)
  #outlines
  for (rng in c("5%","95%")) lines(allYears,tmp2[rng,ac(allYears)]/1e3, lwd=2, col=cols[length(cols)], lty=3)
  #start of projection period
  abline(v=2018, lty=2)
  #Blim,Bpa
  abline(h=c(34,54), lty=2)
  
  if (savePlots) dev.off()
  
  #SSB CV
  ylims <- c(0,50)
  if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0("SSB_CV_",runName,"_",OM,"_",HCR,"_",fv.rep,".",fileFormat)), 
                       type=fileFormat,units="in", height=hght,width=2*wth,bg="white", pointsize=12,dpi=96)
  plot(projYears,100.0*tmp2["CV",ac(projYears)], main = paste0(runName,", ",OM," HCR",HCR,", F=",fv),
       type="l", lwd=2, col=cols[1], ylab="SSB CV (%)", xlab="Year", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  if (savePlots) dev.off()
  
  
    # Rec
  tmp <- DAT[["Rec"]][["worm"]]
  tmp2 <- DAT[["Rec"]][["val"]]
  #ylims <- c(0,ceiling(max(tmp[,,,,,wM],tmp2["95%",])/10000000)*10000000)
  ylims <- c(0,2e6/1e6)
  if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0("Rec_",runName,"_",OM,"_",HCR,"_",fv.rep,".",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth, bg="white",pointsize=12,dpi=96)
  #plot(years,tmp[,ac(years),,,,wM], main = paste0("Recruitment-",bp1,"_",bp2,"_F-",fv),
  #     type="l", lwd=2, col=cols[1], ylab="Recruitment", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  plot(allYears,tmp[,ac(allYears),,,,wM], main = paste0(runName,", ",OM," HCR",HCR,", F=",fv),
       type="n", lwd=2, col=cols[1], ylab="Recruitment (units?)", xlab="Year", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  #10 worm lines
  for (wm in seq(1,10)) lines(allYears,tmp[,ac(allYears),,,,wm]/1e6, lwd=1, col="grey")
  #5th-90th percentile limits
  polygon(c(allYears,rev(allYears)),c(tmp2["5%",ac(allYears)]/1e6,rev(as.numeric(tmp2["95%",ac(allYears)]/1e6))), 
          col=alpha(cols[length(cols)],0.5), border=NA)
  #lines(years,tmp[,ac(years),,,,wM2], lwd=2, col=cols[3])
  #lines(years,tmp[,ac(years),,,,wM], lwd=2, col=cols[1])
  #median
  #lines(years,tmp2["50%",ac(years)], lwd=2, col=cols[length(cols)], ylab="Recruitment", xlab="", ylim=ylims, lty=2)
  lines(allYears,tmp2["50%",ac(allYears)]/1e6, lwd=2, col=cols[length(cols)], lty=1)
  for (rng in c("5%","95%")) lines(allYears,tmp2[rng,ac(allYears)]/1e6, lwd=2, col=cols[length(cols)], lty=3)
  abline(v=2018, lty=2)
  if (savePlots) dev.off()
  
  
  # Risk to Blim
  tmp <- DAT[["pBlim"]][["val"]]
  #ylims <- c(0,max(ceiling(max(tmp)*10)/10,0.06))
  ylims <- c(0,0.5) #50% max
  if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0("pBlim_",runName,"_",OM,"_",HCR,"_",fv.rep,".",fileFormat)), 
                       type=fileFormat,units="in", height=hght,width=2*wth,bg="white", pointsize=12,dpi=96)
  #plot(years,tmp[,ac(years)], main = paste0("pBlim-",bp1,"_",bp2,"_F-",fv),
  #     type="n", lwd=2, col=cols[1], ylab="P(SSB<Blim)", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  plot(projYears,tmp["median",ac(projYears)], main = paste0(runName,", ",OM," HCR",HCR,", F=",fv),
       type="l", lwd=2, col=cols[1], ylab="P(SSB<Blim)", xlab="Year", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  #for (wm in c(5,10)) lines(years,tmp[,ac(years),,,,wm], lwd=2, col=cols[wm])
  #5% line
  abline(h=0.05, lty=2, col=2)
  #statistical periods
  polygon(x = c(lStatPer$ST[1]-0.5,lStatPer$ST[2]+0.5,lStatPer$ST[2]+0.5,lStatPer$ST[1]-0.5), y = c(0,0,0.5,0.5), col = alpha("red",0.1), border=NA)
  polygon(x = c(lStatPer$MT[1]-0.5,lStatPer$MT[2]+0.5,lStatPer$MT[2]+0.5,lStatPer$MT[1]-0.5), y = c(0,0,0.5,0.5), col = alpha("blue",0.1), border=NA)
  polygon(x = c(lStatPer$LT[1]-0.5,lStatPer$LT[2]+0.5,lStatPer$LT[2]+0.5,lStatPer$LT[1]-0.5), y = c(0,0,0.5,0.5), col = alpha("green",0.1), border=NA)
  
  if (savePlots) dev.off()
  
  
  # Risk to Bpa
  tmp <- DAT[["pBpa"]][["val"]]
  #ylims <- c(0,max(ceiling(max(tmp)*10)/10,0.06))
  ylims <- c(0,0.5) #max 50% risk
  if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0("pBpa_",runName,"_",OM,"_",HCR,"_",fv.rep,".",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth,bg="white", pointsize=12,dpi=96)
  #plot(years,tmp[,ac(years)], main = paste0("pBlim-",bp1,"_",bp2,"_F-",fv),
  #     type="n", lwd=2, col=cols[1], ylab="P(SSB<Blim)", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  plot(projYears,tmp["median",ac(projYears)], main = paste0(runName,", ",OM," HCR",HCR,", F=",fv),
       type="l", lwd=2, col=cols[1], ylab="P(SSB<Bpa)", xlab="Year", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  #5% line
  abline(h=0.05, lty=2, col=2)
  #statistical periods
  polygon(x = c(lStatPer$ST[1]-0.5,lStatPer$ST[2]+0.5,lStatPer$ST[2]+0.5,lStatPer$ST[1]-0.5), y = c(0,0,0.5,0.5), col = alpha("red",0.1), border=NA)
  polygon(x = c(lStatPer$MT[1]-0.5,lStatPer$MT[2]+0.5,lStatPer$MT[2]+0.5,lStatPer$MT[1]-0.5), y = c(0,0,0.5,0.5), col = alpha("blue",0.1), border=NA)
  polygon(x = c(lStatPer$LT[1]-0.5,lStatPer$LT[2]+0.5,lStatPer$LT[2]+0.5,lStatPer$LT[1]-0.5), y = c(0,0,0.5,0.5), col = alpha("green",0.1), border=NA)
  if (savePlots) dev.off()
  
  # Extinction proportion
  tmp <- DAT[["pExt"]][["val"]]
  ylims <- c(0,0.5) #max 50% risk
  if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0("pExt_",runName,"_",OM,"_",HCR,"_",fv.rep,".",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth,bg="white", pointsize=12,dpi=96)
  plot(projYears,tmp[ac(projYears)], main = paste0(runName,", ",OM," HCR",HCR,", F=",fv),
       type="l", lwd=2, col=cols[1], ylab="P Extinction", xlab="Year", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  #statistical periods
  polygon(x = c(lStatPer$ST[1]-0.5,lStatPer$ST[2]+0.5,lStatPer$ST[2]+0.5,lStatPer$ST[1]-0.5), y = c(0,0,0.5,0.5), col = alpha("red",0.1), border=NA)
  polygon(x = c(lStatPer$MT[1]-0.5,lStatPer$MT[2]+0.5,lStatPer$MT[2]+0.5,lStatPer$MT[1]-0.5), y = c(0,0,0.5,0.5), col = alpha("blue",0.1), border=NA)
  polygon(x = c(lStatPer$LT[1]-0.5,lStatPer$LT[2]+0.5,lStatPer$LT[2]+0.5,lStatPer$LT[1]-0.5), y = c(0,0,0.5,0.5), col = alpha("green",0.1), border=NA)
  if (savePlots) dev.off()
  
  #Applied F
  tmp <- DAT[["Fbar"]][["worm"]]
  tmp2 <- DAT[["Fbar"]][["val"]]
  ylims <- c(0,ceiling(max(tmp[,,,,,wM],tmp2["95%",])*10)/10)
  if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0("Fbar_",runName,"_",OM,"_",HCR,"_",fv.rep,".",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth,bg="white", pointsize=12,dpi=96)
  #plot(years,tmp[,ac(years),,,,wM], main = paste0("Fbar-",bp1,"_",bp2,"_F-",fv, sep=""),
  #     type="l", lwd=2, col=cols[1], ylab="Applied mean F (ages 3-7)", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  plot(allYears,tmp[,ac(allYears),,,,wM], main = paste0(runName,", ",OM," HCR",HCR,", F=",fv),
       type="n", lwd=2, col=cols[1], ylab="Applied mean F (ages ?)", xlab="Year", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  #10 worm lines
  #for (wm in c(5,10)) lines(years,tmp[,ac(years),,,,wm], lwd=2, col=cols[wm])
  for (wm in seq(1,10)) lines(allYears,tmp[,ac(allYears),,,,wm], lwd=1, col="grey")
  #5th-95th percentile limits
  polygon(c(allYears,rev(allYears)),c(tmp2["5%",ac(allYears)],rev(as.numeric(tmp2["95%",ac(allYears)]))), col=alpha(cols[length(cols)],0.5), border=NA)
  #lines(years,tmp[,ac(years),,,,wM2], lwd=2, col=cols[2])
  #lines(years,tmp[,ac(years),,,,wM], lwd=2, col=cols[1])
  #median
  #lines(years,tmp2["50%",ac(years)], lwd=2, col=cols[length(cols)], ylab="Recruitment", xlab="", ylim=ylims, lty=2)
  lines(allYears,tmp2["50%",ac(allYears)], lwd=2, col=cols[length(cols)], lty=1)
  for (rng in c("5%","95%")) lines(allYears,tmp2[rng,ac(allYears)], lwd=2, col=cols[length(cols)], lty=3)
  abline(v=2018, lty=2)
  if (savePlots) dev.off()
  
  # Catch
  tmp <- DAT[["Catch"]][["worm"]]
  tmp2 <- DAT[["Catch"]][["val"]]
  #ylims <- c(0,ceiling(max(tmp[,,,,,wM],tmp2["95%",])/1000000)*1000000)
  ylims <- c(0,30)
  #if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("Catch_wormMed-",bp1,"_",bp2,"_F-",fv,".",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth, pointsize=12,dpi=96)
  if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0("Catch_",runName,"_",OM,"_",HCR,"_",fv.rep,".",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth,bg="white", pointsize=12,dpi=96)
  #plot(years,tmp[,ac(years),,,,wM], main = paste0("Catch-",bp1,"_",bp2,"_F-",fv),
  #     type="l", lwd=2, col=cols[1], ylab="Catch (t)", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  plot(allYears,tmp[,ac(allYears),,,,wM]/1e3, main = paste0(runName,", ",OM," HCR",HCR,", F=",fv),
       type="n", lwd=2, col=cols[1], ylab="Catch (kt)", xlab="Year", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  #10 worm lines
  #for (wm in c(5,10)) lines(years,tmp[,ac(years),,,,wm], lwd=2, col=cols[wm])
  for (wm in seq(1,10)) lines(allYears,tmp[,ac(allYears),,,,wm]/1e3, lwd=1, col="grey")
  #5th-95th percentile limits
  polygon(c(allYears,rev(allYears)),c(tmp2["5%",ac(allYears)]/1e3,rev(as.numeric(tmp2["95%",ac(allYears)]/1e3))), col=alpha(cols[length(cols)],0.5), border=NA)
  #lines(years,tmp[,ac(years),,,,wM2], lwd=2, col=cols[2])
  #lines(years,tmp[,ac(years),,,,wM], lwd=2, col=cols[1])
  #median
  #lines(years,tmp2["50%",ac(years)], lwd=2, col=cols[length(cols)], ylab="Recruitment", xlab="", ylim=ylims, lty=2)
  lines(allYears,tmp2["50%",ac(allYears)]/1e3, lwd=2, col=cols[length(cols)], lty=1)
  for (rng in c("5%","95%")) lines(allYears,tmp2[rng,ac(allYears)]/1e3, lwd=2, col=cols[length(cols)], lty=3)
  abline(v=2018, lty=2)
  #abline(h=10.887, lty=2)
  #abline(h=4.742, lty=2)
  if (savePlots) dev.off()
  
  
  # TAC change
  # tmp <- DAT[["TACchange"]][["worm"]]
  # tmp2 <- DAT[["TACchange"]][["val"]]
  # ylims <- c(floor(min(tmp[,,,,,wM],tmp2["95%",])*10)/10,ceiling(max(tmp[,,,,,wM],tmp2["95%",])*10)/10)
  # if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("IAV_wormMed-",bp1,"_",bp2,"_F-",fv,".",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth, pointsize=12,dpi=96)
  # plot(years,tmp[,ac(years),,,,wM], main = paste("IAV-",bp1,"_",bp2,"_F-",fv),
  #      type="l", lwd=2, col=cols[1], ylab="TAC change", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
  # #for (wm in c(5,10)) lines(years,tmp[,ac(years),,,,wm], lwd=2, col=cols[wm])
  # polygon(c(years,rev(years)),c(tmp2["5%",ac(years)],rev(as.numeric(tmp2["95%",ac(years)]))), col=alpha(cols[length(cols)],0.5), border=NA)
  # lines(years,tmp[,ac(years),,,,wM2], lwd=2, col=cols[2])
  # lines(years,tmp[,ac(years),,,,wM], lwd=2, col=cols[1])
  # lines(years,tmp2["50%",ac(years)], lwd=2, col=cols[length(cols)], ylab="Recruitment", xlab="", ylim=ylims, lty=2)
  # for (rng in c("5%","95%")) lines(years,tmp2[rng,ac(years)], lwd=2, col=cols[length(cols)], lty=3)
  # abline(v=2016, lty=2)
  # if(length(grep("2025", runName))>1) abline(h=0.25, lty=2) else abline(h=0.20, lty=2)
  # abline(h=-0.2, lty=2)
  # if (savePlots) dev.off()
  
  # Catch density
  # remove until useful
  #tmp <- DAT[["CatDens"]]
  #tmp2 <- DAT[["Catch"]][["bw"]][,"2066-2115"]
  #xpastDens <- hist(DAT[["Catch"]][["val"]]["50%",ac(1981:2015)], plot=F, breaks=6)
  #if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("Catch_Density-",bp1,"_",bp2,"_F-",fv,".",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth, pointsize=12,dpi=96)
  #plot(c(0,tmp$mids,max(tmp$mids)),c(0,tmp$density,0), type="l", xlim=c(0,2e6),
  #     xlab="Catch", ylab="Density", main = paste0("Catch_Density-",bp1,"_",bp2,"_F-",fv), cex.lab=1.5, cex.axis=1.5)
  ##polygon(c(0,xpastDens$mids,max(xpastDens$mids)),c(0,xpastDens$density,0),col=alpha(3,0.5))
  #polygon(c(0,tmp$mids,max(tmp$mids)),c(0,tmp$density,0),col=alpha(2,0.5))
  #abline(v=tmp2["50%",], lty=2, lwd=2)
  #abline(v=tmp2["mean",], lty=3, lwd=2)
  #legend("topright", c("Median", "Average"), lty=c(2,3), lwd=2, bty="n")
  ##abline(v=tmp2["10%",])
  #if (savePlots) dev.off()
  
} # end of FVs loop


######################The Following Not Yet Updated For CSH###########################################


#   ### Applied vs target F
#   appFhigh <- appFLow <- appF <- c()
#   for (fv in Fvals) {
#     appF <- c(appF,DATA[[ac(fv)]][["Fbar"]][["bw"]]["50%","2066-2115"])
#     appFLow <- c(appFLow,DATA[[ac(fv)]][["Fbar"]][["bw"]]["5%","2066-2115"])
#     appFhigh <- c(appFhigh,DATA[[ac(fv)]][["Fbar"]][["bw"]]["95%","2066-2115"])
#   }
#   ylims <- c(0,ceiling(max(Fvals,appFhigh)*10)/10)
#   xlims <- c(0,ceiling(max(Fvals)*10)/10)
#   
#   # Applied F
#   if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("Fappl-",bp1,"_",bp2,".",fileFormat)), type=fileFormat,units="in", height=hght,width=wth, pointsize=12,dpi=96)
#   plot(Fvals,appF, main = paste0("Target_v_Applied_F-",bp1,"_",bp2, sep=""),
#        type="l", lwd=2, col=cols[length(cols)], ylab="Average applied F", xlab="Target F", ylim=ylims, xlim=xlims, lty=2)
#   polygon(c(Fvals,rev(Fvals)),c(appFLow,rev(appFhigh)), col=alpha(2,0.5), border=NA)
#   lines(Fvals,appF, lwd=2, col=cols[length(cols)], lty=2)
#   lines(Fvals,appFhigh, lwd=2, col=cols[length(cols)], lty=3)
#   lines(Fvals,appFLow, lwd=2, col=cols[length(cols)], lty=3)
#   lines(c(0,max(ylims)),c(0,max(ylims)), lty=2, lwd=1.5)
#   if (savePlots) dev.off()
#   
#   ### Tac drops
#   catDrop <- c()
#   for (fv in Fvals) catDrop <- c(catDrop,DATA[[ac(fv)]][["TACdrops30"]][["bw"]]["mean","2066-2115"])
#   ylims <- c(0,ceiling(max(catDrop)*10)/10)
#   #xlims <- c(0,ceiling(max(Fvals)*10)/10)
#   # Applied F
#   if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("CatchDrops-",bp1,"_",bp2,".",fileFormat)), type=fileFormat,units="in", height=hght,width=wth, pointsize=12,dpi=96)
#   plot(Fvals,catDrop, main = paste0(">50%_drops_in_catch-",bp1,"_",bp2),
#        type="l", lwd=2, col=cols[1], ylab="Probability of catch reductions >30%", xlab="Target F", ylim=ylims)#, xlim=xlims)
#   if (savePlots) dev.off()
#   
#   #} # end of BP loop
#   
#   
#   ### pBlim plots
#   univMax <- 0
#   for (yrR in c("2020","2025", "2016-2020","2021-2025","2066-2115")) univMax <- max(univMax,max(ceiling(max(pBlim[[yrR]][[ac(bp1)]])*100)/100,0.06, na.rm=T), na.rm=T)
#   yrR <- "2066-2115"
#   
#   if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("pBlimVsTarF_BlimTrig-",bp1,"_yrs-",yrR,".",fileFormat)), type=fileFormat,units="in", height=hght,width=wth, pointsize=12,dpi=96)
#   #ylims <- c(0,max(ceiling(max(pBlim[[yrR]][[ac(bp1)]])*100)/100,0.06)) 
#   ylims <- c(0,univMax) 
#   plot(Fvals,pBlim[[yrR]][[ac(bp1)]][ac(BP2s[1]),], ylim=ylims,
#        type="l", lwd=2, col=cols[1],
#        main = paste0("Blim_trig: ",bp1/1000000," mil. t; Year(s): ",yrR),
#        xlab="Target F", ylab="P(SSB<Blim)")
#   for (xx in 1:length(BP2s)) lines(Fvals,pBlim[[yrR]][[ac(bp1)]][ac(BP2s[xx]),], lwd=2, col=cols[xx])
#   abline(h=0.05, col=2, lty=2)
#   if (savePlots) dev.off()
#   
#   ### pBpa plots  ## USE PERCEIVED SSB
#   univMax <- 0
#   for (yrR in c("2020","2025", "2016-2020","2021-2025","2066-2115")) univMax <- max(univMax,max(ceiling(max(pBpaPerc[[yrR]][[ac(bp1)]])*100)/100,0.06, na.rm=T), na.rm=T)
#   yrR <- "2066-2115"
#   
#   if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("pBpaVsTarF_BlimTrig-",bp1,"_yrs-",yrR,".",fileFormat)), type=fileFormat,units="in", height=hght,width=wth, pointsize=12,dpi=96)
#   #ylims <- c(0,max(ceiling(max(pBpaPerc[[yrR]][[ac(bp1)]])*100)/100,0.06))
#   ylims <- c(0,univMax)
#   plot(Fvals,pBpaPerc[[yrR]][[ac(bp1)]][ac(BP2s[1]),], ylim=ylims,
#        type="l", lwd=2, col=cols[1],
#        main = paste0("Blim_trig: ",bp1/1000000," mil. t; Year(s): ",yrR),
#        xlab="Target F", ylab="P(perceived SSB<Bpa)")
#   for (xx in 1:length(BP2s)) lines(Fvals,pBpaPerc[[yrR]][[ac(bp1)]][ac(BP2s[xx]),], lwd=2, col=cols[xx])
#   if (savePlots) dev.off()
#   
#   
#   ### tCat plots
#   univMax <- 0
#   for (yrR in c("2020","2025", "2016-2020","2021-2025","2066-2115")) univMax <- max(univMax,max(ceiling(tCatH[[yrR]][[ac(bp1)]])*100, na.rm=T)/100, na.rm=T)
#   univMax <- ceiling(univMax/100000)*100 #converting to thousands too
#   
#   ltMax <- 0
#   ltMax <- max(ltMax,max(ceiling(tCat[["2066-2115"]][[ac(bp1)]])*100, na.rm=T)/100, na.rm=T)
#   
#   #for (bp1 in BP1s) for (yrR in c("2020","2025", "2016-2020","2021-2025","2066-2115")) {
#   yrR <- "2066-2115"
#   
#   if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("Catch_BlimTrig-",bp1,"_yrs-",yrR,".",fileFormat)), type=fileFormat,units="in", height=hght,width=wth, pointsize=12,dpi=96)
#   #ylims <- c(0,max(ceiling(max(pBlim[[yrR]][[ac(bp1)]])*100)/100,0.06)) 
#   ylims <- c(0,univMax) 
#   plot(Fvals,tCat[[yrR]][[ac(bp1)]][ac(BP2s[1]),]/1000, ylim=ylims,
#        type="l", lwd=2, col=cols[length(cols)],
#        main = paste0("Blim_trig: ",bp1/1000000," mil. t; Year(s): ",yrR),
#        xlab="Target F", ylab="Total Catch (kt)", lty=2)
#   polygon(c(Fvals,rev(Fvals)),c(tCatL[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]/1000,rev(tCatH[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]/1000)), lwd=0.1, col=alpha(2,0.5), border=NA)
#   lines(Fvals,tCat[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]/1000, lwd=2, col=cols[length(cols)], lty=2)
#   lines(Fvals,tCatL[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]/1000, lwd=2, col=cols[length(cols)], lty=3)
#   lines(Fvals,tCatH[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]/1000, lwd=2, col=cols[length(cols)], lty=3)
#   #abline(h=ltMax/1000, col=4, lty=2)
#   if (savePlots) dev.off()
#   
#   ### tIAV plots
#   univMax <- 0
#   for (yrR in c("2020","2025", "2016-2020","2021-2025","2066-2115")) univMax <- max(univMax,max(ceiling(tIAVH[[yrR]][[ac(bp1)]])*100, na.rm=T)/100, na.rm=T)
#   univMax <- ceiling(univMax/100000)*100 #converting to thousands too
#   
#   ltMax <- 0
#   ltMax <- max(ltMax,max(ceiling(tIAVH[["2066-2115"]][[ac(bp1)]]*10), na.rm=T)/10, na.rm=T)+0.1
#   
#   #for (bp1 in BP1s) for (yrR in c("2020","2025", "2016-2020","2021-2025","2066-2115")) {
#   yrR <- "2066-2115"
#   
#   if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("IAV_BlimTrig-",bp1,"_yrs-",yrR,".",fileFormat)), type=fileFormat,units="in", height=hght,width=wth, pointsize=12,dpi=96)
#   #ylims <- c(0,max(ceiling(max(pBlim[[yrR]][[ac(bp1)]])*100)/100,0.06)) 
#   ylims <- c(0,ltMax*100) 
#   plot(Fvals,tIAV[[yrR]][[ac(bp1)]][ac(BP2s[1]),]*100, ylim=ylims,
#        type="l", lwd=2, col=cols[length(cols)],
#        main = paste0("Blim_trig: ",bp1/1000000," mil. t; Year(s): ",yrR),
#        xlab="Target F", ylab="Interannual variation in Catch (%)", lty=2)
#   polygon(c(Fvals,rev(Fvals)),c(tIAVL[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]*100,rev(tIAVH[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]*100)), lwd=0.1, col=alpha(2,0.5), border=NA)
#   lines(Fvals,tIAV[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]*100, lwd=2, col=cols[length(cols)], lty=2)
#   lines(Fvals,tIAVL[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]*100, lwd=2, col=cols[length(cols)], lty=3)
#   lines(Fvals,tIAVH[[yrR]][[ac(bp1)]][ac(BP2s[xx]),]*100, lwd=2, col=cols[length(cols)], lty=3)
#   #abline(h=ltMax/1000, col=4, lty=2)
#   if (savePlots) dev.off()
#   

#  } #end of runName loop

# 
# 
# 
# ### ------------------------------------------------------------------------------------------------------
# ###  Median plots by run for F=????
# ### ------------------------------------------------------------------------------------------------------
# 
# ## Run Name
# #cols <- rich.colors(length(rNames)+1)
# cols <- rich.colors(length(rNames))
# 
# ###~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ### Stock development
# # Rec
# stat <- "Rec"
# ylims <- c(0,ceiling(max(runDAT[[1]][[stat]][["val"]]["50%",ac(pltYears)])/1000000)*1000000)
# if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("CSrev_Rec.",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth, pointsize=12,dpi=96)
# plot(pltYears,runDAT[[1]][[stat]][["val"]]["50%",ac(pltYears)], main="Recruitment",
#      type="l", lwd=2, col=cols[1], ylab="Recruitment", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
# for (fv in rev(rNames)) lines(pltYears,runDAT[[ac(fv)]][[stat]][["val"]]["50%",ac(pltYears)], lwd=2, col=cols[which(rNames==fv)])
# for (fv in rev(rNames)) lines(pltYears,runDAT[[ac(fv)]][[stat]][["val"]]["5%",ac(pltYears)], lwd=1, lty=2, col=cols[which(rNames==fv)])
# abline(v=2016, lty=2)
# legend("topleft",legend=c("Run:", rNamesPlot,"5%"), lty=c(NA,rep(1,length(rNames)),3), lwd=c(rep(3,1+length(rNames)),2), col=c(NA,cols,1), bty="n", cex=1.5)
# if (savePlots) dev.off()
# 
# # SSB
# stat <- "SSB"
# ylims <- c(0,ceiling(max(runDAT[[1]][[stat]][["val"]]["50%",ac(pltYears)])/100000)*100000)
# if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("CSrev_SSB.",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth, pointsize=12,dpi=96)
# plot(pltYears,runDAT[[1]][[stat]][["val"]]["50%",ac(pltYears)], main="SSB",
#      type="l", lwd=2, col=cols[1], ylab="SSB (t)", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
# for (fv in rev(rNames)) lines(pltYears,runDAT[[ac(fv)]][[stat]][["val"]]["50%",ac(pltYears)], lwd=2, col=cols[which(rNames==fv)])
# for (fv in rev(rNames)) lines(pltYears,runDAT[[ac(fv)]][[stat]][["val"]]["5%",ac(pltYears)], lwd=1, lty=2, col=cols[which(rNames==fv)])
# abline(v=2016, lty=2)
# abline(h=2250000, lty=3, col=2, lwd=1.5)   # 'Bpa' = second breakpoint. If appHCR=F, then this is used as the single breakpoint #def = 2250000
# abline(h=1500000, lty=2, col=2, lwd=1.5)   # 'Blim' = first breakpoint (only if appHCR is T) #def = 1500000
# legend("topleft",legend=c(c("Run:", rNamesPlot,"5%"),"Bpa","Blim"), lty=c(NA,rep(1,length(rNames)),3,3,2), lwd=c(rep(3,1+length(rNames)),2,2,2), col=c(NA,cols,1,2,2), bty="n", cex=1.5)
# if (savePlots) dev.off()
# 
# # Applied F
# stat <- "Fbar"
# ylims <- c(0,ceiling(max(runDAT[[1]][[stat]][["val"]]["50%",ac(pltYears)])))
# if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("CSrev_AppF.",fileFormat,sep="")), type=fileFormat,units="in", height=hght,width=2*wth, pointsize=12,dpi=96)
# plot(pltYears,runDAT[[1]][[stat]][["val"]]["50%",ac(pltYears)], main="Applied mean F",
#      type="l", lwd=2, col=cols[1], ylab="Mean F", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
# for (fv in rev(rNames)) lines(pltYears,runDAT[[ac(fv)]][[stat]][["val"]]["50%",ac(pltYears)], lwd=2, col=cols[which(rNames==fv)])
# for (fv in rev(rNames)) lines(pltYears,runDAT[[ac(fv)]][[stat]][["val"]]["5%",ac(pltYears)], lwd=1, lty=2, col=cols[which(rNames==fv)])
# abline(v=2016, lty=2)
# abline(h=0.32, lty=2, col=3)
# legend("topleft",legend=c("Run:", rNamesPlot,"5%","Fmsy"), lty=c(NA,rep(1,length(rNames)),3,2), lwd=c(rep(3,1+length(rNames)),2,2), col=c(NA,cols,1,3), bty="n", cex=1.5)
# if (savePlots) dev.off()
# 
# # Catch
# stat <- "Catch"
# ylims <- c(0,ceiling(max(runDAT[[1]][[stat]][["val"]]["50%",ac(pltYears)])/100000)*100000)
# if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("CSrev_Catch.",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth, pointsize=12,dpi=96)
# plot(pltYears,runDAT[[1]][[stat]][["val"]]["50%",ac(pltYears)], main="Catch",
#      type="l", lwd=2, col=cols[1], ylab="Catch (t)", xlab="", ylim=ylims, cex.lab=1.5, cex.axis=1.5)
# for (fv in rev(rNames)) lines(pltYears,runDAT[[ac(fv)]][[stat]][["val"]]["50%",ac(pltYears)], lwd=2, col=cols[which(rNames==fv)])
# for (fv in rev(rNames)) lines(pltYears,runDAT[[ac(fv)]][[stat]][["val"]]["5%",ac(pltYears)], lwd=1, lty=2, col=cols[which(rNames==fv)])
# abline(v=2016, lty=2)
# legend("topleft",legend=c("Run:", rNamesPlot,"5%"), lty=c(NA,rep(1,length(rNames)),3), lwd=c(rep(3,1+length(rNames)),2), col=c(NA,cols,1), bty="n", cex=1.5)
# if (savePlots) dev.off()
# 
# # Catch density
# if (savePlots) Cairo(file = file.path(MSE.Dir,"Results",runName,paste0("CSrev_CatchDens.",fileFormat)), type=fileFormat,units="in", height=hght,width=2*wth, pointsize=12,dpi=96)
# plot(c(0,runDAT[[1]][["CatDens"]]$mids,max(runDAT[[1]][["CatDens"]]$mids)),c(0,runDAT[[1]][["CatDens"]]$density,0), type="l", xlim=c(0,2e6), ylim=c(0,10e-7),
#      xlab="Catch", ylab="Density", main="Catch Density", cex.lab=1.5, cex.axis=1.5, col=cols[1])
# #for (fv in rev(rNames)) polygon(c(0,runDAT[[fv]][["CatDens"]]$mids,max(runDAT[[fv]][["CatDens"]]$mids)),c(0,runDAT[[fv]][["CatDens"]]$density,0),col=alpha(cols[which(rNames==fv)],0.5))
# for (fv in rev(rNames)) {
#   lines(c(0,runDAT[[fv]][["CatDens"]]$mids,max(runDAT[[fv]][["CatDens"]]$mids)),c(0,runDAT[[fv]][["CatDens"]]$density,0),col=cols[which(rNames==fv)], lwd=3)
#   abline(v=runDAT[[fv]][["Catch"]][["bw"]][,"2066-2115"]["50%",], lty=2, lwd=2, col=cols[which(rNames==fv)])
#   abline(v=runDAT[[fv]][["Catch"]][["bw"]][,"2066-2115"]["mean",], lty=3, lwd=2, col=cols[which(rNames==fv)])
#   #  abline(v=runDAT[[fv]][["Catch"]][["bw"]][,"2066-2115"]["10%",], lty=4, lwd=2, col=cols[which(rNames==fv)])
# }
# #legend("topright", c("Median", "Average"), lty=c(2,3), lwd=2, bty="n")
# legend("topright", rNamesPlot, lty=1, lwd=2, bty="n", col=cols)
# if (savePlots) dev.off()
# 
# #############END##########################