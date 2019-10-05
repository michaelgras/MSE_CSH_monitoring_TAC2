#SRR fits


## load stock object, quick look
load(file = file.path(MSE.Dir,"Data",paste0(stockName,"_FLStock_",WG,".Rdata")))

## define reference points here
refPts <- list()
refPts[["Fpa"]] <- Fpa
refPts[["Flim"]] <- Flim
refPts[["Fmsy"]] <- Fmsy
refPts[["Bpa"]] <- Bpa
refPts[["Blim"]] <- Blim
refPts[["BmsyTrig"]] <- NA  
refPts[["Bloss"]] <- min(ssb(stock))

minYear <- range(stock)["minyear"]
maxYear <- range(stock)["maxyear"]

## Stock-Recruit Models
#segmented regression, breakpoint at Bloss
SegregBloss  <- function(ab, ssb) log(ifelse(ssb >= refPts[["Bloss"]], ab$a * refPts[["Bloss"]], ab$a * ssb))
#appMods: "segreg","ricker", "bevholt"; or specials: "SegregBloss" (breakpt. Bloss) or runifSR (random uniform of recent values)

#various fits
FIT.SegRegBloss <- eqsr_fit(window(stock,1970,maxYear), nsamp = nits, remove.years=c(maxYear-1,maxYear), models = c("SegregBloss"))
FIT.Ricker <- eqsr_fit(window(stock,1970,maxYear), nsamp = nits, remove.years=c(maxYear-1,maxYear), models = c("Ricker"))
FIT.SegReg <- eqsr_fit(window(stock,1970,maxYear), nsamp = nits, remove.years=c(maxYear-1,maxYear), models = c("Segreg"))
FIT.BevHolt <- eqsr_fit(window(stock,1970,maxYear), nsamp = nits, remove.years=c(maxYear-1,maxYear), models = c("Bevholt"))
FIT.All <- eqsr_fit(window(stock,1970,maxYear), nsamp = nits, remove.years=c(maxYear-1,maxYear), models = c("Ricker","Bevholt","Segreg"))
FIT.All2 <- eqsr_fit(window(stock,1970,maxYear), nsamp = nits, remove.years=c(maxYear-1,maxYear), models = c("Ricker","Bevholt","Segreg","SegregBloss"))

# save stock-recruit model proportions and parameters
#write.csv(FIT.SegRegBloss$sr.det, file = file.path(MSE.Dir,"Results",paste0("EqSim_",stockName,"_",WG,"_SegRegBloss_SRR_pars.csv")))
#write.csv(FIT.Ricker$sr.det, file = file.path(MSE.Dir,"Results",paste0("EqSim_",stockName,"_",WG,"_Ricker_SRR_pars.csv")))
#write.csv(FIT.SegReg$sr.det, file = file.path(MSE.Dir,"Results",paste0("EqSim_",stockName,"_",WG,"_SegReg_SRR_pars.csv")))
#write.csv(FIT.BevHolt$sr.det, file = file.path(MSE.Dir,"Results",paste0("EqSim_",stockName,"_",WG,"_BevHolt_SRR_pars.csv")))
#write.csv(FIT.All$sr.det, file = file.path(MSE.Dir,"Results",paste0("EqSim_",stockName,"_",WG,"_All_SRR_pars.csv")))
#write.csv(FIT.All2$sr.det, file = file.path(MSE.Dir,"Results",paste0("EqSim_",stockName,"_",WG,"_All2_SRR_pars.csv")))

# Plot raw SRR results
#eqsr_plot(FIT.SegRegBloss,n=2e4)
#eqsr_plot(FIT.Ricker,n=2e4)
#eqsr_plot(FIT.SegReg,n=2e4)
#eqsr_plot(FIT.BevHolt,n=2e4)
#eqsr_plot(FIT.All,n=2e4)
#eqsr_plot(FIT.All2,n=2e4)

#eqsr_plot(FIT.SR_Blim,n=2e4)

#save(FIT.SegRegBloss, FIT.Ricker, FIT.SegReg, FIT.BevHolt, FIT.All, FIT.All2, file=file.path(MSE.Dir,"Data","SRR.RData"))
