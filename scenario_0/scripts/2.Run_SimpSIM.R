# Code to run 'SimpSIM' for CS Herring
# Adapted from David Millar's code for Blue Whiting


## load stock object
#load(file = file.path(MSE.Dir,"Data",paste0(stockName,"_FLStock_",WG,".Rdata")))

# F values to scan over (can be a single value) = value for F above all breakpoints
#Fvals <- c(0.01,0.1,0.12,0.15,0.20,0.26,0.45)
Fvals <- c(0.0001,0.26)

## define reference points here
refPts <- list()
refPts[["Fpa"]] <- Fpa
refPts[["Flim"]] <- Flim
refPts[["Fmsy"]] <- Fmsy
refPts[["Bpa"]] <- Bpa
refPts[["Blim"]] <- Blim
refPts[["BmsyTrig"]] <- NA  

#runame, HCR settings
runName <- stockName

HCR <- 1    #baseline run, no HCR applied, no min TAC
#HCR <- 1.05    #no HCR, min TAC = 500 <-

#runIden <- paste0("HCR",ac(HCR))

#operating model
OM <- "OM1"    #segregbloss stock recruit
#runIden <- "OM2"    #segreg
#runIden <- "OM3"    #ricker, bevholt, segreg

#defaults
refPts[["BmsyTrig"]] <- NA        #no trigger point
minTAC <- 0                       #no minimum TAC
TACchng <- NA                     #no TAC change limits

#HCR characteristics
#SSB breakpoint
if (ac(HCR) %in% c("1","1.0","1.1","1.15","1.2","1.25","1.3","1.35","1.4","10")) refPts[["BmsyTrig"]] <- NA
if (ac(HCR) %in% c("2","2.0","2.1")) refPts[["BmsyTrig"]] <- Blim
if (ac(HCR) %in% c("3","3.0","3.1")) refPts[["BmsyTrig"]] <- Bpa
if (ac(HCR) %in% c("4","4.0","4.1")) refPts[["BmsyTrig"]] <- 61000

#minimum TAC
if (ac(HCR) %in% c("1.05")) minTAC <- 868
if (ac(HCR) %in% c("1.1")) minTAC <- 1000
if (ac(HCR) %in% c("1.15")) minTAC <- 1500
if (ac(HCR) %in% c("1.2")) minTAC <- 2000
if (ac(HCR) %in% c("1.25")) minTAC <- 2500
if (ac(HCR) %in% c("1.3")) minTAC <- 3000
if (ac(HCR) %in% c("1.35")) minTAC <- 3500
if (ac(HCR) %in% c("1.4")) minTAC <- 4000

if (ac(HCR) %in% c("3.1")) minTAC <- 2000


#Starting F (intermediate year F)
#firstF <- 0.41 # From STF F2018

#latest assessment year
assyearNum <- 2018

#Trim off last year of the stock object (why??)
minYear <- range(stock)["minyear"]
maxYear <- range(stock)["maxyear"]
origstock <- stock

#set the Bloss reference point
refPts[["Bloss"]] <- min(ssb(stock))

## Stock-Recruit Models (see 1.3.SRRFit.R)
#load(file=file.path(MSE.Dir,"Data","SRR.RData"))

# Starting population numbers
#read from file of generated pops (see 1.1.GeneratePops.R)
dfstartN <- read_delim(file = file.path(MSE.Dir,"Data","CSH_MCMC_10k.dat"),
                       delim=" ", col_names = FALSE, trim_ws = TRUE)

#Weights and selectivity
#Number of years for averaging when calculating weights
#biology
numAvgYrsB <- 3
#selection
numAvgYrsS <- 3

# resample from years (FALSE) or use average for all (TRUE)
constantVals <- TRUE 

#Observation Model - Assessmnt Forecast errors
#cvF  <- 0;	phiF <-	0; cvSSB <- 0; phiSSB <- 0
cvF <- 0.57; phiF <- 0.10; cvSSB <- 0.54; phiSSB <- 0.36    #31/01/2019 (see STF_History.R)

#RUN SIMULATIONS
if (OM=="OM1"){FIT <- FIT.SegRegBloss}

#flag for application of autocorrelation (TRUE=Yes)
#autocorrelation was shown to be significant in RP estimation 
recAR <- TRUE

#Maximum residual
#this is the residual corresponding to the maximum estimated recruitment
#this is used to potentially trim out unreasonably high recruitments in the simulation
#this is mirrored when trimming (i.e. [maxRecRes,-maxRecRes])
maxRecRes <- 1.5 # Default in SimpSIM is 3

SIM <- SimpSIM(fit = FIT,
               dfstartN = dfstartN,
               intF = firstF,
               bio.years = c(assyearNum-(numAvgYrsB-1), assyearNum),
               bio.const = constantVals,
               sel.years = c(assyearNum-(numAvgYrsB-1), assyearNum),
               sel.const = constantVals,
               Fcv = cvF,
               Fphi = phiF,
               SSBcv = cvSSB,
               SSBphi = phiSSB,
               rhologRec = recAR,
               Btrigger = refPts[["BmsyTrig"]],
               Blim = refPts[["Blim"]],
               Bpa = refPts[["Bpa"]],
               TACchange = NA,
               chngB = 0,
               catTac = 1,
               Fscan = Fvals,
               recruitment.trim = c(maxRecRes,-maxRecRes),
               Nrun = nyr,
               process.error = TRUE,
               verbose = TRUE,
               HCR = HCR,
               minTAC = minTAC)

#annualYield <- colSums(SIM$simStks[["0.15"]]$C * SIM$simStks[["0.15"]]$catW)
#
#Y2020 <- annualYield[4,]
#table(Y2020<4000)
#hist(Y2020, breaks=seq(0,50000,1000))
#
#Y2021 <- annualYield[5,]
#table(Y2021<4000)
#hist(Y2021, breaks=seq(0,50000,1000))
#
#Y2022 <- annualYield[6,]
#table(Y2022<4000)
#hist(Y2022, breaks=seq(0,50000,1000))
#
#Y2023 <- annualYield[7,]
#table(Y2023<4000)
#hist(Y2023, breaks=seq(0,50000,1000))
#
#
####~~~~~~~~~~~~~
#
## To test the SimpSim function line by line
#fit = FIT   # Stock recruitment relationship done in 1.3
#dfstartN = dfstartN # Numbers at age for the initial year
#intF = firstF # fbar for the initial year
#bio.years = c(assyearNum-(numAvgYrsB-1), assyearNum) # 2015 2017
#bio.const = constantVals # Resample from years or use average for all 
#sel.years = c(assyearNum-(numAvgYrsB-1), assyearNum) # 2015 2017
#sel.const = constantVals
#Fcv = cvF # cv for F
#Fphi = phiF # phi for F
#SSBcv = cvSSB # CV for SSB
#SSBphi = phiSSB # phi for SSB
#rhologRec = recAR # Take into account autocorrelation
#Btrigger = refPts[["BmsyTrig"]]
#Blim = refPts[["Blim"]]
#Bpa = refPts[["Bpa"]]
#TACchange = NA # I guess that is used in the implementation model to 
#chngB = 0
#catTac = 1
#Fscan = Fvals # Various F values to be tested
#recruitment.trim = c(maxRecRes,-maxRecRes) # trims recruitment 
#Nrun = nyr # number of years that will be kept ????
#process.error = TRUE # process error is applied to SRR mainly. 
#verbose = TRUE 
#HCR = HCR # HCR that was chosen earlier on 
#minTAC = minTAC # Gives a min TAC to use
#
#save.image(file = file.path(MSE.Dir,"Results",runName,paste0(OM,"_HCR",HCR,"_SimpSIM_Workspace.Rdata")))
#