#calculate statistical outputs for SimpSIM output


runName <- "her-irls"
OM <- "OM1"


print(paste(runName,OM,HCR,sep=","))

#load(file = file.path(MSE.Dir,"Results",paste0(OM,"_HCR",HCR,"_SimpSIM_Workspace.Rdata")))
  
SimRuns <- SIM$simStks

## Percentiles
lastYr <- assyearNum + (nyr-1)
#years <- 1981:lastYr #Change to assYear+x (nyr)
#years <- 1970:lastYr
#percentiles = c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975)
numWorm <- 10
  
### ------------------------------------------------------------------------------------------------------
### Create stock objects for length of simulation
# Stored in a list (one stock object per F value)
Stocks <- list()
stockTemplate <- window(stock, end=lastYr)
mat(stockTemplate)[,ac(assyearNum:lastYr)] <- mat(stockTemplate)[,ac(assyearNum-1)]
m(stockTemplate)[,ac(assyearNum:lastYr)] <- m(stockTemplate)[,ac(assyearNum-1)]
m.spwn(stockTemplate)[,ac(assyearNum:lastYr)] <- m.spwn(stockTemplate)[,ac(assyearNum-1)]
harvest.spwn(stockTemplate)[,ac(assyearNum:lastYr)] <- harvest.spwn(stockTemplate)[,ac(assyearNum-1)]
#stock.n(stockTemplate)[,ac(assyearNum)] <- strtNum

# simStks[[ac(Fbar)]] <- list()
for (ii in Fvals) {
  cat(ii,"\n")
	Stocks[[ac(ii)]] <- propagate(stockTemplate,nits)  
  stock.n(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$N[,,]
  harvest(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$F[,,]
  catch.n(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$C[,,]
  catch.wt(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$catW[,,]
  landings.n(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$L[,,]
  
  #AC - why are stock weights set equal to catch weights???
  #stock.wt(Stocks[[ac(ii)]]) <- discards.wt(Stocks[[ac(ii)]]) <- landings.wt(Stocks[[ac(ii)]]) <- catch.wt(Stocks[[ac(ii)]])
  stock.wt(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$stkW[,,]
  discards.wt(Stocks[[ac(ii)]]) <- catch.wt(Stocks[[ac(ii)]])
  landings.wt(Stocks[[ac(ii)]]) <- catch.wt(Stocks[[ac(ii)]])
  
  discards.n(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- 0
  #for (y in (assyearNum):lastYr) stock.wt(Stocks[[ac(ii)]])[,ac(y),,,,] <- (SimRuns[[ac(ii)]]$stkW[,SimRuns[[ac(ii)]]$stkWran[y-assyearNum+2,]])[,]
  # add up totals
  catch(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)] <- apply(catch.n(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)]*catch.wt(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)],2:6,sum)
  landings(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)] <- apply(landings.n(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)]*landings.wt(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)],2:6,sum)
  discards(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)] <- apply(discards.n(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)]*discards.wt(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)],2:6,sum)
} # end of Fvals loop
  

DATA <- list()

  #loop over fishing mortality values
  for (jj in Fvals) {
    
    cat(jj,"\n")
    
    #percStats <- list()
    CSH_Stats <- list()     #CSH statistics
    
    CSH_Stats[["OM"]] <- OM
    CSH_Stats[["HCR"]] <- HCR
      
    #select the stock object for this F value
    STK <- Stocks[[ac(jj)]]
    
    ## Spawner Stock Biomass (SSB)
    cat("SSB\n")
    statName <- "SSB"
    stat <- ssb(STK)
    #cat(stat[,'2017',,,,1],"\n")
    CSH_Stats[[statName]][["val"]] <- CSHstatPercs(stat, lStatPer=lStatPer)
    CSH_Stats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
    
    #percStats[[statName]][["val"]] <- statPercs(stat, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat, percentiles) 
    
    ## Perceived SSB
    statName <- "SSBdevs"
    stat2 <- SimRuns[[ac(jj)]][["percSSB"]]
    
    #initialise object
    #percStats[[statName]][["worm"]] <- percStats[["SSB"]][["worm"]][,ac((assyearNum-2):lastYr)]  #SSB
    #dimnames(percStats[[statName]][["worm"]])[[2]] <- c("SSBcv","PhiSSB","STDDevs",ac((assyearNum+1):lastYr)) 
    CSH_Stats[[statName]][["worm"]] <- CSH_Stats[["SSB"]][["worm"]][,ac((assyearNum-2):lastYr)]
    #first 3 places for CV, Phi, SD of deviations
    dimnames(CSH_Stats[[statName]][["worm"]])[[2]] <- c("SSBcv","PhiSSB","STDDevs",ac((assyearNum+1):lastYr)) 
    
    #for each worm...calculate annual deviations between observed SSB and true SSB, the standard deviation of these deviations, autocorrelation and cv
    for (iw in 1:numWorm) {
      CSH_Stats[[statName]][["worm"]][,ac((assyearNum+1):lastYr),,,,iw] <- log(stat2[-1,iw])-log(stat[,ac((assyearNum+1):lastYr),,,,iw])
      #percStats[[statName]][["worm"]][,ac((assyearNum+1):lastYr),,,,iw] <- log(stat2[-1,iw])-log(stat[,ac((assyearNum+1):lastYr),,,,iw])
      CSH_Stats[[statName]][["worm"]][,"STDDevs",,,,iw] <- sd(CSH_Stats[[statName]][["worm"]][,ac((assyearNum+1):lastYr),,,,iw])
      #percStats[[statName]][["worm"]][,"STDDevs",,,,iw] <- sd(percStats[[statName]][["worm"]][,ac((assyearNum+1):lastYr),,,,iw]) 
      CSH_Stats[[statName]][["worm"]][,"PhiSSB",,,,iw] <- acf(CSH_Stats[[statName]][["worm"]][,ac((assyearNum+1):lastYr),,,,iw], plot=F)$acf[2] 
      #percStats[[statName]][["worm"]][,"PhiSSB",,,,iw] <- acf(percStats[[statName]][["worm"]][,ac((assyearNum+1):lastYr),,,,iw], plot=F)$acf[2] 
      CSH_Stats[[statName]][["worm"]][,"SSBcv",,,,iw] <- as.numeric(CSH_Stats[[statName]][["worm"]][,"STDDevs",,,,iw]*((1-CSH_Stats[[statName]][["worm"]][,"PhiSSB",,,,iw]^2)^0.5))
      #percStats[[statName]][["worm"]][,"SSBcv",,,,iw] <- as.numeric(percStats[[statName]][["worm"]][,"STDDevs",,,,iw]*((1-percStats[[statName]][["worm"]][,"PhiSSB",,,,iw]^2)^0.5))
    }
    
    
    cat("Risks\n")
    #probability that SSB is below Blim - P(SSB<Blim)
    
    #initialise
    #percStats[["pBlim"]][["val"]] <- stat[,,,,,1]
    #percStats[["pBlim"]][["val"]][] <- NA

    CSH_Stats[["pBlim"]][["val"]] <- CSHstatRisk(SSB = stat, RP = Blim, lStatPer = lStatPer)
    CSH_Stats[["pBpa"]][["val"]] <- CSHstatRisk(SSB = stat, RP = Bpa, lStatPer = lStatPer)
    CSH_Stats[["pExt"]][["val"]] <- CSHstatExtinct(SSB = stat, depletion=0.01, firstYear = assyearNum)
    
    #also complete for observed SSB (see code below for bw implementation - involves a shift of 1 year?)
    
    #annual values
    #for (yy in years) percStats[["pBlim"]][["val"]][,ac(yy)] <- sum(as.numeric(stat[,ac(yy)])<Blim)/nits
    #for (yy in years) CSH_Stats[["pBlim"]][["val"]][,ac(yy)] <- sum(as.numeric(stat[,ac(yy)])<Blim)/nits
    
    #stats in 2020, 2025, max from the period 2018-2022 (next 5 years), 2018-2027 (following 10 years) and 2018-2047 (following 20 years)
    #xx <- FLQuant(NA, dimnames = list(age=c("pBlim"), year=c("2020","2025", "2018-2022","2023-2027","2028-2047")))
    #xx[,"2020"]      <- percStats[["pBlim"]][["val"]][,"2020"]
    #xx[,"2025"]      <- percStats[["pBlim"]][["val"]][,"2025"]
    #xx[,"2018-2022"] <- max(percStats[["pBlim"]][["val"]][,ac(2018:2022)])
    #xx[,"2023-2027"] <- max(percStats[["pBlim"]][["val"]][,ac(2023:2027)])
    #xx[,"2028-2047"] <- max(percStats[["pBlim"]][["val"]][,ac(2028:2047)])
    #percStats[["pBlim"]][["pt"]] <- xx
    
    
    #and for Bpa
    ## P(SSB<Bpa)
    #percStats[["pBpa"]][["val"]] <- stat[,,,,,1]
    #percStats[["pBpa"]][["val"]][] <- NA
    #for (yy in years) percStats[["pBpa"]][["val"]][,ac(yy)] <- sum(as.numeric(stat[,ac(yy)])<Bpa)/nits
    #xx <- FLQuant(NA, dimnames = list(age=c("pBlim"), year=c("2020","2025", "2018-2022","2023-2027","2028-2047")))
    #xx[,"2020"]      <- percStats[["pBpa"]][["val"]][,"2020"]
    #xx[,"2025"]      <- percStats[["pBpa"]][["val"]][,"2025"]
    #xx[,"2018-2022"] <- max(percStats[["pBpa"]][["val"]][,ac(2018:2022)])
    #xx[,"2023-2027"] <- max(percStats[["pBpa"]][["val"]][,ac(2023:2027)])
    #xx[,"2028-2047"] <- max(percStats[["pBpa"]][["val"]][,ac(2028:2047)])
    #percStats[["pBpa"]][["pt"]] <- xx
    
    
    
    #Now using the observed SSB
    #P(SSB<Bpa) PERCEIVED
    #stat3 <- stat[,ac((assyearNum+1):lastYr)]
    #stat3[] <- NA
    #for (ii in 1:nits) stat3[,,,,,ii] <- stat2[-1,ii]
    
    #percStats[["pBpaPerc"]][["val"]] <- stat3[,,,,,1]
    #percStats[["pBpaPerc"]][["val"]][] <- NA
    #for (yy in c(2019:2047)) percStats[["pBpaPerc"]][["val"]][,ac(yy)] <- sum(as.numeric(stat3[,ac(yy)])<Bpa)/nits
    #xx <- FLQuant(NA, dimnames = list(age=c("pBlim"), year=c("2020","2025", "2018-2022","2023-2027","2028-2047")))
    #xx[,"2020"]      <- percStats[["pBpaPerc"]][["val"]][,"2020"]
    #xx[,"2025"]      <- percStats[["pBpaPerc"]][["val"]][,"2025"]
    #xx[,"2018-2022"] <- max(percStats[["pBpaPerc"]][["val"]][,ac(2019:2020)])
    #xx[,"2023-2027"] <- max(percStats[["pBpaPerc"]][["val"]][,ac(2023:2027)])
    #xx[,"2028-2047"] <- max(percStats[["pBpaPerc"]][["val"]][,ac(2028:2047)])
    #percStats[["pBpaPerc"]][["pt"]] <- xx
    
    
    
    #Recruitment
    cat("Recruitment\n")
    statName <- "Rec"
    stat <- rec(STK)
    CSH_Stats[[statName]][["val"]] <- CSHstatPercs(stat, lStatPer=lStatPer)
    CSH_Stats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
    #percStats[[statName]][["val"]] <- statPercs(stat, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat, percentiles) 
    
    #Selection
    cat("Selection\n")
    statName <- "Sel"
    stat <- harvest(STK)
    #cat(stat[,'2017',,,,1],"\n")
    CSH_Stats[[statName]][["val"]] <- CSHstatPercs(stat, lStatPer=lStatPer)
    CSH_Stats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]

    #stock weights
    cat("SW\n")
    statName <- "SW"
    stat <- stock.wt(STK)
    CSH_Stats[[statName]][["val"]] <- CSHstatPercs(stat, lStatPer=lStatPer)
    CSH_Stats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]

    #catch weights
    cat("CW\n")
    statName <- "CW"
    stat <- catch.wt(STK)
    CSH_Stats[[statName]][["val"]] <- CSHstatPercs(stat, lStatPer=lStatPer)
    CSH_Stats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
    
    #FBar
    cat("Fbar\n")
    statName <- "Fbar"
    stat <- fbar(STK)
    #cat(stat[,'2017',,,,1],"\n")
    CSH_Stats[[statName]][["val"]] <- CSHstatPercs(stat, lStatPer=lStatPer)
    CSH_Stats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
    #percStats[[statName]][["val"]] <- statPercs(stat, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat, percentiles) 
    
    #TO DO 
    ## Perceived F
    #statName <- "Fdevs"
    #stat2 <- SimRuns[[ac(jj)]][["intendF"]]
    #percStats[[statName]][["worm"]] <- percStats[["Fbar"]][["worm"]][,ac((assyearNum-2):lastYr)]  #MeanF
    #dimnames(percStats[[statName]][["worm"]])[[2]] <- c("Fcv","PhiF","STDDevs",ac((assyearNum+1):lastYr)) 
    
    #for (iw in 1:numWorm) {
    #  percStats[[statName]][["worm"]][,ac((assyearNum+1):(lastYr-1)),,,,iw] <- log(stat2[1:length(ac((assyearNum+1):(lastYr-1))),iw])-log(stat[,ac((assyearNum+1):(lastYr-1)),,,,iw])
    #  percStats[[statName]][["worm"]][,"STDDevs",,,,iw] <- sd(percStats[[statName]][["worm"]][,ac((assyearNum+1):(lastYr-1)),,,,iw]) 
    #  percStats[[statName]][["worm"]][,"PhiF",,,,iw] <- acf(percStats[[statName]][["worm"]][,ac((assyearNum+1):(lastYr-1)),,,,iw], plot=F)$acf[2] 
    #  percStats[[statName]][["worm"]][,"Fcv",,,,iw] <- as.numeric(percStats[[statName]][["worm"]][,"STDDevs",,,,iw]*((1-percStats[[statName]][["worm"]][,"PhiF",,,,iw]^2)^0.5))
    #}
    
    #yield
    cat("Yield\n")
    statName <- "Catch"
    stat <- catch(STK)
    #cat(stat[,'2017',,,,1],"\n")
    CSH_Stats[[statName]][["val"]] <- CSHstatPercs(stat, lStatPer=lStatPer)
    CSH_Stats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
    #percStats[[statName]][["val"]] <- statPercs(stat, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat, percentiles) 
    
    #To DO
    ## Catch density
    #statName <- "CatDens"
    #stat <- as.numeric(catch(STK)[,ac(2018:2047)])
    #xDens <- hist(stat,breaks=100,plot=F)
    #percStats[[statName]][["mids"]] <- xDens$mids
    #percStats[[statName]][["density"]] <- xDens$density
    
    #TO DO
    #TAC change - need to define exactly what we want to report here
    statName <- "TACchange"
    stat <- catch(STK)
    stat2 <- catch(STK); stat2[] <- 0
    #absolute TAC change
    #for (iy in 1982:lastYr) stat2[,ac(iy)] <- (stat[,ac(iy)]-stat[,ac(iy-1)])/stat[,ac(iy-1)]  
    for (iy in 1982:lastYr) stat2[,ac(iy)] <- abs(stat[,ac(iy)]-stat[,ac(iy-1)])
    CSH_Stats[[statName]][["val"]] <- CSHstatPercs(stat2, lStatPer=lStatPer)
    CSH_Stats[[statName]][["worm"]] <- stat2[,,,,,1:numWorm]
    
    #percStats[[statName]][["val"]] <- statPercs(stat2, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat2[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat2, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat2, percentiles) 
    
    ## Catch Change ABSOLUTE  - how is this different?
    #statName <- "TACchange_abs"
    #stat2    <- catch(STK); stat2[] <- 0
    #for (iy in 1982:lastYr) stat2[,ac(iy)] <- abs( (stat[,ac(iy)]-stat[,ac(iy-1)])/stat[,ac(iy-1)] )  
    #percStats[[statName]][["val"]] <- statPercs(stat2, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat2[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat2, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat2, percentiles) 
    
    ## HCS Catch Change - another version!!
    # As done by Dankert
    #statName <- "TACchange_HCS"
    #stat2    <- catch(STK); stat2[] <- 0
    #for (iy in 1982:lastYr) stat2[,ac(iy)] <- (stat[,ac(iy)]-stat[,ac(iy-1)])/((stat[,ac(iy-1)] + stat[,ac(iy)])/2)
    #percStats[[statName]][["val"]] <- statPercs(stat2, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat2[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat2, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat2, percentiles) 
    
    ## HCS Catch Change ABSOLUTE - and again!!
    # As done by Dankert
    #statName <- "TACchange_HCS_abs"
    #stat2    <- catch(STK); stat2[] <- 0
    #for (iy in 1982:lastYr) stat2[,ac(iy)] <- abs((stat[,ac(iy)]-stat[,ac(iy-1)])/((stat[,ac(iy-1)] + stat[,ac(iy)])/2))
    #percStats[[statName]][["val"]] <- statPercs(stat2, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat2[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat2, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat2, percentiles) 
    
    
    ## Catch drops
    # number of >50% decreases in catch
    #statName <- "TACdrops50"
    #stat2    <- catch(STK); stat2[] <- 0 
    #for (iy in 1982:lastYr) stat2[,ac(iy)] <- (stat[,ac(iy)]-stat[,ac(iy-1)])/stat[,ac(iy-1)]  
    #stat3 <- stat2<(-0.5)
    #percStats[[statName]][["val"]] <- statPercs(stat3, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat3[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat3, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat3, percentiles) 
    # use mean values to compare F levels
    
    ## Catch drops 30%
    # number of >30% decreases in catch
    #statName <- "TACdrops30"
    #stat2    <- catch(STK); stat2[] <- 0 
    #for (iy in 1982:lastYr) stat2[,ac(iy)] <- (stat[,ac(iy)]-stat[,ac(iy-1)])/stat[,ac(iy-1)]  
    #stat3 <- stat2<(-0.3)
    #percStats[[statName]][["val"]] <- statPercs(stat3, years, percentiles)
    #percStats[[statName]][["worm"]] <- stat3[,,,,,1:numWorm]
    #percStats[[statName]][["bw"]] <- pointPercsBW(stat3, percentiles)
    #percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat3, percentiles) 
    
    DATA[[ac(jj)]] <- CSH_Stats
    #DATA[[ac(jj)]] <- percStats
    
  } # end of scens data loop
  
  ## Save data
  out <- list(stats=DATA,run_name=runName,run_set=paste(OM,"_",HCR),fit=FIT)
#  save(out, file = file.path(MSE.Dir,"Results",paste0(OM,"_HCR",HCR,"with_poor_recruitment_SimpSIM_STATS.Rdata")))
  
#} #end of Run loop

########################END#######################################

