####--- Functions for HCRs for multiannual plan evaluation ------####
## D.C.M. Miller, August 2016

### tacFdiff 
#landsel=ldsel; landwt=ldwt; stknum=stknm; nmort=nm; tac=tc;   fmult=0.2

## Function to calculate difference between TAC and fishing at a given F
tacFdiff <- function(fmult, landsel, landwt, stknum, nmort, disrat, tac){
  fmort <- fmult*landsel
  zmort <- fmort+(fmort*disrat)+nmort
  
  return(abs(tac - sum((stknum*(1-exp(-zmort)) * landwt) * (fmort/zmort),na.rm=T)))
  }


#fmort <- 0.1092631*landsel[,r]
#zmort <- fmort+nmort[,r]
#abs(tac[r] - sum((stknum[,r]*(1-exp(-zmort)) * landwt[,r]) * (fmort/zmort),na.rm=T))

### tacF
## Function minimise difference between TAC and fishing at a given F
#tacF(sel,Wy,Ny,M,Cattmp,rsamsel,rsam,chngF,j)
tacF <- function(sel,Wy,Ny,M,Cattmp,rsamsel,rsam,chngF,j){
    Fout <- Ny[1,j-1,]; Fout[]<- NA
  
    landsel <- sel[, rsamsel[j-1,]]
    landwt  <- Wy[, j-1, ]
    stknum  <- Ny[, j-1, ]
    nmort   <- M[, rsam[j-1,]]
    tac     <- Cattmp[j-1,]
    # discards ratio (for calculation of Z)
    disrat <-  0  # no discards for BW
    
    for(r in which(chngF)) Fout[r]   <- optimize(tacFdiff, c(0, 100),landsel=landsel[,r], landwt=landwt[,r], stknum=stknum[,r], nmort=nmort[,r], disrat=disrat,tac=tac[r],tol=0.0000001)$minimum
  
    return(Fout[chngF])
}


####--- Functions for stats and plots for multiannual plan evaluation ------####
## D.C.M. Miller, 30 July 2010

### ------------------------------------------------------------------------------------------------------
###   Stats Functions
### ------------------------------------------------------------------------------------------------------

### runMeans
## Computes the mean accross iterations (6th dimension) of a 6-dim object
runMeans <- function(x) {
  y <- x[,,,,,1]; y[] <- NA
  for (a in 1:dims(y)$age) for (b in 1:dims(y)$year) y[a,b] <- mean(x[a,b]@.Data, na.rm=T)
  return(y)
  }

### statPercs
## Computes the mean, min, max and percentiles accross iterations (6th dimension) of a 6-dim object
statPercs <- function(stat, YRS, percs) {
  
  #YRS = years
  #percs = percentiles
  
  x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean","sd","var"), year=YRS))
  
  #iterations
  ITS <- 1:length(stat[1,1])
  
  for (Y in YRS) {
    x["var",ac(Y)]  <- var(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["sd",ac(Y)]   <- sd(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["mean",ac(Y)] <- mean(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["min",ac(Y)]  <- min(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["max",ac(Y)]  <- max(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x[2:(length(percs)+1),ac(Y)]   <- quantile(stat[,ac(Y),,,,ITS]@.Data,probs=percentiles,na.rm=T)
  }
  
  return(x)
  
}

#extinction rate
fExtYear <- function(x,yrs) {
  return(rev(as.numeric(yrs))[min(which(rev(as.vector(x))==TRUE))] + 1)
}


CSHstatExtinct <- function(SSB, depletion=0.01, firstYear) {
  
  #compute the proportion of interations extinct by year
  #an extinct stock is one where the SSB has fallen below the limit proportion (depletion param - default 1%)
  #of the SSB in the first year of the projection and remains so until the final year
  
  all.years <- dimnames(SSB)$year
  ret <- rep(0,length(all.years))
  names(ret) <- all.years
  
  #not interested in the years before the simulation start
  stat2 <- window(SSB,as.character(firstYear),as.character(dims(SSB)$maxyear))
  #just the iterations that are extinct in the final year
  stat3 <- stat2[,,,,,as.vector(stat2[,as.character(dims(stat2)$maxyear),,,,]<depletion*stat2[,as.character(dims(stat2)$minyear),,,,])]

  #identify the years above/below extinction level
  if(dims(stat3)$iter > 0){
    stat4 <- stat3 > depletion*as.vector(stat3[,as.character(dims(stat3)$minyear),,,,])
    #cumulative sum divided by total number of iterations give proportion of iterations extinct by year
    stat5 <- table(as.vector(apply(stat4,6,fExtYear,yrs=dimnames(stat4)$year)))/dims(stat2)$iter
    ret[names(stat5)] <- stat5
  }
  ret <- cumsum(ret)
  
}

CSHstatRisk <- function(SSB, RP, lStatPer) {
  
  #compute the risk of the supplied SSB falling below the supplied RP for each of the years/periods
  #given in the lStatPer list. For periods the maximum of the annual risks is returned

  #FLQuant for results. If a single year then min,max,mean and median will all have the same value
  x <- FLQuant(NA, dimnames=list(age=c("min","max","mean","median"), year=names(lStatPer)))
  
  for (p in 1:length(lStatPer)){

    nits <- length(SSB[1,1])
    y1 <- ac(lStatPer[[p]][1])
    y2 <- ac(lStatPer[[p]][2])
    y12 <- ac(seq(lStatPer[[p]][1],lStatPer[[p]][2]))

    if (y1==y2){
      x["min",names(lStatPer)[p]] <- x["max",names(lStatPer)[p]] <- x["mean",names(lStatPer)[p]] <- x["median",names(lStatPer)[p]] <- sum(as.numeric(SSB[,y1])<RP)/nits  
    } else {
      x["min",names(lStatPer)[p]] <- min(apply(FLCore::window(SSB,y1,y2),2,function(x){return(sum(x<RP)/nits)}))
      x["max",names(lStatPer)[p]] <- max(apply(FLCore::window(SSB,y1,y2),2,function(x){return(sum(x<RP)/nits)}))
      x["mean",names(lStatPer)[p]] <- mean(apply(FLCore::window(SSB,y1,y2),2,function(x){return(sum(x<RP)/nits)}))
      x["median",names(lStatPer)[p]] <- quantile(as.numeric(apply(FLCore::window(SSB,y1,y2),2,function(x){return(sum(x<RP)/nits)})),probs=0.5,na.rm=T)
    }
    
  }
  
  return(x)
  
}


### statPercs
## Computes the mean, min, max, percentiles, standard deviation and variance across iterations 
## for each of the years and ranges supplied
CSHstatPercs <- function(stat, lStatPer, percs=c(0.05,0.5,0.95)) {
  
  #lPeriods = list of statistical reporting periods
  #percs = percentiles to calculate
  
  #create an FLQuant object to store the results
  #stat goes in age dimension
  #for age based (selection, catch weight, stock weight) age is used
  
  #x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean","sd","var"), year=YRS))
  
  #all iterations
  ITS <- 1:length(stat[1,1])
  
  if (dim(stat)[1]>1){
    
    #age based
    x <- FLQuant(NA, dimnames=list(age=seq(1,dim(stat)[1]), year=names(lStatPer)))
    
    for (p in 1:length(lStatPer)) {
      
      y1 <- ac(lStatPer[[p]][1])
      y2 <- ac(lStatPer[[p]][2])
      y12 <- ac(seq(lStatPer[[p]][1],lStatPer[[p]][2]))
      
      if (y1==y2) {
        for(a in 1:dim(stat)[1]){
          x[ac(a),names(lStatPer)[p]]  <- mean(stat[a,y1,,,,ITS]@.Data, na.rm=T)  
        }
      } else {
        for(a in 1:dim(stat)[1]){
          x[ac(a),names(lStatPer)[p]]  <- mean(stat[a,y12,,,,ITS]@.Data, na.rm=T)  
        }
      }
    }
    
    
    
  } else {
    
    x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean","sd","var","CV"), year=names(lStatPer)))
    
    for (p in 1:length(lStatPer)) {
      
      y1 <- ac(lStatPer[[p]][1])
      y2 <- ac(lStatPer[[p]][2])
      y12 <- ac(seq(lStatPer[[p]][1],lStatPer[[p]][2]))
      
      if (y1==y2) {
        x["var",names(lStatPer)[p]]  <- var(stat[,y1,,,,ITS]@.Data, na.rm=T)
        x["sd",names(lStatPer)[p]]   <- sd(stat[,y1,,,,ITS]@.Data, na.rm=T)
        x["mean",names(lStatPer)[p]] <- mean(stat[,y1,,,,ITS]@.Data, na.rm=T)
        x["CV",names(lStatPer)[p]] <- sd(stat[,y1,,,,ITS]@.Data, na.rm=T)/mean(stat[,y1,,,,ITS]@.Data, na.rm=T)
        x["min",names(lStatPer)[p]]  <- min(stat[,y1,,,,ITS]@.Data, na.rm=T)
        x["max",names(lStatPer)[p]]  <- max(stat[,y1,,,,ITS]@.Data, na.rm=T)
        x[2:(length(percs)+1),names(lStatPer)[p]]   <- quantile(stat[,y1,,,,ITS]@.Data,probs=percs,na.rm=T)
      } else {
        x["var",names(lStatPer)[p]]  <- var(stat[,y12,,,,ITS]@.Data, na.rm=T)
        x["sd",names(lStatPer)[p]]   <- sd(stat[,y12,,,,ITS]@.Data, na.rm=T)
        x["mean",names(lStatPer)[p]] <- mean(stat[,y12,,,,ITS]@.Data, na.rm=T)
        x["CV",names(lStatPer)[p]] <- sd(stat[,y12,,,,ITS]@.Data, na.rm=T)/mean(stat[,y12,,,,ITS]@.Data, na.rm=T)
        x["min",names(lStatPer)[p]]  <- min(stat[,y12,,,,ITS]@.Data, na.rm=T)
        x["max",names(lStatPer)[p]]  <- max(stat[,y12,,,,ITS]@.Data, na.rm=T)
        x[2:(length(percs)+1),names(lStatPer)[p]]   <- quantile(stat[,y12,,,,ITS]@.Data,probs=percs,na.rm=T)
      }
      
    }
    
  }
  
  return(x)
  
}





### Point values and mean
## Computes the values at 2020, 2025 and means over 2016-2020 and 2020-2025 and 2066-2115
pointPercsBW <- function(stat, percs) {
  
  #x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean","sd","var"), year=c("2020","2025", "2016-2020","2021-2025","2066-2115")))
  x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean","sd","var"), year=c("2020","2025", "2018-2022","2023-2027","2028-2047")))
  
  ITS <- 1:length(stat[1,1])
  for (Y in c("2020","2025")) {
    x["var",ac(Y)]  <- var(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["sd",ac(Y)]   <- sd(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["mean",ac(Y)] <- mean(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["min",ac(Y)]  <- min(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["max",ac(Y)]  <- max(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x[2:(length(percs)+1),ac(Y)]   <- quantile(stat[,ac(Y),,,,ITS]@.Data,probs=percentiles,na.rm=T)
    }

  # "2016-2020"
  #x["var","2016-2020"]  <- var(stat[,ac(2016:2020),,,,ITS]@.Data, na.rm=T)
  #x["sd","2016-2020"]   <- sd(stat[,ac(2016:2020),,,,ITS]@.Data, na.rm=T)
  #x["mean","2016-2020"] <- mean(stat[,ac(2016:2020),,,,ITS]@.Data, na.rm=T)
  #x["min","2016-2020"]  <- min(stat[,ac(2016:2020),,,,ITS]@.Data, na.rm=T)
  #x["max","2016-2020"]  <- max(stat[,ac(2016:2020),,,,ITS]@.Data, na.rm=T)
  #x[2:(length(percs)+1),"2016-2020"]   <- quantile(stat[,ac(2016:2020),,,,ITS]@.Data,probs=percentiles,na.rm=T)
  x["var","2018-2022"]  <- var(stat[,ac(2018:2022),,,,ITS]@.Data, na.rm=T)
  x["sd","2018-2022"]   <- sd(stat[,ac(2018:2022),,,,ITS]@.Data, na.rm=T)
  x["mean","2018-2022"] <- mean(stat[,ac(2018:2022),,,,ITS]@.Data, na.rm=T)
  x["min","2018-2022"]  <- min(stat[,ac(2018:2022),,,,ITS]@.Data, na.rm=T)
  x["max","2018-2022"]  <- max(stat[,ac(2018:2022),,,,ITS]@.Data, na.rm=T)
  x[2:(length(percs)+1),"2018-2022"]   <- quantile(stat[,ac(2018:2022),,,,ITS]@.Data,probs=percentiles,na.rm=T)
  
  # "2021-2025"
  #x["var","2021-2025"]  <- var(stat[,ac(2021:2025),,,,ITS]@.Data, na.rm=T)
  #x["sd","2021-2025"]   <- sd(stat[,ac(2021:2025),,,,ITS]@.Data, na.rm=T)
  #x["mean","2021-2025"] <- mean(stat[,ac(2021:2025),,,,ITS]@.Data, na.rm=T)
  #x["min","2021-2025"]  <- min(stat[,ac(2021:2025),,,,ITS]@.Data, na.rm=T)
  #x["max","2021-2025"]  <- max(stat[,ac(2021:2025),,,,ITS]@.Data, na.rm=T)
  #x[2:(length(percs)+1),"2021-2025"]   <- quantile(stat[,ac(2021:2025),,,,ITS]@.Data,probs=percentiles,na.rm=T)
  x["var","2023-2027"]  <- var(stat[,ac(2023:2027),,,,ITS]@.Data, na.rm=T)
  x["sd","2023-2027"]   <- sd(stat[,ac(2023:2027),,,,ITS]@.Data, na.rm=T)
  x["mean","2023-2027"] <- mean(stat[,ac(2023:2027),,,,ITS]@.Data, na.rm=T)
  x["min","2023-2027"]  <- min(stat[,ac(2023:2027),,,,ITS]@.Data, na.rm=T)
  x["max","2023-2027"]  <- max(stat[,ac(2023:2027),,,,ITS]@.Data, na.rm=T)
  x[2:(length(percs)+1),"2023-2027"]   <- quantile(stat[,ac(2023:2027),,,,ITS]@.Data,probs=percentiles,na.rm=T)
  
  # "2066-2115"
  #x["var","2066-2115"]  <- var(stat[,ac(2066:2115),,,,ITS]@.Data, na.rm=T)
  #x["sd","2066-2115"]   <- sd(stat[,ac(2066:2115),,,,ITS]@.Data, na.rm=T)
  #x["mean","2066-2115"] <- mean(stat[,ac(2066:2115),,,,ITS]@.Data, na.rm=T)
  #x["min","2066-2115"]  <- min(stat[,ac(2066:2115),,,,ITS]@.Data, na.rm=T)
  #x["max","2066-2115"]  <- max(stat[,ac(2066:2115),,,,ITS]@.Data, na.rm=T)
  #x[2:(length(percs)+1),"2066-2115"]   <- quantile(stat[,ac(2066:2115),,,,ITS]@.Data,probs=percentiles,na.rm=T)
  x["var","2028-2047"]  <- var(stat[,ac(2028:2047),,,,ITS]@.Data, na.rm=T)
  x["sd","2028-2047"]   <- sd(stat[,ac(2028:2047),,,,ITS]@.Data, na.rm=T)
  x["mean","2028-2047"] <- mean(stat[,ac(2028:2047),,,,ITS]@.Data, na.rm=T)
  x["min","2028-2047"]  <- min(stat[,ac(2028:2047),,,,ITS]@.Data, na.rm=T)
  x["max","2028-2047"]  <- max(stat[,ac(2028:2047),,,,ITS]@.Data, na.rm=T)
  x[2:(length(percs)+1),"2028-2047"]   <- quantile(stat[,ac(2028:2047),,,,ITS]@.Data,probs=percentiles,na.rm=T)
  
  return(x)
  }

## Computes the values at 2020, 2025 and means over 2016-2020 and 2020-2025 and 2066-2115
pointPercsBW_HCS <- function(stat, percs) {
  #x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean"), year=c("2020","2025", "2016-2020","2021-2025","2066-2115")))
  x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean"), year=c("2020","2025", "2018-2022","2023-2027","2028-2047")))
  ITS <- 1:length(stat[1,1])
  for (Y in c("2020","2025")) {
    x["mean",ac(Y)] <- mean(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["min",ac(Y)]  <- min(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x["max",ac(Y)]  <- max(stat[,ac(Y),,,,ITS]@.Data, na.rm=T)
    x[2:(length(percs)+1),ac(Y)]   <- quantile(stat[,ac(Y),,,,ITS]@.Data,probs=percentiles,na.rm=T)
  }
  
  # "2016-2020"
  #meanStat <- apply(stat[,ac(2016:2020)],6,mean)
  #x["mean","2016-2020"] <- mean(meanStat[,,,,,ITS]@.Data, na.rm=T)
  #x["min","2016-2020"]  <- min(meanStat[,,,,,ITS]@.Data, na.rm=T)
  #x["max","2016-2020"]  <- max(meanStat[,,,,,ITS]@.Data, na.rm=T)
  #x[2:(length(percs)+1),"2016-2020"]   <- quantile(meanStat[,,,,,ITS]@.Data,probs=percentiles,na.rm=T)
  meanStat <- apply(stat[,ac(2018:2022)],6,mean)
  x["mean","2018-2022"] <- mean(meanStat[,,,,,ITS]@.Data, na.rm=T)
  x["min","2018-2022"]  <- min(meanStat[,,,,,ITS]@.Data, na.rm=T)
  x["max","2018-2022"]  <- max(meanStat[,,,,,ITS]@.Data, na.rm=T)
  x[2:(length(percs)+1),"2018-2022"]   <- quantile(meanStat[,,,,,ITS]@.Data,probs=percentiles,na.rm=T)
  
  # "2021-2025"
  #meanStat <- apply(stat[,ac(2021:2025)],6,mean)
  #x["mean","2021-2025"] <- mean(meanStat[,,,,,ITS]@.Data, na.rm=T)
  #x["min","2021-2025"]  <- min(meanStat[,,,,,ITS]@.Data, na.rm=T)
  #x["max","2021-2025"]  <- max(meanStat[,,,,,ITS]@.Data, na.rm=T)
  #x[2:(length(percs)+1),"2021-2025"]   <- quantile(meanStat[,,,,,ITS]@.Data,probs=percentiles,na.rm=T)
  meanStat <- apply(stat[,ac(2023:2027)],6,mean)
  x["mean","2023-2027"] <- mean(meanStat[,,,,,ITS]@.Data, na.rm=T)
  x["min","2023-2027"]  <- min(meanStat[,,,,,ITS]@.Data, na.rm=T)
  x["max","2023-2027"]  <- max(meanStat[,,,,,ITS]@.Data, na.rm=T)
  x[2:(length(percs)+1),"2023-2027"]   <- quantile(meanStat[,,,,,ITS]@.Data,probs=percentiles,na.rm=T)
  
  # "2066-2115"
  #meanStat <- apply(stat[,ac(2066:2115)],6,mean)
  #x["mean","2066-2115"] <- mean(meanStat[,,,,,ITS]@.Data, na.rm=T)
  #x["min","2066-2115"]  <- min(meanStat[,,,,,ITS]@.Data, na.rm=T)
  #x["max","2066-2115"]  <- max(meanStat[,,,,,ITS]@.Data, na.rm=T)
  #x[2:(length(percs)+1),"2066-2115"]   <- quantile(meanStat[,,,,,ITS]@.Data,probs=percentiles,na.rm=T)
  meanStat <- apply(stat[,ac(2028:2047)],6,mean)
  x["mean","2028-2047"] <- mean(meanStat[,,,,,ITS]@.Data, na.rm=T)
  x["min","2028-2047"]  <- min(meanStat[,,,,,ITS]@.Data, na.rm=T)
  x["max","2028-2047"]  <- max(meanStat[,,,,,ITS]@.Data, na.rm=T)
  x[2:(length(percs)+1),"2028-2047"]   <- quantile(meanStat[,,,,,ITS]@.Data,probs=percentiles,na.rm=T)
  
  return(x)
}

pointPercs <- function(stat, percs) {
  x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean"), year=c("2016","2017-2020","2017-2021")))
  ITS <- 1:length(stat[1,1])
  
  x["mean","2016"] <- mean(stat[,ac(2016),,,,ITS]@.Data, na.rm=T)
  x["min","2016"]  <- min(stat[,ac(2016),,,,ITS]@.Data, na.rm=T)
  x["max","2016"]  <- max(stat[,ac(2016),,,,ITS]@.Data, na.rm=T)
  x[2:(length(percs)+1),"2016"]   <- quantile(stat[,ac(2016),,,,ITS]@.Data,probs=percentiles,na.rm=T)

  x["mean","2017-2020"] <- mean(stat[,ac(2017:2020),,,,ITS]@.Data, na.rm=T)
  x["min","2017-2020"]  <- min(stat[,ac(2017:2020),,,,ITS]@.Data, na.rm=T)
  x["max","2017-2020"]  <- max(stat[,ac(2017:2020),,,,ITS]@.Data, na.rm=T)
  x[2:(length(percs)+1),"2017-2020"]   <- quantile(stat[,ac(2017:2020),,,,ITS]@.Data,probs=percentiles,na.rm=T)

  x["mean","2017-2021"] <- mean(stat[,ac(2017:2021),,,,ITS]@.Data, na.rm=T)
  x["min","2017-2021"]  <- min(stat[,ac(2017:2021),,,,ITS]@.Data, na.rm=T)
  x["max","2017-2021"]  <- max(stat[,ac(2017:2021),,,,ITS]@.Data, na.rm=T)
  x[2:(length(percs)+1),"2017-2021"]   <- quantile(stat[,ac(2017:2021),,,,ITS]@.Data,probs=percentiles,na.rm=T)

  return(x)
  }  
### ------------------------------------------------------------------------------------------------------
###   Plotting Functions
### ------------------------------------------------------------------------------------------------------

### basePlot
#dat <- statList; mid=plotVal; CIs=ConfInt; pltY=pltYears; ylb=YLB; tit=Tit; legd=0.95
basePlot <- function(dat,mid,CIs,pltY,ylb,tit,legd) {
  numPlts <- length(dat)
  maxY <- max(dat[[1]][["val"]][mid,ac(pltY)],na.rm=T); if (numPlts>1) for (np in 2:numPlts) maxY <- max(c(maxY,dat[[np]][["val"]][mid,ac(pltY)]),na.rm=T)
  minY <- min(dat[[1]][["val"]][mid,ac(pltY)],na.rm=T); if (numPlts>1) for (np in 2:numPlts) minY <- min(c(minY,dat[[np]][["val"]][mid,ac(pltY)]),na.rm=T)
  if (!is.na(CIs)) {
    cis <- c(paste(50-as.numeric(CIs)/2,"%",sep=""),paste(50+as.numeric(CIs)/2,"%",sep=""))
    maxY <- max(dat[[1]][["val"]][cis[2],ac(pltY)],na.rm=T); if (numPlts>1) for (np in 2:numPlts) maxY <- max(c(maxY,dat[[np]][["val"]][cis[2],ac(pltY)]),na.rm=T)
    minY <- min(dat[[1]][["val"]][cis[1],ac(pltY)],na.rm=T); if (numPlts>1) for (np in 2:numPlts) minY <- min(c(minY,dat[[np]][["val"]][cis[1],ac(pltY)]),na.rm=T)
    }
  minY <- min(0,minY)
  plot(pltY, dat[[1]][["val"]][mid,ac(pltY)],
        type="l", pch=16, ylim=c(minY,maxY),
        xlab="Year", ylab=ylb, main=tit, lwd=1.5  )
  if (numPlts>1) for (np in numPlts:1) lines(pltY, dat[[np]][["val"]][mid,ac(pltY)], lwd=1.5, col=np)
  if (!is.na(CIs)) for (np in numPlts:1) for (p in cis) lines(pltY, dat[[np]][["val"]][p,ac(pltY)], lty=2, col=np)
  abline(v=assYear, lty=2, col="grey")
  #if (!is.na(legd)) legend(min(pltY),legd*maxY,c("Median", "90% conf. int."), lty=c(1,2), bty="n")
  legend(min(pltY),legd*maxY,ac(potScens[plotScens,"Description"]), lty=c(1), col=c(1:numPlts), bty="n")
  }
#basePlot(statList,mid="50%",CIs="90",pltY=pltYears,ylb="Spawner Stock Biomass (t)",tit="SSB",legd=0.95)

### baseLines
#To add lines to a baseplot plot
baseLines <- function(dat,mid,pltY,LTY) {
  numPlts <- length(dat)
  lines(pltY, dat[[1]][["val"]][mid,ac(pltY)], lwd=1.5 , lty=LTY)
  if (numPlts>1) for (np in numPlts:1) lines(pltY, dat[[np]][["val"]][mid,ac(pltY)], lwd=1.5, col=np, lty=LTY)
  }
  
### wormPlot
wormPlot <- function(dat,pltY,ylb,tit) {
  numWorms <- length(dat[["worm"]][1,1,,,,])
  maxY <- NA; for (i in 1:numWorms) maxY <- max(c(max(dat[["worm"]][,ac(pltY),,,,i],na.rm=T), maxY), na.rm=T)
  minY <- NA; for (i in 1:numWorms) minY <- min(c(min(dat[["worm"]][,ac(pltY),,,,i],na.rm=T), minY), na.rm=T)
  minY <- min(0,minY)
  plot(pltY, dat[["worm"]][,ac(pltY),,,,1],
        type="l", pch=16, ylim=c(minY,maxY),
        xlab="Year", ylab=ylb, main=paste(tit,": first ",numWorms," runs", sep=""), lwd=1  )
  for (i in 2:numWorms) lines(pltY, dat[["worm"]][,ac(pltY),,,,i], col=grey(1-0.9*i/numWorms))
  abline(v=assYear, lty=2, col="grey")
  }
#wormPlot(dat,mid="50%",CIs="90",pltY=plotYears,ylb="Spawner Stock Biomass (t)",tit="SSB",legd=0.95)

### bwPlot
bwPlot <- function(dat,ylb,tit,legd) {
   numPlts <- length(dat)
   maxY <- max(dat[[1]][["bw"]]["95%",],na.rm=T); if (numPlts>1) for (np in 2:numPlts) maxY <- max(c(maxY,dat[[np]][["bw"]]["95%",]),na.rm=T)
   minY <- min(dat[[1]][["bw"]]["5%",],na.rm=T); if (numPlts>1) for (np in 2:numPlts) minY <- min(c(minY,dat[[np]][["bw"]]["5%",]),na.rm=T)
   minY <- min(0,minY)
   numBox <- length(dimnames(dat[[1]][["bw"]])$year)
   plotPos <- list(); posRel1 <- seq(0.6,1.4,by=1/(1+numPlts))
   for (np in numPlts:1) plotPos[[np]] <- posRel1[np]+2*c(0:(numBox-1))
   plot(plotPos[[1]], as.numeric(dat[[1]][["bw"]]["50%",]@.Data),
        ylim=c(minY,maxY), xlim=c(0,2*numBox), xaxt="n", ylab=ylb, xlab="Years",
        main=tit, cex=2, pch=16)
   axis(1,at=seq(1,2*numBox,by=2),labels=dimnames(dat[[1]][["bw"]])$year,cex.axis=1,las=1)
   for (np in numPlts:1) { 
    points(plotPos[[np]], as.numeric(dat[[np]][["bw"]]["50%",]@.Data), cex=2, pch=15+np, col=np)
    for (wh in 1:numBox) {       
      arrows(plotPos[[np]][wh],dat[[np]][["bw"]]["25%",wh],plotPos[[np]][wh],dat[[np]][["bw"]]["75%",wh],angle=90, code=3, length=0.1, lwd=2, col=np)
      arrows(plotPos[[np]][wh],dat[[np]][["bw"]]["5%",wh],plotPos[[np]][wh],dat[[np]][["bw"]]["95%",wh],angle=90, code=3, length=0.05, lwd=1.5, col=np)
      }
    }
   legend(0,legd*maxY,ac(potScens[scenarios[plotScens],"Description"]), lty=c(1), col=c(1:numPlts), bty="n", pch=c(16:(15+numPlts)))
  }
  
  bwPlotmean <- function(dat,ylb,tit,legd) {
   numPlts <- length(dat)
   maxY <- max(dat[[1]][["bw"]]["95%",],na.rm=T); if (numPlts>1) for (np in 2:numPlts) maxY <- max(c(maxY,dat[[np]][["bw"]]["95%",]),na.rm=T)
   minY <- min(dat[[1]][["bw"]]["5%",],na.rm=T); if (numPlts>1) for (np in 2:numPlts) minY <- min(c(minY,dat[[np]][["bw"]]["5%",]),na.rm=T)
   minY <- min(0,minY)
   numBox <- length(dimnames(dat[[1]][["bw"]])$year)
   plotPos <- list(); posRel1 <- seq(0.6,1.4,by=1/(1+numPlts))
   for (np in numPlts:1) plotPos[[np]] <- posRel1[np]+2*c(0:(numBox-1))
   plot(plotPos[[1]], as.numeric(dat[[1]][["bw"]]["mean",]@.Data),
        ylim=c(minY,maxY), xlim=c(0,2*numBox), xaxt="n", ylab=ylb, xlab="Years",
        main=tit, cex=2, pch=16)
   axis(1,at=seq(1,2*numBox,by=2),labels=dimnames(dat[[1]][["bw"]])$year,cex.axis=1,las=1)
   for (np in numPlts:1) { 
    points(plotPos[[np]], as.numeric(dat[[np]][["bw"]]["mean",]@.Data), cex=2, pch=15+np, col=np)
    for (wh in 1:numBox) {       
      arrows(plotPos[[np]][wh],dat[[np]][["bw"]]["25%",wh],plotPos[[np]][wh],dat[[np]][["bw"]]["75%",wh],angle=90, code=3, length=0.1, lwd=2, col=np)
      arrows(plotPos[[np]][wh],dat[[np]][["bw"]]["5%",wh],plotPos[[np]][wh],dat[[np]][["bw"]]["95%",wh],angle=90, code=3, length=0.05, lwd=1.5, col=np)
      }
    }
   legend(x="bottom",ac(potScens[scenarios[plotScens],"Description"]), lty=c(1), col=c(1:numPlts), bty="n", pch=c(16:(15+numPlts)))
  }