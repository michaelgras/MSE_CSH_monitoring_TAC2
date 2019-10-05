###~~~~~~~~~~~~~~~~~~~***********~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Based on EQSIM code from ICES Github:
# https://github.com/ices-tools-prod/msy/blob/master/R/eqsim.R
# Only 'eqsim_run' edited to SimpSIM (SR, plotting functions etc. not used)
###~~~~~~~~~~~~~~~~~~~+++++++++++~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Copied on 24-08-2016 10:45am CET
###~~~~~~~~~~~~~~~~~~~+++++++++++~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Changes to original:
# all change are marked with a comment. These can be found by searching for '#DM'
###~~~~~~~~~~~~~~~~~~~***********~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

#update for CSH, marked with #CSH

if (1==2) {
  fit=FIT;  
  startN = strtNum; 
  intF = 0.5;
  bio.years = c(assyearNum-numAvgYrsB, assyearNum-1); 
  bio.const = FALSE;
  sel.years = c(assyearNum-numAvgYrsB, assyearNum-1); 
  sel.const = FALSE; 
  Fcv=cvF; 
  Fphi=phiF; 
  SSBcv=cvSSB; 
  SSBphi=phiSSB; 
  rhologRec = recAR;
  Btrigger = refPts[["BmsyTrig"]]; 
  Blim=refPts[["Blim"]]; 
  Bpa=refPts[["Bpa"]]; 
  TACchange=0.2;
  catTac=0.6;
  Fscan = seq(0.1,0.4,len=31);
  recruitment.trim = c(3,-3); 
  Nrun = 5;
  process.error = TRUE;
  verbose=TRUE
}

###~~~~~~~~~~~~~
#' @title simulates a population fished under an HCR
#'
#' @description Bastardisation of EQSIM
#' 
#' @export
#' 
#' @author Colin Millar \email{colin.millar@@jrc.ec.europa.eu}
#'         SimpSIM changes: David Miller
#' 
#' @param fit A list returned from the function fitModels
#' @param bio.years The years to sample maturity, weights and M from
#' @param bio.const A flag, if TRUE (#DM: incorrectly said FALSE) mean of the biological values from the years selected are used
#' @param sel.years The years to sample the selection patterns from
#' @param sel.const A flag, if TRUE (#DM: incorrectly said FALSE) mean of the selection patterns from the years selected are used
#' @param Fscan F values to scan over
#' @param Fcv Assessment error in the advisory year
#' @param Fphi Autocorrelation in assessment error in the advisory year
#' @param SSBcv Spawning stock biomass error in the advisory year
#' @param rhologRec A flag for recruitment autocorrelation. If FALSE (default) then not applied.
#' @param Blim This we know
#' @param Bpa This we know
#' @param recruitment.trim A numeric vector with two log-value clipping the extreme recruitment values from a continuous lognormal distribution. The values must be set as c("high","low"). 
#' @param Btrigger If other than 0 (default) the target F applied is reduced by SSB/Btrigger 
#' @param Nrun The number of years to run in total (#DM: all will be retained)
#' @param process.error Use stochastic recruitment or mean recruitment?  (TRUE = predictive)
#' @param verbose Flag, if TRUE (default) indication of the progress of the simulation is provided in the console. Useful to turn to FALSE when knitting documents.
#' @param extreme.trim Call John Simmonds :-)

SimpSIM <- function(fit,
                    startN=NA, #DM: added for SimpSIM. 
                    intF=0.2, #DM: added option for intermediate year F (i.e. F applied for expected CATCH (not TAC) in the first year)
                    bio.years = c(2008, 2012), # years sample weights, M and mat
                    bio.const = FALSE,
                    sel.years= c(2008, 2012), # years sample sel and discard proportion by number from
                    sel.const = FALSE,
                    Fcv = 0,
                    Fphi = 0,
                    SSBcv = 0,
                    SSBphi = 0, #DM: added for SimpSIM
                    rhologRec = recAR,
                    Btrigger = 0,
                    Blim,
                    Bpa,
                    TACchange=NA, #DM: tac change limits
                    chngB=0, #DM: v3 - added a biomass below which the TAC change rule does not apply  
                    catTac=1, #DM: ratio of first year expected catch to first year TAC (need for tac change in first year)
                    Fscan = seq(0, 1, len = 20), # F values to scan over
                    recruitment.trim = c(3, -3),
                    Nrun = 100, 
                    process.error = TRUE, # use predictive recruitment or mean recruitment? (TRUE = predictive)
                    verbose = TRUE,
                    extreme.trim,
                    HCR = 0) #CSH HCR rule (0=no HCR, 1=1 break point, 2=2 break points
{     
  
  #DM: List to save simulations
  simStks <- list()
  
  if (abs(Fphi) >= 1) stop("Fphi, the autocorelation parameter for log F should be between (-1, 1)")
  if ((recruitment.trim[1] + recruitment.trim[2])> 0) stop("recruitment truncation must be between a high - low range")
  
  btyr1 <- bio.years[1]
  btyr2 <- bio.years[2]
  slyr1 <- sel.years[1]
  slyr2 <- sel.years[2]
  
  # Keep at most 50 simulation years (which will be the last 50 of the Nrun forward simulated years)
  # keep <- min(Nrun, 50)
  #DM: keep all for SimpSIM
  keep <- Nrun
  
  SR <- fit $ sr.sto
  data <- fit $ rby[,c("rec","ssb","year")]
  stk <- fit $ stk
  
  # forecast settings (mean wt etc)
  stk.win <- FLCore::window(stk, start = btyr1, end = btyr2)
  stk.winsel <- FLCore::window(stk, start = slyr1  , end = slyr2)
  
  littleHelper <- function(x,i) {
    x2 <- x
    x2[i] <- NA
    x2[] <- apply(x2,1,mean,na.rm=TRUE)
    x[i] <- x2[i]
    return(x)
  }
  
  west <- matrix(FLCore::stock.wt(stk.win), ncol = btyr2 - btyr1 + 1)
  i <- west == 0
  if(any(i)) west <- littleHelper(west,i)
  weca <- matrix(FLCore::catch.wt(stk.win), ncol = btyr2 - btyr1 + 1)
  i <- weca == 0
  if(any(i)) weca <- littleHelper(weca,i)
  wela <- matrix(FLCore::landings.wt(stk.win), ncol = btyr2 - btyr1 + 1)
  if(any(i)) wela <- littleHelper(wela,i)
  
  Mat <- matrix(FLCore::mat(stk.win), ncol = btyr2 - btyr1 + 1)
  M <- matrix(FLCore::m(stk.win), ncol = btyr2 - btyr1 + 1)
  landings <- matrix(FLCore::landings.n(stk.winsel), ncol = slyr2 - slyr1 + 1)
  # if zero, use 0.10 of minimum value
  
  catch <- matrix(FLCore::catch.n(stk.winsel), ncol = slyr2 - slyr1 + 1)
  sel <- matrix(FLCore::harvest(stk.winsel), ncol = slyr2 - slyr1 + 1)
  Fbar <- matrix(FLCore::fbar(stk.winsel), ncol = slyr2 - slyr1  + 1)
  sel <- sweep(sel, 2, Fbar, "/")
  
  if (sel.const == TRUE) { # take means of selection
    sel[] <- apply(sel, 1, mean)
    landings[]  <- apply(landings, 1, mean)
    catch[]  <- apply(catch, 1, mean)
  }
  
  # 22.2.2014 Added weight of landings per comment from Carmen
  if (bio.const==TRUE){ # take means of wts Mat and M and ratio of landings to catch
    west[] <- apply(west, 1, mean)
    weca[] <- apply(weca, 1, mean)
    wela[] <- apply(wela, 1, mean)
    Mat[] <- apply(Mat, 1, mean)
    M[] <- apply(M, 1, mean) #me
  }
  land.cat= landings / catch  # ratio of number of landings to catch
  
  # TODO: Check if this is sensible
  i <- is.na(land.cat)
  if(any(i)) land.cat[i] <- 1
  
  Fprop <- apply(FLCore::harvest.spwn(stk.winsel), 1, mean)[drop=TRUE] # vmean(harvest.spwn(stk.win))
  Mprop <- apply(FLCore::m.spwn(stk.win), 1, mean)[drop=TRUE] # mean(m.spwn(stk.win))
  
  # get ready for the simulations
  Nmod <- nrow(SR)
  NF <- length(Fscan)
  ages <- FLCore::dims(stk) $ age
  
  #DM: added SSBerr
  ssby <- percSSB <- intendF <- Ferr <- SSBerr <- array(0, c(Nrun,Nmod),dimnames=list(year=1:Nrun,iter=1:Nmod))
  Ny <- Fy <- WSy <- WCy <- Cy <- Wy <- Wl <- Ry <- array(0, c(ages, Nrun, Nmod),
                                                          dimnames=list(age=(range(stk)[1]:range(stk)[2]),year=1:Nrun,iter=1:Nmod))
  #
  # TODO per note from Carmen:
  #  NOTE: If we want Ferr to be a stationary AR(1) process, it would make
  #        more sense to initialise Ferr as a Normal dist with zero mean and
  #        standard deviation of AR(1) marginal distribution, i.e. standard 
  #        deviation of initial Ferr = Fcv/sqrt(1- Fphi^2), instead of just
  #        initialising Ferr=0
  #  2014-03-12: Changed per note form Carmen/John
  Ferr[1,] <- rnorm(n=Nmod, mean=0, sd=1)*Fcv/sqrt(1-Fphi^2)
  for(j in 2:Nrun) { Ferr[j,] <- Fphi*Ferr[j-1,] + Fcv*rnorm(n=Nmod, mean=0, sd=1) }
  

  #2014-03-12: Changed per note form Carmen/John
  #Errors in SSB: this is used when the ICES MSY HCR is applied for F
  #DM: added option to simply invert Ferr for SSBerr (set SSBcv to NA)
  #DM: added SSBphi, applied same initialisation as F
  #SSBerr <- matrix(rnorm(n=Nrun*Nmod, mean=0, sd=1), ncol=Nmod) * SSBcv
  if (is.na(SSBcv)) {
    SSBerr <- -Ferr
  } else {
    SSBerr[1,] <- rnorm(n=Nmod, mean=0, sd=1)*SSBcv/sqrt(1-SSBphi^2)
    for(j in 2:Nrun) { SSBerr[j,] <- SSBphi*SSBerr[j-1,] + SSBcv*rnorm(n=Nmod, mean=0, sd=1) }
  }
    
  rsam <- array(sample(1:ncol(weca), Nrun * Nmod, TRUE), c(Nrun, Nmod))
  rsamsel <- array(sample(1:ncol(sel), Nrun * Nmod, TRUE), c(Nrun, Nmod))
  Wy[] <- c(weca[, c(rsam)])
  Wl[] <- c(wela[, c(rsam)])
  Ry[]  <- c(land.cat[, c(rsamsel)])
  
  # initial recruitment
  R <- mean( data $ rec)
  ssbs <- cats <- lans <- recs <- array(0, c(7, NF))
  
  ferr <- ssbsa <- catsa <- lansa <- recsa <- array(0, c(NF, keep, Nmod))
  begin <- Nrun - keep + 1
  
  # New from Simmonds' 29.1.2014
  #   Residuals of SR fits (1 value per SR fit and per simulation year 
  #     but the same residual value for all Fscan values):
  resids= array(rnorm(Nmod*(Nrun+1), 0, SR$cv),c(Nmod, Nrun+1))
  
  # 2014-03-12: Changed per note form Carmen/John
  #  Autocorrelation in Recruitment Residuals:    
  if(rhologRec){
    fittedlogRec <-  do.call(cbind, lapply( c(1:nrow(fit$sr.sto)), function(i){     
      FUN <- match.fun(fit$sr.sto$model[i])
      FUN(fit$sr.sto[i, ], fit$rby$ssb) } )  ) 
    # Calculate lag 1 autocorrelation of residuals: 
    rhologRec <- apply(log(fit$rby$rec)-fittedlogRec, 2, function(x){cor(x[-length(x)],x[-1])}) 
    #DM: add max of 0.6 if time series is <50yrs
    #using 0.6 resulted in on average 0.4-0.5 AR1 in simulations, so increased value to 0.8 to try cap at 0.6 in sims
    #AC - check this
    if ((stk@range["minyear"]-stk@range["minyear"]+1)<50) rhologRec[] <- min(0.8, max(rhologRec))
    # Draw residuals according to AR(1) process:
    for(j in 2:(Nrun+1)){ resids[,j] <- rhologRec * resids[,j-1] + resids[,j]*sqrt(1 - rhologRec^2) }
  }    
  
  
  # Limit how extreme the Rec residuals can get:
  lims = t(array(SR$cv,c(Nmod,2))) * recruitment.trim
  for (k in 1:Nmod) { resids[k,resids[k,]>lims[1,k]]=lims[1,k]}
  for (k in 1:Nmod) { resids[k,resids[k,]<lims[2,k]]=lims[2,k]}
  # end New from Simmonds 29.1.2014
  
  #AC -  stochastic starting numbers
  noisy.StartN <- array(NA, c(ages, Nmod), dimnames=list(age=(range(stk)[1]:range(stk)[2]),iter=1:Nmod))
  for (iter in 1:Nmod){noisy.StartN[,iter] <- fObsModel(true=startN)}
  
  #if (verbose) loader(0)
  
  # Looping over each F value in Fscan. For each of the Nmod SR fits 
  # (replicates), do a forward simulation during Nrun years
  # There are Rec residuals for each SR fit and year, which take the same
  # values for all Fscan 
  for (i in 1:NF) {
    
    # The F value to test
    Fbar <- Fscan[i]
    
    ############################################################################
    # Population in simulation year 1:
    
    # Zpre: Z that occurs before spawning
    Zpre <- ( sel[,rsamsel[1,]]*Fbar * Fprop + M[,rsam[1,]] * Mprop)
    
    # Zpos: Z that occurs after spawning
    # Zpos not used anywhere
    #DM: blocked, not used# Zpos <- (Fbar * (1-Fprop) * sel[,rsamsel[1,]] + M[,rsam[1,]] * (1-Mprop))
    
    # run Z out to age 50 ...
    # TODO:
    # Comments from Carmen: Zcum is a cumulative sum, but it is done in a strange way:
    #  There is a matrix of F-at-age and a matrix of M-at-age (each has 49 ages, Nmod replicates)
    #  The F and M matrices are summed, giving Z-at-age (49 ages, Nmod replicates)
    #  But then a cumsum is taken considering the Z-at-age matrix as a vector (i.e. not column-wise) ????
    #  This is strange, by applying "cumsum" treating Z-at-age as a vector, really only the first 50 values of
    #  the resulting "Zcum" make sense (all other values seem "wrong", or at least, meaningless)
    #DM: using given starting numbers instead# Zcum <- c(0, cumsum(Fbar * sel[c(1:ages, rep(ages, 49 - ages)), rsamsel[1,]] + M[c(1:ages, rep(ages, 49 - ages)), rsam[1,]]))
    # Carmen: Following from "Zcum", only first 50 elements of N1 make sense ????
    #DM: using given starting numbers instead# N1 <- R * exp(- unname(Zcum))
    
    # set up age structure in first year for all simulations
    # Comments from Carmen:
    #   Ny has dimension = (no. ages, no. simulation yrs "Nrun", no. SR fits "Nmod")
    #   With this code, we seem to be getting always the same population-at-age value for year 1
    #   instead of Nmod different values, as might have been intended ????
    #   (the whole problem is coming from Zcum ==> N1 ==> Ny[,1,] )
    #DM: using given starting numbers instead# Ny[,1,] <- c(N1[1:(ages-1)], sum(N1[ages:50]))
    
    #DM: Change starting point to given values
    #Ny[, 1, ] <- startN
    Ny[, 1, ] <- noisy.StartN

    # calculate ssb in first year using a different stock.wt and Mat selection and M for each simulation
    # Comments from Carmen:
    #   ssby has dimension = (no. simul yrs "Nrun", no. SR fits "Nmod")
    #   SSB in year 1:
    #   although Ny[,1,] has dim no.ages x Nmod, all Nmod values of Ny[,1,] are
    #   the same (because of Zcum issue)
    ssby[1,] <- colSums(Mat[,rsam[1,]] * Ny[,1,] * west[,rsam[1,]] / exp(Zpre)[])
    #DM: save perceived SSB
    percSSB[1,] <- ssby[1, ] * exp(SSBerr[1,]) # get perceived SSBs
    
    # Years 2 to Nrun:
    for (j in 2:Nrun) {
      
      # get ssb from previous year
      SSB <- ssby[j-1,]
      
      # predict recruitment using various models
      if (process.error) {
        # Changes 29.1.2014
        # new random draws each time
        # allrecs <- sapply(unique(SR $ mod), function(mod) exp(match.fun(mod) (SR, SSB) + rnorm(Nmod, 0, SR $ cv)))
        # same random draws used for each F
        ###### 2014-03-13  TMP COMMENT - ERROR OCCURS HERE
        allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR, SSB) + resids[,j]))
        # end Changes 29.1.2014
      } else {
        allrecs <- sapply(unique(SR $ mod), function(mod) exp(match.fun(mod) (SR, SSB)))
      }
      
      # Comment from Carmen:
      #  For each of the Nmod replicates, this selects the appropriate SR model
      #   type to use in that replicate
      #  Note that the order of SR model types that comes out in "select" is
      #   not necessarily the same order in which the SR model types were
      #   entered as inputs -- I presume the **next 2 lines** of code have
      #   been checked to avoid potential bugs due to this reordering  ???? 
      select <- cbind(seq(Nmod), as.numeric(factor(SR $ mod, levels = unique(SR $ mod))))
      
      #DM: Could add uncertainty around starting Rec value, but haven't
      # first year, resample for startYr
      #if (j==2) {
      #  x <- allrecs[select]
        # randomise across years so not the same as year 2
      #  Ny[1, 1, ] <- x[ceiling(runif(length(allrecs),min=0,max=length(allrecs)))]
      #}
      
      Ny[1,j,] <- allrecs[select]
      
      # Comment from Carmen:
      #   Note: it seems that Rec is coded as occurring always at age 1
      #   (i.e. based on SSB in previous year)
      #   Some stocks have Rec at ages other than 1 (e.g. age 0) 
      #    -- is this a problem ????
      
      # apply HCR
      # (intended) Fbar to be applied in year j-1 (depends on SSB in year j-1):
      # 2014-03-12: Changed per note form Carmen/John
      # Fnext <- Fbar * pmin(1, SSB/Btrigger)
      #DM: only use Btrigger if not NA
      
      #CSH - new structure to select 1 of 3 HCR types
      if (HCR==0){
        
        Fnext <- Fbar
        
      } else {
        
        #1 trigger point, requires a value of Btrigger to be supplied
        if (!is.na(Btrigger)) Fnext <- Fbar * pmin(1, SSB * exp(SSBerr[j-1,]) / Btrigger)      
        
        # #DM: new two breakpoint HCR
        # if (is.na(Btrigger)) {
        #   SSBs <- SSB * exp(SSBerr[j-1,]) # get perceived SSBs
        #   Fnext <- Fbar * pmin(1, SSBs * exp(SSBerr[j-1,])) #initiate Fnext object
        #   # Less than Blim <- 0.05
        #   Fnext[SSBs<Blim] <- 0.05
        #   # Between Blim and Bpa
        #   tmpGBlim <- c(SSBs>=Blim); tmpLBpa <- c(SSBs<Bpa)
        #   Fnext[tmpGBlim&tmpLBpa] <- 0.05 + ((SSBs[tmpGBlim&tmpLBpa] - Blim)*(Fbar - 0.05) / (Bpa - Blim)) 
        #   # >Bpa
        #   Fnext[SSBs>=Bpa] <- Fbar
        #   # 20% TAC change limits if >Bpa:
        #   #rhologRec <- apply(log(fit$rby$rec)-fittedlogRec, 2, function(x){cor(x[-length(x)],x[-1])}) 
        #    rm(SSBs)
        # }
        
      }
      
      # apply some noise to the F
      # Notes from Carmen:
      #  Assessment and/or implementation error (modifies intended F to get
      #  realised F)
      #  Error: AR(1) process on log(F) with autocorrel = Fphi, and
      #  conditional stand deviation = Fcv
      #  Might make more sense to have the "Ferr" matrix calculated before
      #  the Fscan loop starts so that the same errors in F are applied to
      #  all Fscan values ???? (as for Rec residuals)
      
      # Outcommented 2014-03-12 because F-error already been drawn outside the
      # loop, so this line here is no longer needed:
      # Ferr[j,] <- Fphi * Ferr[j-1,] + rnorm(Nmod, 0, Fcv)
      
      # realised Fbar in year j-1:
      
      #DM: first save the intended F
      if(j==2) intendF[j-1,] <- intF else 
      #AC - this doesn't work - need to calculate the appropriate F to give the
      #2019 catch at 4742t
      if(j==3) intendF[j-1,] <- 0.17 else
      intendF[j-1,] <- Fnext
      
      #F error
      Fnext <- exp(Ferr[j-1,]) * Fnext
      
      # get a selection pattern for each simulation and apply this to get N
      #DM: comes out all 0s because Fprop=0 and Mprop=0
      Zpre <- rep(Fnext, each = length(Fprop)) * Fprop * sel[, rsamsel[j,]] + M[, rsam[j,]] * Mprop
      
      # get Fy
      Fy[ , j-1, ] <- rep(Fnext, each = ages) * sel[, rsamsel[j-1,]]
      
      #DM: use different (inputted parameter) F to get observed first year catch
      if(j==2) Fy[, j-1, ] <- intF * sel[, rsamsel[j-1,]]
      #AC expected 2019 catch F
      if(j==3) Fy[, j-1, ] <- 0.17 * sel[, rsamsel[j-1,]]
      
      #update numbers at age
      Ny[ -1, j, ] <- Ny[1:(ages-1), j-1, ] * exp(-Fy[1:(ages-1), j-1, ] - M[1:(ages-1), rsam[j-1,]])
      #plus group
      Ny[ages, j, ] <- Ny[ages, j, ] + Ny[ages, j-1, ] * exp(-Fy[ages, j-1, ] - M[ages, rsam[j-1,]])
      
      #calculate true SSB
      ssby[j, ] <- apply(array(Mat[, rsam[j,]] * Ny[,j,] * west[, rsam[j,]] / exp(Zpre), c(ages, Nmod)), 2, sum)
      
      #perceived SSB
      percSSB[j,] <- ssby[j, ] * exp(SSBerr[j,])
      
      #catch numbers
      #DM: shouldn't below be j-1 since it is the catch coming from F and N in j-1? Changed it from j to j-1
      Cy[, j-1, ] <- Ny[, j-1, ] * Fy[, j-1, ] / (Fy[, j-1, ] + M[, rsam[j-1,]]) * (1 - exp(-Fy[, j-1, ] - M[, rsam[j-1,]]))
      
      
      
      
      #DM: check change limits on TAC
      if(!is.na(TACchange)) if (j>2) {
        
        if (j==3) catTACFact <- catTac else catTACFact <- 1
        
        #catch 
        Cattmp  <- apply(Cy * Wy, 2:3, sum)
        #catch change
        CatChng <- Cattmp[j-1,]/Cattmp[j-2,]
        
        #if perceived SSB is greater than limit then check if change in catch exceeds percentage change limit
        tooLow <- (CatChng<((1-TACchange)*catTACFact)) & percSSB[j-1,] > chngB 
        tooHigh <- (CatChng>((1+TACchange)*catTACFact)) & percSSB[j-1,] > chngB
        
        Cattmp[j-1,tooLow] <- Cattmp[j-2,tooLow]*(1-TACchange)*catTACFact
        Cattmp[j-1,tooHigh] <- Cattmp[j-2,tooHigh]*(1+TACchange)*catTACFact
        
        #change Fs when abs(TAC change) > limit
        chngF <- tooLow|tooHigh
        
        #calculate new Fs (using function in "SimpSIM_additional_Funcs.r")
        tmpFs <- tacF(sel,Wy,Ny,M,Cattmp,rsamsel,rsam,chngF,j)
        for (rr in 1:(length(Fy)/(Nrun*10))) Fy[ , j-1, rr] <- tmpFs[rr] * sel[, rsamsel[j-1,]][,rr]
        
        #Updated vectors
        #Numbers
        Ny[ -1, j, ] <- Ny[1:(ages-1), j-1, ] * exp(-Fy[1:(ages-1), j-1, ] - M[1:(ages-1), rsam[j-1,]])
        Ny[ages, j, ] <- Ny[ages, j, ] + Ny[ages, j-1, ] * exp(-Fy[ages, j-1, ] - M[ages, rsam[j-1,]])
        
        #SSB
        ssby[j, ] <- apply(array(Mat[, rsam[j,]] * Ny[,j,] * west[, rsam[j,]] / exp(Zpre), c(ages, Nmod)), 2, sum)
        
        #observed SSB
        percSSB[j,] <- ssby[j, ] * exp(SSBerr[j,])
        
        Cy[, j-1, ] <- Ny[, j-1, ] * Fy[, j-1, ] / (Fy[, j-1, ] + M[, rsam[j-1,]]) * (1 - exp(-Fy[, j-1, ] - M[, rsam[j-1,]]))
        
        intendF[j-1,] <- apply(Fy[3:7, j-1, ],2,mean)/exp(Ferr[j-1,])
        
        } #end TACchange loop
      
    }
    
    # convert to catch weight
    Cw <- Cy * Wy   # catch Numbers *catch wts
    land <- Cy*Ry*Wl # catch Numbers * Fraction (in number) landed and landed wts
    Lan=apply(land,2:3,sum)
    Cat <- apply(Cw, 2:3, sum)
    
    # summarise everything and spit out!
    quants <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
    ssbs[, i] <- quantile(ssby[begin:Nrun, ], quants)
    cats[, i] <- quantile(Cat[begin:Nrun, ], quants)
    lans[, i] <- quantile(Lan[begin:Nrun, ], quants)
    recs[, i] <- quantile(Ny[1, begin:Nrun, ], quants)
    
    
    ferr[i, , ] <- Ferr[begin:Nrun, ]
    ssbsa[i, , ] <- ssby[begin:Nrun, ]
    catsa[i, , ] <- Cat[begin:Nrun, ]
    lansa[i, , ] <- Lan[begin:Nrun, ]
    recsa[i, , ] <- Ny[1, begin:Nrun, ]
    
    #DM: save Ny, Fy, Wy etc.
    simStks[[ac(Fbar)]] <- list()
    simStks[[ac(Fbar)]][["N"]] <- Ny
    simStks[[ac(Fbar)]][["F"]] <- Fy
    simStks[[ac(Fbar)]][["catW"]] <- Wy
    simStks[[ac(Fbar)]][["lanW"]] <- Wl
    simStks[[ac(Fbar)]][["stkW"]] <- west
    simStks[[ac(Fbar)]][["stkWran"]] <- rsam
    simStks[[ac(Fbar)]][["C"]] <- Cy
    simStks[[ac(Fbar)]][["L"]] <- Cy * Ry
    simStks[[ac(Fbar)]][["percSSB"]] <- percSSB
    simStks[[ac(Fbar)]][["intendF"]] <- intendF
    
    #if (verbose) loader(i/NF)
  }
  
  dimnames(ssbs) <- dimnames(cats) <- 
    dimnames(lans) <- dimnames(recs) <- 
    list(quants=c("p025","p05","p25","p50","p75","p95","p975"),
         fmort=Fscan)
  
  rbp2dataframe <- function(x,variable) {
    x <- data.frame(t(x))
    x$variable <- variable
    x$Ftarget <- as.numeric(row.names(x))
    rownames(x) <- NULL
    return(x)
  }
  rbp <- rbind(rbp2dataframe(recs,"Recruitment"),
               rbp2dataframe(ssbs,"Spawning stock biomass"),
               rbp2dataframe(cats,"Catch"),
               rbp2dataframe(lans,"Landings"))
  rbp <- rbp[,c(9,8,1:7)]
  
  # STOCK REFERENCE POINTS
  
  FCrash05 <- Fscan[which.max(cats[2,]):NF][ which(cats[2, which.max(cats[2,]):NF] < 0.05*max(cats[2,]) )[1] ]
  FCrash50 <- Fscan[which.max(cats[4,]):NF][ which(cats[4, which.max(cats[4,]):NF] < 0.05*max(cats[4,]) )[1] ]
  
  
  # Einar amended 30.1.2014
  if(missing(extreme.trim)) {
    catm <- apply(catsa, 1, mean)
    lanm <- apply(lansa, 1, mean)
  } else {
    
    # 2014-03-12 Outcommented per note from Carmen/John - see below
    #x <- catsa
    #i <- x > quantile(x,extreme.trim[2]) |
    #  x < quantile(x,extreme.trim[1])
    #x[i] <- NA
    #catm <- apply(x, 1, mean, na.rm=TRUE)
    #
    #x <- lansa
    #i <- x > quantile(x,extreme.trim[2]) |
    #  x < quantile(x,extreme.trim[1])
    #x[i] <- NA
    #lanm <- apply(x, 1, mean, na.rm=TRUE)
    
    # 2014-03-12: Above replaced with the following per note from Carmen/John
    #  If we want to remove whole SR models, we could use the following code. But it is too extreme, it ends up getting rid of most models:
    # auxi2 <- array( apply(catsa, 1, function(x){auxi<-rep(TRUE,Nmod); auxi[x > quantile(x, extreme.trim[2]) | x < quantile(x, extreme.trim[1])] <- FALSE; x <- auxi } ), dim=c(keep,Nmod,NF))
    # auxi2 <- (1:Nmod)[apply(auxi2, 2, function(x){length(unique(as.vector(x)))})==1]
    # apply(catsa[,,auxi2],1,mean)
    
    # So I think the alternative is not to get rid of whole SR models, but of different SR models depending on the value of F:
    catm <- apply(catsa, 1, function(x){mean(x[x <= quantile(x, extreme.trim[2]) & x >= quantile(x, extreme.trim[1])])})
    lanm <- apply(lansa, 1, function(x){mean(x[x <= quantile(x, extreme.trim[2]) & x >= quantile(x, extreme.trim[1])])})
  }
  
  # end Einar amended 30.1.2014
  
  maxcatm <- which.max(catm)
  maxlanm <- which.max(lanm)
  
  # Einar added 29.1.2014
  rbp$Mean <- NA
  rbp$Mean[rbp$variable == "Catch"] <- catm
  rbp$Mean[rbp$variable == "Landings"] <- lanm
  # end Einar added 29.1.2014
  
  
  catsam <- apply(catsa, c(1,3), mean)
  lansam <- apply(lansa, c(1,3), mean)
  maxpf <- apply(catsam, 2, which.max)
  maxpfl <- apply(lansam, 2, which.max)
  
  FmsyLan <- Fscan[maxpfl]
  msymLan <- mean(FmsyLan)
  vcumLan <- median(FmsyLan)
  fmsy.densLan <- density(FmsyLan)
  vmodeLan <- fmsy.densLan$x[which.max(fmsy.densLan$y)]
  
  FmsyCat <- Fscan[maxpf]
  msymCat <- mean(FmsyCat)
  vcumCat <- median(FmsyCat)
  fmsy.densCat <- density(FmsyCat)
  vmodeCat <- fmsy.densCat$x[which.max(fmsy.densCat$y)]
  
  pFmsyCat  <- data.frame(Ftarget=fmsy.densCat$x,
                          value=cumsum(fmsy.densCat$y * diff(fmsy.densCat$x)[1]),
                          variable="pFmsyCatch")
  pFmsyLan  <- data.frame(Ftarget=fmsy.densLan$x,
                          value=cumsum(fmsy.densLan$y * diff(fmsy.densLan$x)[1]),
                          variable="pFmsyLandings")
  pProfile <- rbind(pFmsyCat,pFmsyLan)
  
  # PA REFERENCE POINTS
  if(!missing(Blim)) {
    pBlim <- apply(ssbsa > Blim, 1, mean)
    
    i <- max(which(pBlim > .95))
    grad <- diff(Fscan[i + 0:1]) / diff(pBlim[i + 0:1])
    flim <- Fscan[i] + grad * (0.95 - pBlim[i]) # linear interpolation i think..
    
    i <- max(which(pBlim > .90))
    grad <- diff(Fscan[i + 0:1]) / diff(pBlim[i + 0:1])
    flim10 <- Fscan[i]+grad*(0.9-pBlim[i]) # linear interpolation i think..
    
    i <- max(which(pBlim > .50))
    grad <- diff(Fscan[i + 0:1]) / diff(pBlim[i + 0:1])
    flim50 <- Fscan[i]+grad*(0.5-pBlim[i]) # linear interpolation i think..
    
    pBlim <- data.frame(Ftarget = Fscan,value = 1-pBlim,variable="Blim")
    pProfile <- rbind(pProfile,pBlim)
  } else {
    flim <- flim10 <- flim50 <- Blim <- NA
  }
  
  if(!missing(Bpa)) {
    pBpa <- apply(ssbsa > Bpa, 1, mean) 
    pBpa <- data.frame(Ftarget = Fscan,value = 1-pBpa,variable="Bpa")
    pProfile <- rbind(pProfile,pBpa)
  } else {
    Bpa <- NA
  }
  
  # GENERATE REF-TABLE 
  catF <- c(flim, flim10, flim50, vcumCat, Fscan[maxcatm], FCrash05, FCrash50)
  lanF <- c(   NA,    NA,     NA, vcumLan, Fscan[maxlanm],       NA,       NA)
  catC <- approx(Fscan, cats[4,], xout = catF)$y
  lanC <- approx(Fscan, lans[4,], xout = lanF)$y
  catB <- approx(Fscan, ssbs[4,], xout = catF)$y
  lanB <- approx(Fscan, ssbs[4,], xout = lanF)$y
  
  Refs <- rbind(catF, lanF, catC, lanC, catB, lanB)
  rownames(Refs) <- c("catF","lanF","catch","landings","catB","lanB")
  colnames(Refs) <- c("F05","F10","F50","medianMSY","meanMSY","FCrash05","FCrash50")
  
  #TODO: id.sim - user specified.
  
  # 2014-03-12 Ammendments per note from Carmen/John
  # CALCULATIONS:
  
  # Fmsy: value that maximises median LT catch or median LT landings 
  auxi <- approx(Fscan, cats[4, ],xout=seq(min(Fscan),max(Fscan),length=200))
  FmsyMedianC <- auxi$x[which.max(auxi$y)]   
  MSYMedianC <- max(auxi$y)
  # Value of F that corresponds to 0.95*MSY:
  FmsylowerMedianC <- auxi$x[ min( (1:length(auxi$y))[auxi$y/MSYMedianC >= 0.95] ) ]
  FmsyupperMedianC <- auxi$x[ max( (1:length(auxi$y))[auxi$y/MSYMedianC >= 0.95] ) ]
  
  auxi <- approx(Fscan, lans[4, ],xout=seq(min(Fscan),max(Fscan),length=200))
  FmsyMedianL <- auxi$x[which.max(auxi$y)]
  MSYMedianL <- max(auxi$y)
  
  # Value of F that corresponds to 0.95*MSY:
  FmsylowerMedianL <- auxi$x[ min( (1:length(auxi$y))[auxi$y/MSYMedianL >= 0.95] ) ]
  FmsyupperMedianL <- auxi$x[ max( (1:length(auxi$y))[auxi$y/MSYMedianL >= 0.95] ) ]
  
  F5percRiskBlim <- flim
  
  refs_interval <- data.frame(FmsyMedianC = FmsyMedianC,
                              FmsylowerMedianC = FmsylowerMedianC,
                              FmsyupperMedianC = FmsyupperMedianC,
                              FmsyMedianL = FmsyMedianL,
                              FmsylowerMedianL = FmsylowerMedianL,
                              FmsyupperMedianL = FmsyupperMedianL,
                              F5percRiskBlim = F5percRiskBlim,
                              Btrigger = Btrigger)
  
  # END 2014-03-12 Ammendments per note from Carmen/John
  
  sim <- list(ibya=list(Mat = Mat, M = M, Fprop = Fprop, Mprop = Mprop, 
                        west = west, weca = weca, sel = sel),
              rbya=list(ferr=ferr),
              rby=fit$rby,
              rbp=rbp,
              Blim=Blim,
              Bpa=Bpa,
              Refs = Refs,
              pProfile=pProfile,
              id.sim=fit$id.sr,
              refs_interval=refs_interval)
  
  #AC temp
  #Error in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  : 
  #                       span is too small
  #not sure if this line is necessary so remove for now 
  #sim <- eqsim_range(sim)
  
  #return(sim)
  #DM: return a lot more info (could trim this down - see what use)
  return(list(simStks = simStks, sim = sim))
  #return(list(simStks = simStks, ibya = list(Mat = Mat, M = M, Fprop = Fprop, Mprop = Mprop, west = west, weca = weca, sel = sel), 
  #            rbya = list(ferr = ferr), rby = fit$rby, rbp = rbp, Blim = Blim, Bpa = Bpa, Refs = Refs, 
  #            pProfile = pProfile, id.sim = fit$id.sr))
  
}



