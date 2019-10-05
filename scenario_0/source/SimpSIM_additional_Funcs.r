####--- Functions for HCRs for multiannual plan evaluation ------####
## D.C.M. Miller, August 2016

### tacFdiff 
#landsel=ldsel; landwt=ldwt; stknum=stknm; nmort=nm; tac=tc;   fmult=0.2

SegregBloss  <- function(ab, ssb) log(ifelse(ssb >= refPts[["Bloss"]], ab$a * refPts[["Bloss"]], ab$a * ssb))

#function to calculate difference between a given catch and that resulting from fishing at a given F
tacFdiff <- function(fmult, landsel, landwt, stknum, nmort, disrat, tac){

  #fmult - multiplier
  #landsel - selectivity
  #landwt - landings weight at age
  #stknum - number at age in the stock
  #nmort - natural mortality
  #disrat - discard ratio
  #tac - the given catch
  
  #fishing mortality at age
  fmort <- fmult*landsel
  #total (fishing + discard + natural) mortality at age
  zmort <- fmort+(fmort*disrat)+nmort
  
  #absolute difference between the catch (tac) and that arising from the application
  #of the catch equation to the stock with supplied fishery vectors
  return(abs(tac - sum((stknum*(1-exp(-zmort)) * landwt) * (fmort/zmort),na.rm=T)))
  
}

### tacF
## Function minimise difference between TAC and fishing at a given F
#tacF(sel,Wy,Ny,M,Cattmp,rsamsel,rsam,chngF,j)
tacF <- function(sel,Wy,Ny,M,Cattmp,rsamsel,rsam,chngF,j){
  
  
  #initialise F vector for output
  Fout <- Ny[1,j-1,]; Fout[]<- NA

  #selection  
  landsel <- sel[,rsamsel[j-1,]]
  #landings weight
  landwt  <- Wy[,j-1,]
  #stock numbers
  stknum  <- Ny[,j-1,]
  #natural mortality
  nmort <- M[,rsam[j-1,]]
  #the TAC
  tac <- Cattmp[j-1,]
  #discards ratio (for calculation of Z)
  disrat <- 0 #no discards
    
  for(r in 1:length(chngF)) Fout[r] <- optimize(tacFdiff, 
                                                c(0, 100),
                                                landsel = landsel[,r], 
                                                landwt = landwt[,r], 
                                                stknum = stknum[,r], 
                                                nmort = nmort[,r], 
                                                disrat = disrat,
                                                tac = tac[r],
                                                tol = 0.0000001)$minimum
  
  return(Fout)

}

fFindF <- function(N,W,M,Sel,tgt,yr){
  
  #return the appropriate fishing mortality vector that yields the provided catch
  
  ages <- nrow(N)
  iters <- ncol(N)
  
  #dimension and initialise return array
  ret <- array(data=NA,dim=c(ages,iters))

  for (i in 1:iters){  
    ret[,i] <- optimize(f = fDelta,
                        interval = c(0,100),
                          N = N[,i],
                          M = M[,i],
                          Sel = Sel[,i],
                          W = W[,i],
                          tgt = tgt,
                          tol = 0.0000001)$minimum
  }
  
  return(ret)
  
}

fDelta <- function(Fmult,N,M,Sel,W,tgt){
  
  #Fmult - fishing mortality multiplier
  #N - population numbers
  #M - natural mortality
  #Sel - selection pattern
  #W - catch weights
  #tgt - the target catch

  #fishing mortality
  F <- Fmult*Sel
  #total mortality
  Z <- F + M
  
  #return the difference between the targetCatch and that arising from the 
  #stock vectors provided
  ret <- abs(tgt - sum(N*(1-exp(-Z))*W*(F/Z),na.rm=TRUE))
  
  ret
  
}

#function to implement stochasticity on starting numbers at age
fObsModel <- function(true,cv,ac=0){
  
  #the true, underlying numbers
  #ac, the autocorrelation
  
  #transformation of true population to observed population carried out in 3 phases
  #1) a normal random bias multiplier is applied to all numbers. This bias is characterised by a mean value and a standard deviation
  #2) a lognormal random noise applied to each number at age. This is unbiased and characterised by a SD on the log transformed data
  #3) an option to include autocorrelation (if ac>0)
  
  cvs <- cv
  ret <- rep(NA,length(true))
  
  #mean of year factor
  yfctobs <- 1
  #constant year bias
  biasobs <- 0
  
  #year factor
  yearfactor <- (1+biasobs)*exp(rnorm(1)*yfctobs)
  yearfactor <- 1
  
  #age factors
  for (a in seq(1,length(cvs))){
    if (cvs[a]>0){ 
      ret[a] <- true[a]*yearfactor*exp(rnorm(1)*cvs[a])
    } else {
      ret[a] <- true[a]*yearfactor
    }
  }
  
  ret
  
}
