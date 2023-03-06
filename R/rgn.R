#******************************************************************
#
# Purpose: Optimize SLS Objective Function with Robust Gauss-Newton Algorithm
#
# Programmer: George Kuczera, Youwei Qin, Dmitri Kavetski
# Created: 21 May 2016 at Newcastle, Australia
# Last modified: 15 July 2018 at Nanjing, China
# Copyright, George Kuczera, Youwei Qin, Dmitri Kavetski, 2018-2023. All rights reserved.
# Modified by Michael Leonard, Dec 2022 without permission
# References
# * Qin2018a: Youwei Qin, Kavetski Dmitri, George Kuczera (2018),
#            A Robust Gauss-Newton algorithm for the optimization of hydrological models: From standard Gauss-Newton to Robust Gauss-Newton,
#            Water Resources Research, accept

# * Qin2018b: Youwei Qin, Kavetski Dmitri, George Kuczera (2018),
#            A Robust Gauss-Newton algorithm for the optimization of hydrological models: Benchmarking against industry-standard algorithms,
#            Water Resources Research, accept
#
#******************************************************************
# ---
# Input
#   p:          Number of parameters
#   n:          Number of observations in calibration
#   x0:         Initial parameters
#   xLo:        Lower bounds on parameters
#   xHi(:):     Upper bounds on parameters
#   cnv:        Convergence data structure
#   decFile:    Name of dumpfile
# ---
# Output
#   info:       Run information data structure
#   x(:):       Final parameters
#   error       Error code, 0 = ok
#   message:    Error message
# ---
# Notes
#   This module follows fairly closely to the pseudocode in Qin2018a,
#   Any issues or bugs, please contact the first author(Email:youwei.qin@uon.edu.au)

#source("constantsMod.R")

NO=FALSE; YES=TRUE
EPS=.Machine$double.eps

rgnConvType=list()
rgnConvType$iterMax=0
rgnConvType$noReduction=0
rgnConvType$noRelChangeF=0
rgnConvType$noRelChangePar=0
rgnConvType$fail=0
rgnConvType$noRelChangeFTol=0.0
rgnConvType$noRelChangeParTol=0.0
rgnConvType$tolSafe=0.0
rgnConvType$dumpResults=0
rgnConvType$logFile=""

rgnInfoType=list()
rgnInfoType$nIter=0
rgnInfoType$termFlag=0
rgnInfoType$nEval=0
rgnInfoType$f=0.0
rgnInfoType$cpuTime=0.0
rgnInfoType$objTime=0.0

rgnSetType=list()
rgnSetType$gtol=0.0
rgnSetType$stol=0.0
rgnSetType$ftol=0.0
rgnSetType$gtolMin=0.0
rgnSetType$tol=0.0
rgnSetType$tolFor=0.0
rgnSetType$alpha=0.0
rgnSetType$sigma=0.0
rgnSetType$c1=0.0
rgnSetType$rho=0.0
rgnSetType$beta=0.0
rgnSetType$hLow=0.0
rgnSetType$hHiFrac=0.0
rgnSetType$xScale=0.0
rgnSetType$fScale=0.0
rgnSetType$nls=0

set <<- rgnSetType

# mimic F90 functionality
PRESENT=function(x=NULL){
  ans=!is.null(x)
  return(ans)
}
SIZE=function(x,k=NULL){
  if(PRESENT(k)){
    return(dim(x)[k]) # matrix
  }else{
    return(length(x)) # vector
  }
}
DOT_PRODUCT=function(u,v){
  return(sum(u*v))
}
MATMUL=function(A,B){
  if(is.matrix(A)&is.matrix(B)){
    return(A%*%B) # both matrices - so proceed with %*%
  }else{
    if(length(A)==1|length(B)==1){
      return(A*B) # a scalar involved, so use regular *
    }else{
      return(A%*%B) # vector/matrix - %*% seems ok
    }
  }
}
TRANSPOSE=function(A){
  return(t(A))
}
MERGE=function(A,val,mask){
  # this is a very shoddy implementation
  # manually handle vector or matrix
  # check if inputs are conformal and fix as needed by assuming shape of mask
  if(is.vector(mask)){ # scalar or vector
    n=length(mask)
    val.arr=val
    A.arr=A
    if(length(val.arr)<n) val.arr=rep(val,n) # assume a scalar provided for val and coerce to a vector
    if(length(A.arr)<n) A.arr=rep(A,n) # assume a scalar provided for A and coerce to a vector
  }else{ # assume it is a matrix - we will not support array dim >2
    nr=nrow(mask)
    nc=ncol(mask)
    val.arr=val
    A.arr=A
    if(!is.matrix(val.arr)) val.arr=matrix(val,nr,nc) # assume a scalar provided for val and coerce to a matrix
    if(!is.matrix(A.arr)) A.arr=matrix(A,nr,nc) # assume a scalar provided for A and coerce to a matrix
  }
  #perform the merge
  ind=which(!mask)
  if(length(ind)>0) A.arr[ind]=val.arr[ind]
  return(A.arr)
}
MAXVAL=function(A,mask){max(A[mask])}
MINVAL=function(A,mask){min(A[mask])}
MIN=function(...){
  dots=list(...) # capture the list of inputs
  n=length(dots)
  nindiv=rep(0,n)
  for(i in 1:n)nindiv[i]=length(dots[[i]])
  mxn=max(nindiv) # max length of any inputted item
  if(mxn==1){ # simple
    return(min(...))
  }else{ # we need to perform elementwise
    out=rep(9999999999999,mxn)
    for(j in 1:mxn){ # repeat for all elements
      for(i in 1:n){ # loop over all inputted objects
        if(j<=nindiv[i]) out[j]=min(out[j],dots[[i]][j]) # protect against not all shapes being conformal
      }
    }
    return(out)
  }
}
MAX=function(...){
  dots=list(...) # capture the list of inputs
  n=length(dots)
  nindiv=rep(0,n)
  for(i in 1:n)nindiv[i]=length(dots[[i]])
  mxn=max(nindiv) # max length of any inputted item
  if(mxn==1){ # simple
    return(max(...))
  }else{ # we need to perform elementwise
    out=rep(-9999999999999,mxn)
    for(j in 1:mxn){ # repeat for all elements
      for(i in 1:n){ # loop over all inputted objects
        if(j<=nindiv[i]) out[j]=max(out[j],dots[[i]][j]) # protect against not all shapes being conformal
      }
    }
    return(out)
  }
}
ABS=function(x){abs(x)}

# Initialize the RGN converge variables
setDefaultRgnConvergeSettings=function(iterMax=NULL, dump=NULL, logFile=NULL, fail=NULL){
  #optional arguments iterMax, fail, dump, logFile
  cnvSet=rgnConvType
  #---
  #
  cnvSet$iterMax = 100; if(PRESENT(iterMax)) cnvSet$iterMax = iterMax
  cnvSet$fail = 100000; if(PRESENT(fail)) cnvSet$fail = fail
  cnvSet$noReduction = 4
  cnvSet$noRelChangeFTol = 1.0e-5
  cnvSet$noRelChangeF = 5
  cnvSet$noRelChangeParTol = 1.0e-5
  cnvSet$noRelChangePar = 5
  cnvSet$tolSafe = 1.0e-14
  cnvSet$dumpResults = 0; if(PRESENT(dump)) cnvSet$dumpResults = dump
  cnvSet$logFile = 'rgnLog.txt'; if(PRESENT(logFile)) cnvSet$logFile = logFile
  return(cnvSet)
}

  # Initialize the RGN constants to global variable
setRgnConstants=function(alpha=NULL, beta=NULL, nls=NULL){
  # optional integer: nls
  # optional real: alpha, beta
  #---
  # NOTE - Side-effect - global variable via <<- operator
  #set = rgnSetType
  set$gtol <<- 1.0e-10
  set$stol <<- 1.0e-11
  set$ftol <<- 1.0e-10
  set$gtolMin <<- 0.1
  set$tol <<- 0.1
  set$tolFor <<- 0.1
  set$alpha <<- 0.001; if(PRESENT(alpha)) set$alpha <<- alpha
  set$sigma <<- 1.0
  set$c1 <<- 0.0001
  set$rho <<- 0.6
  set$nls <<- 4; if(PRESENT(nls)) set$nls <<- nls
  set$beta <<- 10.0; if(PRESENT(beta)) set$beta <<- beta
  set$hLow <<- 1.0e-8
  set$hHiFrac <<- 0.5
  set$xScale <<- 10.0
  set$fScale <<- 1.0
} # END setRgnConstants
#
goto1=function(procnam){
  return(list(error = 1, message = paste("f-",procnam,"RGN objFunc call failed")))
}


objFuncCall = function(simFunc,x,simTarget,weights,...){
  #time for evaluating
  timeObj = vector(length=2)
  timeObj[1] = Sys.time()
  sim = simFunc(x=x,...)
  r = simTarget-sim
  r = r[!is.na(r)]
  f = sum(r^2)
  f = f/2.0
  timeObj[2] = Sys.time()
  timeFunc=timeObj[2]-timeObj[1]
  outObjFunc = list(r=r, f=f, timeFunc=timeFunc) # DM to do: add error number and message to this

#  browser()

  return(outObjFunc)
}


#
# Robust Gauss-Newton code based on Qin2018a
rgn=function(simFunc, x0, xLo, xHi, cnv, simTarget, info, decFile=NULL, weights=NULL, ...){
  # input objFunc - function pointer to objective function
  # input real: p        # Number of parameters
  # input real: n        # Number of observations in calibration
  # input real: x0(:)    # Initial parameters
  # input real: xLo(:)   # Lower bounds on parameters
  # input real: xHi(:)   # Upper bounds on parameters
  # input rgnConvType: cnv      # Convergence data structure
  # output rgnInfoType: info     # Run information data structure
  # output real : x(:)     # Final parameters
  # output integer: error    # error code 0= ok
  # output character: message  # error message
  # output optional character: decFile  # dumpfile name
  # Locals

  p = length(x0)
  n = length(simTarget[!is.na(simTarget)])
  if(is.null(weights)){weights=rep(1,n)}
  nIter=0; i=0; j=0; k=0; m=0; nrls=0; nf=0; iMax=0; nr=0; termCode=0; noReduction=0; noRelChangeF=0; noRelChangePar=0
  forceRelease=FALSE; flag_ls=FALSE; xist=FALSE
  f=0.0; fl=0.0; fh=0.0; fBest=0.0; gMaxFree=0.0; gMaxThawn=0.0; minSingFrac=0.0; sig=0.0; fredExp=0.0; ft=0.0; fls=0.0; cons=0.0; scaledG=0.0; scaledS=0.0; scaledfExp=0.0; scaledfAct=0.0; fredAct=0.0; fOldBest=0.0; maxRelPar=0.0; time=c(0.0,0.0); gradDf=0.0; hessDf=0.0
  #local parameters
  procnam="rgnMain"
  time4fcall=0.0;time4fcallAcc=0.0
  dfm=c("","","","")
  BF=0; BL=1; BFL=2; BH=3; BFH=4;NUL_CON=-1; GRAD_CON=0; SEARCH_CON=1; FRED_CON=2

  #----
  # CONTAINED functions - declared first in R
  updateBest=function(f, x, r,fBest){
    # input real x(:), r(:), f
    update=FALSE;xBest=NULL;rBest=NULL
    if(f < fBest){
      update=TRUE
      fBest = f
      xBest = x
      rBest = r
    }
    return(list(update=update,fBest=fBest,xBest=xBest,rBest=rBest))
  } #END updateBest
  updateHess=function(Hess, k){
    # input integer: k
    # output real Hess(:,:)
    diagK=Hess[k,k]
    Hess[k, ]=zero; Hess[ ,k]=zero
    Hess[k,k]=diagK
    return(Hess)
  } #END updateHess


  error=0                              # Initialize error flag
  message=""                           # Initialize message
  time4fcall=0.0;time4fcallAcc=0.0
  # Allocate work arrays
  h=rep(0.0,p)
  r=rep(0.0,n)
  rBest=rep(0.0,n)
  rl=rep(0.0,n)
  rh=rep(0.0,n)
  xl=rep(0.0,p)
  xh=rep(0.0,p)
  xBest=rep(0.0,p)
  Ja=matrix(0.0,n,p)
  g=rep(0.0,p)
  He=matrix(0.0,p,p)
  as=rep(0,p)
  xScale=rep(0.0,p)
  xp=rep(0.0,p)
  xt=rep(0.0,p)
  delX=rep(0.0,p)
  xls=rep(0.0,p)
  hLo=rep(0.0,p)
  hHi=rep(0.0,p)
  x0ldbest=rep(0.0,p)
  fOptSeries=rep(0.0,cnv$iterMax)
  delXAct=rep(0.0,p)


  if(cnv$dumpResults >= 1){ # Fortran formats are not relevant in R, need to swap over to fortmat() function
    dfm[1]=paste('(a,', p,'g15.7)',sep="")
    dfm[2]=paste('(a,', p,'i3)',sep="")
    dfm[3]=paste('(33x,', p,'g15.7)',sep="")
    dfm[4]=paste('(a,', p,'(i4,11x))',sep="")
    #write(dfm,file=cnv$logFile)
    write("R output",file=cnv$logFile)
  }
  #
  # Assign constants
  setRgnConstants()
  xScale = rep(set$xScale,p)
  hLo = set$hLow; hHi =  set$hHiFrac*(xHi-xLo)
  #
  # Initialize
  as[1:p] = rep(BF,p)  # Assume all initial parameters are free
  nIter = 0
  h = hHi
  forceRelease = NO
  noReduction = 0; noRelChangeF = 0; noRelChangePar = 0
  info$termFlag = 0; info$nEval = 0
  time[1]=Sys.time()
  x = x0
#  tmp=objFunc(x,...); f=tmp$f;rBest=tmp$r;time4fcall=tmp$timeFunc  #SUB2FUNC conversion
  tmp=objFuncCall(simFunc=simFunc,x=x,simTarget=simTarget,weights=weights,...); f=tmp$f;rBest=tmp$r;time4fcall=tmp$timeFunc  #SUB2FUNC conversion
  info$nEval = info$nEval + 1; if(error !=0) return(goto1(procnam))
  fBest = f; xBest = x
  time4fcallAcc=time4fcallAcc+time4fcall
  #CALL userRunTimeMessage ('Starting RGN', -1)
  #-------------
    # RGN iteration loop
  iterLoop=TRUE
  while(iterLoop){
    nIter = nIter + 1
    #
    # Save best result from previous iteration
    fOldBest = fBest; x0ldBest = xBest

    if(cnv$dumpResults >= 1) {
      write('-----------------------------------------',file=cnv$logFile,append=TRUE)
      write(paste('Iteration No.=                 ', nIter),file=cnv$logFile,append=TRUE)
      write(paste('ObjFun Calls=                  ', info$nEval),file=cnv$logFile,append=TRUE)
      write(paste('ObjFun Value f=                 ', f),file=cnv$logFile,append=TRUE)
      write(paste('Parameter set=                  ', paste(x,collapse=" ")),file=cnv$logFile,append=TRUE)
      write(paste('Bound Index=                    ', paste(MERGE(-1, MERGE(0, 1, x[1:p] <= xHi[1:p]), x[1:p] < xLo[1:p]),collapse=" ")),file=cnv$logFile,append=TRUE)
      write(paste('Sampling Scale h=               ', paste(h,collapse=" ")),file=cnv$logFile,append=TRUE)
    }
    #
    # Get Jacobian and update best function result
    xh = x; xl = x; r = rBest
    for(k in 1:p){
      xh[k] = x[k] + h[k]; xh[k] = MIN(xHi[k], xh[k])
      if(cnv$dumpResults >= 2) write(paste('Forward Jacoian sample point:   ', paste(xh,collapse=" ")),file=cnv$logFile,append=TRUE)
#      tmp=objFunc(xh,...); fh=tmp$f;rh=tmp$r;time4fcall=tmp$tFunc #SUB2FUNC conversion
      tmp=objFuncCall(simFunc=simFunc,x=xh,simTarget=simTarget,weights=weights,...); fh=tmp$f;rh=tmp$r;time4fcall=tmp$tFunc #SUB2FUNC conversion
      info$nEval = info$nEval + 1; if(error !=0) return(goto1(procnam)); tmp=updateBest(fh, xh, rh,fBest);if(tmp$update){fBest=tmp$fBest;xBest=tmp$xBest;rBest=tmp$rBest} #SUB2FUNC conversion
      xl[k] = x[k] - h[k]; xl[k] = MAX(xLo[k], xl[k])
      time4fcallAcc=time4fcallAcc+time4fcall
      if(cnv$dumpResults >= 2) write(paste('Backward Jacobian sample Point: ', paste(xl,collapse=" ")),file=cnv$logFile,append=TRUE)
#      tmp=objFunc(xl,...); fl=tmp$f;rl=tmp$r;time4fcall=tmp$tFunc  #SUB2FUNC conversion
      tmp=objFuncCall(simFunc=simFunc,x=xl,simTarget=simTarget,weights=weights,...); fl=tmp$f;rl=tmp$r;time4fcall=tmp$tFunc  #SUB2FUNC conversion
      info$nEval = info$nEval + 1; if(error !=0) return(goto1(procnam)); tmp=updateBest(fl, xl, rl,fBest);if(tmp$update){fBest=tmp$fBest;xBest=tmp$xBest;rBest=tmp$rBest} #SUB2FUNC conversion
      time4fcallAcc=time4fcallAcc+time4fcall
      Ja[ ,k] = (rh-rl)/(xh[k]-xl[k])
      xh[k] = x[k]; xl[k] = x[k]
      if(cnv$dumpResults >= 2) write(paste('Jacobian matrix column:          ', k, fh, fl),file=cnv$logFile,append=TRUE)
    }
    #
    # Calculate gradient and Hessian
    for(i in 1:p){
      g[i] = DOT_PRODUCT(Ja[ ,i],r)
      for(j in 1:i){
        He[i,j] = DOT_PRODUCT(Ja[ ,i],Ja[ ,j])
        He[j,i] = He[i,j]
      }
    }
    #
    # Perform active set update
    for(k in 1:p){
      if(x[k] <= xLo[k] + 10.0*EPS*MAX(ABS(xLo[k]),xScale[k])){
        as[k] = MERGE(BFL, BL, g[k] < 0.0)
        He=updateHess(He, k)
      }else if(x[k] >= xHi[k] - 10.0*EPS*MAX(ABS(xHi[k]),xScale[k])){
        as[k] = MERGE(BFH, BH, g[k] > 0.0)
        He=updateHess(He, k)
      }else{
        as[k] = BF
      }
    }

    if(cnv$dumpResults >= 2) {
      write(paste('Best objective function value f=', fBest),file=cnv$logFile,append=TRUE)
      write(paste('Best parameter x=               ', paste(xBest,collapse=" ")),file=cnv$logFile,append=TRUE)
      write(paste('Gradient g at parameter x=       ', paste(g,collapse=" ")),file=cnv$logFile,append=TRUE)
      write(paste('Hessian at x=                    ', He[1,1]),file=cnv$logFile,append=TRUE)
      if (p>1){
        for(j in 2:p){
          write(He[j,1:j],file=cnv$logFile,append=TRUE)
        }
      }
    }
    #
    # Determine termination code and hence status of forcRelease
    if(nIter > 1){
      termCode = NUL_CON
      scaledG = 0.0
      for(k in 1:p){
        if(as[k] == BF | as[k] == BFL | as[k] == BFH){
          scaledG = MAX (scaledG, ABS(g[k])*MAX(ABS(x[k]),xScale[k])/MAX(f,set$fScale))
        }
      }
      scaledS = 0.0
      for(k in 1:p){
        scaledS = MAX (scaledS, ABS(delXAct[k])/MAX(ABS(x[k]),xScale[k]))
      }
      scaledfExp = ABS(fredExp)/MAX(f,set$fScale)
      scaledfAct = ABS(fredAct)/MAX(f,set$fScale)

      if(scaledG <= set$gtol){
        termCode = GRAD_CON
      }else if(scaledS <= set$stol & scaledG <= set$gtolMin) {
        termCode = SEARCH_CON
      }else if(scaledfExp <= set$ftol & scaledfAct <= set$ftol & scaledG <= set$gtolMin) {
        termCode = FRED_CON
      }

      nf = sum(MERGE(1, 0, as == BF))
      if(nf == 0) {
        forceRelease = YES
      }else if(termCode != NUL_CON) {
        forceRelease = YES
      }else{
        forceRelease = NO
      }
    }

    #
    # Check conditions for releasing parameters
    nrls = sum(MERGE(1, 0, as == BFL | as == BFH))
    nf = sum(MERGE(1, 0, as == BF))
    if(nrls > 0) {
      if(nf > 0) {
        gMaxFree = MAXVAL(ABS(g)*MAX(ABS(x),xScale), mask = as == BF)
        for(k in 1:p){
          if(ABS(g[k])*MAX(ABS(x[k]),xScale[k])>= set$tol*gMaxFree & (as[k] == BFL | as[k] == BFH)) {
            as[k] = BF
          }
        }
      }
      if(forceRelease) {
        iMax = 0
        gMaxThawn = 0.0
        for(k in 1:p){
          if(ABS(g[k]*MAX(ABS(x[k]),xScale[k])) > gMaxThawn & (as[k] == BFL | as[k] == BFH)) {
            gMaxThawn = ABS(g[k]*MAX(ABS(x[k]),xScale[k])); iMax = k
          }
        }
        if(iMax > 0) as[iMax] = BF
        for(k in 1:p){
          if(ABS(g[k]*MAX(ABS(x[k]),xScale[k])) > set$tolFor*gMaxThawn & (as[k] == BFL | as[k] == BFH)) as[k] = BF
        }
      }
    }

    #
    # Solve normal equations after removing non-free parameters
    if(cnv$dumpResults >= 2) write(paste('Active set=                     ', paste(as,collapse=" ")),file=cnv$logFile,append=TRUE)
    nr = sum(MERGE(rep(1,length(as)), 0, as == BF))

    HeRdc=matrix(0.0,nr,nr); delXRdc=rep(0.0,nr); gRdc=rep(0.0,nr); tsv=rep(0.0,nr)

    j = 0
    for(k in 1:p){
      if(as[k] == BF) {
      j = j + 1; gRdc[j] = g[k]
      m = 0
      for(i in 1:p){
        if(as[i] == BF) {
          m = m + 1; HeRdc[m,j] = He[i,k]
        }
      }
      HeRdc[j, ] = HeRdc[ ,j]
      }
    }
    minSingFrac = set$alpha*sqrt(EPS)
    tmp=svdSolve(m=nr, n=nr, A=HeRdc, b=-gRdc, x=delXRdc, tS=tsv, error=error, message=message, minSingFrac=minSingFrac);delXRdc=tmp$x;tsv=tmp$tS;error=tmp$error;message=tmp$message #SUB2FUNC conversion

    if(error !=0) return(goto1(procnam))
    j = 0
    for(k in 1:p){
      if(as[k] == BF) {
      j = j + 1; delX[k] = delXRdc[j]
      }else{
        delX[k] = 0.0
      }
    }
    if(cnv$dumpResults >= 1) write(paste('Truncated SV=                   ', paste(tsv,collapse=" ")),file=cnv$logFile,append=TRUE)
    #
    # Project search step onto box constraints
    if(cnv$dumpResults >= 2) write(paste('SVD delX=                       ', paste(delX,collapse=" ")),file=cnv$logFile,append=TRUE)
    xt = x + delX
    xp = MIN(xHi, MAX(xLo, xt))
    delX = xp - x
    if(cnv$dumpResults >= 2) {
      write(paste('Projected delX=                 ', paste(delX,collapse=" ")),file=cnv$logFile,append=TRUE)
      write(paste('Projected xp=                   ', paste(xp,collapse=" ")),file=cnv$logFile,append=TRUE)
    }
    #
    # Update delXRdc for calculation fredExp
    j = 0
    for(k in 1:p){
      if(as[k] == BF) {
        j = j + 1
        delXRdc[j] = delX[k]
      }
    }
    #
    # Calculate the expected function reduction with constrained delXRdc
    gradDf = DOT_PRODUCT(gRdc,delXRdc)
    hessDf = DOT_PRODUCT(delXRdc,MATMUL(HeRdc,delXRdc))
    fredExp = gradDf + 0.5*hessDf

    #
    # Perform inexact line search
    sig = MIN (set$sigma, 1.0)
    cons = set$c1*MIN(0.0, DOT_PRODUCT(delX,g))
    if(cnv$dumpResults >= 3) write(paste('Cons=                            ', cons),file=cnv$logFile,append=TRUE)
    flag_ls = NO
    for(i in 0: set$nls){
      xt = x + sig*delX
#      tmp=objFunc(xt,...);rl=tmp$r;ft=tmp$f;time4fcall=tmp$tFunc #SUB2FUNC conversion
      tmp=objFuncCall(simFunc=simFunc,x=xt,simTarget=simTarget,weights=weights,...);rl=tmp$r;ft=tmp$f;time4fcall=tmp$tFunc #SUB2FUNC conversion
      info$nEval = info$nEval + 1; if(error !=0) return(goto1(procnam))
      time4fcallAcc=time4fcallAcc+time4fcall
      if(cnv$dumpResults >= 3) {
        write(paste('xt=                             ', paste(xt,collapse=" ")),file=cnv$logFile,append=TRUE)
        write(paste('ft=                             ', ft),file=cnv$logFile,append=TRUE)
        write(paste( 'ft+sig=                         ',ft + sig*cons),file=cnv$logFile,append=TRUE)
      }
      if(ft < f + sig*cons) {
        xls = xt; fls = ft; flag_ls = YES
        if(cnv$dumpResults >= 1) write(paste('Line search successful at iteration', i ,' with sigma=', sig),file=cnv$logFile,append=TRUE)
        break
      }else{
        sig = set$rho*sig
      }
    }
    if(!flag_ls) {
      if(cnv$dumpResults >= 1) write('Line search failed',file=cnv$logFile,append=TRUE)
      fls = f; xls = x
    }
    fredAct = fls - f
    delXAct = xls - x # # Save actual search step for termination calculation
    #
    # Update best variables
    if(fBest < fls) {    # Jacobian evaluation produced better f than line search
      x = xBest; f = fBest
      flag_ls = YES
    }else{
      x = xls; f = fls
      #CALL updateBest (fls, xls, rl)
      tmp=updateBest(fls, xls, rl,fBest);if(tmp$update){fBest=tmp$fBest;xBest=tmp$xBest;rBest=tmp$rBest} #SUB2FUNC conversion
    }
    #
    # Store the value of best objective function for termination check
    fOptSeries[nIter] = fBest
    #
    # Update sampling scale
    if(flag_ls) {
      #         if(flag_ls & !(noReduction > cnv$fail | noRelChangeF > cnv$fail | noRelChangePar > cnv$fail)) {  #GAK enhance
      h = MIN (set$beta*h, hHi)
    }else{
      h = MAX (h/set$beta, hLo)
    }
    #
    # Check for convergence
    if(nIter >= cnv$iterMax) {
      info$termFlag = 1; break
    }
    if(nIter > 1) {
      noRelChangeF = 0
      for(k in MAX(1,nIter - cnv$noRelChangeF + 1):nIter){
        if(ABS((fOptSeries[k]-fOptSeries[nIter])/(fOptSeries[k]+cnv$tolSafe)) <= cnv$noRelChangeFTol) {
          noRelChangeF = noRelChangeF + 1
        }
      }
      if(noRelChangeF >= cnv$noRelChangeF) {
        info$termFlag = 2; break
      }

      noReduction = MERGE (noReduction+1, 0, f >= fOldBest)
      if(noReduction >= cnv$noReduction) {
        info$termFlag = 3; break
      }

      maxRelPar = -hugeRe
      for(k in 1:p){
        if(as[k] == BF) {
          maxRelPar = MAX (maxRelPar, ABS((x0ldBest[k]-x[k])/(x0ldBest[k]+cnv$tolsafe)))
        }
      }
      noRelChangePar = MERGE (noRelChangePar+1, 0, maxRelPar >= 0.0 & maxRelPar < cnv$noRelChangeParTol)
      if(noRelChangePar >= cnv$noRelChangePar) {
        info$termFlag = 4; EXIT
      }
    }
  } #END iterLoop

  #
  # Save optional information
  time[2]=Sys.time(); info$cpuTime = time[2] - time[1];info$objTime=time4fcallAcc
  info$nIter = nIter; info$f = f
  write(paste('RGN ended with termination code: ', info$termFlag, ' f=', info$f),file=cnv$logFile,append=TRUE)
  if(cnv$dumpResults >= 1) {
    write(paste('>>>>> RGN ended with termination code: ', info$termFlag),file=cnv$logFile,append=TRUE)
    write(paste('      number of function calls:    ', info$nEval),file=cnv$logFile,append=TRUE)
    write(paste('      cpu time (sec):               ', info$cpuTime),file=cnv$logFile,append=TRUE)
  }
  return(list(x=x,info=info,error=error,message=message))
} # END rgn

# ------------------------------ EVERYTHING BELOW THIS LINE COULD BE REPLACED WITH AN R NATIVE SVDSOLVER, e.g. svd()------------------------------------
svdSolve=function(m, n, A, b, x=NULL, Ainv=NULL, S=NULL, tS=NULL, error, message, minSingFrac=NULL, minSingVal=NULL, cn=NULL){
  # Solves Ax=b using SVD decomposition followed setting singular values to zero and then back substitution
  # input integer m,n
  # input real A(:,:)
  # input real b(:)
  # input optional minSingFrac
  # output
  #output real optional x(:)
  #output real optional Ainv(:,:)
  #output real optional S(:)
  #output real optional tS(:)
  error = 0
  message = 'ok'
  # output real optional minSingVal
  # output real optional cn
  # local
  i=0; j=0; k=0; nite=0
  U=matrix(0,m,n)
  SD=matrix(0,n,n)
  W=rep(n,0)
  V=matrix(0,n,n)
  tmp=rep(n,0)
  wMin=0.0

  #----
  # Check consistency of dimension
  if(SIZE(A,1) != m) { error = 1; message = 'm not same as assumed size in A(m,n)' }
  if(SIZE(A,2) != n) { error = 1; message = 'n not same as assumed size in A(m,n)' }
  if(PRESENT(b) & PRESENT(x)) {
    if(SIZE(b) != n)   { error = 1; message = 'n not same as assumed size in b(n)'   }
    if(SIZE(x) != n)   { error = 1; message = 'n not same as assumed size in x(n)'   }
  }
  if(PRESENT(S)) {
    if(SIZE(S) != n)   { error = 1; message = 'n not same as assumed size in S(n)'   }
  }
  if(error!=0) {
    return(list(x=x, Ainv=Ainv, S=S, tS=tS, error, message, error=error,message=message, minSingVal=minSingVal, cn=cn))
  }
    # Perform SVD of A

  # Singular value decomposition
  tmp=svdDecomp (a=A, u=U, s=SD, v=V, nite=nite);U=tmp$u;SD=tmp$s;V=tmp$v;nite=tmp$nite #SUB2FUNC conversion

  # Dealing with U and V, in the opposite direction
  U=-U; V=-V
  for(n in 1:SIZE(V,1)) W[n] = SD[n,n]
  if(PRESENT(S)) S = W

  # Zero singular values
  if(PRESENT(minSingFrac)){
    wMin = minSingFrac
  }else{
    wMin = sqrt(EPS)
  }
  wMin = MAXVAL(W)*wMin
  if(PRESENT(minSingVal)) minSingVal = wMin
  W = MERGE(W, 0.0, W > wMin)
  if(PRESENT(tS)) tS = W
  if(PRESENT(cn)) cn = MAXVAL(W)/MINVAL(W)
  #
  # Get x using back substitution
  if(PRESENT(b) & PRESENT(x)) {
    tmp.out=svdBackSub (m=m, n=n, U=U, W=W, V=V, b=b, x=x, error=error, message=message);x=tmp.out$x;error=tmp.out$error;message=tmp.out$message # SUB2FUNC conversion
  }
  #
  # Get inverse
  if(PRESENT(Ainv)){
    if(m == n){
      for(i in 1:n){
        for(j in 1:i){
          for(k in 1:n){
            if(W[k] > 0.0) {
              tmp[k] = V[i,k]/W[k]
            }else{
              tmp[k] = 0.0
            }
          }
          Ainv[i,j] = DOT_PRODUCT(tmp[1:n],U[j,1:n])
          Ainv[j,i] = Ainv[i,j]
        }
      }
    }else{
      error = 1; message = 'cannot get inverse for non-square matrix A'
    }
  }
  #
  return(list(x=x, Ainv=Ainv, S=S, tS=tS, error, message, error=error,message=message, minSingVal=minSingVal, cn=cn))
} #END svdSolve

# Singular value decomposition
svdDecomp=function(a, u, s, v, nite){
  # real input A(:,:)
  # outputs
  #u=matrix(0.0,SIZE(a,1), SIZE(a,2))
  #s=matrix(0.0,SIZE(a,2), SIZE(a,2))
  #v=matrix(0.0,SIZE(a,2), SIZE(a,2))
  #nite=0
  # locals
  q1=matrix(0.0,SIZE(a,1), SIZE(a,2))
  u1=matrix(0.0,SIZE(a,1), SIZE(a,1))
  q=matrix(0.0,SIZE(a,2), SIZE(a,2))
  e=matrix(0.0,SIZE(a,2), SIZE(a,2))
  f=rep(0.0,SIZE(a,2))
  err=0.0
  n=0
  # init u,v,u1
  for(n in 1:SIZE(a,1)) u1[n,n] = 1.0
  for(n in 1:SIZE(a,2)) v[n,n] = 1.0
  # initial state:
  tmp=Qr(a=a, q=q1, r=s);q1=tmp$q;s=tmp$r # SUB2FUNC conversion
  u = MATMUL(u1, q1)
  tmp=Qr(TRANSPOSE(s), q, s);q=tmp$q;s=tmp$r # SUB2FUNC conversion
  v = MATMUL(v, q)
  # iterate while converged:
  nite = 1
  #while(TRUE){
  for(i in 1:1000){ # this gives a timeout option when we do not achieve convergence to precision - EPS
    tmp=Qr(TRANSPOSE(s), q, s);q=tmp$q;s=tmp$r # SUB2FUNC conversion
    u = MATMUL(u, q)
    tmp=Qr(TRANSPOSE(s), q, s);q=tmp$q;s=tmp$r # SUB2FUNC conversion
    v = MATMUL(v, q)
    # check the error:
    e = Triu(s)
    f = Diag_ele(s)
    err = Norm(as.vector(e))/ Norm(f) # RESHAPE conversion, carefully checked instance column from matrix to vector
    nite = nite + 1
    #print(paste(nite,err,EPS)) #MLDHACK DEBUG
#    if(err < (EPS)^0.65) break #############3for some reason I cannot get same precision, so MLHACK here as temp fix - precision seems to max out at 1e-11
    if(err < EPS) break # DM: this seems to work for hymod example
  }
  if(i>1000) print(paste("convergence issue, only achieved:",err))
  return(list(u=u, s=s, v=v, nite=nite))
} #END svdDecomp

# L2-norm
Norm=function(x){
  # x is a real vector
  return(sqrt(sum(x**2)))
}
# Diagonal elements
Diag_ele=function(a){
  # A is a real matrix
  # v is a real vector

  i=0;n=0

  n = min(c(SIZE(a,1), SIZE(a,2)))
  v=rep(0.0,n)
  for(i in 1:n){v[i] = a[i,i]}
  return(v)
} #END Diag_ele
# Upper triangular part
Triu=function(a){
  # A is a real matrix
  # au is a real vector

  i=0;j=0;n=0;m=0

  m = SIZE(a,1)
  n = SIZE(a,2)
  au=matrix(0.0,m,n)
  for(i in 1:m){
    for(j in (i+1):n){
      if(i+1 <= n) au[i,j] = a[i,j]
    }
  }
  return(au)
} # END Triu

# Modified Gram-Schmidt process
Qr=function(a,q,r){
  # input real a(:,:)
  #q=matrix(0.0,SIZE(a,1), SIZE(a,2))
  #r=matrix(0.0,SIZE(a,2), SIZE(a,2))
  #local
  a0=matrix(0.0,SIZE(a,1), SIZE(a,2))
  k=0;n=0

  n = SIZE(a,2)
  a0 = a
  for(k in 1:n){
    r[k,k] = Norm(a0[ ,k])
    #       q(:,k) = a0(:,k) / r(k,k)
#    ind=which(abs(a0[ ,k])>0.000000001)  #FIXME_DK: rough fix will need checks+refinement
    ind = which(a0[,k]!=0.) # DM: this seems to work for now
    if(length(ind)>0) q[ind,k] = a0[ind,k] / r[k,k]
    if(k!=n){
      r[k,(k+1):n] = MATMUL(q[ ,k], a0[ ,(k+1):n])
      a0[ ,(k+1):n] = a0[ ,(k+1):n] - MATMUL(matrix(q[ ,k:k],nrow=n,ncol=1), matrix(r[k:k,(k+1):n],nrow=1,ncol=(n-k)))
    }
  }
  return(list(q=q,r=r))
} #END Qr

svdBackSub=function(m, n, U, W, V, b, x, error, message){
  # Solves Ax=b using SVD back substitution
  # Singular value decomposition of A(m,n) = U(m,n) * W(n) *Vtranspose (n,n)
  #input integer: m, n
  #input real: U(:,:), W(:), V(:,:), b(:)
  #output real: x(:)
  #output integer: error
  #output character output: message
  j=0 # local integer
  tmp=rep(0.0,n) # local real vector
  #----
  # Check consistency of dimension
  error = 0; message = 'ok'

  if(SIZE(U,1) != m) { message = 'm not same as assumed size in U(m,n)' ; error = 1}
  if(SIZE(U,2) != n) { message = 'n not same as assumed size in U(m,n)' ; error = 1}
  if(SIZE(W) != n)   { message = 'n not same as assumed size in W(n)'   ; error = 1}
  if(SIZE(V,1) != n) { message = 'n not same as assumed size in V(n,n1)'; error = 1}
  if(SIZE(V,2) != n) { message = 'n not same as assumed size in V(n1,n)'; error = 1}
  if(SIZE(b) != n)   { message = 'n not same as assumed size in b(n)'   ; error = 1}
  if(SIZE(x) != n)   { message = 'n not same as assumed size in x(n)'   ; error = 1}

  if(error==0){
    # Perform back substitution
    for(j in 1:n){
      if(abs(W[j]) > 0.0000000001){
        tmp[j] = DOT_PRODUCT(U[1:m,j],b[1:m])/W[j]
      }else{
        tmp[j] = 0.0
      }
    }
    for(j in 1:n){
      x[j] = DOT_PRODUCT(V[j,1:n],tmp[1:n])
    }
  }
  return(list(x=x,error=error,message=message))
} #END svdBackSub

