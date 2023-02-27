#path="/home/mike/temp/Robust-Gauss-Newton-Algorithm-master/R/"
#setwd(path)
#source("rgn.R")

rm(list=ls())

path = 'C:/Users/a1065639/Box/2021_foreSIGHT/RGN_package/RGN/'
setwd(path)
devtools::load_all()

testRGN=function(){
  # Purpose: Calibrate 2D Rosenbrock function with Robust Gauss-Newton Algorithm (RGN)
  # ---
  # Programmer: Youwei Qin, Dmitri Kavetski, Michael Leonard
  # Created: July 2018 AD, Hohai University, China.
  # Last modified: October 2018 AD, Hohai University, China
  # Copyright, Youwei Qin, Dmitri Kavetski, Michael Leonard, 2018-2023. All rights reserved.
  # ---
  # This is the demo for calibrating Rosenbrock function with RGN
  # The core of RGN is recorded in rgn.f90
  # The data exchange between RGN and Rosenbrock function is through "objFunc"
  # and the sum of least squares objective function value is evaluated and returned to RGN subroutine.
  # The public variables were shared through subroutine "constantsMod.f90"
  #******************************************************************

  p=2; n=2
  x0=rep(0.0,p); xLo=rep(0.0,p); xHi=rep(0.0,p); x=rep(0.0,p)
  cnv=rgnConvType
  error=0
  message=""
  info=rgnInfoType
  #EXTERNAL objFunc

  #----
    #Write out the message what is running
  print(" Calibrating Rosenbrock with RGN, approximate running time 1-2 seconds")
  print("")

  #error=0                                              # Initialize error flag
  #message=""                                           # Initialize message
  x0 =  c(-1.0,  0.0)                             # Start point of the search, with the optimum at [1.0 1.0]
  xLo = c(-1.5, -1.0)                             # Low bound
  xHi = c( 1.5,  3.0)                             # Upper bound

  cnv=setDefaultRgnConvergeSettings (dump=10, fail=0)
  #
  # key input parameters: p is the number of parameters to be optimized
  #                       n is the number of residuals
  tmp= rgn(objFunc=objFunc, p=2, n=2, x0=x0, xLo=xLo, xHi=xHi, cnv=cnv, x=x, info=info, error=error, message=message) #SUB2FUNC conversion
  error=tmp$error;message=tmp$message;x=tmp$x;info=tmp$info

  if(error != 0){
    print(message)
    readline()
  }
  print(paste("Best parameter set:     ", paste(x,collapse=" ")))
  print(paste("Best objfunc value:     ", info$f))
  print(paste("Number of objfunc calls:", info$nEval))
  print(paste("Total iteration:         ", info$nIter))
  print(paste("Termination flag:        ", info$termFlag))
  print(paste("CPU time:                ",info$cpuTime))
} #END PROGRAM testRGN

objFunc=function(x){
  #
  #time for evaluating
  timeObj=rep(0,2)
  timeObj[1]=Sys.time()
  f = 0.0; r=rep(0,2)
  r[1] = 1.0-x[1]
  r[2]=10.0*(x[2]-x[1]**2)                      # Compute residual
  f = f + r[1]**2+r[2]**2                          # Calculate objective function
  f = f/2.0
  print(paste(f,paste(x,collapse=" ")))  # MLHack
  timeObj[2]=Sys.time()
  timeFunc=timeObj[2]-timeObj[1] #############timeFunc not returned at present, need to implement a list and then extract f, timeFunc
  return(list(f=f,r=r,timeFunc=timeFunc,error=0,message="ok"))
} #END objFunc


testRGN() # call main program
