rm(list=ls())

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

  ##### setup parameter bounds
  x0 =  c(-1.0,  0.0)                             # Start point of the search, with the optimum at [1.0 1.0]
  xLo = c(-1.5, -1.0)                             # Low bound
  xHi = c( 1.5,  3.0)                             # Upper bound

  ##### settings
  info=rgnInfoType
  cnv=setDefaultRgnConvergeSettings (dump=10, fail=0)

  #----
    #Write out the message what is running
  print(" Calibrating Rosenbrock with RGN, approximate running time 1-2 seconds")
  tmp= rgn(simFunc=simFunc,
           x0=x0, xLo=xLo, xHi=xHi,
           target=c(0,0),
           cnv=cnv, info=info,
           ) #SUB2FUNC conversion
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

simFunc=function(x){
  r=rep(0,2)
  r[1] = 1.0-x[1]
  r[2]=10.0*(x[2]-x[1]**2)                      # Compute residual
  return(r)
} #END simFunc


testRGN() # call main program
