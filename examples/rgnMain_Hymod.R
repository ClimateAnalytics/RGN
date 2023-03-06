rm(list=ls())

devtools::load_all()

################################################################

simFunc = function(x,stateVal,nWarmUp,rain,pet){

  #Hymod Parameters and states
  S = vector(length=length(x))

#  procnam="ObjFunc"
  message = ''
  error = 0
  #---
  #
  flexS=TRUE         # Allow fix of Smax
  #Assign parameters
  Smax=x[1]            # Maximum storage capacity
  b=x[2]               # Degree of spatial variability of the soil moisture capacity
  alpha=x[3]           # Factor distributing the flow between slow and quick release reservoirs
  Ks=x[4]              # Residence time of the slow release reservoir
  Kq=x[5]              # Residence time of the quick release reservoir

  # Initialize surfacewater storages and baseflowstorage, and the initial storage of C1,C2,C3
  S[1]=stateVal[1];S[2]=stateVal[2];S[3]=stateVal[3];S[4]=stateVal[4]; S[5]=stateVal[5]
  Qs=stateVal[6];Qq=stateVal[7];Q=stateVal[8]

  # check feas of state wrt pars unless flexi-state
  if ((S[1]>Smax)&(!flexS)){
    message=paste0("f-",procnam,"/Soil moisture exceeds")
    error=-10
    return(list(error=error,message=message))
  }
  # * Allows convenient initialization and adjustment of states
  if(flexS){
    if(S[1]>Smax){S[1]=Smax}
  }

  nSim = length(rain)

  Qvec = vector(length = nSim)

  for (i in 1:nSim){

    qsimf<-.Fortran("hymod_f90",
                    precip=as.double(rain[i]), pet=as.double(pet[i]), S=as.double(S), Smax=as.double(Smax), b=as.double(b), alpha=as.double(alpha), Ks=as.double(Ks), Kq=as.double(Kq),
                    Qs=as.double(1), Qq=as.double(1), Q=as.double(1),
                    err = as.integer(1))
    Qvec[i] = qsimf$Q
    S = qsimf$S
  }

  return(Qvec[nWarmUp:nSim])

}

################################################################

testRGN=function(){
  # Purpose: Calibrate HYMOD parameters with Robust Gauss-Newton Algorithm (RGN)
  # ---
  # Programmer: Youwei Qin, Dmitri Kavetski, Michael Leonard
  # Created: July 2018 AD, Hohai University, China.
  # Last modified: October 2018 AD, Hohai University, China
  # Copyright, Youwei Qin, Dmitri Kavetski, Michael Leonard, 2018-2023. All rights reserved.
  # ---
  # This is the demo for calibrating HYMOD with RGN
  # The core of RGN is recorded in rgn.f90; the core of HYMOD is recoded in hymod.f90
  # The data exchange between RGN and HYMOD is through "objFunc", where the HYMOD is called,
  # and the sum of least squares objective function value is evaluated and returned to RGN subroutine.
  # The public variables were shared through subroutine "constantsMod.f90"

  # Firstly the information about parameters are loaded, such as parameter name, initial value, lower and upper limit;
  # Then the input data was loaded, which include the rainfall, ET, and runoff
  # Finally, the initial states for storage are loaded.
  #******************************************************************

  ##### setup parameter bounds
  x0 = c(400.,0.5,0.1,0.2,0.1)
  xLo = c(1.,0.1,0.05,0.000001,0.000001)
  xHi = c(1000.,2.,0.95,0.99999,0.99999)

  ##### settings
  info=rgnInfoType
#  cnv = setDefaultRgnConvergeSettings(dump=10, fail=0)
  cnv = setDefaultRgnConvergeSettings(dump=0, fail=0)

  ##### setup initial states
  stateVal = c(100.0,30.0,27.0,25.0,30.0,0.0,0.0,0.0)

  #### read data
  dataFileName = "examples/BassRiver.dat"
  d = read.csv(dataFileName)
  rain = d[,1]
  pet = d[,2]
  obsQ = d[,3]
  nData = length(rain)

  #### set warmup period
  nWarmUp = 365

  #### run RGN
  cat("Calibrating Hymod with RGN, approximate running time 10-40 seconds\n")
  cat("\n")
  tmp= rgn(simFunc=simFunc,
           x0=x0, xLo=xLo, xHi=xHi,
           simTarget=obsQ[nWarmUp:nData],
           cnv=cnv, info=info,
           stateVal=stateVal, nWarmUp=nWarmUp,rain=rain,pet=pet) #SUB2FUNC conversion
  error=tmp$error;message=tmp$message;x=tmp$x;info=tmp$info

  #### write output
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

  # #### run RGN with multi-starts
  # nReps = 10
  # xMat = matrix(nrow = nReps,ncol = length(x0))
  # fVec = vector(length=nReps)
  # cat("Calibrating Hymod with RGN - multistarts \n")
  # for (r in 1:nReps){
  #   cat(r,"\n")
  #   x0 = xLo + runif(length(x0))*(xHi-xLo)
  #   cat(x0,'\n')
  #   tmp= rgn(simFunc=simFunc,
  #            x0=x0, xLo=xLo, xHi=xHi,
  #            target=obsQ[nWarmUp:nData],
  #            cnv=cnv, info=info,
  #            stateVal=stateVal, nWarmUp=nWarmUp,rain=rain,pet=pet) #SUB2FUNC conversion
  #   error=tmp$error;message=tmp$message;x=tmp$x;info=tmp$info
  #   xMat[r,] = x
  #   fVec[r] = info$f
  #   cat(x,'\n')
  #   cat(info$f,'\n')
  # }

} #END PROGRAM testRGN

################################################################

testRGN() # call main program
