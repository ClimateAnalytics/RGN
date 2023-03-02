rm(list=ls())

# path = 'C:/Users/a1065639/Box/2021_foreSIGHT/RGN_package/RGN/'
# setwd(path)
devtools::load_all()

################################################################

simFunc = function(params,stateVal,nWarmUp,rain,pet){

  #Hymod Parameters and states
  S = vector(length=length(params))

#  procnam="ObjFunc"
  message = ''
  error = 0
  #---
  #
  flexS=TRUE         # Allow fix of Smax
  #Assign parameters
  Smax=params[1]            # Maximum storage capacity
  b=params[2]               # Degree of spatial variability of the soil moisture capacity
  alpha=params[3]           # Factor distributing the flow between slow and quick release reservoirs
  Ks=params[4]              # Residence time of the slow release reservoir
  Kq=params[5]              # Residence time of the quick release reservoir

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

  p=5
  x0=rep(0.0,p); xLo=rep(0.0,p); xHi=rep(0.0,p); x=rep(0.0,p)
  cnv=rgnConvType
  error=0
  message=""
  info=rgnInfoType

#  procnam="testMain"
  cat("Calibrating Hymod with RGN, approximate running time 10-40 seconds\n")
  cat("\n")

#  parName[1] = "Maximum storage capacity"
  xLo[1] = 1.
  xHi[1] = 1000.
  x0[1] = 400.

#  parName[2] = "Degree of spatial variability of the soil moisture capacity"
  xLo[2] = 0.1
  xHi[2] = 2.
  x0[2] = 0.5

#  parName[3] = "Factor disturbing the flow between slow and quick release reservoirs"
  xLo[3] = 0.05
  xHi[3] = 0.95
  x0[3] = 0.1

#  parName[4] = "Residence time of the slow release reservoir"
  xLo[4] = 0.000001
  xHi[4] = 0.99999
  x0[4] = 0.2

#  parName[5] = "Residence time of the quick release reservoirs"
  xLo[5] = 0.000001
  xHi[5] = 0.99999
  x0[5] = 0.1

  #Continue reading the File, the dataFileName is the name of the input file, includes rainfall, runoff, evaporation
  dataFileName = "examples/BassRiver.dat"

  nWarmUp = 365

  nState = 8

  stateName = stateVal = c()

#  stateName[1] = "initial storage of soil moister tank"
  stateVal[1] = 100.0
#  stateName[2] = "initial storage of the slow flow tank"
  stateVal[2] = 30.0
#  stateName[3] = "initial storage of the first quick flow tank"
  stateVal[3] = 27.0
#  stateName[4] = "initial storage of the second quick flow tank"
  stateVal[4] =   25.0
#  stateName[5] = "initial storage of the third quick flow tank"
  stateVal[5] = 30.0
#  stateName[6] = "intial slow flow (not necessary)"
  stateVal[6] = 0.0
#  stateName[7] = "intial quick flow (not necessary)"
  stateVal[7] = 0.0
#  stateName[8] = "intial total flow (not necessary)"
  stateVal[8] = 0.0

  d = read.csv(dataFileName)

  rain = d[,1]
  pet = d[,2]
  obsQ = d[,3]
  nData = length(rain)

  #Part 2: Run RGN
  #Initialize the RGN default settings
  cnv = setDefaultRgnConvergeSettings(dump=10, fail=0)
  #Call RGN optimization algorithms
  # key input parameters: p is the number of parameters to be optimized
  #                       n is the number of residuals

  tmp= rgn(simFunc=simFunc, p=p, n=nData-nWarmUp+1, x0=x0, xLo=xLo, xHi=xHi, cnv=cnv, x=x, info=info, error=error, message=message,
           stateVal=stateVal, target=obsQ[nWarmUp:nData], nWarmUp=nWarmUp,rain=rain,pet=pet) #SUB2FUNC conversion
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

################################################################

testRGN() # call main program
