simFunc_hymod = function(x,stateVal,nWarmUp,rain,pet){
  S = vector(length=length(x))
  flexS=TRUE         # Allow fix of Smax
  #Assign parameters
  Smax=x[1]; b=x[2]; alpha=x[3]; Ks=x[4]; Kq=x[5]
  # Initialize surfacewater storages and baseflowstorage, and the initial storage of C1,C2,C3
  S[1]=stateVal[1];S[2]=stateVal[2];S[3]=stateVal[3];S[4]=stateVal[4]; S[5]=stateVal[5]; Qs=stateVal[6];Qq=stateVal[7];Q=stateVal[8]
  # check feas of state wrt pars unless flexi-state
  if ((S[1]>Smax)&(!flexS)){
    message=paste0("f-",procnam,"/Soil moisture exceeds")
    error=-10
    return(list(error=error,message=message))
  }
  if(flexS){
    if(S[1]>Smax){S[1]=Smax}
  }
  nSim = length(rain)
  Qvec = vector(length = nSim)
  for (i in 1:nSim){
    qsimf<-.Fortran("hymod_f90",
                    precip=as.double(rain[i]), pet=as.double(pet[i]), S=as.double(S), Smax=as.double(Smax),
                    b=as.double(b), alpha=as.double(alpha), Ks=as.double(Ks), Kq=as.double(Kq),
                    Qs=as.double(1), Qq=as.double(1), Q=as.double(1),
                    err = as.integer(1))
    Qvec[i] = qsimf$Q
    S = qsimf$S
  }
  return(Qvec[nWarmUp:nSim])
}
