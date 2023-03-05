rm(list=ls())

# path = 'C:/Users/a1065639/Box/2021_foreSIGHT/RGN_package/RGN/'
# setwd(path)
devtools::load_all()

################################################################

nse <- function(
    ### Calculates the Nash-Sutcliffe Efficiency (NSE) for an observed predicted time series
  ##title<<Calculates Nash_Sutcliffe
  ##value<< returns a value for the nse
  ##references<< \url{http://en.wikipedia.org/wiki/Nash-Sutcliffe_model_efficiency_coefficient}
  obs, ##<< vector of observed values
  pred, ##<< vector of predicted values
  na.rm=TRUE ##<< logical, Should missing values (including \code{NaN} be removed), passed to \code{\link{sum}} and \code{\link{mean}}
) {
  nse =(1 - (sum((obs - pred)^2,na.rm=na.rm) / sum((obs - mean(obs,na.rm=na.rm))^2,na.rm=na.rm) ))
  return(nse)
}

################################################################

simFunc = function(params,InputsModel=InputsModel,
                   Ind_Run=Ind_Run,
                   RunOptions=RunOptions){

  names(params) = c('X1','X2','X3','X4')

  OutputsModel <- RunModel_GR4J(InputsModel = InputsModel,
                                RunOptions = RunOptions, Param = params)

  return(OutputsModel$Qsim)

}

################################################################
# setup data and GR4J settings

library(airGR)

## loading catchment data
data(L0123001)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1999-12-31"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

################### RGN calibration ###################

# setup parameter bounds
x0 = c(200,0,50,1)
xLo = c(100,-100,1,0.5)
xHi = c(20000,100,500,10)

# settings
info=rgnInfoType
cnv = setDefaultRgnConvergeSettings(dump=10, fail=0)

# run RGN
cat("Calibrating GR4J with RGN\n")
cat("\n")
tmp= rgn(simFunc=simFunc,
         x0=x0, xLo=xLo, xHi=xHi,
         target=BasinObs$Qmm[Ind_Run],
         cnv=cnv, info=info,
         InputsModel=InputsModel,
         Ind_Run=Ind_Run,
         RunOptions=RunOptions) #SUB2FUNC conversion
error=tmp$error;message=tmp$message;x=tmp$x;info=tmp$info

# write output
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

simBest = simFunc(params=x,
                  InputsModel=InputsModel,
                  Ind_Run=Ind_Run,
                  RunOptions=RunOptions)


print(paste("NSE:                ",nse(obs=BasinObs$Qmm[Ind_Run],pred = simBest,na.rm = T)))

################### airGR calibration ###################

time1 = Sys.time()
# calibration criterion: preparation of the InputsCrit object
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                               RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
# preparation of CalibOptions object
CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_GR4J, FUN_CALIB = Calibration_Michel)
# calibration
OutputsCalib <- Calibration_Michel(InputsModel = InputsModel, RunOptions = RunOptions,
                                   InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                   FUN_MOD = RunModel_GR4J)
time2 = Sys.time()
print(paste("Total iteration:         ", OutputsCalib$NRuns))
print(paste("CPU time:                ",time2-time1))

################################################################
