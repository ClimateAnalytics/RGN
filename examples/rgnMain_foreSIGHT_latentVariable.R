# foreSIGHT multisite calibration.
# Note can be run as single site model with keepSite=1

rm(list=ls())

devtools::load_all('C:/Users/a1065639/Work/foreSIGHT/')
devtools::load_all('C:/Users/a1065639/Work/RGN/')

###########

data(barossaDat)
#keepSite = c(1:10,12:13)
keepSite = c(1)
barossa_obs$P = barossa_obs$P[,keepSite]

###############################

attPerturb <- c("P_ann_tot_m","P_ann_wettest6monSeasRatio")
#attHold <- c("P_ann_P99","P_ann_avgDSDT0.999_m", "P_ann_nWetT0.999_m","P_ann_wettest6monPeakDay")
attHold <- c("P_ann_P99","P_ann_avgDSDT0.999_m", "P_ann_nWetT0.999_m","P_ann_wettest6monPeakDay","P_ann_P90","P_ann_maxDSD_m")

attSel = c(attPerturb,attHold)
obsAtts <- calculateAttributes(barossa_obs, attSel)

# settings for exposure space
attPerturbSamp = c(1,1)
attPerturbMin = c(1,1)
attPerturbMax = c(1,1)

# attPerturbSamp = c(2,2)
# attPerturbMin = c(0.9,0.9)
# attPerturbMax = c(1.1,1.1)

expSpace <- createExpSpace(attPerturb = attPerturb,
                           attPerturbSamp = attPerturbSamp,
                           attPerturbMin = attPerturbMin,
                           attPerturbMax = attPerturbMax,
                           attPerturbType = "regGrid",
                           attHold = attHold)

###############################

controlFileList <- list()

# specify the alternate model selections
controlFileList[["modelType"]] <- list()
controlFileList[["modelType"]][["P"]] <- "latent"

controlFileList[["modelParameterVariation"]] <- list()
controlFileList[["modelParameterVariation"]][["P"]] <- "harmonic"
#controlFileList[["modelParameterVariation"]][["P"]] <- "annual"

controlFileList[["spatialOptions"]] <- list()
controlFileList[["spatialOptions"]][["spatCorFac"]] = 1.

###############################
# run with RGN - no weights

controlFileList[["penaltyAttributes"]] = NULL
controlFileList[["penaltyWeights"]] <- NULL

controlFileList[["optimisationArguments"]] <- list()
#controlFileList[["optimisationArguments"]][["optimizer"]] <- 'RGN'
#controlFileList[["optimisationArguments"]][["optimizer"]] <- 'CMAES'
controlFileList[["optimisationArguments"]][["optimizer"]] <- 'NLSLM'

controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

WG_calls = 0

Rprof()
time1 = Sys.time()
sim = foreSIGHT::generateScenarios(reference = barossa_obs,
                                         expSpace = expSpace,
                                         seedID = 1,
                                         controlFile = paste0(tempdir(), "controlFile.json"))
time2 = Sys.time()
time_sim = time2 - time1
Rprof(NULL)

calls = WG_calls

print(summaryRprof())

plotScenarios(sim)

pause

###############################
# run with RGN - no weights

controlFileList[["penaltyAttributes"]] = NULL
controlFileList[["penaltyWeights"]] <- NULL

controlFileList[["optimisationArguments"]] <- list()
controlFileList[["optimisationArguments"]][["optimizer"]] <- 'RGN'

controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

WG_calls = 0

Rprof()
time1 = Sys.time()
sim_RGN_1 = foreSIGHT::generateScenarios(reference = barossa_obs,
                                         expSpace = expSpace,
                                         seedID = 1,
                                         controlFile = paste0(tempdir(), "controlFile.json"))
time2 = Sys.time()
time_RGN_1 = time2 - time1
Rprof(NULL)

calls_RGN_1 = WG_calls

print(summaryRprof())

plotScenarios(sim_RGN_1)

###############################
# run with RGN 10 multistarts - no weights

controlFileList[["optimisationArguments"]] <- list()
controlFileList[["optimisationArguments"]][["optimizer"]] <- 'RGN'
controlFileList[["optimisationArguments"]][["nMultiStart"]] <- 10

controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

WG_calls = 0

Rprof()
time1 = Sys.time()
sim_RGN_10 = foreSIGHT::generateScenarios(reference = barossa_obs,
                                          expSpace = expSpace,
                                          seedID = 1,
                                          controlFile = paste0(tempdir(), "controlFile.json"))
time2 = Sys.time()
time_RGN_10 = time2 - time1
Rprof(NULL)

calls_RGN_10 = WG_calls

print(summaryRprof())

plotScenarios(sim_RGN_10)

pause

###############################
# run with optim 10 multistarts - no weights

controlFileList[["optimisationArguments"]] <- list()
controlFileList[["optimisationArguments"]][["optimizer"]] <- 'optim.LBFGSB'
controlFileList[["optimisationArguments"]][["nMultiStart"]] <- 10

controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

WG_calls = 0

Rprof()
time1 = Sys.time()
sim_optim_10 = foreSIGHT::generateScenarios(reference = barossa_obs,
                                          expSpace = expSpace,
                                          seedID = 1,
                                          controlFile = paste0(tempdir(), "controlFile.json"))
time2 = Sys.time()
time_optim_10 = time2 - time1
Rprof(NULL)

calls_optim_10 = WG_calls

print(summaryRprof())

plotScenarios(sim_optim_10)

pause

###############################
# run with GA no weights

# add user-specified values for optimisation arguments
controlFileList[["optimisationArguments"]] <- list()
controlFileList[["optimisationArguments"]][["maxiter"]] <- 200
controlFileList[["optimisationArguments"]][["run"]] <- 100
controlFileList[["optimisationArguments"]][["popSize"]] <- 1000

controlFileList[["penaltyAttributes"]] = NULL
controlFileList[["penaltyWeights"]] <- NULL

controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

WG_calls = 0

Rprof()
time1 = Sys.time()
sim_GA_1 = foreSIGHT::generateScenarios(reference = barossa_obs,
                                   expSpace = expSpace,
                                   seedID = 1,
                                   controlFile = paste0(tempdir(), "controlFile.json"))
time2 = Sys.time()
time_GA_1 = time2 - time1
Rprof(NULL)

calls_GA_1 = WG_calls

print(summaryRprof())

plotScenarios(sim_GA_1)

###############################
# run with GA - with weights

controlFileList[["optimisationArguments"]] <- list()
controlFileList[["optimisationArguments"]][["maxiter"]] <- 200
controlFileList[["optimisationArguments"]][["run"]] <- 100
controlFileList[["optimisationArguments"]][["popSize"]] <- 1000

# specify the penalty settings in a list
controlFileList[["penaltyAttributes"]] <- c("P_ann_tot_m","P_ann_wettest6monSeasRatio","P_ann_wettest6monPeakDay")
controlFileList[["penaltyWeights"]] <- c(2.,2.,1.)

controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

WG_calls = 0

Rprof()
time1 = Sys.time()
sim_GA_1_pw = foreSIGHT::generateScenarios(reference = barossa_obs,
                                         expSpace = expSpace,
                                         seedID = 1,
                                         controlFile = paste0(tempdir(), "controlFile.json"))
time2 = Sys.time()
time_GA_1_pw = time2 - time1
Rprof(NULL)

calls_GA_1_pw = WG_calls

print(summaryRprof())

plotScenarios(sim_GA_1_pw)

pause

save.image("C:/Users/a1065639/Work/RGN/foreSIGHT_latent_compare_opt.RData")

###############################

load("C:/Users/a1065639/Work/RGN/foreSIGHT_latent_compare_opt.RData")

controlFileList[["penaltyAttributes"]] = NULL
controlFileList[["penaltyWeights"]] = NULL

# #################
#
# parOpt_RGN = sim_RGN$Rep1$Target1$parS
#
# controlFileList[["modelParameterBounds"]] <- list()
# controlFileList[["modelParameterBounds"]][["P"]] <- list()
#
# for (p in 1:length(parName)){
#   controlFileList[["modelParameterBounds"]][["P"]][[parName[p]]] = c(parOpt_RGN[p],parOpt_RGN[p])
# }
#
# controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
# write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))
#
# sim_RGN_1 = foreSIGHT::generateScenarios(reference = barossa_obs,
#                                          expSpace = expSpace,
#                                          seedID = 1,
#                                          controlFile = paste0(tempdir(), "controlFile.json"))
#
# ###############################
#
# parOpt_GA = sim_GA$Rep1$Target1$parS
#
# controlFileList[["modelParameterBounds"]] <- list()
# controlFileList[["modelParameterBounds"]][["P"]] <- list()
#
# for (p in 1:length(parName)){
#   controlFileList[["modelParameterBounds"]][["P"]][[parName[p]]] = c(parOpt_GA[p],parOpt_GA[p])
# }
#
# controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
# write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))
#
# sim_GA_1 = foreSIGHT::generateScenarios(reference = barossa_obs,
#                                        expSpace = expSpace,
#                                        seedID = 1,
#                                        controlFile = paste0(tempdir(), "controlFile.json"))
#
# ###############################
#
# parOpt_GA_pw = sim_GA_pw$Rep1$Target1$parS
#
# controlFileList[["modelParameterBounds"]] <- list()
# controlFileList[["modelParameterBounds"]][["P"]] <- list()
#
# for (p in 1:length(parName)){
#   controlFileList[["modelParameterBounds"]][["P"]][[parName[p]]] = c(parOpt_GA_pw[p],parOpt_GA_pw[p])
# }
#
# controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
# write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))
#
# sim_GA_pw_1 = foreSIGHT::generateScenarios(reference = barossa_obs,
#                                         expSpace = expSpace,
#                                         seedID = 1,
#                                         controlFile = paste0(tempdir(), "controlFile.json"))

#################

attPerturbSamp = c(1,1)
attPerturbMin = c(0.9,0.9)
attPerturbMax = c(0.9,0.9)

expSpace <- createExpSpace(attPerturb = attPerturb,
                           attPerturbSamp = attPerturbSamp,
                           attPerturbMin = attPerturbMin,
                           attPerturbMax = attPerturbMax,
                           attPerturbType = "regGrid",
                           attHold = attHold)

#sim = sim_RGN_10
sim = sim_GA_1

parOpt = sim$Rep1$Target1$parS

controlFileList[["modelParameterBounds"]] <- list()
controlFileList[["modelParameterBounds"]][["P"]] <- list()

d=foreSIGHT::viewModelParameters(variable = 'P',modelType = 'latent',modelParameterVariation = 'harmonic')

parName = d[,1]

boundsAll = list()
for (p in 1:length(parName)){
  boundsAll[[parName[p]]] = c(parOpt[p],parOpt[p])
}

free_pars = c(1,4,5,6,7,8,9,10)

for (i in 1:length(free_pars)){
  p1 = free_pars[i]
  for (j in (i+1):length(free_pars)){
    p2 = free_pars[j]



# p1 = 1
# p2 = 8

# n1 = 10
# n2 = 10
#
# par1_list = seq(d[p1,2],d[p1,3],length=n1)
# par2_list = seq(d[p2,2],d[p2,3],length=n2)

# par1_list = seq(0.4,0.6,length=n1)
# par2_list = seq(-2,-1,length=n2)

# par1_list = c(parOpt_RGN[p1])
# par2_list = c(parOpt_RGN[p2])

# par1_list = parOpt_RGN[p1]*seq(0.5,1.5,length=11)
# #par2_list = parOpt_RGN[p2]*seq(1.5,0.5,length=11)
# par2_list = parOpt_RGN[p2]*seq(0.5,1.5,length=11)

parFac1 = seq(0.8,1.2,length=11)
if (parOpt[p1]<0){parFac1=rev(parFac1)}

parFac2 = seq(0.8,1.2,length=11)
if (parOpt[p2]<0){parFac2=rev(parFac2)}


par1_list = parOpt[p1]*parFac1
#par2_list = parOpt_RGN[p2]*seq(1.5,0.5,length=11)
par2_list = parOpt[p2]*parFac2


score = matrix(nrow=length(par1_list),ncol=length(par2_list))
for (i1 in 1:length(par1_list)){
  print(i1)
  par1 = par1_list[i1]
  for (i2 in 1:length(par2_list)){
    par2 = par2_list[i2]
    controlFileList[["modelParameterBounds"]][["P"]] = boundsAll
    controlFileList[["modelParameterBounds"]][["P"]][[parName[p1]]] = c(par1,par1)
    controlFileList[["modelParameterBounds"]][["P"]][[parName[p2]]] = c(par2,par2)
    controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
    write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))
    sim_tmp = foreSIGHT::generateScenarios(reference = barossa_obs,
                                             expSpace = expSpace,
                                             seedID = 1,
                                             controlFile = paste0(tempdir(), "controlFile.json"))
    score[i1,i2] = sim_tmp$Rep1$Target1$score
  }
}

par(mar=c(3,3,3,3))
filled.contour(x = par1_list,y=par2_list,z=-score,
               xlab=parName[p1],ylab=parName[p2],
               plot.title = {points(x = parOpt[p1], y = parOpt[p2])})
title(paste(parName[p1],'vs',parName[p2]))
  }
}

#################
