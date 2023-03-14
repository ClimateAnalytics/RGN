# foreSIGHT multisite calibration.
# Note can be run as single site model with keepSite=1

rm(list=ls())

devtools::load_all('C:/Users/a1065639/Work/foreSIGHT/')
devtools::load_all('C:/Users/a1065639/Work/RGN/')

###########

data(barossaDat)
keepSite = c(1:10,12:13)
#keepSite = c(1)
barossa_obs$P = barossa_obs$P[,keepSite]

###############################

attPerturb <- c("P_ann_tot_m","P_ann_wettest6monSeasRatio")
attHold <- c("P_ann_P99","P_ann_avgDSDT0.999_m", "P_ann_nWetT0.999_m","P_ann_wettest6monPeakDay")

attSel = c(attPerturb,attHold)
obsAtts <- calculateAttributes(barossa_obs, attSel)

# settings for exposure space
attPerturbSamp = c(1,1)
attPerturbMin = c(1,1)
attPerturbMax = c(1,1)

expSpace <- createExpSpace(attPerturb = attPerturb,
                           attPerturbSamp = attPerturbSamp,
                           attPerturbMin = attPerturbMin,
                           attPerturbMax = attPerturbMax,
                           attPerturbType = "regGrid",
                           attHold = attHold)

###############################

# specify the penalty settings in a list
controlFileList <- list()
controlFileList[["penaltyAttributes"]] <- c("P_ann_tot_m","P_ann_wettest6monSeasRatio","P_ann_wettest6monPeakDay")
controlFileList[["penaltyWeights"]] <- c(2.,2.,1.)

# specify the alternate model selections
controlFileList[["modelType"]] <- list()
controlFileList[["modelType"]][["P"]] <- "latent"

controlFileList[["modelParameterVariation"]] <- list()
controlFileList[["modelParameterVariation"]][["P"]] <- "harmonic"

controlFileList[["spatialOptions"]] <- list()
controlFileList[["spatialOptions"]][["spatCorFac"]] = 1.

###############################
# # run with GA
#
# # add user-specified values for optimisation arguments
# controlFileList[["optimisationArguments"]] <- list()
# controlFileList[["optimisationArguments"]][["maxiter"]] <- 200
# controlFileList[["optimisationArguments"]][["run"]] <- 100
# controlFileList[["optimisationArguments"]][["popSize"]] <- 1000
#
# controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
# write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))
#
# WG_calls = 0
#
# Rprof()
# sim_GA = foreSIGHT::generateScenarios(reference = barossa_obs,
#                                    expSpace = expSpace,
#                                    seedID = 1,
#                                    controlFile = paste0(tempdir(), "controlFile.json"))
# Rprof(NULL)
#
# calls_GA = WG_calls
#
# print(summaryRprof())
#
# plotScenarios(sim_GA)

###############################
# run with RGN

controlFileList[["optimisationArguments"]] <- list()
controlFileList[["optimisationArguments"]][["optimizer"]] <- 'RGN'

controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

WG_calls = 0

Rprof()
sim_RGN = foreSIGHT::generateScenarios(reference = barossa_obs,
                                   expSpace = expSpace,
                                   seedID = 1,
                                   controlFile = paste0(tempdir(), "controlFile.json"))
Rprof(NULL)

calls_RGN = WG_calls

print(summaryRprof())

plotScenarios(sim_RGN)

###############################


