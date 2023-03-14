# example foreSIGHT calibration based on "Detailed Tutorial" vignette "Use Case B5" (without penalty weights)

rm(list=ls())

# download latest version of foreSIGHT from https://github.com/ClimateAnalytics/foreSIGHT , and use development branch
devtools::load_all('C:/Users/a1065639/Work/foreSIGHT/')
devtools::load_all('C:/Users/a1065639/Work/RGN/')

###########################################################

# Selected attributes
attPerturb <- c("P_ann_tot_m", "P_DJF_tot_m")
attHold <- c("P_MAM_tot_m", "P_JJA_tot_m", "P_ann_nWet_m")

# Sampling bounds and strategy
attPerturbType = "regGrid"
attPerturbSamp = c(2, 2)
attPerturbMin = c(0.9, 0.9)
attPerturbMax = c(1.3, 1.3)

# Creating the exposure space
expSpace <- createExpSpace(attPerturb = attPerturb,
                           attPerturbSamp = attPerturbSamp,
                           attPerturbMin = attPerturbMin,
                           attPerturbMax = attPerturbMax,
                           attPerturbType = attPerturbType,
                           attHold = attHold)
data("tankDat")

###########################################################

WG_calls = 0

#Rprof()
time1 = Sys.time()
sim_GA <- generateScenarios(reference = tank_obs,   # reference time series
                         expSpace = expSpace,    # exposure space
                         numReplicates = 1,      # number of replicates
                         seedID = 1)
time2 = Sys.time()
time_GA = time2 - time1
#Rprof(NULL)
plotScenarios(sim_GA)

calls_GA = WG_calls

###########################################################

controlFileList = list()
controlFileList[["optimisationArguments"]] <- list()
controlFileList[["optimisationArguments"]][["optimizer"]] <- 'RGN'

controlFileList$optimisationArguments$rgnSettings = list()
controlFileList$optimisationArguments$rgnSettings$nMultiStart = 1

# write the list into a JSON file
controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

time1 = Sys.time()

WG_calls = 0

sim_RGN_1 <- generateScenarios(reference = tank_obs,   # reference time series
                         expSpace = expSpace,    # exposure space
                         numReplicates = 1,      # number of replicates
                         seedID = 1,
                         controlFile = paste0(tempdir(), "controlFile.json"))
time2 = Sys.time()
time_RGN_1 = time2 - time1
plotScenarios(sim_RGN_1)

calls_RGN_1 = WG_calls

###########################################################

controlFileList = list()
controlFileList[["optimisationArguments"]] <- list()
controlFileList[["optimisationArguments"]][["optimizer"]] <- 'RGN'

controlFileList$optimisationArguments$rgnSettings = list()
controlFileList$optimisationArguments$rgnSettings$nMultiStart = 10

# write the list into a JSON file
controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

Rprof()
time1 = Sys.time()

WG_calls = 0

sim_RGN_10 <- generateScenarios(reference = tank_obs,   # reference time series
                         expSpace = expSpace,    # exposure space
                         numReplicates = 1,      # number of replicates
                         seedID = 1,
                         controlFile = paste0(tempdir(), "controlFile.json"))
time2 = Sys.time()
time_RGN_10 = time2 - time1
Rprof(NULL)
plotScenarios(sim_RGN_10)

calls_RGN_10 = WG_calls

###########################################################
