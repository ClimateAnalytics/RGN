# -----------

setupModelParNames = function(){
  modelParNames = list()
  modelTypeList = c('wgen','latent')
  modelParameterVariationList = c('annual','harmonic')
  for (modelType in modelTypeList){
    for (modelParameterVariation in modelParameterVariationList){
      d=foreSIGHT::viewModelParameters(variable = 'P',
                                       modelType = modelType,
                                       modelParameterVariation = modelParameterVariation)
      modelParNames[[modelType]][[modelParameterVariation]] = d[,1]
    }
  }
  return(modelParNames)
}

# determine model parameter names
modelParNames = setupModelParNames()

###########################################################

setupControlFile = function(modelType,modelParameterVariation,optimizer,optimizerNmulti,
                            par=NULL,freePars=NULL,optimizerSeed=NULL){

  # setup control file
  controlFileList = list()
  controlFileList$modelType$P <- modelType
  controlFileList$modelParameterVariation$P <- modelParameterVariation

  if (grepl('SCE',optimizer)){
    controlFileList$optimisationArguments$optimizer <- 'SCE'
    if (optimizer=='SCE'){
      nComplex = 5
    } else {
      nComplex = as.integer(strsplit(optimizer,'SCE')[[1]][[2]])
    }
    controlFileList$optimisationArguments$sceSettings = list(nComplex=nComplex)
  } else {
    controlFileList$optimisationArguments$optimizer <- optimizer
  }

  controlFileList$optimisationArguments$nMultiStart = optimizerNmulti

  if (!is.null(optimizerSeed)){
    controlFileList$optimisationArguments$seed <- optimizerSeed
  }

  if(optimizer=='GA'){
    controlFileList$optimisationArguments$pcrossover = 0.8
    controlFileList$optimisationArguments$pmutation = 0.1
    controlFileList$optimisationArguments$maxiter = 100
    controlFileList$optimisationArguments$popSize = 50
    controlFileList$optimisationArguments$run = 100
  }

  parName = modelParNames[[modelType]][[modelParameterVariation]]

  if (!is.null(par)){
    controlFileList$modelParameterBounds$P = list()
    for (p in 1:length(parName)){
      if (!(parName[p] %in% freePars)){
        parVal = par[p]
        controlFileList$modelParameterBounds$P[[parName[p]]] = c(parVal,parVal)
      }
    }
  } else if ((modelType=='latent')&(modelParameterVariation=='annual')){
    controlFileList$modelParameterBounds$P = list()
    controlFileList$modelParameterBounds$P$lambda = c(1.3,3.6)
    controlFileList$modelParameterBounds$P$mu = c(-13,1.6)
    controlFileList$modelParameterBounds$P$sigma = c(1.3,10.2)
    controlFileList$modelParameterBounds$P$alpha = c(0.18,0.9)
  }  else if ((modelType=='latent')&(modelParameterVariation=='harmonic')){
    controlFileList$modelParameterBounds$P = list()
    controlFileList$modelParameterBounds$P$alpha_m = c(0.18,0.9) # rho
    controlFileList$modelParameterBounds$P$alpha_amp = c(0,0)
    controlFileList$modelParameterBounds$P$alpha_ang = c(0,0)
    controlFileList$modelParameterBounds$P$sigma_m = c(1.5,10.) # sigma
    controlFileList$modelParameterBounds$P$sigma_amp = c(0.04,2.7)
    controlFileList$modelParameterBounds$P$sigma_ang = c(0.02,4.5)
    controlFileList$modelParameterBounds$P$mu_m = c(-10.8,0.8) # mu
    controlFileList$modelParameterBounds$P$mu_amp = c(0.18,8.)
    controlFileList$modelParameterBounds$P$mu_ang = c(0.11,4.4)
    controlFileList$modelParameterBounds$P$lambda_m = c(1.4,3.1) # beta
    controlFileList$modelParameterBounds$P$lambda_amp = c(0.02,0.8)
    controlFileList$modelParameterBounds$P$lambda_ang = c(0.33,4.2)
  }

  controlFileJSON <- jsonlite::toJSON(controlFileList, pretty = TRUE, auto_unbox = TRUE)
  write(controlFileJSON, file = paste0(tempdir(), "controlFile.json"))

}

# -----------

setupExpSpace = function(attList){

  # setup exposure space
  attPerturb = attList[1]
  if (length(attList)>1){
    attHold = attList[2:length(attList)]
  } else {
    attHold = NULL
  }

  attPerturbType = "regGrid"
  attPerturbSamp = c(1)
  attPerturbMin = c(1)
  attPerturbMax = c(1)

  # Creating the exposure space
  expSpace <- createExpSpace(attPerturb = attPerturb,
                             attPerturbSamp = attPerturbSamp,
                             attPerturbMin = attPerturbMin,
                             attPerturbMax = attPerturbMax,
                             attPerturbType = attPerturbType,
                             attHold = attHold)

  return(expSpace)

}

# -----------

simRun = function(clim_ref,attList,nYrs,seedID=1,
                  modelType,modelParameterVariation,
                  optimizer,optimizerNmulti,
                  par=NULL,freePars=NULL,plotScen=F,optimizerSeed=optimizerSeed){

  setupControlFile(modelType=modelType,modelParameterVariation=modelParameterVariation,
                   optimizer=optimizer,optimizerNmulti=optimizerNmulti,
                   par=par,freePars=freePars,optimizerSeed=optimizerSeed)
  expSpace = setupExpSpace(attList=attList)

  sim <- generateScenarios(reference = clim_ref,   # reference time series
                           expSpace = expSpace,    # exposure space
                           numReplicates = 1,      # number of replicates
                           seedID = seedID,
                           controlFile = paste0(tempdir(), "controlFile.json"),
                           simLengthNyrs = nYrs)

  if(plotScen){plotScenarios(sim)}

  return(sim)

}

# -----------

doRuns = function(clim_ref,options){

  sim = list()
  sim$options = options
  for (modelType in options$modelTypeList){
    sim[[modelType]] = list()
    for (optimizer in options$optimizerList){
      print(optimizer)
      sim[[modelType]][[optimizer]] = simRun(clim_ref=clim_ref,attList=options$attList,
                                             nYrs=options$nYrs,seedID=options$seedID,
                                             modelType=modelType,
                                             modelParameterVariation=options$modelParameterVariation,
                                             optimizer=optimizer,
                                             optimizerNmulti=options$optimizerNmulti,
                                             par=options$par,freePars=options$freePars,
                                             optimizerSeed=options$optimizerSeed)
    }
  }

  return(sim)
}

#########################

doRunsPar = function(clim_ref,options){

  library(foreach)
  library(doParallel)

  nChunks = options$nChunks
  optimizerNmulti = options$optimizerNmulti

  optimizerNmultiChunk =optimizerNmulti / nChunks

  slurm_ntasks <- as.numeric(Sys.getenv("SLURM_NTASKS")) # Obtain environment variable SLURM_NTASKS
  if (!is.na(slurm_ntasks)) {
    cores = slurm_ntasks # if slurm_ntasks is numerical, then assign it to cores
  } else {
    cores = 1
    #    stop("Cores not found")
  }

  c1 <- makeCluster(cores)
  registerDoParallel(c1)

  simAll <- foreach (chunk=1:nChunks) %dopar% {

    source(paste0(options$paper.code.dir,'setup_paths_libs.R'))
    source(paste0(options$paper.code.dir,'run_settings.R'))

    optimizerSeed = ((chunk-1)*optimizerNmultiChunk+1):(chunk*optimizerNmultiChunk)

    print(optimizerSeed)

    optionsPar = options
    optionsPar$optimizerNmulti=optimizerNmultiChunk
    optionsPar$optimizerSeed=optimizerSeed

    to.simAll = doRuns(clim_ref=clim_ref,options=optionsPar)
  }

  stopCluster(c1)

  simM = list()
  simM$options = list()
  simM$options$optimizerList = optimizerList
  simM$options$optimizerNmulti = optimizerNmulti
  simM[[modelType]] = list()
  for (optimizer in optimizerList){
    simM[[modelType]][[optimizer]] = list()
    simM[[modelType]][[optimizer]]$Rep1 = list()
    simM[[modelType]][[optimizer]]$Rep1$Target1 = list()
    simM[[modelType]][[optimizer]]$Rep1$Target1$fMulti = c()
    simM[[modelType]][[optimizer]]$Rep1$Target1$callsMulti = c()
    simM[[modelType]][[optimizer]]$Rep1$Target1$parsMulti = c()
    simM[[modelType]][[optimizer]]$Rep1$Target1$callsTraceMulti = list()
    simM[[modelType]][[optimizer]]$Rep1$Target1$fTraceMulti = list()
  }
  score_best = -999
  for (chunk in 1:nChunks){
    for (optimizer in optimizerList){
      simM[[modelType]][[optimizer]]$Rep1$Target1$fMulti =
        c(simM[[modelType]][[optimizer]]$Rep1$Target1$fMulti,
          simAll[[chunk]][[modelType]][[optimizer]]$Rep1$Target1$fMulti)
      simM[[modelType]][[optimizer]]$Rep1$Target1$callsMulti =
        c(simM[[modelType]][[optimizer]]$Rep1$Target1$callsMulti,
          simAll[[chunk]][[modelType]][[optimizer]]$Rep1$Target1$callsMulti)
      simM[[modelType]][[optimizer]]$Rep1$Target1$parsMulti =
        rbind(simM[[modelType]][[optimizer]]$Rep1$Target1$parsMulti,
              simAll[[chunk]][[modelType]][[optimizer]]$Rep1$Target1$parsMulti)
      for (r in 1:optimizerNmultiChunk){
        rNow = r+(chunk-1)*optimizerNmultiChunk
        simM[[modelType]][[optimizer]]$Rep1$Target1$callsTraceMulti[[rNow]] =
          simAll[[chunk]][[modelType]][[optimizer]]$Rep1$Target1$callsTraceMulti[[r]]
        simM[[modelType]][[optimizer]]$Rep1$Target1$fTraceMulti[[rNow]] =
          simAll[[chunk]][[modelType]][[optimizer]]$Rep1$Target1$fTraceMulti[[r]]
      }
      if (simAll[[chunk]][[modelType]][[optimizer]]$Rep1$Target1$score > score_best){
        score_best = simAll[[chunk]][[modelType]][[optimizer]]$Rep1$Target1$score
        chunk_best = chunk
      }
    }
  }
  for (optimizer in optimizerList){
    simM[[modelType]][[optimizer]]$Rep1$Target1$score = simAll[[chunk_best]][[modelType]][[optimizer]]$Rep1$Target1$score
    simM[[modelType]][[optimizer]]$Rep1$Target1$par = simAll[[chunk_best]][[modelType]][[optimizer]]$Rep1$Target1$par
    simM[[modelType]][[optimizer]]$Rep1$Target1$attSim = simAll[[chunk_best]][[modelType]][[optimizer]]$Rep1$Target1$attSim
    simM[[modelType]][[optimizer]]$Rep1$Target1$P = simAll[[chunk_best]][[modelType]][[optimizer]]$Rep1$Target1$P
  }

  return(simM)

}

########################

check_sim = function(sim1,sim2){

  if(!(all(sim1$options$optimizerList == sim2$options$optimizerList))){browser()}
  if(!(all(sim1$options$optimizerNmulti == sim2$options$optimizerNmulti))){browser()}
  for (optimizer in optimizerList){
    if(!(all(sim1[[modelType]][[optimizer]]$Rep1$Target1$fMulti == sim2[[modelType]][[optimizer]]$Rep1$Target1$fMulti))){browser()}
    if(!(all(sim1[[modelType]][[optimizer]]$Rep1$Target1$callsMulti == sim2[[modelType]][[optimizer]]$Rep1$Target1$callsMulti))){browser()}
    if(!(all(sim1[[modelType]][[optimizer]]$Rep1$Target1$parsMulti == sim2[[modelType]][[optimizer]]$Rep1$Target1$parsMulti))){browser()}
    if(!(all(sim1[[modelType]][[optimizer]]$Rep1$Target1$score == sim2[[modelType]][[optimizer]]$Rep1$Target1$score))){browser()}
    for (r in 1:optimizerNmulti){
      if(!(all(sim1[[modelType]][[optimizer]]$Rep1$Target1$callsTraceMulti[[r]] == sim2[[modelType]][[optimizer]]$Rep1$Target1$callsTraceMulti[[r]]))){browser()}
      if(!(all(sim1[[modelType]][[optimizer]]$Rep1$Target1$fTraceMulti[[r]] == sim2[[modelType]][[optimizer]]$Rep1$Target1$fTraceMulti[[r]]))){browser()}
    }
    if(!(all(sim1[[modelType]][[optimizer]]$Rep1$Target1$par == sim2[[modelType]][[optimizer]]$Rep1$Target1$par))){browser()}
    if(!(all(sim1[[modelType]][[optimizer]]$Rep1$Target1$attSim == sim2[[modelType]][[optimizer]]$Rep1$Target1$attSim))){browser()}
    if(!(all(sim1[[modelType]][[optimizer]]$Rep1$Target1$P$sim == sim2[[modelType]][[optimizer]]$Rep1$Target1$P$sim))){browser()}
  }
}


#########################

