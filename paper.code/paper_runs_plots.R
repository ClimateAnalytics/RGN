rm(list=ls())

devtools::load_all('C:/Users/a1065639/Work/foreSIGHT/')
devtools::load_all('C:/Users/a1065639/Work/RGN/')

#dataDirname = 'C:/Users/a1065639/Work/RGN/paper/2_phoenix/'
#dataDirname = 'C:/Users/a1065639/Work/RGN/paper/3_phoenix/'
dataDirname = 'C:/Users/a1065639/Work/RGN/paper/4_phoenix_paper.runs.2/'

#------------------------


# # thresh = 0.02
# fname1 = paste0(dataDirname,'expt_1.latent.annual.10yrs.100multiStart.RData')
# load(fname1)
# sim1 = sim

# thresh = 0.01
# fname2 = paste0(dataDirname,'expt_1.wgen.annual.10yrs.100multiStart.RData')
# load(fname2)
# sim2 = sim

#------------------------

# fname1 = paste0(dataDirname,'expt_1.latent.annual.10yrs.100multiStart.RData')
# load(fname1)
# sim1 = sim
# names(sim1)[2] = paste0(names(sim1)[2],', 10 yrs')

# # fname2 = paste0(dataDirname,'expt_2a.latent.annual.2yrs.100multiStart.RData')
# # load(fname2)
# # sim2 = sim
# # names(sim2)[2] = paste0(names(sim2)[2],', 2 yrs')
#
# # fname2 = paste0(dataDirname,'expt_2b.latent.annual.100yrs.100multiStart.RData')
# # load(fname2)
# # sim2 = sim
# # names(sim2)[2] = paste0(names(sim2)[2],', 100 yrs')

# thresh = 0.02
# fname2 = paste0(dataDirname,'expt_2c.latent.annual.1000yrs.50multiStart.RData')
# load(fname2)
# sim2 = sim
# names(sim2)[2] = paste0(names(sim2)[2],', 1000 yrs')

#------------------------

# fname1 = paste0(dataDirname,'expt_1.wgen.annual.10yrs.100multiStart.RData')
# load(fname1)
# sim1 = sim
# names(sim1)[2] = paste0(names(sim1)[2],', 10 yrs')

# # fname2 = paste0(dataDirname,'expt_2a.wgen.annual.2yrs.100multiStart.RData')
# # load(fname2)
# # sim2 = sim
# # names(sim2)[2] = paste0(names(sim2)[2],', 2 yrs')
# # names(sim2)[2] = paste0(names(sim2)[2],', 1000 yrs'
#
# # fname2 = paste0(dataDirname,'expt_2a.wgen.annual.2yrs.100multiStart.RData')
# # load(fname2)
# # sim2 = sim
# # names(sim2)[2] = paste0(names(sim2)[2],', 2 yrs')
#
# # fname2 = paste0(dataDirname,'expt_2b.wgen.annual.100yrs.100multiStart.RData')
# # load(fname2)
#
#
# # fname2 = paste0(dataDirname,'expt_2a.wgen.annual.2yrs.100multiStart.RData')
# # load(fname2)
# # sim2 = sim
# # names(sim2)[2] = paste0(names(sim2)[2],', 2 yrs')
# #
# # # fname2 = paste0(dataDirname,'expt_2b.wgen.annual.100yrs.100multiStart.RData')
# # # load(fname2)
# # sim2 = sim
#
#
# # thresh = 0.01
# fname2 = paste0(dataDirname,'expt_2c.wgen.annual.1000yrs.50multiStart.RData')
# load(fname2)
# sim2 = sim
# names(sim2)[2] = paste0(names(sim2)[2],', 1000 yrs')

#------------------------

# fname1 = paste0(dataDirname,'expt_1.latent.annual.10yrs.100multiStart.RData')
# load(fname1)
# sim1 = sim
# names(sim1)[2] = paste0(names(sim1)[2],', annual')
#
# fname2 = paste0(dataDirname,'expt_3.latent.harmonic.10yrs.100multiStart.RData')
# load(fname2)
# sim2 = sim
# names(sim2)[2] = paste0(names(sim2)[2],', harmonic')

# fname1 = paste0(dataDirname,'expt_1.wgen.annual.10yrs.100multiStart.RData')
# load(fname1)
# sim1 = sim
# names(sim1)[2] = paste0(names(sim1)[2],', annual')
#
# fname2 = paste0(dataDirname,'expt_3.wgen.harmonic.10yrs.100multiStart.RData')
# load(fname2)
# sim2 = sim
# names(sim2)[2] = paste0(names(sim2)[2],', harmonic')

# sim = c(sim1,sim2)

# fname = paste0(dataDirname,'expt_3.latent.harmonic.10yrs.100multiStart.RData')
# load(fname)

# fname = paste0(dataDirname,'expt_3.wgen.harmonic.10yrs.100multiStart.RData')
# load(fname)

# fname = paste0(dataDirname,'expt_1.latent.annual.10yrs.100multiStart.RData')
# load(fname)

###########################################################

setupControlFile = function(modelType,modelParameterVariation,optimizer,optimizerNmulti,
                            par=NULL,freePars=NULL){

  # setup control file
  controlFileList = list()
  controlFileList$modelType$P <- modelType
  controlFileList$modelParameterVariation$P <- modelParameterVariation

  controlFileList$optimisationArguments$optimizer <- optimizer
  controlFileList$optimisationArguments$nMultiStart = optimizerNmulti

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
#    controlFileList$modelParameterBounds$P$alpha_amp = c(0,0.5)
#    controlFileList$modelParameterBounds$P$alpha_ang = c(0,6.28)
    controlFileList$modelParameterBounds$P$sigma_m = c(1.5,10.) # sigma
    controlFileList$modelParameterBounds$P$sigma_amp = c(0.04,2.7)
    controlFileList$modelParameterBounds$P$sigma_ang = c(0.02,4.5)
    controlFileList$modelParameterBounds$P$mu_m = c(-10.8,0.8) # mu
    controlFileList$modelParameterBounds$P$mu_amp = c(0.18,8.)
    controlFileList$modelParameterBounds$P$mu_ang = c(0.11,4.4)
    controlFileList$modelParameterBounds$P$lambda_m = c(1.4,3.1) # beta
    controlFileList$modelParameterBounds$P$lambda_amp = c(0.02,0.8)
    #controlFileList$modelParameterBounds$P$lambda_amp = c(0.02,2)
    controlFileList$modelParameterBounds$P$lambda_ang = c(0.33,4.2)
    #controlFileList$modelParameterBounds$P$lambda_ang = c(0.,6.28)
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
                  par=NULL,freePars=NULL,plotScen=F){

  setupControlFile(modelType=modelType,modelParameterVariation=modelParameterVariation,
                   optimizer=optimizer,optimizerNmulti=optimizerNmulti,
                   par=par,freePars=freePars)
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
                                  par=options$par,freePars=options$freePars)
    }
  }

  return(sim)
}

# -----------
#dirname = "C:/Users/a1065639/Work/RGN/paper/1_prelim/"
#dirname = "C:/Users/a1065639/Work/RGN/paper/3_phoenix/"

# -----------
# determine model parameter names
modelParNames = setupModelParNames()

# -----------
# # climate data
# data("tankDat")
# clim_ref = tank_obs
# clim_ref$Temp = NULL

# -----------
# WG seed
seedID = 1

# pars to fit
freePars = NULL
par = NULL

# -----------
# plotting options
colListDefault = c('cyan','magenta','brown','grey','red','green','blue')
ltyListDefault = c(1,1,1,2,1,1,2)
pchListDefault = seq(0,6)

# -----------
# plot OF and OF calls

plot_OF_calls = function(sim,modelTypeList=NULL,
                         colList=colListDefault,ltyList=ltyListDefault,
                         pchList=pchListDefault,
                         ymax_OF=0.5,ymax_calls=1e4,OF_rmse=T,optimizerList=NULL,
                         plot_calls=T){

  if(is.null(optimizerList)){optimizerList = sim$options$optimizerList}
  optimizerNmulti = sim$options$optimizerNmulti

#  if (is.null(modelTypeList)){modelTypeList = names(sim)[names(sim) %in% c('wgen','latent')]}
  if (is.null(modelTypeList)){modelTypeList = names(sim)[names(sim)!='options']}

#  par(mfrow=c(2,1),mar=c(5,5,1,1))

#  y = seq(1,optimizerNmulti)/optimizerNmulti

  npar = length(sim[[modelTypeList[1]]][[optimizerList[1]]]$Rep1$Target1$par)
  nAtt = length(sim[[modelTypeList[1]]][[optimizerList[1]]]$Rep1$Target1$attSim)

  if (length(optimizerList)==1){colList = 'black'}

  best = c()
  for (m in 1:length(modelTypeList)){
    modelType = modelTypeList[m]
    best[m] = 999
    for (o in 1:length(optimizerListAll)){
      optimizer = optimizerListAll[o]
      x = -sim[[modelType]][[optimizer]]$Rep1$Target1$score
      if (OF_rmse){x = x/sqrt(nAtt)}
      if (x < best[m]){best[m]=x}
    }
  }

  plot(x=NULL,y=NULL,ylim=c(0.,ymax_OF),xlim=c(0,1),xaxs='i',xlab='Fraction of multi-starts',ylab='')
  for (m in 1:length(modelTypeList)){
    modelType = modelTypeList[m]
    for (o in 1:length(optimizerList)){
      optimizer = optimizerList[o]
      x = sim[[modelType]][[optimizer]]$Rep1$Target1$fMulti
      if (OF_rmse){x = x/sqrt(nAtt)}
      s = sort(x,index.return=T)
      x.sort = s$x
      y = seq(1,length(x))/length(x)
#      browser()
      if (length(optimizerList)==1){x.sort=x.sort-best[m]}
      lines(y,x.sort,col=colList[o],type='l',lty=m,lwd=2)
#      abline(h=best[m],lty=2)
#      points(y,x.sort,col=colList[o],pch=pchList[o])
    }
  }
#  if(!is.null(OF_line)){abline(h=OF_line,lty=3)}
  if (length(optimizerList)==1){
    title(optimizerLabelList[optimizerList])
    if(optimizer==optimizerListAll[1]){legend('topleft',legend=modelTypeList,lty=1:length(modelTypeList))}
    mtext(text='Difference from best OF value',side=2,line = 3,las=0)
  } else {
    title('Objective function')
    legend('topleft',legend=optimizerLabelList[optimizerList],col=colList,lwd=2)
    legend('top',legend=modelTypeList,lty=1:length(modelTypeList))
    mtext(text='Objective function',side=2,line = 3,las=0)
  }

  if (plot_calls){
    plot(x=NULL,y=NULL,ylim=c(1,ymax_calls),xlim=c(0,1),log='y',xaxs='i',yaxs='i',xlab='Fraction of multi-starts',ylab='')
    mtext(text='Function calls',side=2,line = 3,las=0)
    for (m in 1:length(modelTypeList)){
      modelType = modelTypeList[m]
      for (o in 1:length(optimizerList)){
        optimizer = optimizerList[o]
        x = sim[[modelType]][[optimizer]]$Rep1$Target1$callsMulti
        s = sort(x,index.return=T)
        x.sort = s$x
        y = seq(1,length(x))/length(x)
        lines(y,x.sort,col=colList[o],type='l',lty=m,lwd=2)
  #      points(y,x.sort,col=colList[o],pch=pchList[o])
      }
    }
    title('Function calls')
  }
}

# -----------

# plot_OF_surface = function(sim,modelType,modelParameterVariation,parList=NULL,optimizer,OF_rmse=T,
#                            multiStartList=NULL,facList=seq(0.95,1.05,length=101),attListSurface=attList){
#
#   parName = modelParNames[[modelType]][[modelParameterVariation]]
#
#   if (is.null(parList)){parList = parName}
#
#   if(is.null(multiStartList)){
#     multiStartList = c('best')
#   }
#
#   for (om in multiStartList){
#
# #    browser()
#
#     if (om=='best'){
#       parOpt = sim[[modelType]][[optimizer]]$Rep1$Target1$par
#     } else {
#       parOpt = sim[[modelType]][[optimizer]]$Rep1$Target1$parsMulti[om,]
#     }
#     names(parOpt) = parName
#
#     for (fp2 in 1:length(parList)){
#       freePar = parList[fp2]
#       parTmp = parOpt
# #      if (parOpt[freePar]!=0){
#         freeParVal_list = parOpt[freePar]*facList
# #      } else {
# #        freeParVal_list = 0.1*(d[p1,3]-d[p1,2])*seq(0,1,length=21)
# #      }
#
#       score = c()
#       for (p in 1:length(freeParVal_list)){
#
#
#
#         parTmp[freePar] = freeParVal_list[p]
#         print(parTmp)
#         simTmp = simRun(clim_ref=clim_ref,attList=attListSurface,nYrs=nYrs,seedID=seedID,
#                         modelType=modelType,modelParameterVariation=modelParameterVariation,
#                         optimizer='XXX',optimizerNmulti=1,
#                         par=parTmp,freePars=c())
#         score[p] = simTmp$Rep1$Target1$score
#         if (OF_rmse){score[p] = score[p] / sqrt(length(parOpt))}
#       }
#       plot(freeParVal_list,-score,type='l',xlab=freePar,ylab='Objective function')
#       abline(v=parOpt[freePar],lty=2)
#       title(freePar)
#     }
#   }
#
# }

# -----------

plot_OF_surface = function(sim,modelType,modelParameterVariation,parList=NULL,optimizer,OF_rmse=T,
                           multiStartList=NULL,
                           facList=seq(0.95,1.05,length=101),parValList=NULL,
                           attListSurface=attList,seed=1,plotType='1d',plot_OF=T,
                           color.palette='viridis'){

  parName = modelParNames[[modelType]][[modelParameterVariation]]

  if (is.null(parList)){parList = parName}

  if(is.null(multiStartList)){
    multiStartList = c('best')
  }

  o = sim[[modelType]][[optimizer]]$Rep1$Target1
  ndays = length(o$P$sim)
#  browser()
  set.seed(seed)
  randomVector <- stats::runif(ndays) # Random vector to be passed into weather generator to reduce runtime
  if(modelType=='latent'){
    randomUnitNormalVector = stats::qnorm(randomVector)
    randomVector = NULL
    seed = NULL
  }
  att_ref = calculateAttributes(clim_ref,attList)

  for (om in multiStartList){

    if (om=='best'){
      parOpt = sim[[modelType]][[optimizer]]$Rep1$Target1$par
    } else {
      parOpt = sim[[modelType]][[optimizer]]$Rep1$Target1$parsMulti[om,]
    }
    names(parOpt) = parName

    if (plotType == '1d'){

      for (fp in 1:length(parList)){
        freePar = parList[fp]
        parTmp = parOpt
        if (is.null(parValList[[freePar]])){
          freeParVal_list = parOpt[freePar]*facList
        } else {
          freeParVal_list = parValList[[freePar]]
        }

        score = c()
        for (p in 1:length(freeParVal_list)){
          parTmp[freePar] = freeParVal_list[p]
          print(parTmp)
          score[p] = calc_score(modelType,par=parTmp,ndays,
                                randomUnitNormalVector,
                                randomVector,
                                seed,
                                att_ref,
                                attListSurface,
                                plot_OF=plot_OF)
#          browser()
        }
        plot(freeParVal_list,score,type='l',xlab=freePar,ylab='Objective function')
        abline(v=parOpt[freePar],lty=2)
        title(freePar)
      }

    } else if (plotType == '2d'){

      for (fp1 in 1:(length(parList)-1)){
        freePar1 = parList[fp1]
        if (is.null(parValList[[freePar1]])){
          if (parOpt[freePar1] > 0){
            freePar1Val_list = parOpt[freePar1]*facList
          } else {
            freePar1Val_list = parOpt[freePar1]*rev(facList)
          }
        } else {
          freePar1Val_list = parValList[[freePar1]]
        }

        for (fp2 in (fp1+1):length(parList)){
          freePar2 = parList[fp2]

          if (is.null(parValList[[freePar2]])){
            if (parOpt[freePar2] > 0){
              freePar2Val_list = parOpt[freePar2]*facList
            } else {
              freePar2Val_list = parOpt[freePar2]*rev(facList)
            }
          } else {
            freePar2Val_list = parValList[[freePar2]]
          }

          score = matrix(nrow=length(freePar1Val_list),ncol=length(freePar2Val_list))
          parTmp = parOpt

          for (p1 in 1:length(freePar1Val_list)){
            print(p1)
            parTmp[freePar1] = freePar1Val_list[p1]

            for (p2 in 1:length(freePar2Val_list)){
              parTmp[freePar2] = freePar2Val_list[p2]
#              print(parTmp)

#              browser()

              # P_sim = calc_P_sim(modelType,par=parTmp,ndays,
              #                    randomUnitNormalVector=randomUnitNormalVector,
              #                    randomVector=randomVector,
              #                    seed=seed)
              # clim_sim = clim_ref
              # clim_sim$P = P_sim$sim
              # att_sim = calculateAttributes(clim_sim,attListSurface)
              # resid = (att_sim - att_ref)/ att_ref
              # score[p1,p2] = sqrt(mean(resid^2))

              score[p1,p2] = calc_score(modelType,par=parTmp,ndays,
                                    randomUnitNormalVector,
                                    randomVector,
                                    seed,
                                    att_ref,
                                    attListSurface,
                                    plot_OF=plot_OF)

            }
          }

          # par(mfrow=c(1,1),mar=c(5,5,5,5))
          # contour(x = freePar1Val_list,y=freePar2Val_list,z = score,
          #                xlab = freePar1, ylab=freePar2,
          #                plot.title = {points(parOpt[freePar1],parOpt[freePar2])})
          # title(paste(freePar1,'vs',freePar2))

          par(mfrow=c(1,1),mar=c(7,7,7,7))
          if(color.palette=='viridis'){
            col = viridis::viridis(n=30,direction=-1)
          }
          filled.contour(x = freePar1Val_list,y=freePar2Val_list,z = score,#xlab = freePar1, ylab=freePar2,]
                         col=col,
                         plot.title = {
                           points(parOpt[freePar1],parOpt[freePar2])
                           title(xlab=freePar1,cex.lab=2)
                           mtext(freePar2,2,cex=2,line=3,las=0)})
          title(paste(freePar1,'vs',freePar2))

        }
      }
    }

  }

}

# -----------

calc_P_sim = function(modelType,par,ndays,randomUnitNormalVector=NULL,randomVector=NULL,seed=1){

  if (modelType=='latent'){
    parAlpha <- rep_len(par[1], length.out = ndays)
    parSigma <- rep_len(par[2], length.out = ndays)
    parMu <- rep_len(par[3], length.out = ndays)
    parLambda <- rep_len(par[4], length.out = ndays)
    parTS <- list(sigma = parSigma, mu = parMu, lambda = parLambda, alpha = parAlpha)
    P_new = P_latent(parTS=parTS, randomUnitNormalVector=randomUnitNormalVector)
  } else if (modelType == 'wgen'){
    parPdd <- rep_len(par[1], length.out = ndays)
    parPwd <- rep_len(par[2], length.out = ndays)
    parAlpha <- rep_len(par[3], length.out = ndays)
    parBeta <- rep_len(par[4], length.out = ndays)
    parTS <- list(pdd = parPdd, pwd = parPwd, alpha = parAlpha, beta = parBeta)
    P_new = P_WGEN(parTS=parTS, randomVector=randomVector,modelEnv=NULL,N=seed)
  }
}

# -----------

calc_score = function(modelType,par,ndays,
                      randomUnitNormalVector,
                      randomVector,
                      seed,
                      att_ref,
                      attListSurface,
                      plot_OF=T){

  P_sim = calc_P_sim(modelType,par,ndays,
                     randomUnitNormalVector,
                     randomVector,
                     seed)

  clim_sim = clim_ref
  date1 = as.Date(paste0(clim_sim$year,'/',clim_sim$month,'/',clim_sim$day,sep=''))[1]
  dates = date1 + seq(0,(ndays-1))
  clim_sim$year = as.integer(format(dates,'%Y'))
  clim_sim$month = as.integer(format(dates,'%m'))
  clim_sim$day = as.integer(format(dates,'%d'))
  clim_sim$P = P_sim$sim
  att_sim = calculateAttributes(clim_sim,attListSurface)
  if (plot_OF){
    resid = (att_sim - att_ref)/ att_ref
    score = sqrt(mean(resid^2))
  }
  else {
    score = att_sim
  }
  return(score)

}



# -----------


plot_relMulti = function(sim,scen,thresh,xlim_calls=1e9,OF_rmse=T,colList=colListDefault,pchList=pchListDefault){

  Nms = length(sim[[scen]][[1]]$Rep1$Target1$fMulti)
  Np = length(sim[[scen]][[1]]$Rep1$Target1$par)
  Natt = length(sim[[scen]][[1]]$Rep1$Target1$attSim)

  N = 1:Nms

  pMulti = aveCallsMulti = list()
  for (optimizer in optimizerList){
    f = sim[[scen]][[optimizer]]$Rep1$Target1$fMulti
    if (OF_rmse){f = f / sqrt(Natt)}
    p1 = length(which(f<thresh))/length(f)
    pMulti[[optimizer]] = 1-(1-p1)^N
    calls = sim[[scen]][[optimizer]]$Rep1$Target1$callsMulti
    aveCalls = mean(calls)
    aveCallsMulti[[optimizer]] = aveCalls
  }

  plot(x=NULL,y=NULL,xlim=c(100,xlim_calls),ylim=c(0,1),log='x',
       xlab='Function calls',ylab=paste0('Reliability (OF<',thresh,')'),
       main='Reliability')
  for (o in 1:length(optimizerList)){
    optimizer = optimizerList[o]
    lines(N*aveCallsMulti[[optimizer]],pMulti[[optimizer]],col=colListDefault[o])
    points(N*aveCallsMulti[[optimizer]],pMulti[[optimizer]],col=colListDefault[o],pch=pchList[o],cex=0.5)
  }
  legend('bottomright',legend=optimizerList,col=colList,lty=1,pch=pchList,cex=0.8)

}

# -----------


plot_relMulti_1 = function(sim,scen,thresh_list,pThresh=0.95,OF_rmse=T,colList=colListDefault,pchList=pchListDefault){

  Nms = length(sim[[scen]][[1]]$Rep1$Target1$fMulti)
  Np = length(sim[[scen]][[1]]$Rep1$Target1$par)
  Natt = length(sim[[scen]][[1]]$Rep1$Target1$attSim)

  N = 1:Nms

  maxY = 1
  minCalls = list()
  for (optimizer in optimizerList){
    f = sim[[scen]][[optimizer]]$Rep1$Target1$fMulti
    if (OF_rmse){f = f / sqrt(Natt)}
    calls = sim[[scen]][[optimizer]]$Rep1$Target1$callsMulti
    aveCalls = mean(calls)
    minCalls[[optimizer]] = c()
    for (t in 1:length(thresh_list)){
      thresh = thresh_list[t]
      p1 = length(which(f<thresh))/length(f)
    pMulti = 1-(1-p1)^N
    keep = which(pMulti>pThresh)
     if (length(keep)>0){
       minMS = min(keep)
     } else {
       minMS = NA
     }
     minCalls[[optimizer]][t] = minMS*aveCalls
     maxY = max(maxY,minCalls[[optimizer]][t],na.rm=T)
#      N = log(1-pThresh)/log(1-p1)

    }
  }

  plot(x=NULL,y=NULL,xlim=c(min(thresh_list),max(thresh_list)),ylim=c(1,maxY),log='y',
       xlab='Threshold',ylab='Calls',
       main='Reliability')
  for (o in 1:length(optimizerList)){
    optimizer = optimizerList[o]
    lines(thresh_list,minCalls[[optimizer]],col=colListDefault[o])
  }
  # plot(x=NULL,y=NULL,ylim=c(min(thresh_list),max(thresh_list)),xlim=c(1,maxY),log='x',
  #      ylab='Threshold',xlab='Calls',
  #      main='Reliability')
  # for (o in 1:length(optimizerList)){
  #   optimizer = optimizerList[o]
  #   lines(minCalls[[optimizer]],thresh_list,col=colListDefault[o])
  # }
  legend('bottomright',legend=optimizerList,col=colList,lty=1,pch=pchList,cex=0.8)

}

# -----------


plot_relMulti_2 = function(sim,scen,pThresh=0.95,OF_rmse=T,
                           colList=colListDefault,pchList=pchListDefault,
                           callsMin=1e2,callsMax=1e6,
                           threshMin=0,threshMax=0.3){

  Nms = length(sim[[scen]][[1]]$Rep1$Target1$fMulti)
  Np = length(sim[[scen]][[1]]$Rep1$Target1$par)
  Natt = length(sim[[scen]][[1]]$Rep1$Target1$attSim)

  N = 1:Nms

  maxY = 1
  callsMulti = threshMulti = list()
  for (optimizer in optimizerList){
    f = sim[[scen]][[optimizer]]$Rep1$Target1$fMulti
    if (OF_rmse){f = f / sqrt(Natt)}
    calls = sim[[scen]][[optimizer]]$Rep1$Target1$callsMulti
    aveCalls = mean(calls)
    threshMulti[[optimizer]] = callsMulti[[optimizer]] = c()
    for (n in 1:Nms){
      p = 1 - (1-pThresh)^(1/n)
      threshMulti[[optimizer]][n] = quantile(f,p)
      callsMulti[[optimizer]][n] = n*aveCalls
    }
  }

  plot(x=NULL,y=NULL,
       xlim=c(callsMin,callsMax),
       ylim=c(threshMin,threshMax),log='x',
       xlab='Calls',ylab='Objective function',
       main='Objective function value (95% reliable) for number of function calls')
  for (o in 1:length(optimizerList)){
    optimizer = optimizerList[o]
    lines(callsMulti[[optimizer]],threshMulti[[optimizer]],col=colListDefault[o])
    points(callsMulti[[optimizer]],threshMulti[[optimizer]],col=colListDefault[o],pch=pchListDefault[o])
  }
  # plot(x=NULL,y=NULL,ylim=c(min(thresh_list),max(thresh_list)),xlim=c(1,maxY),log='x',
  #      ylab='Threshold',xlab='Calls',
  #      main='Reliability')
  # for (o in 1:length(optimizerList)){
  #   optimizer = optimizerList[o]
  #   lines(minCalls[[optimizer]],thresh_list,col=colListDefault[o])
  # }
#  legend('topright',legend=optimizerList,col=colList,lty=1,pch=pchList,cex=0.8)

}

# ###########################################################

data("barossaDat")
clim_ref = barossa_obs
clim_ref$P = clim_ref$P[,'X23300']

# ###########################################################
# optimizers

#optimizerList = c('RGN','CMAES','NLSLM','GA','SCE','optim.LBFGSB')
#optimizerList = c('RGN','CMAES','NLSLM','GA','SCE','optim.LBFGSB','NMKB')
#optimizerList = c('RGN','CMAES','GA','SCE','optim.LBFGSB','NMKB')
optimizerList = c('RGN','SCE','NMKB')
#optimizerList = c('RGN','optim.LBFGSB','NMKB')
#optimizerList = c('GA')
#optimizerList = c('optim.LBFGSB')
#optimizerList = c('optim.LBFGSB')
# #optimizerList = c('Rvmmin','optim.LBFGSB')
# optimizerList = c('Rcgmin','NLSLM','NMKB','RGN','optim.LBFGSB')
# #optimizerNmulti = 100
optimizerListAll = optimizerList

optimizerNmulti = 30

# # ------------
#
# # stochastic model
# #modelTypeList = c('wgen','latent')
modelTypeList = c('wgen')
#modelTypeList = c('latent')
#
# ------------
# expt 1

modelParameterVariation = 'annual'
attList =  c("P_ann_tot_m","P_ann_P99","P_ann_avgWSD_m","P_ann_nWet_m")
nYrs = 10

name = paste0(modelParameterVariation,'.',nYrs)
options = list(attList=attList,nYrs=nYrs,seedID=seedID,
               modelTypeList=modelTypeList,modelParameterVariation=modelParameterVariation,
               optimizerList=optimizerList,optimizerNmulti=optimizerNmulti,
               par=par,freePars=freePars)
sim = doRuns(clim_ref=clim_ref,options=options)
#fname = paste0(dirname,name,'.RData')
#save.image(file=fname)

# plot_OF_calls(sim,ymax_OF = 1,ymax_calls = 1e4)


pause

# ------------
# expt 2 new

# modelParameterVariation = 'annual'
# attList =  c("P_ann_tot_m","P_ann_P99_m10yrBlock","P_ann_avgWSD_m","P_ann_nWet_m")
# nYrs = 1000
# optimizerNmulti = 3
#
# name = paste0(modelParameterVariation,'.',nYrs)
# options = list(attList=attList,nYrs=nYrs,seedID=seedID,
#                modelTypeList=modelTypeList,modelParameterVariation=modelParameterVariation,
#                optimizerList=optimizerList,optimizerNmulti=optimizerNmulti,
#                par=par,freePars=freePars)
# sim = doRuns(clim_ref=clim_ref,options=options)
# #fname = paste0(dirname,name,'.RData')
# #save.image(file=fname)
#
# plot_OF_calls(sim,ymax_OF = 1,ymax_calls = 1e4)
#
# pause

#
# # ------------
# # expt 2a
#
# modelParameterVariation = 'annual'
# attList =  c("P_ann_tot_m","P_ann_P99","P_ann_avgWSD_m","P_ann_nWet_m")
# nYrs = 2
#
# name = paste0(modelParameterVariation,'.',nYrs)
# options = list(attList=attList,nYrs=nYrs,seedID=seedID,
#                modelTypeList=modelTypeList,modelParameterVariation=modelParameterVariation,
#                optimizerList=optimizerList,optimizerNmulti=optimizerNmulti,
#                par=par,freePars=freePars)
# sim = doRuns(clim_ref=clim_ref,options=options)
# fname = paste0(dirname,name,'.RData')
# save.image(file=fname)
#
# # ------------
# # expt 2b
#
# modelParameterVariation = 'annual'
# attList =  c("P_ann_tot_m","P_ann_P99","P_ann_avgWSD_m","P_ann_nWet_m")
# nYrs = 100
#
# name = paste0(modelParameterVariation,'.',nYrs)
# options = list(attList=attList,nYrs=nYrs,seedID=seedID,
#                modelTypeList=modelTypeList,modelParameterVariation=modelParameterVariation,
#                optimizerList=optimizerList,optimizerNmulti=optimizerNmulti,
#                par=par,freePars=freePars)
# sim = doRuns(clim_ref=clim_ref,options=options)
# fname = paste0(dirname,name,'.RData')
# save.image(file=fname)
#
# # ------------
# expt 3

modelParameterVariation = 'harmonic'
attList =  c("P_DJF_tot_m","P_DJF_P99","P_DJF_avgWSD_m","P_DJF_nWet_m",
             "P_MAM_tot_m","P_MAM_P99","P_MAM_avgWSD_m","P_MAM_nWet_m",
             "P_JJA_tot_m","P_JJA_P99","P_JJA_avgWSD_m","P_JJA_nWet_m",
             "P_SON_tot_m","P_SON_P99","P_SON_avgWSD_m","P_SON_nWet_m")
nYrs = 10

name = paste0(modelParameterVariation,'.',nYrs)
options = list(attList=attList,nYrs=nYrs,seedID=seedID,
               modelTypeList=modelTypeList,modelParameterVariation=modelParameterVariation,
               optimizerList=optimizerList,optimizerNmulti=optimizerNmulti,
               par=par,freePars=freePars)
sim = doRuns(clim_ref=clim_ref,options=options)
#fname = paste0(dirname,name,'.RData')
#save.image(file=fname)

plot_OF_calls(sim,ymax_OF = 1,ymax_calls = 1e4)

maxCalls = 0
for (optimizer in optimizerList){
  for (r in 1:optimizerNmulti){
    # print(sim$latent[[1]]$Rep1$Target1$callsMultiTrace[[r]])
    maxCalls = max(maxCalls,sim$latent[[optimizer]]$Rep1$Target1$callsMultiTrace[[r]])
  }
}


#plot(x=NULL,xlim=c(0,maxCalls),ylim=c(1e-4,1),main=optimizerList[1],log='y')
plot(x=NULL,xlim=c(0,maxCalls),ylim=c(0,2),main=optimizerList[1],xaxs='i',yaxs='i')
for (o in 1:length(optimizerList)){
  optimizer = optimizerList[o]
  for (r in 1:optimizerNmulti){
    x = sim$latent[[optimizer]]$Rep1$Target1$callsMultiTrace[[r]]
    y = -sim$latent[[optimizer]]$Rep1$Target1$fMultiTrace[[r]]
    lines(x,y,col=colListDefault[o])
    points(max(x),min(y),col=colListDefault[o])
  }
}
legend('topright',optimizerList,col=colListDefault[1:length(optimizerList)],lty=1)

pause
#
# ###########################################################
#
# name = paste0(modelParameterVariation,'.',nYrs)
# options = list(attList=attList,nYrs=nYrs,seedID=seedID,
#                modelTypeList=modelTypeList,modelParameterVariation=modelParameterVariation,
#                optimizerList=optimizerList,optimizerNmulti=optimizerNmulti,
#                par=par,freePars=freePars)
# sim = doRuns(clim_ref=clim_ref,options=options)
# fname = paste0(dirname,name,'.RData')
# save.image(file=fname)

optimizerList = c('NLSLM','optim.LBFGSB','NMKB','CMAES','GA','SCE','RGN')
optimizerListAll = optimizerList
optimizerLabelList = c('LM','LBFGS','NM','CMAES','GA','SCE','RGN')
names(optimizerLabelList) = optimizerList


par(mfrow=c(3,1),mar=c(5,5,1,1))

plot_OF_calls(sim,ymax_OF = 1,optimizerList=optimizerList,ymax_calls = 5e4)

#par(mfrow=c(1,1))
# plot_relMulti(sim,scen=modelTypeList[1],thresh,xlim_calls = 1e8)
# abline(h=0.95,lty=2)

# plot_relMulti(sim,scen=modelTypeList[1],thresh,xlim_calls = 1e8)

#plot_relMulti_2(sim,scen=modelTypeList[1])

if(length(names(sim))==2){
#  plot_relMulti_2(sim,scen=modelTypeList[1],threshMin=0.16,threshMax=0.4,callsMin=1e2,callsMax=1e6)
#  plot_relMulti_2(sim,scen=modelTypeList[1],threshMin=0.0,threshMax=0.4,callsMin=1e2,callsMax=1e6)
  plot_relMulti_2(sim,scen=modelTypeList[1],threshMin=0.0,threshMax=0.6,callsMin=1e3,callsMax=1e6)
}

#plot_OF_calls(sim,ymax_OF = 1.5,optimizerList=optimizerList,ymax_calls = 1e6)
# #plot_OF_calls(sim,modelType='wgen',ymax_OF = 1)
#
#

if(length(names(sim))>2){
  par(mfrow=c(2,4))
  for (optimizer in optimizerList){
    plot_OF_calls(sim,ymax_OF = 1,optimizerList=optimizer,ymax_calls = 5e4,plot_calls=F)
  }
}

###############################
# # LV, 10 yrs, annual
#
# 1d
# par(mfrow=c(2,2))
# plot_OF_surface(sim,modelType=modelTypeList[1],modelParameterVariation=modelParameterVariation,
#                 optimizer='RGN',attListSurface=attList,
#                 facList = seq(0.99,1.01,length.out=201))
# # 2d
# plot_OF_surface(sim,modelType=modelTypeList[1],modelParameterVariation=modelParameterVariation,
#                 optimizer='NMKB',multiStartList = 3,attListSurface=attList,plot_OF = T,
#                 facList = seq(0.98,1.02,length.out=31),plotType = '2d',parList = c('alpha','mu'))

# 1d - flat
# par(mfrow=c(2,2))
# plot_OF_surface(sim,modelType=modelTypeList[1],modelParameterVariation=modelParameterVariation,
#                 optimizer='RGN',attListSurface=attList,
#                 facList = seq(0.8,1.2,length.out=21),
#                 parList = c('mu','sigma'),
#                 parValList = list(mu=seq(-10,1,1),sigma=seq(0.01,3,0.02)))
#
# par(mfrow=c(2,2))
# plot_OF_surface(sim,modelType=modelTypeList[1],modelParameterVariation=modelParameterVariation,
#                 optimizer='RGN',attListSurface=attList,
#                 parList = c('mu','sigma'),
#                 facList = seq(0.8,1.2,length.out=11),
#                 parValList = list(mu=seq(-6,-1,0.2),sigma=seq(0.1,1,0.05)),
#                 plotType = '2d')


###############################
# WGEN, 10 yrs, annual

# 1d
par(mfrow=c(2,2))
plot_OF_surface(sim,modelType=modelTypeList[1],modelParameterVariation=modelParameterVariation,
                optimizer='NMKB',multiStartList = 5,attListSurface=attList,
                facList = seq(0.99,1.01,length.out=201))

# # 2d
# plot_OF_surface(sim,modelType=modelTypeList[1],modelParameterVariation=modelParameterVariation,
#                 optimizer='NMKB',multiStartList = 5,attListSurface=attList,plot_OF = T,
#                 facList = seq(0.98,1.02,length.out=31),plotType = '2d',parList = c('pdd','beta'))


pause

par(mfrow=c(2,2))
plot_OF_surface(sim,modelType=modelTypeList[1],modelParameterVariation=modelParameterVariation,
                optimizer='NMKB',multiStartList = 5,attListSurface=attList,
                facList = seq(0.995,1.005,length.out=21))

pause

# par(mfrow=c(2,2))
# plot_OF_surface(sim,modelType=modelTypeList[1],modelParameterVariation=modelParameterVariation,
#                 optimizer='NMKB',multiStartList = 5,attListSurface=attList,
#                 facList = seq(0.995,1.005,length.out=101))


#par(mfrow=c(2,2))

# par(mfrow=c(2,2))
# plot_OF_surface(sim,modelType=modelTypeList[1],modelParameterVariation=modelParameterVariation,
#                 optimizer='RGN',attListSurface=attList,plot_OF = T,
#                 facList = seq(0.98,1.02,length.out=11),plotType = '2d')
pause

# #
# plot_OF_surface(sim,modelType='latent',modelParameterVariation=modelParameterVariation,
#                 optimizer='RGN',attListSurface=attList,
#                 facList = seq(0.98,1.02,length.out=5),
#                 plotType='2d')


# plot_OF_surface(sim,modelType='wgen',modelParameterVariation=modelParameterVariation,
#                   optimizer='RGN',attListSurface=attList,
#                   facList = 1)
#
# plot_OF_surface_1(sim,modelType='wgen',modelParameterVariation=modelParameterVariation,
#                   optimizer='RGN',attListSurface=attList,
#                   facList = 1)

#pause
#
#
# plot_OF_calls(sim,ymax_OF = 1,ymax_calls = 1e5)
#
# #thresh = 0.001
#thresh = 0.01
thresh = 0.02
#thresh = 0.1
#thresh = 0.25
#thresh = 0.4
scen = 'latent'
#scen = 'wgen'
# #scen = 'latent, 10 yrs'
# #scen = 'wgen, 1000 yrs'
# #scen = 'latent, 1000 yrs'
# #scen = 'wgen, 10 yrs'
#
par(mfrow=c(1,1))
plot_relMulti(sim,scen,thresh,xlim_calls = 1e8)
abline(h=0.95,lty=2)

pause

#############################################

o = sim$latent$RGN$Rep1$Target1

ndays = length(o$P$sim)

pars = o$par

set.seed(1)

randomVector <- stats::runif(ndays) # Random vector to be passed into weather generator to reduce runtime

randomUnitNormalVector = stats::qnorm(randomVector)
parAlpha <- rep_len(pars[1], length.out = ndays)
parSigma <- rep_len(pars[2], length.out = ndays)
parMu <- rep_len(pars[3], length.out = ndays)
parLambda <- rep_len(pars[4], length.out = ndays)

parTS <- list(sigma = parSigma, mu = parMu, lambda = parLambda, alpha = parAlpha)

P_new = P_latent(parTS=parTS, randomUnitNormalVector=randomUnitNormalVector)

