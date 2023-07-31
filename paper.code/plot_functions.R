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

plot_OFtrace = function(sim,optimizerList,modelType,ylim=c(1e-5,1),log='y'){

  maxCalls = 0
  for (optimizer in optimizerList){
    for (r in 1:optimizerNmulti){
      # print(sim$latent[[1]]$Rep1$Target1$callsMultiTrace[[r]])
      maxCalls = max(maxCalls,sim[[modelType]][[optimizer]]$Rep1$Target1$callsTraceMulti[[r]])
    }
  }
  plot(x=NULL,xlim=c(0,maxCalls),ylim=ylim,log=log)
  #plot(x=NULL,xlim=c(0,maxCalls),ylim=c(0,2),main=optimizerList[1],xaxs='i',yaxs='i')
  for (o in 1:length(optimizerList)){
    optimizer = optimizerList[o]
    for (r in 1:optimizerNmulti){
      x = sim[[modelType]][[optimizer]]$Rep1$Target1$callsTraceMulti[[r]]
      y = -sim[[modelType]][[optimizer]]$Rep1$Target1$fTraceMulti[[r]]
      lines(x,y,col=colListDefault[o])
      points(max(x),min(y),col=colListDefault[o])
    }
  }
  legend('topright',optimizerList,col=colListDefault[1:length(optimizerList)],lty=1)

}



# -----------
#
#
# plot_relMulti = function(sim,scen,thresh,xlim_calls=1e9,OF_rmse=T,colList=colListDefault,pchList=pchListDefault){
#
#   Nms = length(sim[[scen]][[1]]$Rep1$Target1$fMulti)
#   Np = length(sim[[scen]][[1]]$Rep1$Target1$par)
#   Natt = length(sim[[scen]][[1]]$Rep1$Target1$attSim)
#
#   N = 1:Nms
#
#   pMulti = aveCallsMulti = list()
#   for (optimizer in optimizerList){
#     f = sim[[scen]][[optimizer]]$Rep1$Target1$fMulti
#     if (OF_rmse){f = f / sqrt(Natt)}
#     p1 = length(which(f<thresh))/length(f)
#     pMulti[[optimizer]] = 1-(1-p1)^N
#     calls = sim[[scen]][[optimizer]]$Rep1$Target1$callsMulti
#     aveCalls = mean(calls)
#     aveCallsMulti[[optimizer]] = aveCalls
#   }
#
#   plot(x=NULL,y=NULL,xlim=c(100,xlim_calls),ylim=c(0,1),log='x',
#        xlab='Function calls',ylab=paste0('Reliability (OF<',thresh,')'),
#        main='Reliability')
#   for (o in 1:length(optimizerList)){
#     optimizer = optimizerList[o]
#     lines(N*aveCallsMulti[[optimizer]],pMulti[[optimizer]],col=colListDefault[o])
#     points(N*aveCallsMulti[[optimizer]],pMulti[[optimizer]],col=colListDefault[o],pch=pchList[o],cex=0.5)
#   }
#   legend('bottomright',legend=optimizerList,col=colList,lty=1,pch=pchList,cex=0.8)
#
# }
#
# # -----------
#
#
# plot_relMulti_1 = function(sim,scen,thresh_list,pThresh=0.95,OF_rmse=T,colList=colListDefault,pchList=pchListDefault){
#
#   Nms = length(sim[[scen]][[1]]$Rep1$Target1$fMulti)
#   Np = length(sim[[scen]][[1]]$Rep1$Target1$par)
#   Natt = length(sim[[scen]][[1]]$Rep1$Target1$attSim)
#
#   N = 1:Nms
#
#   maxY = 1
#   minCalls = list()
#   for (optimizer in optimizerList){
#     f = sim[[scen]][[optimizer]]$Rep1$Target1$fMulti
#     if (OF_rmse){f = f / sqrt(Natt)}
#     calls = sim[[scen]][[optimizer]]$Rep1$Target1$callsMulti
#     aveCalls = mean(calls)
#     minCalls[[optimizer]] = c()
#     for (t in 1:length(thresh_list)){
#       thresh = thresh_list[t]
#       p1 = length(which(f<thresh))/length(f)
#       pMulti = 1-(1-p1)^N
#       keep = which(pMulti>pThresh)
#       if (length(keep)>0){
#         minMS = min(keep)
#       } else {
#         minMS = NA
#       }
#       minCalls[[optimizer]][t] = minMS*aveCalls
#       maxY = max(maxY,minCalls[[optimizer]][t],na.rm=T)
#       #      N = log(1-pThresh)/log(1-p1)
#
#     }
#   }
#
#   plot(x=NULL,y=NULL,xlim=c(min(thresh_list),max(thresh_list)),ylim=c(1,maxY),log='y',
#        xlab='Threshold',ylab='Calls',
#        main='Reliability')
#   for (o in 1:length(optimizerList)){
#     optimizer = optimizerList[o]
#     lines(thresh_list,minCalls[[optimizer]],col=colListDefault[o])
#   }
#   # plot(x=NULL,y=NULL,ylim=c(min(thresh_list),max(thresh_list)),xlim=c(1,maxY),log='x',
#   #      ylab='Threshold',xlab='Calls',
#   #      main='Reliability')
#   # for (o in 1:length(optimizerList)){
#   #   optimizer = optimizerList[o]
#   #   lines(minCalls[[optimizer]],thresh_list,col=colListDefault[o])
#   # }
#   legend('bottomright',legend=optimizerList,col=colList,lty=1,pch=pchList,cex=0.8)
#
# }

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
