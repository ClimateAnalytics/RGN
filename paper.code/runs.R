source('C:/Users/a1065639/Work/RGN/paper.code/setup.R')

#args = commandArgs(trailingOnly=TRUE)

# expt = args[1]
# modelTypeList = args[2]
# optimizerNmulti = as.integer(args[3])

expt = 'expt_1'
modelType = 'latent'
optimizerNmulti = 10

# ------------

if (expt %in% c('expt_1')){
  modelParameterVariation = 'annual'
  attList =  c("P_ann_tot_m","P_ann_P99","P_ann_avgWSD_m","P_ann_nWet_m")
} else if (expt %in% c('expt_2')){
  modelParameterVariation = 'annual'
  attList =  c("P_ann_tot_m","P_ann_P99_m10yrBlock","P_ann_avgWSD_m","P_ann_nWet_m")
} else if (expt %in% c('expt_3')){
  modelParameterVariation = 'harmonic'
  attList =  c("P_DJF_tot_m","P_DJF_P99","P_DJF_avgWSD_m","P_DJF_nWet_m",
               "P_MAM_tot_m","P_MAM_P99","P_MAM_avgWSD_m","P_MAM_nWet_m",
               "P_JJA_tot_m","P_JJA_P99","P_JJA_avgWSD_m","P_JJA_nWet_m",
               "P_SON_tot_m","P_SON_P99","P_SON_avgWSD_m","P_SON_nWet_m")
}

if (expt %in% c('expt_1','expt_3')){
  nYrs = 10
} else if (expt %in% c('expt_2')){
  nYrs = 1000
}

name = paste0(expt,'.',modelType,'.',modelParameterVariation,'.',nYrs,'yrs.',optimizerNmulti,'multiStart')
options = list(attList=attList,nYrs=nYrs,seedID=seedID,
               modelTypeList=modelType,modelParameterVariation=modelParameterVariation,
               optimizerList=optimizerList,optimizerNmulti=optimizerNmulti,
               par=par,freePars=freePars)
sim = doRuns(clim_ref=clim_ref,options=options)
fname = paste0(out.dir,name,'.RData')
save.image(file=fname)
