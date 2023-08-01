rm(list=ls())

paper.code.dir = 'C:/Users/a1065639/Work/RGN/paper.code/'
source(paste0(paper.code.dir,'setup_paths_libs.R'))
source(paste0(paper.code.dir,'run_settings.R'))

modelType = 'latent'
fname1 = paste0(out.dir,'expt_1.latent.annual.10yrs.10multiStart.RData')
load(fname1)

plot_OF_calls(sim,ymax_OF = 1,ymax_calls = 1e4)

plot_OFtrace(sim=sim,optimizerList=optimizerList,modelType=modelType,ylim=c(1e-4,1))

