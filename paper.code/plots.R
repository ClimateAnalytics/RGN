source('C:/Users/a1065639/Work/RGN/paper.code/setup.R')

modelType = 'latent'
fname1 = paste0(out.dir,'expt_1.latent.annual.10yrs.10multiStart.RData')
load(fname1)

plot_OF_calls(sim,ymax_OF = 1,ymax_calls = 1e4)

plot_OFtrace(sim=sim,optimizerList=optimizerList,modelType=modelType,ylim=c(1e-4,1))

