rm(list=ls())

foreSIGHT.dir = 'C:/Users/a1065639/Work/foreSIGHT/'
rgn.dir = 'C:/Users/a1065639/Work/RGN/'
out.dir = 'C:/Users/a1065639/Work/RGN/paper/5_laptop_paper.runs/'
paper.code.dir = paste0(rgn.dir,'paper.code/')

devtools::load_all(foreSIGHT.dir)
devtools::load_all(rgn.dir)

source(paste0(paper.code.dir,'run_functions.R'))
source(paste0(paper.code.dir,'plot_functions.R'))

data("barossaDat")
clim_ref = barossa_obs
clim_ref$P = clim_ref$P[,'X23300']

# optimizerList = c('optim.LBFGSB','NMKB','CMAES','GA','SCE','RGN')
# optimizerLabelList = c('LBFGS','NM','CMAES','GA','SCE','RGN')
# optimizerList = c('optim.LBFGSB','NMKB','RGN')
# optimizerLabelList = c('LBFGS','NM','RGN')
# optimizerList = c('NMKB')
# optimizerLabelList = c('NM')
#optimizerList = c('RGN')
#optimizerLabelList = c('RGN')
optimizerList = c('SCE','SCE10')
optimizerLabelList = c('SCE','SCE10')
optimizerListAll = optimizerList
names(optimizerLabelList) = optimizerList

# WG seed
seedID = 1

# -----------
# determine model parameter names
modelParNames = setupModelParNames()

# pars to fit
freePars = NULL
par = NULL
