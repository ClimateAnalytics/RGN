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
# optimizerList = c('SCE','SCE10')
# optimizerLabelList = c('SCE','SCE10')
optimizerList = c('optim.LBFGSB','NMKB','CMAES','GA',
                  'SCE','SCE3','SCE10','RGN')
optimizerLabelList = c('LBFGS','NM','CMAES','GA',
                       'SCE','SCE3','SCE10','RGN')
optimizerListAll = optimizerList
names(optimizerLabelList) = optimizerList

# WG seed
seedID = 1




