foreSIGHT.dir = 'C:/Users/a1065639/Work/foreSIGHT/'
rgn.dir = 'C:/Users/a1065639/Work/RGN/'
out.dir = 'C:/Users/a1065639/Work/RGN/paper/5_laptop_paper.runs/'
paper.code.dir = paste0(rgn.dir,'paper.code/')

devtools::load_all(foreSIGHT.dir)
devtools::load_all(rgn.dir)

source(paste0(paper.code.dir,'run_functions.R'))
source(paste0(paper.code.dir,'plot_functions.R'))

