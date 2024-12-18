# call/install packages ---------------------------------------------------
## package list
packFontHandler <- function(){
  packages <- c(
    'ggplot2',
    'flextable',
    'officer',
    'extrafont',
    'dplyr',
    'metafor',
    'esc',
    'dmetar',
    'tidyverse',
    'boot',
    'rms',
    'plotly',
    'scatterplot3d',
    'qpcR',
    'MuMIn',
    'parallel',
    'openair',
    'EnvStats',
    'equatags',
    'js',
    'ggfun',
    'processx',
    'reticulate'
  )
  
  ## install or load packages
  if (!require(install.load)) {
    install.packages('install.load')
  }
  
  install.load::install_load(packages)
  
  devtools::install_github('ropensci/plotly', upgrade = 'never')
  
  ## dmetar must be installed from github
  if (!require('remotes')) {
    install.packages('remotes')
  }
  remotes::install_github('MathiasHarrer/dmetar')
  
  eval(metafor:::.MuMIn,envir = globalenv())
  
# reconcile fonts ---------------------------------------------------------
  remotes::install_version('Rttf2pt1', version = '1.3.8')
  font_import(prompt = FALSE, pattern = 'calibri')
  fonts()
  loadfonts(device = 'win')
  windowsFonts()
}