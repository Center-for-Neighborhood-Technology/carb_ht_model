#
# install and load all the libraries that will be needed
#
source("./utilities/manage_packages.R")
#
# suppress warnings
#
options(warn = -1)
#
# now we can do the fits and save the result there will be a few plots 
#  if you set make_plots to TRUE
#  and messages that you should to confirm things are going well.
#
rm(list=ls())
make_plots<-TRUE
source("./model_training/ols_fits.R")
#
# Now run the model, you will be asked for what household
#
source("./model_training/run_models.R")
#
# restore warnings
#
options(warn = 0)
