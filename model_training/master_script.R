#
# this script is helps you run all the scripts to train the models.
#  first get the ACS data
#
source("./data_prep/get_acs_data.R")
#
#  now make all the gravity indicators
#
source("./data_prep/make_gravity.R")
#
#  normalize the VMT from the CNT model to the CARB county vmt data
#
source("./data_prep/normalize_vmt.R")
#
# now create the data for doing the fit
#
source("./data_prep/create_fitting_data.R")
#
# now we can do the fits and save the result there will be a few plots if you set make_plots to TRUE
#  and messages that you should to confirm things are going well.
#
rm(list=ls())
make_plots<-FALSE
source("./model_training/ols_fits.R")
#
# Run Road Pricing scenario
# set road use cost as price/mile in dollars
#
rm(list=ls())
price_per_mile<-0.1
source("./road_pricing_scenario/road_pricing.R")
#
# Start working on the pda/green zone scenario
#  remove the water from the blkgrps, places, pdas, and SUPERDISTRICTS in MTC area
#  this only has to be run once.
#
source("./pda_scenario/mtc_cut_out_water.R")
#
# now get the overlapping fraction of all of these geometries
#
source("./pda_scenario/mtc_overlap_fractions.R")
#  
#  in order to classify block groups you can run the clustering script
#
source('./utilities/cluster_ca_blkgrps.R')
