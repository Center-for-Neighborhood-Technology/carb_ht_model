#
# install and load all the libraries that will be needed
#
source("./utilities/manage_packages.R")
#
# this script is helps you run all the scripts to train the models.
#
#  first get all the block groups and cut out the water, 
#   only do this once it takes a long time!
#
source(".data_prep/get_census_geos.R")
#
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
