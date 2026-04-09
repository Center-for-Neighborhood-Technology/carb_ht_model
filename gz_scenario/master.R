#
# this script is helps you run all the scripts to run MTC PDA Growth scenario
#  suppress warnings
#
options(warn = -1)
#
# install and load all the libraries that will be needed
#
source("./utilities/manage_packages.R")
#
# this script is helps you run all the scripts to run MTC PDA Growth scenario
#   first allocate the growth
#
source("./gz_scenario/gz_allocation.R")
#
#  now create the data to run the models
#
source("./gz_scenario/create_model_data.r")
#
#  run the models and calculate the affordability indexes
#
source("./gz_scenario/calc_ht.R")
#
# now if you want to run the "flat" allocations you need to
#
source("./gz_scenario/gz_flat_allocation.R")
#
#  now create the data to run the models
#
source("./gz_scenario/create_flat_model_data.r")
#
#  run the models and calculate the affordability indexes
#
source("./gz_scenario/calc_flat_ht.R")
#
# finally there is a first take on analyzing these data
#
source("./gz_scenario/analysis.R")
#
# turn warning back on
#
options(warn = 0)
