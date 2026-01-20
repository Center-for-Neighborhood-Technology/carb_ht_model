#
# this script is helps you run all the scripts to run MTC PDA Growth scenario
#   first allocate the growth
#
source("./pda_scenario/mtc_pda_allocation.R")
#
#  now create the data to run the models
#
source("./pda_scenario/create_model_data.r")
#
#  run the models and calculate the affordability indexes
#
source("./pda_scenario/calc_ht.R")
#
# now if you want to run the "flat" allocations you need to
#
source("./pda_scenario/mtc_flat_allocation.R")
#
#  now create the data to run the models
#
source("./pda_scenario/create_model_data_flat.r")
#
#  run the models and calculate the affordability indexes
#
source("./pda_scenario/calc_flat_ht.R")
#
# finally there is a first take on analyzing these data
#
source("./pda_scenario/analysis.R")
