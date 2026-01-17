library(openxlsx2)
library(dplyr)
library(sf)
library(DescTools)
library(rapportools)
rm(list=ls())
#
# load in some utility functions
#
source("./utilities/ht_calculations.R")
source("./utilities/get_data_functions.R")
source("./utilities/ploting_scripts.R")
source("./utilities/overlaps_and_splits.R")
#
# Get the model inputs from the xlsx file
#
carb_ht_model<-wb_load("./excel_files/carb_ht_model.xlsx")
pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")

pda_model_inputs <-wb_read(pda_scenario,sheet='model_inputs')
head(pda_model_inputs)
error_plot(pda_model_inputs$hhs23,pda_model_inputs$households,xl='hh23',yl='households')
#
# now load up these data in the right format to run the model
#
# make sure the fitting data is numeric
#
for(i in 2:length(names(pda_model_inputs))){
  suppressWarnings(pda_model_inputs[[names(pda_model_inputs)[i]]]<-as.numeric(pda_model_inputs[[names(pda_model_inputs)[i]]]))
}
error_plot(pda_model_inputs$hhs23,pda_model_inputs$households,xl='hh23',yl='households')
#
# get dependent, independent and household variable names
#
vars <- as.data.frame(wb_read(carb_ht_model,sheet='variable_definitions'))
dep_vars<-as.data.frame(subset(vars,vars$var_type == 'dependent',select=c('variable','transformation','fit_type')))
ind_vars <- as.data.frame(subset(vars,vars$var_type == 'independent',select=c('variable','transformation','fit_type')))
hh_vars<- (subset(vars,vars$fit_type == 'hh',select=c('variable')))
#
# now set up and pull just the independent variables including the intercept (value=1)
#
names(pda_model_inputs)
hts_ind<-pda_model_inputs[c('geoid',ind_vars$variable)]
hts_ind["(Intercept)"]<-1.0
head(hts_ind)
#
# load up the model data frame to 
#
modeled<-as.data.frame(hts_ind[c("geoid",hh_vars[,1],'frac_rent_hu')])
#
# eliminate block groups with no households, and with na for housing inputs
#
modeled<-subset(modeled,complete.cases(modeled))
hts_ind<-subset(hts_ind,hts_ind$geoid %in% modeled$geoid)
#
# now run the model
#
modeled<-predict_model(dep_vars,hts_ind,modeled,1,5)
head(modeled)
#
# get prices and cost things out and caclulate h+t
#
ht_model<-calculate_costs_ht(modeled)
head(ht_model)
#
# save scenario in scenarios .xlsx file
#
this_sheet<-'ht_model'
if(this_sheet %in% wb_get_sheet_names(pda_scenario)){pda_scenario$remove_worksheet(this_sheet)}
pda_scenario$add_worksheet(this_sheet)
pda_scenario$add_data(this_sheet,as.data.frame(ht_model),colNames = TRUE)

wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)


blkgrps<-read_sf(dsn = "./tiger/mtc/", 
                 layer = "blkgrps")

ht_model<-merge(ht_model,subset(blkgrps, select = c('geoid','geometry')))
#
# save shape file 
#
write_sf(ht_model, "./tiger/mtc/ht_model.shp")





