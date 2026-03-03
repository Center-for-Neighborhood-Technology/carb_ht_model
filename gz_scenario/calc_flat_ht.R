#
# clear memory
#
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
gz_scenario<-wb_load("./excel_files/gz_scenario.xlsx")
#
# get gis location
#
gz_index<-wb_read(gz_scenario,sheet='index')
gis_folder<-gz_index$value[1]
#
# get model inputs
#
gz_model_flat_inputs <-wb_read(gz_scenario,sheet='model_flat_inputs')
#
# now load up these data in the right format to run the model
#
# make sure the fitting data is numeric
#
for(i in 2:length(names(gz_model_flat_inputs))){
  suppressWarnings(gz_model_flat_inputs[[names(gz_model_flat_inputs)[i]]]<-as.numeric(gz_model_flat_inputs[[names(gz_model_flat_inputs)[i]]]))
}
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
hts_ind<-gz_model_flat_inputs[c('geoid',ind_vars$variable)]
hts_ind["(Intercept)"]<-1.0
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
#
# get prices and cost things out and caclulate h+t
#
ht_model<-calculate_costs_ht(modeled)
#
# save scenario in scenarios .xlsx file
#
gz_scenario<-add_xls_tab(gz_scenario,'ht_flat_model',ht_model)
wb_save(gz_scenario,file="./excel_files/gz_scenario.xlsx",overwrite = TRUE)
#
# save shape file 
#
blkgrps<-read_sf(dsn = gis_folder, 
                 layer = "blkgrps")

ht_model<-merge(ht_model,subset(blkgrps, select = c('geoid','geometry')))
write_sf(ht_model, paste(gis_folder,"ht_flat_model.gpkg",sep=''),delete_dsn=TRUE)





