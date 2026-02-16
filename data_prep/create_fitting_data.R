#
# clear all memory before starting
#
rm(list=ls())
#
# read in the whole xlsx file
#
data_xls<-wb_load("./excel_files/data_prep.xlsx")
acs_2023<- wb_read(data_xls,sheet='blkgrp_acs_2023')
acs_2023<-as.data.frame(subset(acs_2023,acs_2023$GEOID %like% '06%'))

fitting_data<-as.data.frame(acs_2023$GEOID)
names(fitting_data)[names(fitting_data) == "acs_2023$GEOID"]<-"GEOID"

modeled_vmt<- wb_read(data_xls,sheet='vmt')
modeled_vmt<-subset(modeled_vmt,select=c('GEOID','normalized_vmt_per_hh'))
names(modeled_vmt)<-c('GEOID','vmt_per_hh')
modeled_vmt<-as.data.frame(left_join(fitting_data,modeled_vmt,by='GEOID'))

block_size<- wb_read(data_xls,sheet='block_size')
block_size<-as.data.frame(left_join(fitting_data,block_size,by='GEOID'))

transit_vars <- wb_read(data_xls,sheet='transit_vars')
transit_vars<-as.data.frame(left_join(fitting_data,transit_vars,by='GEOID'))

gravity_10<- wb_read(data_xls,sheet='gravity_10')
names(gravity_10)[is.na(names(gravity_10))] <-'rid'
gravity_10<-as.data.frame(left_join(fitting_data,gravity_10,by='GEOID'))

ht_models<-wb_load("./excel_files/carb_ht_model.xlsx")
modeling_vars<- wb_read(ht_models,sheet='variable_definitions')
#
# fill in the data
#  suppress warnings when variables are #NA
#
for(i in 1:length(modeling_vars$variable)){
  suppressWarnings(fitting_data[[modeling_vars$variable[i]]]<-
                     as.numeric(eval(parse(text=(modeling_vars$formula[i])))))
}
#
# make all variables numeric
#
for(i in 2:length(names(fitting_data))){
  nm<-names(fitting_data)[i]
  fitting_data[[nm]]<-as.numeric(fitting_data[[nm]])
}
#
# save the data to be fit
#
ht_models$remove_worksheet("fitting_data")
ht_models$add_worksheet("fitting_data")
ht_models$add_data("fitting_data",as.data.frame(fitting_data),colNames = TRUE, rowNames = FALSE,)

wb_save(ht_models,file="./excel_files/carb_ht_model.xlsx",overwrite = TRUE)

