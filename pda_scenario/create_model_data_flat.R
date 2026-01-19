library(DescTools)
library(openxlsx2)
#
# This script takes the allocated household and jobs and overwrites 
#    the independent variables that are affected by these changes for the Flat scenario
#
# first clear all memory before starting and source the utilities
#
rm(list=ls())
source("./utilities/get_data_functions.R")
source("./utilities/ploting_scripts.R")
source("./utilities/overlaps_and_splits.R")
#
# load in spread sheet needed
#
pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")
carb_ht_model<-wb_load("./excel_files/carb_ht_model.xlsx")
#
# get the path for the gis files needed
#
pda_index<-wb_read(pda_scenario,sheet='index')
gis_folder<-pda_index$value[1]
#
# get the blkgrp flat projections
#
blkgp_projections<-wb_read(pda_scenario,sheet='blkgrp_flat_prj')
#
# get the flat fitting inputs
#
flat_inputs<-wb_read(carb_ht_model,sheet='fitting_data')
names(flat_inputs)[1]<-'geoid'
flat_inputs<-subset(flat_inputs,select = c('geoid',names(flat_inputs)[7:length(names(flat_inputs))]))
flat_inputs<-merge(blkgp_projections,flat_inputs)
#
# now start the overwriting variables
#  first the housing varibles
#
flat_inputs$households<-flat_inputs$hhs50
flat_inputs$gross_hh_density<-flat_inputs$hhs50/flat_inputs$lacres
flat_inputs$housing_units<-flat_inputs$housing_units*flat_inputs$hhs50/flat_inputs$hhs23
flat_inputs$jobs<-flat_inputs$jobs50
#
# now calc the new AllTransit stats 
#  this takes forever, so only do it if you have to for the first time.
#
blkgrps<-read_sf(dsn = gis_folder, 
                   layer = "blkgrps")
calc_all_transit_vars=FALSE
if(calc_all_transit_vars){
  #
  # get the transit sheds for mtc
  #
  tas<-read_sf(dsn = "./tiger/", 
                   layer = "ca_30_min_transit_sheds")
  tas<-subset(tas,tas$geoid %in% blkgrps$geoid )
  names(tas)[1]<-'tas_geoid'
  write_sf(tas, paste(gis_folder,"tas.shp",sep=''))
  #
  # sum up the jobs from the block groups to the TAS layer, 
  #   but don't save the fraction table becuse it is too big
  #
  tas_flat_jobs <- sum_var_using_fractions(pda_scenario,'tas','tas_geoid','blkgrps','geoid',
                                      flat_inputs,'jobs50',0)
  pda_scenario<-add_xls_tab(pda_scenario,'tas_flat_jobs',tas_flat_jobs)
}
tas_flat_jobs<-wb_read(pda_scenario,sheet='tas_flat_jobs')
flat_inputs$tas_flat_jobs<-flat_inputs$job_density_tas<-0.0
i<-1
for(i in 1:length(tas_flat_jobs$tas_geoid)){
  t<-tas_flat_jobs[i,]
  flat_inputs[flat_inputs$geoid == t$tas_geoid, 'tas_flat_jobs'] <- t$jobs50
}
flat_inputs$job_density_tas<- flat_inputs$tas_flat_jobs/as.numeric(flat_inputs$tas_acres)
flat_inputs[as.numeric(flat_inputs$tas_acres) == 0, 'job_density_tas'] <- 0
#
# load the data needed for the gravity measures and 
#   go from fraction of job type (from the clustering) to number of jobs for gravity calcs.
#
do_gravity=FALSE
if(do_gravity){
  gravity_measures<-c('households','frac_rent_hu','frac_sfd','housing_units',"jobs")
  for_gravity_calcs<-subset(flat_inputs,
                            select=c('geoid',gravity_measures))
  for(gv in gravity_measures){
    for_gravity_calcs<-subset(for_gravity_calcs,!is.na(as.numeric(for_gravity_calcs[[gv]])))
    for_gravity_calcs[[gv]]<-as.numeric(for_gravity_calcs[[gv]])
  }
  
  names(for_gravity_calcs)[6]<-"C000"
  for_gravity_calcs$renter_occupied_hu<-(for_gravity_calcs$housing_units)*(for_gravity_calcs$frac_rent_hu)
  for_gravity_calcs$hu_1_detached<-(for_gravity_calcs$housing_units)*(for_gravity_calcs$frac_sfd)
  #
  # create gravity data frame and order by geoid
  #
  gravity_10_flat<-st_point_on_surface(blkgrps)
  order_indices <- order(gravity_10_flat$geoid)
  gravity_10_flat<-gravity_10_flat[order_indices, ]
  #
  # assign center point to data
  #
  for_gravity_calcs<-merge(for_gravity_calcs,subset(gravity_10_flat,select=c('geoid','geometry')))
  #
  # define vars to calculate gravity for 
  #
  gravity_vars<-c('households','renter_occupied_hu','hu_1_detached','housing_units',"C000")
  #
  # do the gravity for 10 miles
  #
  for(gv in gravity_vars){
    gravity_10_flat[[gv]]<-0
  }
  gravity_10_flat<-subset(gravity_10_flat,select=c('geoid',gravity_vars))
  tst<-gravity_10_flat[1,]
  i<-1
  for(i in 1:length(gravity_10_flat$geoid)){
    tst<-gravity_10_flat[i,]
    stfid<-tst$geoid
    #
    #  fill the distance in miles
    #
    for_gravity_calcs$d<-as.double(st_distance( st_transform(tst$geometry ,3857),
                                              st_transform(for_gravity_calcs$geometry ,3857)))/1609.34
    sbset10<-subset(for_gravity_calcs,for_gravity_calcs$d<10)
    sbset10$d2<-ifelse(sbset10$d<1,1.0,sbset10$d*sbset10$d)
    for(gv in gravity_vars){
      gravity_10_flat[i,gv] <- sum(sbset10[[gv]]/sbset10$d2)
    }
  }
  #
  # save xlsx sheet
  #
  pda_scenario<-add_xls_tab(pda_scenario,'gravity_10_flat',as.data.frame(st_drop_geometry(gravity_10_flat)))
}
gravity_10<-wb_read(pda_scenario,sheet='gravity_10_flat')
#
# first save only the measures needed as inputs
#
independent_variables<-wb_read(carb_ht_model,sheet='variable_definitions')

independent_variables<-subset(independent_variables,independent_variables$var_type=='independent')
#
# now overwrite the gravity measures
#
gravity_variables<-subset(independent_variables,independent_variables$variable %like% '%_gravity10%')
grv_names<-names(gravity_10)
ind_names<-independent_variables$variable
i<-1
final_gravity<-as.data.frame(gravity_10$geoid)
names(final_gravity)<-'geoid'
for(i in 1:length(gravity_variables$variable)){
  final_gravity[[gravity_variables$variable[i]]]<-as.numeric(eval(parse(text=(gravity_variables$formula[i]))))
}
gravity_needs<-c('hh_gravity10','rent_gravity10','sfd_gravity10','emp_gravity10')
flat_inputs[gravity_needs]
for(gn in gravity_needs){
  flat_inputs[gn]<-NULL
}
flat_inputs<-merge(flat_inputs,subset(final_gravity,select=c('geoid',gravity_needs)))
pda_scenario<-add_xls_tab(pda_scenario,'flat_inputs',flat_inputs)
wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)
flat_inputs_geo<-merge(flat_inputs,subset(blkgrps,select=c('geoid','geometry')))
write_sf(flat_inputs_geo, paste(gis_folder,"model_flat_inputs.shp",sep=''))

