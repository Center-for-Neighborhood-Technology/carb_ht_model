#
# this allocates the new hhs and jobs to the various types
#
# clear all memory before starting and source the utilities
#
rm(list=ls())
source("./utilities/get_data_functions.R")
source("./utilities/ploting_scripts.R")
source("./utilities/overlaps_and_splits.R")
#
# load in spread sheet needed
#
gz_scenario<-wb_load("./excel_files/gz_scenario.xlsx")
carb_ht_model<-wb_load("./excel_files/carb_ht_model.xlsx")
#
# get gis folder
#
index<-wb_read(gz_scenario,sheet='index')
gis_folder<-index$value[1]
#
# get the blkgrp projections
#
blkgp_projections<-wb_read(gz_scenario,sheet='blkgrp_projections')
#
# get the land area from the shapefile
#
blkgrps<-read_sf(dsn = gis_folder, 
                 layer = "blkgrps")
blkgrps<-subset(blkgrps,select=c('geoid','lacres'))
names(blkgrps)[1]<-'geoid'
blkgp_projections<-merge(blkgp_projections,blkgrps)
#
# get the base fitting inputs
#
model_inputs<-wb_read(carb_ht_model,sheet='fitting_data')
names(model_inputs)[1]<-'geoid'
model_inputs<-subset(model_inputs,model_inputs$geoid %in% blkgp_projections$geoid)
model_inputs<-subset(model_inputs,
                     select = c('geoid',names(model_inputs)[7:length(names(model_inputs))]))
model_inputs<-merge(blkgp_projections,model_inputs)
#
# now start the overwriting variables
#  first the housing variables
#
model_inputs$households<-model_inputs$hhs50
model_inputs$gross_hh_density<-model_inputs$hhs50/model_inputs$lacre
model_inputs$housing_units<-model_inputs$housing_units*model_inputs$hhs50/model_inputs$hhs23
model_inputs$jobs<-model_inputs$jobs50
#
# now calc the new AllTransit stats 
#  this takes forever, so only do it if you have to for the first time.
#
calc_all_transit_vars=FALSE
if(calc_all_transit_vars){  
  #
  # get the transit sheds for SACOG
  #
  tas<-st_read("./gis/ca_30_min_transit_sheds.gpkg")
  tas<-subset(tas,tas$geoid %in% blkgrps$geoid )
  #
  # save this shape file so we can do a proportional sum against it.
  #
  names(tas)[1]<-'tas_geoid'
  tas_shp<-paste(gis_folder,'tas.shp',sep='')
  write_sf(tas, tas_shp)
  #
  # sum up the jobs from the block groups to the TAS layer, 
  #   but don't save the fraction table because it is too big
  #
  tas_jobs <- sum_var_using_fractions(gz_scenario,'tas','tas_geoid','blkgrps','geoid',
                                      model_inputs,'jobs50',0)
  gz_scenario<-add_xls_tab(gz_scenario,'tas_jobs',tas_jobs)
}
tas_jobs<-wb_read(gz_scenario,sheet='tas_jobs')
model_inputs$tas_jobs<-model_inputs$job_density_tas<-0.0
i<-1
for(i in 1:length(tas_jobs$tas_geoid)){
  t<-tas_jobs[i,]
  model_inputs[model_inputs$geoid == t$tas_geoid, 'tas_jobs'] <- t$jobs50
}
model_inputs$job_density_tas<- model_inputs$tas_jobs/as.numeric(model_inputs$tas_acres)
model_inputs[as.numeric(model_inputs$tas_acres) == 0, 'job_density_tas'] <- 0
#
# load the data needed for the gravity measures and 
#   go from fraction of job type (from the clustering) to number of jobs for gravity calcs.
#
do_gravity=FALSE
if(do_gravity){
  gravity_measures<-c('households','frac_rent_hu','frac_sfd','housing_units','jobs')
  for_gravity_calcs<-subset(model_inputs,
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
  gravity_10<-st_point_on_surface(blkgrps)
  order_indices <- order(gravity_10$geoid)
  gravity_10<-gravity_10[order_indices, ]
  #
  # assign center point to data
  #
  for_gravity_calcs<-merge(for_gravity_calcs,subset(gravity_10,select=c('geoid','geometry')))
  #
  # define vars to calculate gravity for 
  #
  gravity_vars<-c('households','renter_occupied_hu','hu_1_detached','housing_units','C000')
  #
  # do the gravity for 10 miles
  #
  for(gv in gravity_vars){
    gravity_10[[gv]]<-0
  }
  gravity_10<-subset(gravity_10,select=c('geoid',gravity_vars))
  names(gravity_10)
  tst<-gravity_10[1,]
  i<-1
  for(i in 1:length(gravity_10$geoid)){
    tst<-gravity_10[i,]
    stfid<-tst$geoid
    #
    #  fill the distance in miles
    #
    for_gravity_calcs$d<-as.double(st_distance( st_transform(tst$geometry ,3857),
                                              st_transform(for_gravity_calcs$geometry ,3857)))/1609.34
    sbset10<-subset(for_gravity_calcs,for_gravity_calcs$d<10)
    sbset10$d2<-ifelse(sbset10$d<1,1.0,sbset10$d*sbset10$d)
    for(gv in gravity_vars){
      gravity_10[i,gv] <- sum(sbset10[[gv]]/sbset10$d2)
    }
  }
  #
  # save xlsx sheet
  #
  gz_scenario<-add_xls_tab(gz_scenario,'gravity_10',gravity_10)
}
gravity_10<-wb_read(gz_scenario,sheet='gravity_10')
#
# first save only the measures needed as inputs
#
independent_variables<-wb_read(carb_ht_model,sheet='variable_definitions')

independent_variables<-subset(independent_variables,independent_variables$var_type=='independent')
#
# now overwrite the gravity measures
#
gravity_variables<-subset(independent_variables,
                          independent_variables$variable %like% '%_gravity10')
grv_names<-names(gravity_10)
ind_names<-independent_variables$variable
i<-1
final_gravity<-as.data.frame(gravity_10$geoid)
names(final_gravity)<-'geoid'
for(i in 1:length(gravity_variables$variable)){
  final_gravity[[gravity_variables$variable[i]]]<-as.numeric(eval(parse(text=(gravity_variables$formula[i]))))
}
gravity_needs<-c('hh_gravity10','rent_gravity10','sfd_gravity10','emp_gravity10')
model_inputs[gravity_needs]
for(gn in gravity_needs){
  model_inputs[gn]<-NULL
}
#
# save spread sheet
#
model_inputs<-merge(model_inputs,subset(final_gravity,select=c('geoid',gravity_needs)))
gz_scenario<-add_xls_tab(gz_scenario,'model_inputs',model_inputs)
wb_save(gz_scenario,file="./excel_files/gz_scenario.xlsx",overwrite = TRUE)
#
# save shape file
#
model_inputs_geo<-merge(model_inputs,subset(blkgrps,select=c('geoid','geometry')))
write_sf(model_inputs_geo,paste(gis_folder,"model_inputs.shp",sep=''))
