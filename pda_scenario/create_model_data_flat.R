library(dplyr)
library(sf)
library(eia)
library(rapportools)
library(tidycensus)
library(DescTools)
library(units)
library(openxlsx2)
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
pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")
carb_ht_model<-wb_load("./excel_files/carb_ht_model.xlsx")
#
# get the blkgrp projections
#
blkgp_projections<-wb_read(pda_scenario,sheet='blkgrp_base_prj')
error_plot(blkgp_projections$hhs23,blkgp_projections$hhs50,xl='hh23',yl='hh50')
error_plot(blkgp_projections$jobs22,blkgp_projections$jobs50,xl='jobs22',yl='jobs50')
#
# get the base fitting inputs
#
base_inputs<-wb_read(carb_ht_model,sheet='fitting_data')
names(base_inputs)[1]<-'geoid'
base_inputs<-subset(base_inputs,select = c('geoid',names(base_inputs)[7:length(names(base_inputs))]))
base_inputs<-merge(blkgp_projections,base_inputs)
names(base_inputs)
length(base_inputs$geoid)
error_plot(base_inputs$hhs23,base_inputs$hhs50,xl='hh23',yl='hh50')
error_plot(base_inputs$jobs22,base_inputs$jobs50,xl='jobs22',yl='jobs50')
#
# now start the overwriting variables
#  first the housing varibles
#
base_inputs$households<-base_inputs$hhs50
base_inputs$gross_hh_density<-base_inputs$hhs50/base_inputs$lacres
base_inputs$housing_units<-base_inputs$housing_units*base_inputs$hhs50/base_inputs$hhs23
base_inputs$jobs<-base_inputs$jobs50
error_plot(base_inputs$hhs23,base_inputs$households,xl='hh23',yl='households')
#
# now calc the new AllTransit stats 
#  this takes forever, so only do it if you have to for the first time.
#
blkgrps<-read_sf(dsn = "./tiger/mtc/", 
                   layer = "blkgrps")
calc_all_transit_vars=FALSE
if(calc_all_transit_vars){
  tas<-read_sf(dsn = "./tiger/", 
                   layer = "ca_30_min_transit_sheds")
  
  tas<-subset(tas,tas$geoid %in% blkgrps$geoid )
  names(tas)[1]<-'tas_geoid'
  write_sf(tas, "./tiger/mtc/tas.shp")
  names(blkgrps)
  length(tas$tas_geoid)
  source("./utilities/overlaps_and_splits.R")
  tas_base_jobs <- sum_var_using_fractions(pda_scenario,'tas','tas_geoid','blkgrps','geoid',
                                      base_inputs,'jobs50')
  pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")
  head(tas_base_jobs) 
  length(tas_base_jobs$tas_geoid)
  add_xls_tab(pda_scenario,'tas_base_jobs',tas_base_jobs)
  wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)
}
tas_base_jobs<-wb_read(pda_scenario,sheet='tas_base_jobs')
base_inputs$tas_base_jobs<-base_inputs$job_density_tas<-0.0
i<-1
for(i in 1:length(tas_base_jobs$tas_geoid)){
  t<-tas_base_jobs[i,]
  base_inputs[base_inputs$geoid == t$tas_geoid, 'tas_base_jobs'] <- t$jobs50
}
base_inputs$job_density_tas<- base_inputs$tas_base_jobs/as.numeric(base_inputs$tas_acres)
base_inputs[as.numeric(base_inputs$tas_acres) == 0, 'job_density_tas'] <- 0
error_plot(base_inputs$hhs23,base_inputs$households,xl='hh23',yl='households')

names(base_inputs)
error_plot(base_inputs$hhs23,base_inputs$households,xl='hh23',yl='households')
#
# load the data needed for the gravity measures and 
#   go from fraction of job type (from the clustering) to number of jobs for gravity calcs.
#
do_gravity=FALSE
if(do_gravity){
  gravity_measures<-c('households',
    'frac_rent_hu','frac_sfd','housing_units',
    "jobs","CNS01","CNS02","CNS03","CNS04","CNS05",                                                                 
    "CNS06","CNS07","CNS08","CNS09","CNS10",                                                                 
    "CNS11","CNS12","CNS13","CNS14","CNS15",                                                                 
    "CNS16","CNS17","CNS18","CNS19","CNS20")
  for_gravity_calcs<-subset(base_inputs,
                            select=c('geoid',gravity_measures))
  for(gv in gravity_measures){
    for_gravity_calcs<-subset(for_gravity_calcs,!is.na(as.numeric(for_gravity_calcs[[gv]])))
    for_gravity_calcs[[gv]]<-as.numeric(for_gravity_calcs[[gv]])
    if(gv %like% 'CNS%'){
      for_gravity_calcs[[gv]]<-for_gravity_calcs[[gv]]*for_gravity_calcs$jobs
    }
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
  head(gravity_10)
  #
  # assign center point to data
  #
  for_gravity_calcs<-merge(for_gravity_calcs,subset(gravity_10,select=c('geoid','geometry')))
  #
  # define vars to calculate gravity for 
  #
  gravity_vars<-c('households','renter_occupied_hu','hu_1_detached','housing_units',
                  "C000","CNS01","CNS02","CNS03","CNS04",
                  "CNS05","CNS06","CNS07","CNS08","CNS09","CNS10",
                  "CNS11","CNS12","CNS13","CNS14","CNS15","CNS16",
                  "CNS17","CNS18","CNS19","CNS20")
  #
  # do the gravity for 10 miles
  #
  for(gv in gravity_vars){
    gravity_10[[gv]]<-0
  }
  gravity_10<-subset(gravity_10,select=c('geoid',gravity_vars))
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
  head(gravity_10)
  #
  # save xlsx sheet
  #
  pda_scenario$remove_worksheet("gravity_10")
  pda_scenario$add_worksheet("gravity_10")
  pda_scenario$add_data("gravity_10",as.data.frame(st_drop_geometry(gravity_10)),colNames = TRUE, rowNames = TRUE,)
  
  wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)
}
gravity_10<-wb_read(pda_scenario,sheet='gravity_10')
#
# first save only the measures needed as inputs
#
independent_variables<-wb_read(carb_ht_model,sheet='variable_definitions')

independent_variables<-subset(independent_variables,independent_variables$var_type=='independent')
names(base_inputs)
head(base_inputs)
error_plot(base_inputs$hhs23,base_inputs$households,xl='hh23',yl='households')
#
# now overwrite the gravity measures
#
gravity_variables<-subset(independent_variables,independent_variables$variable %like% '%gravity10%')
grv_names<-names(gravity_10)
ind_names<-independent_variables$variable
i<-1
final_gravity<-as.data.frame(gravity_10$geoid)
names(final_gravity)<-'geoid'
for(i in 1:length(gravity_variables$variable)){
  final_gravity[[gravity_variables$variable[i]]]<-as.numeric(eval(parse(text=(gravity_variables$formula[i]))))
}
head(final_gravity)
head(base_inputs)
gravity_needs<-c('hh_gravity10','rent_gravity10','sfd_gravity10','emp_gravity10')
base_inputs[gravity_needs]
for(gn in gravity_needs){
  base_inputs[gn]<-NULL
}
base_inputs<-merge(base_inputs,subset(final_gravity,select=c('geoid',gravity_needs)))
error_plot(base_inputs$hhs23,base_inputs$households,xl='hh23',yl='households')
names(base_inputs)
head(base_inputs)
add_xls_tab(pda_scenario,'base_inputs',base_inputs)
wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)
base_inputs_geo<-merge(base_inputs,subset(blkgrps,select=c('geoid','geometry')))
st_write(base_inputs_geo, "./tiger/mtc/base_inputs_geo.geojson",append=FALSE, driver = "GeoJSON")

