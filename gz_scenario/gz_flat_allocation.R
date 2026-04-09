#
# clear all memory before starting
#
rm(list=ls())
source("./utilities/get_data_functions.R")
source("./utilities/overlaps_and_splits.R")
source("./utilities/ploting_scripts.R")
source("./utilities/cut_water_out_of_layer.R")
#
# load in spread sheet needed
#
data_prep<-wb_load("./excel_files/data_prep.xlsx")
gz_scenario<-wb_load("./excel_files/gz_scenario.xlsx")
#
# get tiger path
#
index<-wb_read(gz_scenario,sheet='index')
gis_folder<-index$value[1]
#
# read in the sacog counties
#
sacog_counties<-wb_read(gz_scenario,sheet='SACOG_counties')

blkgrps_og<-read_sf(dsn = "./gis/blkgrps.gpkg")
blkgrps_og<-subset(blkgrps_og,substring(blkgrps_og$geoid,3,5) %in% sacog_counties$fipco)
blkgrps<-cut_water_out_of_layer(blkgrps_og,'geoid',gis_folder,"blkgrps",1)
#
# read in 2023 block group acs households from census api
#
blkgrp_hhs_2023 <- get_acs_variable('block group','B25009_001','households','geoid',2023,'06',sacog_counties$fipco)
names(blkgrp_hhs_2023)[1]<-'geoid'
#
# read in the jobs for the block groups in the sacog counties
#
lodes_2022<-wb_read(data_prep,sheet='lodes_2022')
lodes_2022<-subset(lodes_2022,substring(lodes_2022$w_bg,1,5) %in% paste('06',sacog_counties$fipco,sep=''))
blkgrp_jobs_22<-subset(lodes_2022, select=c('w_bg','C000'))
names(blkgrp_jobs_22)[1]<-'geoid'
blkgrp_projections<-merge(blkgrp_hhs_2023,blkgrp_jobs_22)
names(blkgrp_projections)<-c('geoid','hhs23','jobs22')
#
# get totals of hhs and jobs in sacag area
#
tothhs<-round(sum(blkgrp_projections$hhs23))
totjobs<-round(sum(blkgrp_projections$jobs22))
#
# now scale up hh and jobs to match the goals
#
# 32 percent of planned regional housing growth, creating an estimated 84,000 new homes 
# 40 percent of planned employment growth, or about 100,000 new jobs 
# 16 percent reduction of VMT per capita in Green Zones, double the regional reduction per capita of 8 percent 
# Reducing the number of homes in high fire risk areas by 10,000 units over 20 years.  
# for the whole region 
#
total_hh_goal<-84000/0.32 + tothhs
total_job_goal<-100000/0.40 + totjobs
#
# now get the scale factors
#
hhr<-total_hh_goal/tothhs
jobr<-total_job_goal/totjobs
#
# now scale up all block groups
#
blkgrp_projections$hhs50<-(blkgrp_projections$hhs23)*hhr
blkgrp_projections$jobs50<-(blkgrp_projections$jobs22)*jobr
print(c('just checking, allocated hhs total = ',sum(blkgrp_projections$hhs50),' and goal=',total_hh_goal))
print(c('just checking, allocated jobs total = ',sum(blkgrp_projections$jobs50),' and goal=',total_job_goal))
#
# save spread sheet
#
gz_scenario<-add_xls_tab(gz_scenario,'blkgrp_flat_projections',blkgrp_projections)
wb_save(gz_scenario,file="./excel_files/gz_scenario.xlsx",overwrite = TRUE)
