library(openxlsx2)
library(sf)
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
# get gis path
#
index<-wb_read(gz_scenario,sheet='index')
gis_folder<-index$value[1]
#
#get the block groups and gzs
#
gzs_og<-read_sf(dsn = gis_folder, 
              layer = "Green_Means_Go_Green_Zones")
gzs<-cut_water_out_of_layer(gzs_og,names(gzs_og)[1],gis_folder,"gzs",1)
#
# read in the sacog counties
#
sacog_counties<-wb_read(gz_scenario,sheet='SACOG_counties')

blkgrps_og<-read_sf(dsn = './tiger/', 
                    layer = "california_blkgrps_2023")

blkgrps_og<-subset(blkgrps_og,blkgrps_og$COUNTYFP %in% sacog_counties$fipco)
blkgrps_og<-subset(blkgrps_og,select=c('GEOID','geometry'))
names(blkgrps_og)[1]<-'geoid'
blkgrps<-cut_water_out_of_layer(blkgrps_og,'geoid',gis_folder,"blkgrps",1)
#
# read in 2023 block group acs households from census api
#
blkgrp_hhs_2023 <- get_acs_variable('block group','B25009_001','households','GEOID',2023,'06',sacog_counties$fipco)
names(blkgrp_hhs_2023)[1]<-'geoid'
#
# read in the jobs for the block groups in the sacog counties
#
lodes_2022<-wb_read(data_prep,sheet='lodes_2022')
lodes_2022<-subset(lodes_2022,substring(lodes_2022$GEOID,1,5) %in% paste('06',sacog_counties$fipco,sep=''))
blkgrp_jobs_22<-subset(lodes_2022, select=c('GEOID','C000'))
names(blkgrp_jobs_22)[1]<-'geoid'
#
# now merge hhs and jobs
#
blkgrp_projections<-merge(blkgrp_hhs_2023,blkgrp_jobs_22)
names(blkgrp_projections)<-c('geoid','hhs23','jobs22')
#
# now allocate the hhs and jobs in each block group that are in any green zone
#
bg_fracs<-get_overlap_fractions(gz_scenario,'blkgrps','gzs','geoid','gz_id')
bg_frac_in_gz<-aggregate(x = bg_fracs$frac_of_blkgrps, by = list(Category = bg_fracs$geoid), FUN = sum)
names(bg_frac_in_gz)<-c('geoid','frac')
#
# now allocate per fraction 
#
blkgrp_projections<-merge(blkgrp_projections,bg_frac_in_gz,by = "geoid", all = TRUE)
blkgrp_projections$frac[is.na(blkgrp_projections$frac)] <- 0
blkgrp_projections$hhs23_gz<-blkgrp_projections$frac*blkgrp_projections$hhs23
blkgrp_projections$jobs22_gz<-blkgrp_projections$frac*blkgrp_projections$jobs22
#
# get totals of hhs and jobs in and out of GZs
#
tothhs<-sum(blkgrp_projections$hhs23)
totjobs<-sum(blkgrp_projections$jobs22)

tothhs_ingz<-round(sum(blkgrp_projections$hhs23_gz))
totjobs_ingz<-round(sum(blkgrp_projections$jobs22_gz))

tothh_outgz<-tothhs-tothhs_ingz
totjobs_outgz<-totjobs-totjobs_ingz
#
# now scale up hh and jobs in the gz's to match the goals
#
# 32 percent of planned regional housing growth, creating an estimated 84,000 new homes 
# 40 percent of planned employment growth, or about 100,000 new jobs 
# 16 percent reduction of VMT per capita in Green Zones, double the regional reduction per capita of 8 percent 
# Reducing the number of homes in high fire risk areas by 10,000 units over 20 years.  
#
#
gz_hh_goal<-84000+tothhs_ingz
gz_job_goal<-100000+totjobs_ingz
#
# and for the whole region 
#
total_hh_goal<-84000/0.32 + tothhs
total_job_goal<-100000/0.40 + totjobs
#
# now get the scale factors
#
in_gs_hhr<-gz_hh_goal/tothhs_ingz
in_gs_jobr<-gz_job_goal/totjobs_ingz

out_gs_hhr<-(total_hh_goal-gz_hh_goal)/tothh_outgz
out_gs_jobr<-(total_job_goal-gz_job_goal)/totjobs_outgz

blkgrp_projections$hhs50<-(blkgrp_projections$hhs23-blkgrp_projections$hhs23_gz)*out_gs_hhr +
  blkgrp_projections$hhs23_gz*in_gs_hhr
blkgrp_projections$jobs50<-(blkgrp_projections$jobs22-blkgrp_projections$jobs22_gz)*out_gs_jobr +
  blkgrp_projections$jobs22_gz*in_gs_jobr
print(c('just checking, allocated hhs total = ',sum(blkgrp_projections$hhs50),' and goal=',total_hh_goal))
print(c('just checking, allocated jobs total = ',sum(blkgrp_projections$jobs50),' and goal=',total_job_goal))
#
# save spread sheet
#
gz_scenario<-add_xls_tab(gz_scenario,'blkgrp_projections',blkgrp_projections)
wb_save(gz_scenario,file="./excel_files/gz_scenario.xlsx",overwrite = TRUE)



