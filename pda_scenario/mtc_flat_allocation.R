#
# clear all memory before starting
#
rm(list=ls())
#
# get the current year(s) 
#
source("./utilities/current_years.R")
source("./utilities/get_data_functions.R")
source("./utilities/overlaps_and_splits.R")
source("./utilities/ploting_scripts.R")
#
# load in spread sheet needed
#
data_prep<-wb_load("./excel_files/data_prep.xlsx")
pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")
#
# get the location of the gis files
#
pda_index<-wb_read(pda_scenario,sheet='index')
gis_folder<-pda_index$value[1]
#
# for the flat allocation only use the blkgrps and just allocate to the block groups
#

blkgrps<-read_sf(dsn = gis_folder, 
                    layer = "blkgrps")
#
# read in the mtc counties
#
mtc_counties<-wb_read(pda_scenario,sheet='MTC_counties')
#
# read in current_acs_year block group acs households from census api
#
blkgrp_hhs_current <- get_acs_variable('block group','B25009_001','households','geoid',current_acs_year,'06',mtc_counties$fipco)
names(blkgrp_hhs_current)<-c('geoid','hhs23')
#
# read in the jobs for the mtc counties
#
lodes_current<-wb_read(data_prep,sheet='lodes_current')
names(lodes_current)[2]<-'geoid'
lodes_current<-subset(lodes_current,substring(lodes_current$geoid,1,5) %in% paste('06',mtc_counties$fipco,sep=''))
blkgrp_jobs_22<-as.data.frame(subset(lodes_current, select=c('geoid','C000')))
names(blkgrp_jobs_22)<-c('geoid','jobs22')
#
# start building the projections
#
blkgrp_flat_prj<-merge(blkgrp_hhs_current,blkgrp_jobs_22)
names(blkgrp_flat_prj)
#
# allocate the current_acs_year households
#
tot_hhs_current<-sum(blkgrp_flat_prj$hhs23) 
tot_job_current<-sum(blkgrp_flat_prj$jobs22)
#
# from the bay area plan
#
total_hh_goal<-4043000
total_job_goal<-5408000

blkgrp_flat_prj$hhs50<-blkgrp_flat_prj$hhs23*total_hh_goal/tot_hhs_current
blkgrp_flat_prj$jobs50<-blkgrp_flat_prj$jobs22*total_job_goal/tot_job_current
blkgrp_flat_prj<-merge(blkgrp_flat_prj,subset(st_drop_geometry(blkgrps),select=c('geoid','lacres')))
head(blkgrp_flat_prj)
#
# save this result to the xlxs tab
#
this_sheet<-'blkgrp_flat_prj'
if((this_sheet %in% pda_scenario$sheet_names)){
  pda_scenario$remove_worksheet(this_sheet)
}
pda_scenario$add_worksheet(this_sheet)
pda_scenario$add_data(paste(this_sheet),as.data.frame(blkgrp_flat_prj),colNames = TRUE)
wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)
#
# save shape file
#
blkgrp_flat_prj_geom<-merge(blkgrp_flat_prj,subset(blkgrps,select=c('geoid','geometry')))
write_sf(blkgrp_flat_prj_geom,paste(gis_folder,"blkgrp_flat_prj_geom.gpkg",sep=''))
