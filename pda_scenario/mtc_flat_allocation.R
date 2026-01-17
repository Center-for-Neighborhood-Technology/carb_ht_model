library(openxlsx2)
#
# clear all memory before starting
#
rm(list=ls())
source("./utilities/get_data_functions.R")
source("./utilities/overlaps_and_splits.R")
source("./utilities/ploting_scripts.R")
#
# load in spread sheet needed
#
data_prep<-wb_load("./excel_files/data_prep.xlsx")
pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")
#
# make the aggregate geometry out of SUPERDISTRICS and pdas
#
blkgrps<-read_sf(dsn = "./tiger/mtc/", 
                    layer = "blkgrps")
#
# read in the mtc counties
#
mtc_counties<-wb_read(pda_scenario,sheet='MTC_counties')
#
# read in 2023 block group acs households from census api
#
blkgrp_hhs_2023 <- get_acs_variable('block group','B25009_001','households','GEOID',2023,'06',mtc_counties$fipco)
names(blkgrp_hhs_2023)<-c('geoid','hhs23')
#
# read in the jobs for the mtc counties
#
lodes_2022<-wb_read(data_prep,sheet='lodes_2022')
lodes_2022<-subset(lodes_2022,substring(lodes_2022$GEOID,1,5) %in% paste('06',mtc_counties$fipco,sep=''))
blkgrp_jobs_22<-as.data.frame(subset(lodes_2022, select=c('GEOID','C000')))
names(blkgrp_jobs_22)<-c('geoid','jobs22')
#
# start building the projections
#
blkgrp_base_prj<-merge(blkgrp_hhs_2023,blkgrp_jobs_22)
names(blkgrp_base_prj)
#
# allocate the 2023 households
#
tot_hhs_2023<-sum(blkgrp_base_prj$hhs23) 
tot_job_2022<-sum(blkgrp_base_prj$jobs22)
#
# from the bay area plan
#
total_hh_goal<-4043000
total_job_goal<-5408000

blkgrp_base_prj$hhs50<-blkgrp_base_prj$hhs23*total_hh_goal/tot_hhs_2023
blkgrp_base_prj$jobs50<-blkgrp_base_prj$jobs22*total_job_goal/tot_job_2022
blkgrp_base_prj<-merge(blkgrp_base_prj,subset(st_drop_geometry(blkgrps),select=c('geoid','lacres')))
head(blkgrp_base_prj)
#
# save this result to the xlxs tab
#
this_sheet<-'blkgrp_base_prj'
if((this_sheet %in% pda_scenario$sheet_names)){
  pda_scenario$remove_worksheet(this_sheet)
}
pda_scenario$add_worksheet(this_sheet)
pda_scenario$add_data(paste(this_sheet),as.data.frame(blkgrp_base_prj),colNames = TRUE)
wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)
#
# save shape file
#
blkgrp_base_prj_geom<-merge(blkgrp_base_prj,subset(blkgrps,select=c('geoid','geometry')))
write_sf(blkgrp_base_prj_geom, "./tiger/mtc/blkgrp_base_prj_geom.shp")
head(blkgrp_base_prj_geom)
