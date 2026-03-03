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
pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")
#
# make the aggregate geometry out of SUPERDISTRICS and pdas
#
pda_index<-wb_read(pda_scenario,sheet='index')
gis_folder<-pda_index$value[1]
mtc_pdas<-read_sf(dsn = gis_folder, 
              layer = "mtc_pdas")
pdas<-cut_water_out_of_layer(mtc_pdas,names(mtc_pdas)[1],gis_folder,"pdas",1)

mtc_sdistricts<-read_sf(dsn = gis_folder, 
                    layer = "travel_model_super_districts")
sdistricts<-cut_water_out_of_layer(mtc_sdistricts,'suprdistid',gis_folder,"sdistricts",1)

mtc_blkgrps<-read_sf(dsn = gis_folder, 
                    layer = "mtc_blkgrps")
blkgrps<-cut_water_out_of_layer(mtc_blkgrps,'geoid',gis_folder,"blkgrps",1)
#
# cut out pdas from SUPERDISTRICS
#
agg_geom<-cut_polygons_rmapshaper(sdistricts,pdas)
#
# get the columns in sync
#
pdas$OBJECTID<-NULL
pdas$pda_name<-NULL
pdas$county<-NULL
pdas$jurisdicti<-NULL

names(pdas)[1]<-c('obj_id')
pdas$type<-'pda'
agg_geom$county_fip<-NULL
names(agg_geom)[1]<-c('obj_id')

agg_geom$type<-'superdistrict'
names(agg_geom)<-names(pdas)
#
# append them together
#
agg_geom <- rbind(subset(agg_geom,select=c("obj_id","lacres","type","geometry")), 
                  subset(pdas    ,select=c("obj_id","lacres","type","geometry")))
#
# save shape file
#
write_sf(agg_geom,paste(gis_folder,"agg_geom.gpkg",sep=''))
#
# read in the mtc counties
#
mtc_counties<-wb_read(pda_scenario,sheet='MTC_counties')
#
# read in 2023 block group acs households from census api
#
blkgrp_hhs_2023 <- get_acs_variable('block group','B25009_001','households','GEOID',2023,'06',mtc_counties$fipco)
names(blkgrp_hhs_2023)[1]<-'geoid'
#
# read in the jobs for the mtc counties
#
lodes_2022<-wb_read(data_prep,sheet='lodes_2022')
names(lodes_2022)[2]<-'GEOID'
lodes_2022<-subset(lodes_2022,substring(lodes_2022$GEOID,1,5) %in% paste('06',mtc_counties$fipco,sep=''))
blkgrp_jobs_22<-subset(lodes_2022, select=c('GEOID','C000'))
names(blkgrp_jobs_22)[1]<-'geoid'
#
# allocate the 2023 households from blkgrps to agg_geom
#
agg_geom_hh <- sum_var_using_fractions(pda_scenario,'agg_geom','obj_id','blkgrps','geoid',
                                              blkgrp_hhs_2023,'households')
agg_geom_job<- sum_var_using_fractions(pda_scenario,'agg_geom','obj_id','blkgrps','geoid',
                                                blkgrp_jobs_22,'C000')
agg_geom_projections<-merge(agg_geom_hh,agg_geom_job,  by='obj_id', all=TRUE)
names(agg_geom_projections)<-c('obj_id','hh23','jobs22')
#
# allocate the 2023 households from blkgrps to super districts
#
sdistricts_hh <- sum_var_using_fractions(pda_scenario,'sdistricts','suprdistid','blkgrps','geoid',
                                               blkgrp_hhs_2023,'households')
pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")
sdistricts_job <- sum_var_using_fractions(pda_scenario,'sdistricts','suprdistid','blkgrps','geoid',
                                                  blkgrp_jobs_22,'C000')
sdistricts_projections<-merge(sdistricts_hh,sdistricts_job,  by='suprdistid', all=TRUE)
names(sdistricts_projections)<-c('suprdistid','hh23','jobs22')
#
# get the hh and jobs allocation in 2050
#
sdistricts_allocation<-wb_read(pda_scenario,sheet='super_district_allocation')
sdistricts_allocation<-subset(sdistricts_allocation,
                              !(sdistricts_allocation$COUNTY =='REGION'),
                              select=c('SUPER-DISTRICT','SUPERDISTRICT NAME','HOUSEHOLDS 2050','JOBS 2050'))
names(sdistricts_allocation)<-c('suprdistid','sdname','hh2050','jobs2050')
head(sdistricts_allocation)

sdistricts_projections<-merge(sdistricts_projections, sdistricts_allocation, by='suprdistid', all.x=TRUE, all.y=FALSE)
head(sdistricts_projections)
#
# now pro rate the san francisco hhs and jobs using the 2023 numbers
#
sum_sf_hh23<-sdistricts_projections$hh23[1]+sdistricts_projections$hh23[2]+sdistricts_projections$hh23[3]+sdistricts_projections$hh23[4]
sum_sf_jobs22<-sdistricts_projections$jobs22[1]+sdistricts_projections$jobs22[2]+sdistricts_projections$jobs22[3]+sdistricts_projections$jobs22[4]
for(i in 1:4){
  sdistricts_projections$hh2050[i]<-sdistricts_projections$hh23[i]*sdistricts_allocation$hh2050[1]/sum_sf_hh23
  sdistricts_projections$jobs2050[i]<-sdistricts_projections$jobs22[i]*sdistricts_allocation$jobs2050[1]/sum_sf_jobs22
  sdistricts_projections$sdname[i]<-paste("San Fransico ",i,sep='')
}
sum(sdistricts_projections$hh2050[1:4])
sum(sdistricts_projections$jobs2050[1:4])
sum(sdistricts_projections$hh2050)
sum(sdistricts_projections$jobs2050 )
#
# calculate ratio of 2023 hhs and 2022 jobs to super districts
#
sdistricts_projections$hh_rat<-sdistricts_projections$hh2050/sdistricts_projections$hh23
sdistricts_projections$job_rat<-sdistricts_projections$jobs2050/sdistricts_projections$jobs22 
head(sdistricts_projections)
#
# now allocate ratios to agg_geom
#
agg_geom_hhr <- average_var_using_fractions(pda_scenario,'agg_geom','obj_id','sdistricts','suprdistid',
                                               sdistricts_projections,'hh_rat')
pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")
agg_geom_jobsr <- average_var_using_fractions(pda_scenario,'agg_geom','obj_id','sdistricts','suprdistid',
                                                 sdistricts_projections,'job_rat')
agg_geom_projections$hh_rat<-agg_geom_projections$job_rat<-agg_geom_projections$type<-NULL
agg_geom_projections<-merge(agg_geom_projections, agg_geom_hhr, by='obj_id', all=T)
agg_geom_projections<-merge(agg_geom_projections, agg_geom_jobsr, by='obj_id', all=T)
agg_geom_projections<-merge(agg_geom_projections, 
                            st_drop_geometry(subset(agg_geom,select=c('obj_id','type'))), 
                            by='obj_id', all=T)
agg_geom_projections$hh50<-agg_geom_projections$hh23*agg_geom_projections$hh_rat
agg_geom_projections$jobs50<-agg_geom_projections$jobs22*agg_geom_projections$job_rat
pdas_projections<-subset(agg_geom_projections,agg_geom_projections$type=='pda')

tot_hhs_pda<-sum(pdas_projections$hh50) 
tot_job_pda<-sum(pdas_projections$jobs50)
#
# from the bay area plan
#
total_hh_goal<-4043000
total_job_goal<-5408000
#
# now scale up hh and jobs in the pda's to match the goals
#
pda_hh_goal<-1672000
pda_job_goal<-2561000

pdas_projections$hh50_scale<-pdas_projections$jobs50_scale<-NULL

pda_hh_ratio <- as.numeric(pda_hh_goal)/as.numeric(tot_hhs_pda)
pda_job_ratio<- as.numeric(pda_job_goal)/as.numeric(tot_job_pda)

pdas_projections$hh50_scale <- pdas_projections$hh50*pda_hh_ratio
pdas_projections$jobs50_scale <- pdas_projections$jobs50*pda_job_ratio

head(pdas_projections)
tot_hhs_scale_pda<-sum(pdas_projections$hh50_scale) 
tot_job_scale_pda<-sum(pdas_projections$jobs50_scale)
print(paste("Just checking in pdas hh total =",tot_hhs_scale_pda,'and goal =',pda_hh_goal))
print(paste("Just checking in pdas job total =",tot_job_scale_pda,'and goal =',pda_job_goal))
#
# scale up non-pdas areas to match their goals
#
sdist_hh_goal<-total_hh_goal-pda_hh_goal
sdist_job_goal<-total_job_goal-pda_job_goal

sdist_projections<-subset(agg_geom_projections,agg_geom_projections$type=='superdistrict')
sdist_projections$hh50_scale<-sdist_projections$jobs50_scale<-NULL

tot_hhs_sdist<-sum(sdist_projections$hh50) 
tot_job_sdist<-sum(sdist_projections$jobs50)

sd_hh_ratio<-as.numeric(sdist_hh_goal)/as.numeric(tot_hhs_sdist)
sd_job_ratio<-as.numeric(sdist_job_goal)/as.numeric(tot_job_sdist)

sdist_projections$hh50_scale <- sdist_projections$hh50*sd_hh_ratio
sdist_projections$jobs50_scale <- sdist_projections$jobs50*sd_job_ratio

tot_hhs_scale_sd<-sum(sdist_projections$hh50_scale) 
tot_job_scale_sd<-sum(sdist_projections$jobs50_scale)
print(paste("Just checking in non-pdas hh total =",tot_hhs_scale_sd,'and goal =',sdist_hh_goal))
print(paste("Just checking in non-pdas job total =",tot_job_scale_sd,'and goal =',sdist_job_goal))
#
# now append pda and sdist together in one data frame
#
agg_projections <- rbind(sdist_projections, pdas_projections)
#
# calculate the ratio of 2023 hhs and 2022 jobs to 2050 hhs and jobs
#
agg_projections$hhr_tot<-agg_projections$hh50_scale/agg_projections$hh23
agg_projections$jobr_tot<-agg_projections$jobs50_scale/agg_projections$jobs22
#
# save this result to the xlxs tab
#
this_sheet<-'agg_projections'
if((this_sheet %in% pda_scenario$sheet_names)){
  pda_scenario$remove_worksheet(this_sheet)
}
pda_scenario$add_worksheet(this_sheet)
pda_scenario$add_data(paste(this_sheet),as.data.frame(agg_projections),colNames = TRUE)
wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)
#
# save shape file
#
agg_proj_geom<-merge(agg_projections,subset(agg_geom,select=c('obj_id','lacres','geometry')))
write_sf(agg_proj_geom, paste(gis_folder,"agg_geom.gpkg",sep=''))
head(agg_proj_geom)
#
# now get blkgrps ratios from agg_geos
#
blkgp_projections<-merge(blkgrp_hhs_2023,blkgrp_jobs_22)
names(blkgp_projections)<-c('geoid','hhs23','jobs22')
blkgp_hh50_ratio <- average_var_using_fractions(pda_scenario,'blkgrps','geoid','agg_geom','obj_id',
                                             agg_projections,'hhr_tot')
wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)

blkgp_projections<-merge(blkgp_projections,blkgp_hh50_ratio)
blkgp_job50_ratio <- average_var_using_fractions(pda_scenario,'blkgrps','geoid','agg_geom','obj_id',
                                             agg_projections,'jobr_tot')
blkgp_projections<-merge(blkgp_projections,blkgp_job50_ratio)
blkgp_projections<-merge(blkgp_projections,st_drop_geometry(subset(blkgrps,select = c('geoid','lacres'))))
blkgp_projections$hhs50<-blkgp_projections$hhs23*blkgp_projections$hhr_tot
blkgp_projections$jobs50<-blkgp_projections$jobs22*blkgp_projections$jobr_tot
head(blkgp_projections)
hist(blkgp_projections$hhr_tot)
sum(blkgp_projections$hhs50)
sum(blkgp_projections$jobs50)
plot(blkgp_projections$hhs23,blkgp_projections$hhs50)
error_plot(blkgp_projections$hhs23,blkgp_projections$hhs50,yl='hh50',xl='hh23')
plot(blkgp_projections$jobs22  ,blkgp_projections$jobs50)
error_plot(blkgp_projections$jobs22  ,blkgp_projections$jobs50,yl='jobs50',xl='jobs23')
#
# save spread sheet
#
this_sheet<-'blkgp_projections'
if((this_sheet %in% pda_scenario$sheet_names)){
  pda_scenario$remove_worksheet(this_sheet)
}
pda_scenario$add_worksheet(this_sheet)
pda_scenario$add_data(this_sheet,as.data.frame(blkgp_projections),colNames = TRUE)
wb_save(pda_scenario,file="./excel_files/pda_scenario.xlsx",overwrite = TRUE)
#
# save shape file 
#
blkgp_projections_geom<-merge(blkgp_projections,subset(blkgrps,select=c('geoid','geometry')))
write_sf(blkgp_projections_geom,paste(gis_folder,"blkgp_projections.gpkg",sep=''))

