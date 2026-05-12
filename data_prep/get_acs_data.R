#
# clear all memory before starting
#
rm(list=ls())
#
# get the current year(s) 
#
source("./utilities/current_years.R")
#
# read in the whole xlsx file
#
data_xls<-wb_load("./excel_files/data_prep.xlsx")
#
# get the ACS variables in California and bordering states
#
vars<- wb_read(data_xls,sheet='acs_variable_definitions')
states=c("CA","AZ","NV","OR")
#census_api_key("fc6abea375161387c17e17c48ece068ba03be8c9",install = TRUE,overwrite=TRUE)
blkgrp_dat_current <- get_acs(geography = "block group", variables = vars$acs_var,
                           state =states, geometry = FALSE, year = current_acs_year,output='wide')
blkgrp_dat_current<-blkgrp_dat_current[,c('GEOID',paste(vars$acs_var,'E',sep=''))]
names(blkgrp_dat_current)<-c('geoid',vars$var_name)
#
# read in California block group shape file to get land area etc.
#
blkgrp_shp<-st_read("./gis/blkgrps.gpkg")
blkgrp_area<-subset(st_drop_geometry(blkgrp_shp),select=c('geoid','lacres'))
blkgrp_dat_current<-as.data.frame(left_join(blkgrp_dat_current,blkgrp_area,by='geoid'))
#
# save to xlxs  
#
data_xls$remove_worksheet("blkgrp_acs_current")
data_xls$add_worksheet("blkgrp_acs_current")
data_xls$add_data("blkgrp_acs_current",as.data.frame(blkgrp_dat_current),colNames = TRUE, rowNames = TRUE,)

wb_save(data_xls,file="./excel_files/data_prep.xlsx",overwrite = TRUE)




