library(openxlsx2)
library(tidycensus)
library(dplyr)
library(sf)
library(DescTools)
#
# clear all memory before starting
#
rm(list=ls())
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
blkgrp_dat_2023 <- get_acs(geography = "block group", variables = vars$acs_var,
                           state =states, geometry = FALSE, year = 2023)
#
# transform the returned data into a more usable form with more intuitive names.
#
blkgrp_acs_2023<-{}
blkgrp_acs_2023$GEOID<-unique(blkgrp_dat_2023$GEOID)
names(blkgrp_dat_2023)
for(i in 1:length(vars$acs_var)){
  sbset<-subset(blkgrp_dat_2023,select=c('GEOID','estimate'),blkgrp_dat_2023$variable==vars$acs_var[i])
  sbset$estimate[is.na(sbset$estimate)]<-''
  sbset$estimate<-as.numeric(sbset$estimate)
  names(sbset)[names(sbset) == "estimate"] <-vars$var_name[i]
  a<-as.data.frame(merge(blkgrp_acs_2023,sbset,by='GEOID'))
  blkgrp_acs_2023<-a
}
#
# read in California block group shape file to get land area etc.
#
blkgrp_shp<-read_sf(dsn = "./tiger", layer = "california_blkgrps_2023")
blkgrp_area<-subset(st_drop_geometry(blkgrp_shp),select=c('GEOID','ALAND'))
blkgrp_acs_2023<-as.data.frame(left_join(blkgrp_acs_2023,blkgrp_area,by='GEOID'))

data_xls$remove_worksheet("blkgrp_acs_2023")
data_xls$add_worksheet("blkgrp_acs_2023")
data_xls$add_data("blkgrp_acs_2023",as.data.frame(blkgrp_acs_2023),colNames = TRUE, rowNames = TRUE,)

wb_save(data_xls,file="./excel_files/data_prep.xlsx",overwrite = TRUE)


