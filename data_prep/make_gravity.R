library(openxlsx2)
library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(lehdr)
#
# read in the whole xlsx file
#
data_xls<-wb_load("./excel_files/data_prep.xlsx")
blkgrp_acs_2023<- wb_read(data_xls,sheet='blkgrp_acs_2023')
#
# now get the LEHD LODES data
#
lodes_dat <- grab_lodes(state = c("az","ca", "nv", "or"), year =2022, 
                        version = "LODES8",lodes_type = "wac", 
                        job_type = "JT00", segment = "S000", 
                        state_part = "", agg_geo = "bg") 
lodes_dat<-subset(lodes_dat,select=c("w_bg","C000","CNS01","CNS02","CNS03","CNS04",
                                     "CNS05","CNS06","CNS07","CNS08","CNS09","CNS10",
                                     "CNS11","CNS12","CNS13","CNS14","CNS15","CNS16",
                                     "CNS17","CNS18","CNS19","CNS20") )
data_xls$remove_worksheet("lodes_2022")
data_xls$add_worksheet("lodes_2022")
data_xls$add_data("lodes_2022",as.data.frame(lodes_dat),colNames = TRUE, rowNames = TRUE,)
#
# open the tiger shape files and get the centroids
#
blkgrp_shp<-read_sf(dsn = "./tiger", layer = "california_blkgrps_2023")
blkgrp_shp$gas_region <- NULL
blkgrp_azcanvor<-do.call("rbind", list(read_sf(dsn = "./tiger", layer = "az_blkgrps_2023"),
                                       blkgrp_shp,
                                       read_sf(dsn = "./tiger", layer = "nv_blkgrps_2023"),
                                       read_sf(dsn = "./tiger", layer = "or_blkgrps_2023")))
#
# Assuming tiger shp is in EPSG:4269 - NAD83 (per Tiger Line file CRS) coordinates
#   create centroid points from intptlat/lon and add in lodes and acs vars we want to 
#   measure gravity for.
#
blkgrp_azcanvor <- st_as_sf(blkgrp_azcanvor,crs = 4269, remove = FALSE)
blkgrp_azcanvor$pt<-st_as_sf(as.data.frame(blkgrp_azcanvor),
                             coords = c("INTPTLON","INTPTLAT"),
                             crs = 4269, remove = FALSE)
blkgrp_azcanvor <- left_join(blkgrp_azcanvor,lodes_dat, by=c('GEOID'='w_bg'))
acs_vars<-subset(as.data.frame(blkgrp_acs_2023),select=c('GEOID','households','occupied_hu','housing_units',
                                                         'renter_occupied_hu','hu_1_detached'))
blkgrp_azcanvor <- left_join(blkgrp_azcanvor,acs_vars, by=c('GEOID'))
#
# create centroid points for California block groups
#
blkgrp_shp <- st_as_sf(blkgrp_shp,crs = 4269, remove = FALSE)
blkgrp_shp$pt<-st_as_sf(as.data.frame(blkgrp_shp),
                        coords = c("INTPTLON","INTPTLAT"),
                        crs = 4269, remove = FALSE)
#
# create gravity data frame and order by GEOID
#
gravity<-subset(as.data.frame(blkgrp_shp),select=c('GEOID','COUNTYFP'))
order_indices <- order(gravity$GEOID)
gravity<-as.data.frame(gravity[order_indices, ])
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
for(g in 1:length(gravity_vars)){
  blkgrp_azcanvor[[gravity_vars[g]]]<-ifelse(is.na(blkgrp_azcanvor[[gravity_vars[g]]]),
                                             as.double(0),
                                             as.double(blkgrp_azcanvor[[gravity_vars[g]]]))
  gravity[[gravity_vars[g]]]<-0
}
cnty=''
i<-1
for(i in 1:length(gravity$GEOID)){
  cnty_old=cnty
  cnty=gravity$COUNTYFP[i]
  if(cnty!=cnty_old){print(paste("Starting county = ",cnty))}
  stfid<-gravity$GEOID[i]
  tst<-subset(blkgrp_azcanvor,blkgrp_azcanvor$GEOID==stfid)
  #
  #  fill the distance in miles
  #
  blkgrp_azcanvor$d<-as.double(st_distance( st_transform(tst$pt,3857),
                                            st_transform(blkgrp_azcanvor$pt,3857)))/1609.34
  sbset10<-subset(as.data.frame(blkgrp_azcanvor),blkgrp_azcanvor$d<10)
  sbset10$d2<-ifelse(sbset10$d<1,1.0,sbset10$d*sbset10$d)
  grv10<-{}
  grv10$GEOID=stfid
  grv10$COUNTYFP<-cnty
  for(g in 1:length(gravity_vars)){
    grv10[gravity_vars[g]] <- sum(sbset10[[gravity_vars[g]]]/sbset10$d2)
  }
  gravity[i,]<-as.data.frame(grv10)
}

data_xls$remove_worksheet("gravity_10")
data_xls$add_worksheet("gravity_10")
data_xls$add_data("gravity_10",as.data.frame(gravity),colNames = TRUE, rowNames = TRUE,)


wb_save(data_xls,file="./excel_files/data_prep.xlsx",overwrite = TRUE)
