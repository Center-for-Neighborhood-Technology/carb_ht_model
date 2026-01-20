library(openxlsx2)
library(dplyr)
library(sf)
library(DescTools)
library(eia)
library(rapportools)
library(units)
#
# clear all memory before starting
#
rm(list=ls())
source("./utilities/overlaps_and_splits.R")
#
# get water 
#
water<-read_sf(dsn = "./tiger/", 
               layer = "water")
names(water)[1]<-'water_objectid'
#
# get CA counties
#
counties<-read_sf(dsn = "./tiger/", 
                    layer = "ca_counties_2023")
names(counties)
#
# get the block groups in from tiger line files
#
blkgrps_og<-read_sf(dsn = "./tiger/", 
                 layer = "california_blkgrps_2023")
blkgrps_og<-subset(blkgrps_og,select=c('GEOID','geometry'))
names(blkgrps_og)[1]<-'geoid'
blkgrps_og$county<-substr(blkgrps_og$geoid,1,5)
length(blkgrps_og$geoid)
#
# calculate area in acres
#
blkgrps_og$acres<-set_units(st_area(blkgrps_og$geometry),'acre')
#
# cut water out of block groups
#
blkgrps_og<-st_transform(blkgrps_og, crs = 4326)
blkgrps<-cut_polygons(blkgrps_og,water,'geoid','water_objectid',1)
#
# get the new land area and save it
#
blkgrps$lacres<-set_units(st_area(blkgrps$geometry),'acre')
write_sf(blkgrps, "./tiger/blkgrps.shp")
#
# get the counties and get area in acres
#
counties<-read_sf(dsn = "./tiger/", 
                  layer = "ca_counties_2023")
names(counties)
head(counties)
counties<-subset(counties,select = c('GEOID','NAME','geometry'))
names(counties)<-c('co_geoid','co_name','geometry')
counties$acres<-set_units(st_area(counties),'acre')
#
# cut out water get new land area and save it
#
counties<-cut_polygons(counties,water,'co_geoid','water_objectid',1)
counties$lacres<-set_units(st_area(counties),'acre')
write_sf(counties, "./tiger/counties.shp")
#
# get the places and get area in acres
#
places<-read_sf(dsn = "./tiger/", 
                  layer = "tl_2023_06_place")
names(places)
head(places)
places<-subset(places,select = c('GEOID','NAME','geometry'))
names(places)<-c('pl_geoid','pl_name','geometry')
places$acres<-set_units(st_area(places),'acre')
#
# cut out water get new land area and save it
#
places<-cut_polygons(places,water,'pl_geoid','water_objectid',1)
places$lacres<-set_units(st_area(places),'acre')
write_sf(places, "./tiger/places.shp")
