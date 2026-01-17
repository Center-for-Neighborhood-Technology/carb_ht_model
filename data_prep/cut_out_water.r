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
# this is how I built the water layer from the USGS NHD data
#
#
# Replace "your_file.gpkg" with the actual path to your GeoPackage file
#gpkg_file_path <- "C:/Users/pmh/carb/water/NHD_H_California_State_GDB.gdb"
#
# List all layers and their geometry types
#st_layers(gpkg_file_path)
# Read a specific layer (replace "layer_name" with the actual name from the list)
#
#
#my_layer1 <- st_read(gpkg_file_path, layer = "NHDArea")
#my_layer2 <- st_read(gpkg_file_path, layer = "NHDWaterbody")
#
#my_layer1 <-read_sf(dsn = "C:/Users/pmh/carb/water/", 
#                       layer = "waterarea")
#my_layer2 <-read_sf(dsn = "C:/Users/pmh/carb/water/", 
#                    layer = "waterbody")
#
# View the first few rows and structure of the layer
#head(my_layer2)
#summary(my_layer1)
#names(my_layer1)
#
#water<-(rbind(subset(my_layer1,select=c('OBJECTID','geometry')),
#             subset(my_layer2,select=c('OBJECTID','geometry'))))
#names(water)<-c('water_objectid','geometry')
#water<-st_transform(water,st_crs(pdas))
#water<-st_set_crs(water,st_crs(pdas))
#class(water)
#write_sf(water, "./tiger/water.shp")
#names(water)
#
# get water 
#
water<-read_sf(dsn = "./tiger/", 
               layer = "water")
names(water)[1]<-'water_objectid'
#
# get the block groups in from tiger line files
#
blkgrps_og<-read_sf(dsn = "./tiger/mtc/", 
                 layer = "mtc_blkgrps")
blkgrps_og<-subset(blkgrps_og,select=c('geoid','geometry'))
blkgrps_og$county<-substr(blkgrps_og$geoid,1,5)                
#
# calculate area in acres
#
blkgrps_og$acres<-set_units(st_area(blkgrps_og$geometry),'acre')
#
# cut water out of block groups
#
blkgrps_og<-st_transform(blkgrps_og, crs = 4326)
blkgrps<-cut_polygons(blkgrps_og,water,'geoid','water_objectid',0)
#
# get the new land area and save it
#
blkgrps$lacres<-set_units(st_area(blkgrps$geometry),'acre')
write_sf(blkgrps, "./tiger/mtc/blkgrps.shp")
#
# get the pdas in MTC and get area in acres
#
pdas<-read_sf(dsn = "./tiger/mtc/", 
                layer = "mtc_pdas")
pdas<-subset(pdas,select = c('joinkey','pda_name','geometry'))
pdas$acres<-set_units(st_area(pdas),'acre')
#
# cut out water get new land area and save it
#
pdas<-cut_polygons(pdas,water,'joinkey','water_objectid')
pdas$lacres<-set_units(st_area(pdas),'acre')
write_sf(pdas, "./tiger/mtc/pdas.shp")
#
# now get SUPERDISTRICTS
#
sdistricts_og<-read_sf(dsn = "./tiger/mtc/", 
                 layer = "mtc_superdistricts")
sdistricts_og<-subset(sdistricts_og,select = c('suprdistid','geometry'))
sdistricts_og$acres<-set_units(st_area(sdistricts_og),'acre')
head(sdistricts_og)
#
# cut out water get new land area and save it
#
sdistricts<-cut_polygons(sdistricts_og,water,'suprdistid','water_objectid')
sdistricts$lacres<-set_units(st_area(sdistricts),'acre')
write_sf(sdistricts, "./tiger/mtc/sdistricts.shp")

