#
# clear all memory before starting
#
rm(list=ls())
#
# load some utilities
#
source("./utilities/overlaps_and_splits.R")
#
# get tiger shape file for California block groups
#
blkgrps<-block_groups(state='06',year=2023)
blkgrps<-subset(blkgrps,select=c('GEOID','ALAND','AWATER','INTPTLAT','INTPTLON','geometry'))
#
# get the water shape files and cut it out
#  the pacific coast within 12 Nautical Miles of shore
#
water1<-read_sf(dsn = "./gis", layer = "california_coastal_waters_12nm")
#
# get the water shape files and cut it out
#  USGS water areas for California
#
water2<-read_sf(dsn = "./gis", layer = "waterarea")
#
# get the water shape files and cut it out
#  USGS water bodies for California
#
water3<-read_sf(dsn = "./gis", layer = "waterbody")
#
# transform all to blkgrps crs and append into one layer
#
water1<-st_transform(water1,st_crs(blkgrps))
water2<-st_transform(water2,st_crs(blkgrps))
water3<-st_transform(water3,st_crs(blkgrps))
names(water1)[1] <-'OBJECTID'
water<-rbind(water1,subset(water2,select=c('OBJECTID','geometry')))
water<-rbind(water,subset(water3,select=c('OBJECTID','geometry')))
water$OBJECTID<-1:length(water$OBJECTID)
#
# now cut out the water from the block groups
#
blkgrps<-cut_polygons(blkgrps,water,'GEOID','OBJECTID',plt_lvl=1)
#
# save the water shape file
#
st_write(water,dsn='./gis/water.shp',delete_dsn=TRUE)
#
# save block groups
#
blkgrps$lacres<-set_units(st_area(blkgrps$geometry),'acre')
st_write(blkgrps,dsn='./gis/blkgrps.shp')
