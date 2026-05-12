#
# clear all memory before starting
#
rm(list=ls())
#
# get the current year(s) 
#
source("./utilities/current_years.R")
#
# load some utilities
#
source("./utilities/overlaps_and_splits.R")
#
# get tiger shape file for California block groups
#
blkgrps<-block_groups(state='06',year=current_acs_year)
#
# get the water shape files and cut it out
#  the pacific coast within 12 Nautical Miles of shore
#
water1<-read_sf(dsn = "./gis/water_shp_files", layer = "california_coastal_waters_12nm")
#
# get the water shape files and cut it out
#  USGS water areas for California
#
water2<-read_sf(dsn = "./gis/water_shp_files", layer = "waterarea")
#
# get the water shape files and cut it out
#  USGS water bodies for California
#
water3<-read_sf(dsn = "./gis/water_shp_files", layer = "waterbody")
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
names(water)<-tolower(names(water))
#
# save the water geopackage file
#
st_write(water,dsn='./gis/water.gpkg',delete_dsn=TRUE)
#
# first block groups
#
blkgrps<-subset(blkgrps,blkgrps$ALAND>0,select=c('GEOID','ALAND','AWATER','INTPTLAT','INTPTLON','geometry'))
names(blkgrps)<-tolower(names(blkgrps))
#
# now cut out the water from the block groups
#
blkgrps<-cut_polygons_rmapshaper(blkgrps, water)
#
# get land area and save block groups geopackage file
#
sf::sf_use_s2(FALSE)
blkgrps$lacres<-set_units(st_area(blkgrps$geometry),'acre')
sf::sf_use_s2(TRUE)
st_write(blkgrps,dsn='./gis/blkgrps.gpkg',delete_dsn=TRUE)
#
# now do counties
#
counties<-counties(state='06',year=current_acs_year)
counties<-subset(counties,counties$ALAND>0,select=c('GEOID','NAME','CBSAFP','ALAND','AWATER','INTPTLAT','INTPTLON','geometry'))
names(counties)<- tolower(names(counties))
#
# now cut out the water from the counties
#
counties<-cut_polygons_rmapshaper(counties, water)
#
# get land area and save counties geopackage file
#
sf::sf_use_s2(FALSE)
counties$lacres<-set_units(st_area(counties$geometry),'acre')
sf::sf_use_s2(TRUE)
st_write(counties,dsn='./gis/counties.gpkg',delete_dsn=TRUE)
#
# now do places
#
places<-places(state='06',year=current_acs_year)
places<-subset(places,places$ALAND>0,select=c('GEOID','NAME','ALAND','AWATER','INTPTLAT','INTPTLON','geometry'))
names(places)<- tolower(names(places))
#
# now cut out the water from the places
#
places<-cut_polygons_rmapshaper(places, water)
#
# get land area and save places geopackage file
#
sf::sf_use_s2(FALSE)
places$lacres<-set_units(st_area(places$geometry),'acre')
sf::sf_use_s2(TRUE)
st_write(places,dsn='./gis/places.gpkg',delete_dsn=TRUE)
#
# now do cbsas
#
cbsas<-core_based_statistical_areas(year=current_acs_year)
cbsas<-subset(cbsas,grepl('CA', cbsas$NAME) )
cbsas<-subset(cbsas,cbsas$ALAND>0,select=c('GEOID','NAME','ALAND','AWATER','INTPTLAT','INTPTLON','geometry'))
names(cbsas)<-tolower(names(cbsas))
#
# now cut out the water from the cbsas
#
cbsas<-cut_polygons_rmapshaper(cbsas, water)
#
# get land area and save cbsas geopackage file
#
sf::sf_use_s2(FALSE)
cbsas$lacres<-set_units(st_area(cbsas$geometry),'acre')
sf::sf_use_s2(TRUE)
st_write(cbsas,dsn='./gis/cbsas.gpkg',delete_dsn=TRUE)

