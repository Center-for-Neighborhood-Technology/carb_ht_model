library(units)
cut_water_out_of_layer<-function(layer_og,ndx,destination,shp_name,verbose=0){
  #
  # get utilities needed
  #
  source("./utilities/overlaps_and_splits.R")
  file_there<-list.files(destination)
  print(file_there)
  is_there<-length(file_there[grep(shp_name,file_there)])>0
  print(is_there)
  over_write<-TRUE
  if(is_there){
    over_write<-askYesNo(paste('Removing Water:\n','The file',shp_name,"is already in",destination,"do you want to overwrite it?"))
    print(c('over_write=',over_write))
  }  
  if(!over_write){
    layer<-read_sf(dsn = destination, 
                    layer = shp_name)
  }
  else{
    #
    # get water 
    #
    water<-read_sf(dsn = "./gis/", 
                   layer = "water")
    names(water)[1]<-'water_objectid'
    #
    # cut water out of layer
    #
    layer_og<-st_transform(layer_og, crs = 4326)
    layer<-cut_polygons(layer_og,water,ndx,'water_objectid',verbose)
    #
    # get the new land area and save it
    #
    layer$lacres<-set_units(st_area(layer$geometry),'acre')
    write_sf(layer, paste(destination,shp_name,'.shp',sep=''))
  }
  layer
}
