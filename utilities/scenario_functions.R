#
# get other utilities
#
source("./utilities/ht_calculations.R")
#
# library of functions useful for running the scenarios
#
get_overlap_fraction_table<-function(xls_file,shape1,shape2){
  wrksheet<-paste(shape1,'_',shape2,'_fracs',sep='')
  if(!(wrksheet %in% xls_file$sheet_names)){
    wrksheet<-paste(shape2,'_',shape1,'_fracs',sep='')
    if(!(wrksheet %in% xls_file$sheet_names)){
      wrksheet<-'none'     
    }
  }
  wrksheet
}

get_overlap_fractions<-function(xls_file,shape1,shape2,ndx1,ndx2){
  #
  # get the sheet that is needed or create it if it not there
  #
  wrksheet<-get_overlap_fraction_table(xls_file,shape1,shape2)
  pda_index<-wb_read(pda_scenario,sheet='index')
  gis_folder<-pda_index$value[1]
  if(wrksheet == 'none'){
    #
    # get shape files
    #
    shp1<-read_sf(dsn = gis_folder, 
                  layer = shape1)
    shp2<-read_sf(dsn = gis_folder, 
                  layer = shape2)
    #
    # now shp1 and shp2 fracs
    #
    wrksheet<-paste(shape1,'_',shape2,'_fracs',sep='')
    fracs <- intersect_polygons(shp1,shp2,ndx1,ndx2,shape1,shape2)
    head(fracs)
    add_xls_tab(xls_file,wrksheet,fracs)
  }
  wb_read(xls_file,sheet=wrksheet)             
}

allocate_acs_var_using_fractions<-function(xls_file,shape1,ndx1,shape2,ndx2,data_tab,var_name){
  # 
  # xls_file is the excel file
  # shape1 is the name of the shape file that we want to aggrigate to 
  # ndx1 is the index of shape1
  # shape2 is the name of the shape file that we want to aggrigate over
  # ndx2 is the index of shape2 
  # data_tab is the data we are agrigating, with index ndx2
  # var_name is the data field we are summing up
  #
  fracs<-get_overlap_fractions(xls_file,shape1,shape2,ndx1,ndx2)
  fracs<-merge(fracs,subset(data_tab,select=c(ndx2,var_name)),ndx2)
  fracs[[var_name]]<-fracs[[paste('frac_of_',shape2,sep='')]]*fracs[[var_name]]
  agg_data<-aggregate(x = fracs[[var_name]], by = list(Category = fracs[[ndx1]]), FUN = sum)
  names(agg_data)<-c(ndx1,var_name)
  agg_data
}

intersect_polygons<-function(p1,p2,ndx1,ndx2,nm1,nm2){
  #
  # this function makes a data.frame with the area of intersection of two polygons, and the fraction that 
  #  it covers for each polygon is for each
  #
  #   p1 - is one polygon layer
  #   p2 - is the other polygon layer
  #   ndx1 - the index into layer p1
  #   ndx2 - the index into layer p2
  #   nm1 - the name of layer p1
  #   nm2 - the name of layer p2
  #   
  # for example if you want to find the fraction of jurisdictions covered by pdas:
  #
  # p1<-pdas
  # p2<-jurisdictions
  # ndx1<-'joinkey'
  # ndx2<-"jurname"
  # nm1<-'pda'
  # nm2<-'jrsdctns'
  #
  sf_use_s2(FALSE)
  p1$acres<-set_units(st_area(p1$geometry),'acre')
  p2$acres<-set_units(st_area(p2$geometry),'acre')
  p1_p2_intersecting <- st_join(p1, p2, join = st_intersects)
  p1_p2_intersecting<-st_drop_geometry(p1_p2_intersecting)
  p1_p2_intersecting<-subset(p1_p2_intersecting,select=c(ndx1,ndx2))
  names(p1_p2_intersecting)
  
  p1_p2_intersecting<-subset(p1_p2_intersecting,!is.na(p1_p2_intersecting[[ndx2]]))
  head(p1_p2_intersecting)

  for(i in 1:length(p1_p2_intersecting[[ndx1]])){
    id1<-p1_p2_intersecting[[ndx1]][i]
    id2<-p1_p2_intersecting[[ndx2]][i]
    # print(c('this is',i,id1,id2))
    p1_p2_intersecting$intersection_acres[i]<-set_units(
      st_area(st_intersection(st_make_valid(subset(p1,p1[[ndx1]]==id1)$geometry), 
                              st_make_valid(subset(p2,p2[[ndx2]]==id2)$geometry))),'acre')
    # print("that is good")
    p1_p2_intersecting[[paste('frac_of_',nm1,sep='')]][i]<-
      p1_p2_intersecting$intersection_acres[i]/subset(p1,p1[[ndx1]]==id1)$acres
    p1_p2_intersecting[[paste('frac_of_',nm2,sep='')]][i]<-
      p1_p2_intersecting$intersection_acres[i]/subset(p2,p2[[ndx2]]==id2)$acres
  }
  sf_use_s2(TRUE)
  p1_p2_intersecting
}


overwrite_modeled_hh_vars<-function(htd,county_hh_data){
  modeled<-as.data.frame(htd[names(county_hh_data)])
  c=1
  for(c in 1:length(county_hh_data$GEOID)){
    cnty<-county_hh_data$GEOID[c]
    this_hhs<-subset(county_hh_data,county_hh_data$GEOID==cnty)
    for(i in 2:length(names(county_hh_data))){
      hhv<-names(county_hh_data)[i]
      modeled[which(modeled$GEOID %like% paste(cnty,'%',sep='')),][[hhv]]<-this_hhs[[hhv]]
    }
  }
  modeled
}

