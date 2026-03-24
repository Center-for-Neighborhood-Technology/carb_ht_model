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

get_acs_variable<-function(acs_geo,acs_var,var_name,ndx,yr,st,cnts){
  #
  #
  #
  # acs_geo<-'block group'
  # acs_var<-'B25009_001'
  # var_name<-'households'
  # ndx<-'GEOID'
  # yr<-2023
  # st<-'06'
  # cnts<-mtc_counties$fipco
  #
  #census_api_key("fc6abea375161387c17e17c48ece068ba03be8c9",install = TRUE,overwrite=TRUE)
  acs_data<-get_acs(geography = acs_geo, variables = acs_var,
                                          state=st,county =cnts, geometry = FALSE, year = yr)
  names(acs_data)[4]<-var_name
  acs_data<-subset(acs_data,select=c(ndx,var_name))
  acs_data
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

#
# this function gets data at the county level
#
get_county_acs<-function(hh_census_vars,states){
  #
  # get acs data
  #
  county_dat_2023 <- get_acs(geography = "county", variables = hh_census_vars$acs_var,
                             state =states, geometry = FALSE, year = 2023)
  acs_2023<-{}
  acs_2023$GEOID<-unique(county_dat_2023$GEOID)
  names(county_dat_2023)
  for(i in 1:length(hh_census_vars$acs_var)){
    sbset<-subset(county_dat_2023,select=c('GEOID','estimate'),county_dat_2023$variable==hh_census_vars$acs_var[i])
    sbset$estimate[is.na(sbset$estimate)]<-''
    names(sbset )[names(sbset) == "estimate"] <-hh_census_vars$var_name[i]
    a<-as.data.frame(merge(acs_2023,sbset,by='GEOID'))
    acs_2023<-a
  }
  names(acs_2023)
  head(acs_2023)
  #
  county_hh_data<-as.data.frame(acs_2023$GEOID)
  names(county_hh_data)[names(county_hh_data) == "acs_2023$GEOID"]<-"GEOID"
  #
  # fill in the data
  #
  for(i in 1:length(hh_vars$variable)){
    county_hh_data[[hh_vars$variable[i]]]<-as.numeric(eval(parse(text=(hh_vars$formula[i]))))
  }
  county_hh_data[is.na(county_hh_data)] <- ""
  names(county_hh_data)
  head(county_hh_data)
  length(county_hh_data$GEOID)
  county_hh_data
}
#
# this function gets data at the cbsa and put it into the county data set level
#
get_cbsa_acs<-function(hh_census_vars,state){
  #
  # the cbsa to county indexes are in the shape file
  #
  county_gpkg<-st_read(dsn = "./gis/counties.gpkg")
  county_gpkg<-st_drop_geometry(county_gpkg)
  cbsas<-unique(county_gpkg$cbsa)
  rm(county_gpkg)
  #
  # get acs data
  #
  cbsa_dat_2023 <- get_acs(geography = "cbsa", variables = hh_census_vars$acs_var,
                           geometry = FALSE, year = 2023)
  acs_2023<-{}
  acs_2023$GEOID<-unique(cbsa_dat_2023$GEOID)
  names(cbsa_dat_2023)
  for(i in 1:length(hh_census_vars$acs_var)){
    sbset<-subset(cbsa_dat_2023,select=c('GEOID','estimate'),cbsa_dat_2023$variable==hh_census_vars$acs_var[i])
    sbset$estimate[is.na(sbset$estimate)]<-''
    names(sbset )[names(sbset) == "estimate"] <-hh_census_vars$var_name[i]
    a<-as.data.frame(merge(acs_2023,sbset,by='GEOID'))
    acs_2023<-a
  }
  names(acs_2023)
  acs_2023<-subset(acs_2023,acs_2023$GEOID %in% cbsas)
  head(acs_2023)
  #
  acs_hh_data<-as.data.frame(acs_2023$GEOID)
  names(acs_hh_data)[names(acs_hh_data) == "acs_2023$GEOID"]<-"GEOID"
  acs_hh_data<-as.data.frame(subset(acs_hh_data,acs_hh_data$GEOID %in% cbsas))
  #
  # fill in the data
  #
  for(i in 1:length(hh_vars$variable)){
    acs_hh_data[[hh_vars$variable[i]]]<-as.numeric(eval(parse(text=(hh_vars$formula[i]))))
  }
  acs_hh_data[is.na(acs_hh_data)] <- ""
  names(acs_hh_data)
  head(acs_hh_data)
  length(acs_hh_data$GEOID)
  acs_hh_data
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

#
# this function get data at the state level
#
get_state_acs<-function(hh_census_vars,state){
  #
  # get acs data
  #
  state_dat_2023 <- get_acs(geography = "state", variables = hh_census_vars$acs_var,
                            state =state,geometry = FALSE, year = 2023)
  acs_2023<-{}
  acs_2023$GEOID<-unique(state_dat_2023$GEOID)
  names(state_dat_2023)
  for(i in 1:length(hh_census_vars$acs_var)){
    sbset<-subset(state_dat_2023,select=c('GEOID','estimate'),state_dat_2023$variable==hh_census_vars$acs_var[i])
    sbset$estimate[is.na(sbset$estimate)]<-''
    names(sbset )[names(sbset) == "estimate"] <-hh_census_vars$var_name[i]
    a<-as.data.frame(merge(acs_2023,sbset,by='GEOID'))
    acs_2023<-a
  }
  names(acs_2023)
  head(acs_2023)
  #
  acs_hh_data<-as.data.frame(acs_2023$GEOID)
  names(acs_hh_data)[names(acs_hh_data) == "acs_2023$GEOID"]<-"GEOID"
  #
  # fill in the data
  #
  for(i in 1:length(hh_vars$variable)){
    acs_hh_data[[hh_vars$variable[i]]]<-as.numeric(eval(parse(text=(hh_vars$formula[i]))))
  }
  acs_hh_data[is.na(acs_hh_data)] <- ""
  names(acs_hh_data)
  head(acs_hh_data)
  length(acs_hh_data$GEOID)
  acs_hh_data
}
