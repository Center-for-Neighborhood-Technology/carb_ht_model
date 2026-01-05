library(sf)
library(tidycensus)
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
  if(wrksheet == 'none'){
    #
    # get shape files
    #
    shp1<-read_sf(dsn = "C:/Users/pmh/CNT/CNT Team Site - Documents/Urban Analytics/2024 CARB/tiger/mtc", 
                  layer = shape1)
    shp2<-read_sf(dsn = "C:/Users/pmh/CNT/CNT Team Site - Documents/Urban Analytics/2024 CARB/tiger/mtc", 
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

add_xls_tab<-function(xls_file,this_sheet,sheet_data){
  #
  # this function add a sheet to the xls_file and returns it
  #
  if(this_sheet %in% xls_file$sheet_names){
    xls_file$remove_worksheet(this_sheet)
  }
  xls_file$add_worksheet(this_sheet)
  xls_file$add_data(paste(this_sheet),as.data.frame(sheet_data),colNames = TRUE)
  xls_file
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





cut_polygons<-function(polygons,cutters,ndx,cut_ndx,plt_lvl){
#
# this function cuts out areas that are in the cutters layer, out of the polygons layer
#     polygons - layer to be cut and then returned
#     cutters - layer to cut with
#     ndx - index into polygons 
#     cut_ndx - index into cutters
#     plt_lvl - level of plotting you want to see as this goes throught he polygons.
#
# As an example polygons = block groups and cutter = water in order to remove the water area from the block groups.
#  polygons<-blkgrpsf
#  cutters<-water
#  ndx<-'geoid'
#  cut_ndx<-'water_objectid'
#
  ints<-st_join(polygons, cutters, join = st_intersects)
  ints<-subset(ints,!is.na(ints[[cut_ndx]]))
  ints_ndxs<-unique(ints[[ndx]])
  cnt<-length(polygons[[ndx]])
  i<-20
  for(i in 1:cnt){
    this_ndx<-polygons[[ndx]][i]
    if(this_ndx %in% ints_ndxs){
      print(paste('number ',i,'out of ',cnt,' this_ndx=',this_ndx))
      this_plyg<-subset(polygons,polygons[[ndx]]==this_ndx)
      if(st_geometry_type(this_plyg$geometry)=='MULTIPOLYGON'){
        zz<-subset(ints,ints[[ndx]] == this_ndx)
        xx<-this_plyg$geometry
        ww<-subset(cutters,cutters[[cut_ndx]] %in% zz[[cut_ndx]])
        yy<-st_intersection(this_plyg$geometry, 
                            ww$geometry)
        yy
        for(k in 1:length(yy)){
          xx<-st_difference(st_make_valid(xx),st_make_valid(yy[k]))
          xx<-subset(xx,st_geometry_type(xx)=='MULTIPOLYGON' | st_geometry_type(xx)=='POLYGON')
          if(plt_lvl==2){
            print(paste('polygon type=',st_geometry_type(xx),'this cut type=',st_geometry_type(yy[k])))
            plot(xx,col = "pink", border = "blue")
          }
        }
        polygons$geometry[i] <-st_make_valid(xx)
        if(plt_lvl!=0){
          plot(this_plyg$geometry,col = "blue", border = "lightgreen")
          plot(polygons$geometry[i],col = "red", border = "lightgreen", add=TRUE)
        }
      }
    }
  }
  polygons
}






get_county_hh_vars<-function(carb_output,hh_choice,htd,hud_inc=0,hud_size=0){
#
# read in excel output file that has some data we need and we will store the calculated values in
#
  hh_census_vars<-wb_read(carb_output,sheet='hh_census_vars')
  state=c("06")
  county_hh_data<-get_county_acs(hh_census_vars,state)
  if(hh_choice=='ami'){
    cbsa_hh_data<-get_cbsa_acs(hh_census_vars,state)
    county_shp<-read_sf(dsn = "C:/Users/pmh/carb/tiger", layer = "tl_2023_ca_county")
    county_shp <- st_drop_geometry(county_shp)
    names(county_shp)
    cnty_cbsa<-as.data.frame(subset(county_shp,county_shp$in_cbsa, select = c(GEOID,cbsa)))
    cbsas<-unique(cnty_cbsa$cbsa)
    for(c in 1:length(cbsas)){
      this_cbsa<-cbsas[c]
      this_cbsa_data<-subset(cbsa_hh_data,cbsa_hh_data$GEOID==this_cbsa)
      these_counties<-subset(cnty_cbsa,cnty_cbsa$cbsa==this_cbsa, select=c(GEOID))
      for(i in 2:length(names(county_hh_data))){
        hhv<-names(county_hh_data)[i]
        county_hh_data[which(county_hh_data$GEOID %in% these_counties$GEOID),][[hhv]]<-this_cbsa_data[[hhv]]
      }
    }
  } else if(hh_choice=='state'){
    state_hh_data<-get_state_acs(hh_census_vars,state)
    for(i in 2:length(names(county_hh_data))){
      hhv<-names(county_hh_data)[i]
      county_hh_data[[hhv]]<-state_hh_data[[hhv]]
    }
  }else if(hh_choice %like% 'hud%'){
    county_hh_data<-get_hud_acs(carb_output,hh_census_vars,state,hud_inc,hud_size)
  }
  county_hh_data
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
  # the cbsa to county indecies are in the shape file
  #
  county_shp<-read_sf(dsn = "C:/Users/pmh/carb/tiger", layer = "tl_2023_ca_county")
  county_shp<-st_drop_geometry(county_shp)
  cbsas<-unique(county_shp$cbsa)
  rm(county_shp)
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

overwrite_model_hh_inputs<-function(htd,county_hh_data){
#
# this function overwrites the household variables for the independent variables
#
  for(c in 1:length(county_hh_data$GEOID)){
    cnty<-county_hh_data$GEOID[c]
    this_hhs<-subset(county_hh_data,county_hh_data$GEOID==cnty)
    for(i in 2:length(names(county_hh_data))){
      hhv<-names(county_hh_data)[i]
      hts_ind[which(hts_ind$GEOID %like% paste(cnty,'%',sep='')),][[hhv]]<-this_hhs[[hhv]]
    }
  }
  hts_ind
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
#
# this sets up the HUD household and gets the hh data by county
#
get_hud_acs<-function(carb_output,hh_census_vars,state,hud_inc,hud_size){
  #
  # first read in HUD household data from the spread sheet
  #
  hud_hhs_data<-wb_read(carb_output,sheet='hud_hhs')
  hud_hhs_data$GEOID<-paste('06',hud_hhs_data$county_fips,sep='')
  hud_hhs$prefix<-c('ELI',
                    'l50',
                    'l80')
  hud_hh <-paste(hud_hhs$prefix[hud_inc],'_',hud_size,sep='')
  hh_workers<-paste('hh_by_workers_',hud_size,'person',sep='')
  if(hud_size>=4){
    hh_workers<-paste('hh_by_workers_',hud_size,'person_plus',sep='')
  }
  hud_hh_data<-subset(hud_hhs_data,select=c('GEOID',hud_hh))
  hud_hh_data$hh_size<-hud_size
  #
  # get acs data
  #
  workers_by_hh_size<-subset(hh_census_vars,
                             (substring(hh_census_vars$var_name,1,nchar(hh_workers))== hh_workers) | 
                               (hh_census_vars$var_name %in% c('total_workers','commuters_total')))
  county_dat_2023 <- get_acs(geography = "county", variables = workers_by_hh_size$acs_var,
                            state =state,geometry = FALSE, year = 2023)
  acs_2023<-{}
  acs_2023$GEOID<-unique(county_dat_2023$GEOID)
  names(county_dat_2023)
  for(i in 1:length(workers_by_hh_size$acs_var)){
    sbset<-subset(county_dat_2023,select=c('GEOID','estimate'),county_dat_2023$variable==workers_by_hh_size$acs_var[i])
    sbset$estimate<-as.numeric(sbset$estimate)
    names(sbset)[names(sbset) == "estimate"] <-workers_by_hh_size$var_name[i]
    a<-as.data.frame(merge(acs_2023,sbset,by='GEOID'))
    acs_2023<-a
  }
  names(acs_2023)
  head(acs_2023)
  acs_2023$wphh<-0
  for(i in 5:(4+hud_size)){
    n<-(as.numeric(i)-4)
    acs_2023$wphh <- acs_2023$wphh + (n*acs_2023[[workers_by_hh_size$var_name[i]]])/acs_2023[[workers_by_hh_size$var_name[3]]]
  }
  acs_2023$commuter_per_hh<-acs_2023$wphh*acs_2023$commuters_total/acs_2023$total_workers 
  head(acs_2023)
  cphh<-subset(acs_2023,select=c('GEOID','commuter_per_hh'))
  hud_hh_data<-as.data.frame(left_join(hud_hh_data,cphh,by='GEOID'))
  names(hud_hh_data)<-c('GEOID','median_hh_income','avg_hh_size','avg_commuters')
  hud_hh_data
}

predict_model<-function(dep_vars,hts_ind,modeled,start,end){
  for(i in start:end){
    #    for(i in 1:length(dep_vars[,1])){
    ed<-dep_vars[i,1]
    hcoef <- as.data.frame(wb_read(carb_input,sheet=paste(ed,'_coef',sep='')))
    names(hcoef)<-c('var_name','estimate','std_err','t_value','p_value')
    thcf<-hcoef$estimate
    thcf_vars<-hcoef$var_name
    thhtd<-hts_ind[thcf_vars]
    head(thhtd)
    j=1
    for(j in 1:length(thhtd[,1])){
      x<-unlist(thhtd[j,])
      y<-x%*%thcf
      if(dep_vars$fit_type[i]=='quasibinomial'){
        modeled[j,ed]<-exp(y)
        modeled[j,ed]<-round((modeled[j,ed])/(1+(modeled[j,ed])),4)
      }
      else if(dep_vars$transformation[i]=='log'){
        modeled[j,ed]<-round(exp(y),4)
      } else {
        modeled[j,ed]<-round(y,4)
      }
    }
    print(paste("Just finished modeling",ed))
  }
  modeled
}

get_prices<-function(modeled){
  #
  # Now run the cost of each transportation component
  #
  # first get the cost of each
  #  gas prices/electricity/diesel
  # first get the gas price regions assigned to each block group
  #
  blkgrp_shp<-read_sf(dsn = "C:/Users/pmh/carb/tiger", layer = "tl_2023_06_bg")
  names(blkgrp_shp)
  blkgrp_gas_region<-subset(st_drop_geometry(blkgrp_shp),select=c('GEOID','gas_region'))
  modeled<-as.data.frame(left_join(modeled,st_drop_geometry(blkgrp_gas_region),by='GEOID'))
  modeled$gas_region<-toupper(modeled$gas_region)
  head(modeled)
  rm('blkgrp_shp','blkgrp_gas_region')
  #
  # get eia directory and metadata it you need it
  #
  #eia_dir("petroleum/pri")
  #eia_metadata("petroleum/pri/gnd")
  #
  # gas prices for regular gas only san francisco and los angeles have differnt prices for CA
  #
  eia_set_key('B9481CDAC0731D687B3247FC3C12E20D')
  gas_prices<-eia_data(dir='petroleum/pri/gnd',  data = "value",
                       facets = list(product='EPMR',duoarea=c('SCA','Y05LA','Y05SF')),
                       freq = "annual",
                       start = "2023",end="2024",
                       sort = list(cols = "duoarea", order = "asc"),)
  duas<-unique(gas_prices$duoarea)
  modeled$gas_price<-0.0
  for(c in 1:length(duas)){
    dua<-duas[c]
    this_price<-gas_prices$value[c]
    modeled[which(modeled$gas_region %like% paste('%',dua,'%',sep='')),]['gas_price']<-this_price
  }
  #
  # now get diesel prices in California - for now there is only one???
  #
  diesel_prices<-eia_data(dir='petroleum/pri/gnd',  data = "value",
                          facets = list(product='EPD2D',duoarea=c('SCA')),
                          freq = "annual",
                          start = "2023",end="2024",
                          sort = list(cols = "duoarea", order = "asc"),)
  modeled$diesel_price<-as.numeric(diesel_prices$value)
  rm(diesel_prices)
  #
  # get electricity prices in California - for now there is only one???
  #
  electric_prices<-eia_data(
    dir = "electricity/retail-sales",
    data = "price",
    facets = list(sectorid = c("RES"), stateid = "CA"),
    freq = "annual",
    start = "2023",end="2023"
  )
  modeled$electric_price<-as.numeric(electric_prices$price)/100.0
  rm(electric_prices)
  #
  #  now auto ownership costs
  #
  inflation_from_2010<-wb_read(carb_output,sheet='inflation_from_2010')
  inflation_from_2010<-subset(inflation_from_2010,inflation_from_2010$year==2023)
  #
  # adjust the 2010 costs from the LAI research to 2023 dollars
  #
  auto_cost_service_flow<-wb_read(carb_output,sheet='auto_cost_service_flow')
  auto_cost_service_flow$service_flow_value<-auto_cost_service_flow$service_flow_value*inflation_from_2010$auto_inflation
  auto_cost_service_flow$finance_cost <-auto_cost_service_flow$finance_cost*inflation_from_2010$auto_inflation
  auto_cost_service_flow$fixed_ownership <-auto_cost_service_flow$fixed_ownership*inflation_from_2010$auto_inflation
  auto_cost_service_flow$drivability <- auto_cost_service_flow$drivability*inflation_from_2010$cpi
  auto_cost_service_flow$fuel <- auto_cost_service_flow$fuel*inflation_from_2010$cpi
  auto_cost_service_flow$max_income <- auto_cost_service_flow$max_income*inflation_from_2010$cpi
  for(i in 1:length(modeled$median_hh_income)){
    bin<-min(which(modeled$median_hh_income[i]<auto_cost_service_flow$max_income))
    modeled$auto_cost_income_bin[i]<-bin
    modeled$auto_own_cost_per_veh<-auto_cost_service_flow$service_flow_value[bin]+
      auto_cost_service_flow$finance_cost[bin]+
      auto_cost_service_flow$fixed_ownership[bin]
    modeled$operation_fraction<-(1.0+
                                   (auto_cost_service_flow$drivability[bin]/auto_cost_service_flow$fuel[bin]))
  }
  #
  # now get the average fuel per mile for each fuel - gas, diesel and electricity
  #
  data_prep<-wb_load("C:/Users/pmh/carb/data_prep.xlsx")
  vmt_normalization<-as.data.frame(wb_read(data_prep,sheet='vmt_normalization'))
  c<-1
  modeled$gas_mpg_per_frac<-modeled$hybrid_mpg_per_frac<-modeled$diesel_mpg_per_frac<-modeled$ev_mpkwh_per_frac<-modeled$hybrid_mpkwh_per_frac<-0
  for(c in 1:length(vmt_normalization$GEOID)){
    geoid<-vmt_normalization$GEOID[c]
    this_gas_mpg<-vmt_normalization$gas_mpg[c]/vmt_normalization$gas_frac_vmt[c]
    this_hybrid_mpg<-vmt_normalization$hybrid_mpg[c]/vmt_normalization$hybrid_frac_vmt[c]
    this_diesel_mpg<-vmt_normalization$diesel_mpg[c]/vmt_normalization$diesel_frac_vmt[c]
    this_ev_mpkwh<-vmt_normalization$ev_mpkwh[c]/vmt_normalization$ev_frac_vmt[c]
    this_hybrid_mpkwh<-vmt_normalization$hybrid_mpkwh[c]/vmt_normalization$hybrid_frac_vmt[c]
    modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['gas_mpg_per_frac']<-this_gas_mpg
    modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['hybrid_mpg_per_frac']<-this_hybrid_mpg
    modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['diesel_mpg_per_frac']<-this_diesel_mpg
    modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['ev_mpkwh_per_frac']<-this_ev_mpkwh
    modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['hybrid_mpkwh_per_frac']<-this_hybrid_mpkwh
  }
  modeled
}


