#
# this is a script that is a library of functions useful for data from various sources
#
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
  acs_data<-get_acs(geography = acs_geo, variables = acs_var,
                                          state=st,county =cnts, geometry = FALSE, year = yr)
  names(acs_data)[4]<-var_name
  acs_data<-subset(acs_data,select=c(ndx,var_name))
  acs_data
}

get_county_hh_vars<-function(carb_output,hh_choice,htd,hud_inc=0,hud_size=0){
#
# read in excel output file that has some data we need and we will store the 
#  calculated values in
#
  hh_census_vars<-wb_read(carb_output,sheet='hh_census_vars')
  state=c("06")
  county_hh_data<-get_county_acs(hh_census_vars,state)
  if(hh_choice=='ami'){
    cbsa_hh_data<-get_cbsa_acs(hh_census_vars,state)
    county_gpkg<-st_read(dsn = "./gis/counties.gpkg")
    county_gpkg <- st_drop_geometry(county_gpkg)
    names(county_gpkg)
    cnty_cbsa<-as.data.frame(subset(county_gpkg,county_gpkg$in_cbsa, select = c(GEOID,cbsa)))
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

