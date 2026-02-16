#
# clear all memory before starting
#
rm(list=ls())
#
# read in the modeling inputs xlsx file
#
input_data<-wb_load("./excel_files/data_prep.xlsx")
#
# get the data
#
modeled_vmt <- wb_read(input_data,sheet='vmt')
modeled_vmt <- subset(modeled_vmt,select = c('GEOID','vmt_per_hh'))
acs_vars<- wb_read(input_data,sheet='blkgrp_acs_2023')
acs_vars<-subset(acs_vars,select=c('GEOID','households'))
modeled_vmt<-as.data.frame(left_join(modeled_vmt,acs_vars,by='GEOID'))
modeled_vmt$total_hh_vmt<-as.numeric(modeled_vmt$vmt_per_hh)*as.numeric(modeled_vmt$households)
for(i in 2:length(names(modeled_vmt))){
  modeled_vmt[[names(modeled_vmt[i])]]<-as.numeric(modeled_vmt[[names(modeled_vmt[i])]])
}
rm(acs_vars)
cnty_modeled_vmt<-as.data.frame(aggregate((modeled_vmt$total_hh_vmt), 
                                          by=list(substr(modeled_vmt$GEOID,1,5)), FUN=sum, na.rm=TRUE))
names(cnty_modeled_vmt)<-c("GEOID","total_modeled_vmt")
#
# read in excel output file that has some data we need and we will store the calculated values in
#
hh_census_vars<-wb_read(input_data,sheet='hh_census_vars')
states=c("CA")

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
acs_2023<-subset(acs_2023,select=c('GEOID','households','agg_hh_vehicles'))
cnty_modeled_vmt<-as.data.frame(left_join(cnty_modeled_vmt,acs_2023,by='GEOID'))
rm(acs_2023,county_dat_2023,hh_census_vars,states,sbset)
#
# Get the county estimate of VMT and Autos from the EMFAC Emissions Inventory
#
emfac_vmt <- wb_read(input_data,sheet='emfac_county_vmt',start_row = 9 )
#
# get county names and GEOID from tiger shape file
#
counties_shp<-read_sf(dsn = "./tiger", layer = "ca_counties_2023")
ca_counties<-subset(st_drop_geometry(counties_shp),select=c('GEOID','NAME'))
names(ca_counties)<-c('GEOID','Region')
emfac_vmt<-as.data.frame(left_join(ca_counties,emfac_vmt,by='Region'))
rm(counties_shp,ca_counties)
#
# now get total daily vmt from EMFAC data
# and the total number of vehicles
#
vt<-subset(emfac_vmt,select=c('GEOID','Region',"Vehicle Category","Fuel","Population","Total VMT"))
vmt_total<-as.data.frame(aggregate((vt$"Total VMT"), 
                                          by=list(vt$GEOID), FUN=sum, na.rm=TRUE))
names(vmt_total)<-c('GEOID','emfac_vmt')
veh_total<-as.data.frame(aggregate((vt$"Population"), 
                                   by=list(vt$GEOID), FUN=sum, na.rm=TRUE))
names(veh_total)<-c('GEOID','emfac_vehicles')

cnty_modeled_vmt<-as.data.frame(left_join(cnty_modeled_vmt,merge(vmt_total,veh_total,'GEOID'),by='GEOID'))
rm(vt,vmt_total,veh_total)
#
# now calculate the ratio of acs hh vehicles with the emfac vehicles
#
cnty_modeled_vmt$veh_ratio<-as.numeric(cnty_modeled_vmt$agg_hh_vehicles)/as.numeric(cnty_modeled_vmt$emfac_vehicles)
cnty_modeled_vmt$emfac_scaled_annual_hh_vmt<-cnty_modeled_vmt$emfac_vmt * cnty_modeled_vmt$veh_ratio * 365
cnty_modeled_vmt$vmt_scale_factor<-cnty_modeled_vmt$emfac_scaled_annual_hh_vmt/cnty_modeled_vmt$total_modeled_vmt
#
# now scale up each block group's VMT
#
modeled_vmt$cnty_geoid<-substr(modeled_vmt$GEOID,1,5)
cfctrs<-subset(cnty_modeled_vmt,select= c('GEOID','vmt_scale_factor'))
names(cfctrs)<-c('cnty_geoid','vmt_scale_factor')
modeled_vmt<-as.data.frame(left_join(modeled_vmt,cfctrs,by='cnty_geoid'))
modeled_vmt$normalized_vmt_per_hh<-modeled_vmt$vmt_per_hh * modeled_vmt$vmt_scale_factor
rm(cfctrs)
input_data$remove_worksheet("vmt")
input_data$add_worksheet("vmt")
input_data$add_data("vmt",as.data.frame(modeled_vmt),colNames = TRUE)
#
# now get vmt, mpg, fuel and energy consumption for various vehicles
#
mv<-subset(emfac_vmt,select=c('GEOID',"Vehicle Category","Fuel","Total VMT",'EVMT',
                              'Fuel Consumption',
                              'Energy Consumption'))
#
agg<-as.data.frame(aggregate((mv$"Total VMT"), 
                                   by=list(mv$GEOID,mv$Fuel), FUN=sum, na.rm=TRUE))
names(agg)<-c('GEOID','fuel','vmt')
#
a<-as.data.frame(aggregate((mv$"Fuel Consumption"), 
                                         by=list(mv$GEOID,mv$Fuel), FUN=sum, na.rm=TRUE))
names(a)<-c('GEOID','fuel','fuel_cons')
agg<-merge(agg,a,by=c('GEOID','fuel'))
#
a<-as.data.frame(aggregate((mv$"Energy Consumption"), 
                                         by=list(mv$GEOID,mv$Fuel), FUN=sum, na.rm=TRUE))
names(a)<-c('GEOID','fuel','electric_cons')
agg<-merge(agg,a,by=c('GEOID','fuel'))
#
# Loop through fuels and assign vmt etc
#
fuels<-unique(agg$fuel)
for(i in 1:length(fuels)){
  fuel<-fuels[i]
  f<-sub("-", "_",(sub(" ", "_", tolower(fuel))))
  vmt_col<-paste(f,'_vmt',sep='')
  frac_col<-paste(f,'_frac_vmt',sep='')
  mpg_col<-paste(f,'_mpg',sep='')
  fuel_col<-paste(f,'_cons',sep='')
  electric_col<-paste(f,'_electric_cons',sep='')
  mpkwh_col<-paste(f,'_mpkwh',sep='')
  these_veh<-agg[agg$fuel == fuel,]
  cnty_modeled_vmt<-left_join(cnty_modeled_vmt,
                              these_veh,
                              by='GEOID')
  cnty_modeled_vmt[vmt_col]<-cnty_modeled_vmt$vmt
  cnty_modeled_vmt[frac_col]<-cnty_modeled_vmt[vmt_col]/cnty_modeled_vmt$emfac_vmt
  cnty_modeled_vmt[fuel_col]<-cnty_modeled_vmt$fuel_cons
  cnty_modeled_vmt[electric_col]<-cnty_modeled_vmt$electric_cons
  cnty_modeled_vmt[mpg_col]<-0
  cnty_modeled_vmt[mpg_col][cnty_modeled_vmt[fuel_col]>0,]<-(cnty_modeled_vmt[vmt_col])/(cnty_modeled_vmt[fuel_col]*1000)
  cnty_modeled_vmt[mpkwh_col]<-0
  cnty_modeled_vmt[mpkwh_col][cnty_modeled_vmt[electric_col]>0,]<-(cnty_modeled_vmt[vmt_col])/(cnty_modeled_vmt[electric_col])
  cnty_modeled_vmt[names(agg)[2:length(names(agg))]]<-NULL
}
#
# add sheet of normalization data
#
input_data$remove_worksheet("vmt_normalization")
input_data$add_worksheet("vmt_normalization")
input_data$add_data("vmt_normalization",as.data.frame(cnty_modeled_vmt),colNames = TRUE, rowNames = TRUE,)

wb_save(input_data,file="./excel_files/data_prep.xlsx",overwrite = TRUE)

