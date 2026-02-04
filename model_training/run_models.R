library(openxlsx2)
library(sf)
library(dplyr)
library(DescTools)
#library(eia)
#
# read in the modeling input and output xlsx file
#
carb_input<-wb_load("./excel_files/carb_ht_model.xlsx")
carb_output<-wb_load("./excel_files/carb_ht_outputs.xlsx")
#
# get the data
#
htd <- wb_read(carb_input,sheet='fitting_data')
#
# load in the names of all the ACS data
#
for(i in 2:length(names(htd))){
  htd[[names(htd)[i]]]<-as.numeric(htd[[names(htd)[i]]])
}
htd[is.na(htd)]<-as.numeric("")
head(htd)
#
# get dependent, independent and household variable names
#
vars <- as.data.frame(wb_read(carb_input,sheet='variable_definitions'))
dep_vars<-as.data.frame(subset(vars,vars$var_type == 'dependent',select=c('variable','transformation','fit_type')))
ind_vars <- as.data.frame(subset(vars,vars$var_type == 'independent',select=c('variable','transformation','fit_type')))
hh_vars<- (subset(vars,vars$fit_type == 'hh',select=c('variable','formula')))
#
# now set up and pull just the independent variables
#
names(htd)
hts_ind<-htd[c('GEOID',ind_vars$variable)]
hts_ind["(Intercept)"]<-1.0
head(hts_ind)
#
# ask user what household to run
#
hhs<-{}
hhs$options <- c("Local Household",
                 "Typical County Household", 
                 "Typical Metro/Micro/County Household", 
                 "Typical State Household")
hhs$prefix<-c('local',
              'county',
              'ami',
              'state')
choice <- select.list(
  choices = hhs$options,
  title = 'Choose the household:',
  multiple = FALSE ,
  graphics = TRUE
)
hud_inc<-0
hud_size<-0
if(choice==5){
  hud_hhs<-{}
  hud_hhs$income_options <- c("Extremely Low Income",
                              "Low Income", 
                              "Moderate Income")
  hud_inc <- menu(hud_hhs$income_options,title='Choose the HUD income level:') 
  hud_size <- menu(1:8,title='Choose Number of People in the household:')
  hud_hhs$prefix<-c('ELI',
                    'l50',
                    'l80')
  hud_hh <-paste(hud_hhs$prefix[hud_inc],'_',hud_size,sep='')
  hhs$prefix[choice]<-paste('hud_',hud_hh,sep='')
}
#
# build the hh inputs into the modeling data
#
if(choice=='Local Household'){
  modeled<-hts_ind[c("GEOID",hh_vars[,1])]
} else{
  county_hh_data<-get_county_hh_vars(carb_output,hhs,choice,htd,hud_inc,hud_size)
  modeled<-overwrite_modeled_hh_vars(htd,county_hh_data)
  hts_ind<-overwrite_model_hh_inputs(htd,county_hh_data)
}

head(hts_ind)

blkgrp_shp<-read_sf(dsn = "./tiger/", layer = "ca_blkgrps")
model_inputs_geo<-merge(hts_ind,subset(blkgrp_shp,select=c('GEOID','geometry')))

st_write(model_inputs_geo, paste("./tiger/",hhs$prefix[choice],"model_inputs_geo.geojson",sep=""),append=FALSE, driver = "GeoJSON")
#
# we will need fraction of renters to calculate overall housing cost
#
modeled$frac_rent_hu<-htd$frac_rent_hu
#
# Now calculate the model for all rows if possible
#
for(i in 1:length(dep_vars[,1])){
  ed<-dep_vars[i,1]
  hcoef <- as.data.frame(wb_read(carb_input,sheet=paste(ed,'_coef',sep='')))
  names(hcoef)<-c('var_name','estimate','std_err','t_value','p_value')
  thcf<-hcoef$estimate
  thcf_vars<-hcoef$var_name
  thhtd<-hts_ind[thcf_vars]
  head(thhtd)
  j=1
  for(j in 1:length(thhtd[,1])){
    if(dep_vars$fit_type[i]=='quasibinomial'){
      modeled[j,ed]<-exp(sum(thhtd[j,]*thcf))
      modeled[j,ed]<-round((modeled[j,ed])/(1+(modeled[j,ed])),4)
    }
    else if(dep_vars$transformation[i]=='log'){
      modeled[j,ed]<-round(exp(sum(thhtd[j,]*thcf)),4)
    } else {
      modeled[j,ed]<-round(sum(thhtd[j,]*thcf),4)
    }
  }
  print(paste("Just finished modeling",ed))
}
head(modeled)
#
# Now run the cost of each transportation component
#
# first get the cost of each
#  gas prices/electricity/diesel
# first get the gas price regions assigned to each block group
#
blkgrp_gas_region<-as.data.frame(wb_read(data_prep,sheet='gas_blkgrp_regions'))
modeled<-as.data.frame(left_join(modeled,st_drop_geometry(blkgrp_gas_region),by='GEOID'))
modeled$gas_region<-toupper(modeled$gas_region)
head(modeled)
rm('blkgrp_shp','blkgrp_gas_region')
eia_set_key('B9481CDAC0731D687B3247FC3C12E20D')
#
# get eia directory and metadata it you need it
#
#eia_dir("petroleum/pri")
#eia_metadata("petroleum/pri/gnd")
#
# gas prices for regular gas only san francisco and los angeles have differnt prices for CA
#
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
rm(gas_prices)
modeled$gas_region <- NULL
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
  modeled$vmt_price_per_mile<-(1.0+
                                 (auto_cost_service_flow$drivability[bin]/auto_cost_service_flow$fuel[bin]))
}
modeled$gas_region<-NULL
modeled$auto_cost_income_bin <-NULL

head(modeled)
#
# now get the average fuel per mile for each fuel - gas, diesel and electricity
#
data_prep<-wb_load("./excel_files/data_prep.xlsx")
vmt_normalization<-as.data.frame(wb_read(data_prep,sheet='vmt_normalization'))
names(vmt_normalization)
c<-1
modeled$gas_mpg_per_frac<-modeled$hybrid_mpg_per_frac<-modeled$diesel_mpg_per_frac<-modeled$ev_mpkwh_per_frac<-modeled$hybrid_mpkwh_per_frac<-0
for(c in 1:length(vmt_normalization$GEOID)){
  geoid<-vmt_normalization$GEOID[c]
  this_gas_mpg<-vmt_normalization$gasoline_mpg[c]/vmt_normalization$gasoline_frac_vmt[c]
  this_hybrid_mpg<-vmt_normalization$plug_in_hybrid_mpg[c]/vmt_normalization$plug_in_hybrid_frac_vmt[c]
  this_diesel_mpg<-vmt_normalization$diesel_mpg[c]/vmt_normalization$diesel_frac_vmt[c]
  this_ev_mpkwh<-vmt_normalization$electricity_mpkwh[c]/vmt_normalization$electricity_frac_vmt[c]
  this_hybrid_mpkwh<-vmt_normalization$plug_in_hybrid_mpkwh[c]/vmt_normalization$plug_in_hybrid_frac_vmt[c]
  modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['gas_mpg_per_frac']<-this_gas_mpg
  modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['hybrid_mpg_per_frac']<-this_hybrid_mpg
  modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['diesel_mpg_per_frac']<-this_diesel_mpg
  modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['ev_mpkwh_per_frac']<-this_ev_mpkwh
  modeled[which(modeled$GEOID %like% paste(geoid,'%',sep='')),]['hybrid_mpkwh_per_frac']<-this_hybrid_mpkwh
}
#
# calculate auto costs
#
modeled$auto_own_cost<-as.numeric(modeled$auto_own_cost_per_veh)*as.numeric(modeled$autos_per_hh)
#
# now vmt costs
#
modeled$vmt_cost_per_mile<-
  ((as.numeric(modeled$gas_price)/as.numeric(modeled$gas_mpg_per_frac)) + 
     (as.numeric(modeled$gas_price)/as.numeric(modeled$hybrid_mpg_per_frac)) +
     (as.numeric(modeled$diesel_price)/as.numeric(modeled$diesel_mpg_per_frac)) +
     (as.numeric(modeled$electric_price)/as.numeric(modeled$ev_mpkwh_per_frac)) +     
     (as.numeric(modeled$electric_price)/as.numeric(modeled$hybrid_mpkwh_per_frac)))
modeled$vmt_cost<-as.numeric(modeled$vmt_per_hh)*as.numeric(modeled$vmt_price_per_mile)*
  modeled$vmt_cost_per_mile
#
# clean up the vehicle pricing and cost calculations 
#
modeled$gas_price<-modeled$diesel_price<-modeled$electric_price<-
  modeled$auto_own_cost_per_veh<-modeled$gas_mpg<-modeled$diesel_mpg<-modeled$mpkwh<-
  modeled$hybrid_mpkwh_per_frac<-modeled$ev_mpkwh_per_frac<-modeled$gas_mpg_per_frac<-
  modeled$diesel_mpg_per_frac<-modeled$hybrid_mpg_per_frac<-modeled$vmt_price_per_mile<-
  modeled$vmt_cost_per_mile <-NULL
#
# now transit costs
#
alpha_beta<-as.data.frame(wb_read(data_prep,sheet='transit_alpha_beta'))
modeled<-as.data.frame(left_join(modeled,alpha_beta,by='GEOID'))
modeled[which(is.na(modeled$alpha)),]['alpha']<-as.numeric(0)
modeled[which(is.na(modeled$beta)),]['beta']<-as.numeric(0)
modeled$transit_cost<- (as.numeric(modeled$beta) * as.numeric(modeled$frac_transit_j2w) * modeled$avg_commuters )
modeled$transit_trips_<- (as.numeric(modeled$alpha) * as.numeric(modeled$frac_transit_j2w) * modeled$avg_commuters )
#
# clean up 
#
modeled$alpha<-modeled$beta<-NULL
#
# calculate h and h and h+t
#
modeled$t_cost<-modeled$auto_own_cost+modeled$vmt_cost+modeled$transit_cost
modeled$t<-modeled$t_cost/modeled$median_hh_income
modeled$h_rent<-as.numeric(modeled$median_gross_rent)*12/modeled$median_hh_income
modeled$h_own<-as.numeric(modeled$median_smoc_mortgage)*12/modeled$median_hh_income
modeled$h<-((1-as.numeric(modeled$frac_transit_j2w))*modeled$h_own)+((as.numeric(modeled$frac_transit_j2w))*modeled$h_rent)
modeled$ht<-modeled$h+modeled$t
head(modeled)
hist(modeled$h)
hist(modeled$t)
hist(modeled$ht)
plot(modeled$h,modeled$t)

this_sheet<-paste(hhs$prefix[choice],"_hh_model",sep='')
carb_output$remove_worksheet(this_sheet)
carb_output$add_worksheet(this_sheet)
carb_output$add_data(paste(this_sheet),as.data.frame(modeled),colNames = TRUE)

wb_save(carb_output,file="./excel_files/carb_ht_outputs.xlsx",overwrite = TRUE)

blkgrps<-read_sf(dsn = "./tiger/mtc/", 
                 layer = "blkgrps")
names(blkgrps)[1]<-'GEOID'
modeled<-merge(modeled,subset(blkgrps, select = c('GEOID','lacres','geometry')))
#
# save shape file 
#
write_sf(modeled, paste("./tiger/",this_sheet,".shp",sep=''))

