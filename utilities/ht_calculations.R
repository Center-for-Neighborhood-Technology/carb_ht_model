#
# get the current year(s) 
#
source("./utilities/current_years.R")

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

predict_model<-function(dep_vars,hts_ind,modeled,start=1,end=5){
  carb_ht_model<-wb_load("./excel_files/carb_ht_model.xlsx")
  data_prep<-wb_load("./excel_files/data_prep.xlsx")
  i<-5
  for(i in start:end){
    #    for(i in 1:length(dep_vars[,1])){
    ed<-dep_vars[i,1]
    hcoef <- as.data.frame(wb_read(carb_ht_model,sheet=paste(ed,'_coef',sep='')))
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
  data_prep<-wb_load("./excel_files/data_prep.xlsx")
  #
  # Now run the cost of each transportation component
  #
  # first get the cost of each
  #  gas prices/electricity/diesel
  # first get the gas price regions assigned to each block group
  #
  blkgrp_gas_region<-as.data.frame(wb_read(data_prep,sheet='gas_blkgrp_regions'))
  names(blkgrp_gas_region)[1]<-'geoid'
  modeled<-as.data.frame(left_join(modeled,blkgrp_gas_region,by='geoid'))
  modeled$gas_region<-toupper(modeled$gas_region)
  rm('blkgrp_gas_region')
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
                       start = "current_acs_year",end="current_acs_year",
                       sort = list(cols = "duoarea", order = "asc"),)
  duas<-unique(gas_prices$duoarea)
  modeled$gas_price<-0.0
  dulst<-unique(modeled$gas_region)
  for(c in 1:length(duas)){
    dua<-duas[c]
    this_price<-gas_prices$value[c]
    inhere<-dulst %like% paste('%',dua,'%',sep='')
    goforit<-FALSE
    for(inh in inhere){goforit<-goforit || inh} 
    if(goforit){
      modeled[which(modeled$gas_region %like% paste('%',dua,'%',sep='')),]['gas_price']<-this_price
    }
  }
  #
  # now get diesel prices in California - for now there is only one???
  #
  diesel_prices<-as.data.frame(eia_data(dir='petroleum/pri/gnd',  data = "value",
                          facets = list(product='EPD2D',duoarea=c('SCA')),
                          freq = "annual",
                          start = "current_acs_year",end="current_acs_year",
                          sort = list(cols = "duoarea", order = "asc"),))
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
    start = "current_acs_year",end="current_acs_year"
  )
  modeled$electric_price<-as.numeric(electric_prices$price)/100.0
  rm(electric_prices)
  #
  #  now auto ownership costs
  #
  inflation_from_2010<-wb_read(data_prep,sheet='inflation_from_2010')
  inflation_from_2010<-subset(inflation_from_2010,inflation_from_2010$year==current_acs_year)
  #
  # adjust the 2010 costs from the LAI research to current_acs_year dollars
  #
  auto_cost_service_flow<-wb_read(data_prep,sheet='auto_cost_service_flow')
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
  vmt_normalization<-as.data.frame(wb_read(data_prep,sheet='vmt_normalization'))
  names(vmt_normalization)[2]<-'geoid'
  c<-1
  modeled$gas_mpg_per_frac<-
    modeled$hybrid_mpg_per_frac<-
    modeled$diesel_mpg_per_frac<-
    modeled$ev_mpkwh_per_frac<-
    modeled$hybrid_mpkwh_per_frac<-0
  vmt_normalization<-subset(vmt_normalization,vmt_normalization$geoid %in% unique(substr(modeled$geoid,1,5)))
  names(vmt_normalization)
  for(c in 1:length(vmt_normalization$geoid)){
    geoid<-vmt_normalization$geoid[c]
    this_gas_mpg<-vmt_normalization$gasoline_mpg[c]/vmt_normalization$gasoline_frac_vmt[c]
    this_hybrid_mpg<-vmt_normalization$plug_in_hybrid_mpg[c]/vmt_normalization$plug_in_hybrid_frac_vmt[c]
    this_diesel_mpg<-vmt_normalization$diesel_mpg[c]/vmt_normalization$diesel_frac_vmt[c]
    this_ev_mpkwh<-vmt_normalization$electricity_mpkwh[c]/vmt_normalization$electricity_frac_vmt[c]
    this_hybrid_mpkwh<-vmt_normalization$plug_in_hybrid_mpkwh[c]/vmt_normalization$plug_in_hybrid_frac_vmt[c]
    modeled[which(modeled$geoid %like% paste(geoid,'%',sep='')),]['gas_mpg_per_frac']<-this_gas_mpg
    modeled[which(modeled$geoid %like% paste(geoid,'%',sep='')),]['hybrid_mpg_per_frac']<-this_hybrid_mpg
    modeled[which(modeled$geoid %like% paste(geoid,'%',sep='')),]['diesel_mpg_per_frac']<-this_diesel_mpg
    modeled[which(modeled$geoid %like% paste(geoid,'%',sep='')),]['ev_mpkwh_per_frac']<-this_ev_mpkwh
    modeled[which(modeled$geoid %like% paste(geoid,'%',sep='')),]['hybrid_mpkwh_per_frac']<-this_hybrid_mpkwh
  }
  modeled
}

calculate_costs_ht<-function(modeled) {
  #
  # get the price of everything
  #
  modeled<-get_prices(modeled)
  #
  # now vmt costs
  #
  modeled$vmt_cost_per_mile<-
    ((as.numeric(modeled$gas_price)/as.numeric(modeled$gas_mpg_per_frac)) + 
       (as.numeric(modeled$gas_price)/as.numeric(modeled$hybrid_mpg_per_frac)) +
       (as.numeric(modeled$diesel_price)/as.numeric(modeled$diesel_mpg_per_frac)) +
       (as.numeric(modeled$electric_price)/as.numeric(modeled$ev_mpkwh_per_frac)) +     
       (as.numeric(modeled$electric_price)/as.numeric(modeled$hybrid_mpkwh_per_frac)))
  modeled$vmt_cost<-(as.numeric(modeled$vmt_per_hh)*
                       as.numeric(modeled$operation_fraction)*modeled$vmt_cost_per_mile)
  #
  # calculate auto costs
  #
  modeled$auto_own_cost<-as.numeric(modeled$auto_own_cost_per_veh)*as.numeric(modeled$autos_per_hh)
  #
  # clean up the vehicle pricing and cost calculations 
  #
  modeled$gas_price<-modeled$diesel_price<-modeled$electric_price<-
    modeled$auto_own_cost_per_veh<-modeled$gas_mpg<-modeled$diesel_mpg<-modeled$mpkwh<-
    modeled$hybrid_mpkwh_per_frac<-modeled$ev_mpkwh_per_frac<-modeled$gas_mpg_per_frac<-
    modeled$diesel_mpg_per_frac<-modeled$hybrid_mpg_per_frac<-modeled$operation_fraction<-
    modeled$vmt_cost_per_mile <-NULL
  #
  # now transit costs
  #
  data_prep<-wb_load("./excel_files/data_prep.xlsx")
  alpha_beta<-as.data.frame(wb_read(data_prep,sheet='transit_alpha_beta'))
  names(alpha_beta)[1]<-'geoid'
  modeled<-as.data.frame(left_join(modeled,alpha_beta,by='geoid'))
  modeled[which(is.na(modeled$alpha)),]['alpha']<-as.numeric(0)
  modeled[which(is.na(modeled$beta)),]['beta']<-as.numeric(0)
  modeled$transit_cost<- (as.numeric(modeled$beta) * as.numeric(modeled$frac_transit_j2w) * modeled$avg_commuters )
  modeled$transit_trips<- (as.numeric(modeled$alpha) * as.numeric(modeled$frac_transit_j2w) * modeled$avg_commuters )
  #
  # clean up 
  #
  names(modeled)
  modeled$alpha<-modeled$beta<-modeled$gas_region<-modeled$auto_cost_income_bin<-NULL
  #
  # calculate h and h and h+t
  #
  modeled$t_cost<-modeled$auto_own_cost+modeled$vmt_cost+modeled$transit_cost
  modeled$t<-as.numeric(modeled$t_cost/modeled$median_hh_income)
  modeled$h_rent<-as.numeric(modeled$median_gross_rent)*12/modeled$median_hh_income
  modeled$h_own<-as.numeric(modeled$median_smoc_mortgage)*12/modeled$median_hh_income
  modeled$h<-((1-as.numeric(modeled$frac_transit_j2w))*modeled$h_own)+((as.numeric(modeled$frac_transit_j2w))*modeled$h_rent)
  modeled$ht<-as.numeric(modeled$h+modeled$t)
  head(modeled)
  hist(modeled$h)
  hist(modeled$t)
  hist(modeled$ht)
  plot(modeled$h,modeled$t)
  modeled
}

#
# this sets up the HUD household and gets the hh data by county
#
get_hud_acs<-function(carb_output,hh_census_vars,state,hud_inc,hud_size){
  #
  # first read in HUD household data from the spread sheet
  #
  hud_hhs_data<-wb_read(carb_output,sheet='hud_hhs')
  hud_hhs_data$geoid<-paste('06',hud_hhs_data$county_fips,sep='')
  hud_hhs$prefix<-c('ELI',
                    'l50',
                    'l80')
  hud_hh <-paste(hud_hhs$prefix[hud_inc],'_',hud_size,sep='')
  hh_workers<-paste('hh_by_workers_',hud_size,'person',sep='')
  if(hud_size>=4){
    hh_workers<-paste('hh_by_workers_',hud_size,'person_plus',sep='')
  }
  hud_hh_data<-subset(hud_hhs_data,select=c('geoid',hud_hh))
  hud_hh_data$hh_size<-hud_size
  #
  # get acs data
  #
  workers_by_hh_size<-subset(hh_census_vars,
                             (substring(hh_census_vars$var_name,1,nchar(hh_workers))== hh_workers) | 
                               (hh_census_vars$var_name %in% c('total_workers','commuters_total')))
  county_dat_current <- get_acs(geography = "county", variables = workers_by_hh_size$acs_var,
                             state =state,geometry = FALSE, year = current_acs_year,output='wide')
  acs_current<-county_dat_current[,c('GEOID',paste(workers_by_hh_size$acs_var,'E',sep=''))]
  names(acs_current)<-c('geoid',workers_by_hh_size$var_name)
  names(acs_current)
  head(acs_current)
  acs_current$wphh<-0
  i<-5
  for(i in 5:min((4+hud_size),7)){
    n<-(as.numeric(i)-4)
    acs_current$wphh <- acs_current$wphh + (n*acs_current[[workers_by_hh_size$var_name[i]]])/acs_current[[workers_by_hh_size$var_name[3]]]
  }
  acs_current$commuter_per_hh<-acs_current$wphh*acs_current$commuters_total/acs_current$total_workers 
  head(acs_current)
  cphh<-subset(acs_current,select=c('geoid','commuter_per_hh'))
  hud_hh_data<-as.data.frame(left_join(hud_hh_data,cphh,by='geoid'))
  names(hud_hh_data)<-c('geoid','median_hh_income','avg_hh_size','avg_commuters')
  hud_hh_data
}

overwrite_model_hh_inputs<-function(htd,county_hh_data){
  #
  # this function overwrites the household variables for the independent variables
  #
  for(c in 1:length(county_hh_data$geoid)){
    cnty<-county_hh_data$geoid[c]
    this_hhs<-subset(county_hh_data,county_hh_data$geoid==cnty)
    for(i in 2:length(names(county_hh_data))){
      hhv<-names(county_hh_data)[i]
      hts_ind[which(hts_ind$geoid %like% paste(cnty,'%',sep='')),][[hhv]]<-this_hhs[[hhv]]
    }
  }
  hts_ind
}


