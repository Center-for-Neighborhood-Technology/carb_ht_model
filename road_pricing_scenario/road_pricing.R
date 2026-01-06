library(openxlsx2)
library(dplyr)
library(sf)
library(DescTools)
library(rapportools)
#
# set the price per mile
#
price_per_mile<-0.04


#
# load in some utility functions
#
source("./utilities/choose_hhs.R")
source("./utilities/ht_calculations.R")
#
# get household
# 
hh<-{}
hh$prefix<-'local'
hh$name <-'Local Household'
hh$income <-0
hh$size <-0
#
# read in the modeling input and output xlsx file
#
carb_ht_model<-wb_load("./excel_files/carb_ht_model.xlsx")
data_prep<-wb_load("./excel_files/data_prep.xlsx")
#
# get the data
#
htd <- wb_read(carb_ht_model,sheet='fitting_data')
#
# make sure the fitting data is numeric
#
for(i in 2:length(names(htd))){
  suppressWarnings(htd[[names(htd)[i]]]<-as.numeric(htd[[names(htd)[i]]]))
}
#
# get dependent, independent and household variable names
#
vars <- as.data.frame(wb_read(carb_ht_model,sheet='variable_definitions'))
dep_vars<-as.data.frame(subset(vars,vars$var_type == 'dependent',select=c('variable','transformation','fit_type')))
ind_vars <- as.data.frame(subset(vars,vars$var_type == 'independent',select=c('variable','transformation','fit_type')))
hh_vars<- (subset(vars,vars$fit_type == 'hh',select=c('variable','formula')))
#
# now set up and pull just the independent variables including the intercept (value=1)
#
hts_ind<-htd[c('GEOID','households',ind_vars$variable)]
hts_ind["(Intercept)"]<-1.0
#
# build the hh inputs into the modeling data
#
modeled<-hts_ind[c("GEOID",'households',hh_vars[,1],'frac_rent_hu')]
names(modeled)[1]<-'geoid'
head(modeled)
head(hts_ind)
#
# eliminate block groups with no households, and with na for housing inputs
#
modeled<-subset(modeled,modeled$median_hh_income>2499 & modeled$median_hh_income<250000 & modeled$households>0)
modeled<-subset(modeled,complete.cases(modeled))
hts_ind<-subset(hts_ind,hts_ind$GEOID %in% modeled$geoid)
#
# get the price of everything
#
modeled<-get_prices(modeled)
#
# Now save the original values
#
d<-array(0,dim=c(5,5))
transit_boost<-1.0
hts_ind$bus_tci_og<-hts_ind$bus_tci
hts_ind$other_tci_og<-hts_ind$other_tci
modeled$gas_price_og<-modeled$gas_price
modeled$diesel_price_og<-modeled$diesel_price
#
# set the elasticity
#  
# according to Betty Deakin's paper elasticity is -0.1 for 
#   vmt against total price increase for driving 
#
vmt_elasticity<-(-0.1)
#
# for this scenario we have to remove the state excise tax on gas and diesel fuel
# https://cdtfa.ca.gov/taxes-and-fees/special-taxes-and-fees-tax-rates/fuel-taxes.htm
#
gas_tax_per_gallon<-as.numeric(0.612)
diesel_tax_per_gallon<-as.numeric(0.466)
#
# take the tax out of gas and diesel prices
#
modeled$gas_price<-as.numeric(modeled$gas_price_og) - gas_tax_per_gallon
modeled$diesel_price<-as.numeric(modeled$diesel_price_og)-diesel_tax_per_gallon
#
# now vmt costs og with taxes included
#
modeled$vmt_cost_per_mile_og<-
  ((as.numeric(modeled$gas_price_og)/as.numeric(modeled$gas_mpg_per_frac)) + 
     (as.numeric(modeled$gas_price_og)/as.numeric(modeled$hybrid_mpg_per_frac)) +
     (as.numeric(modeled$diesel_price_og)/as.numeric(modeled$diesel_mpg_per_frac)) +
     (as.numeric(modeled$electric_price)/as.numeric(modeled$ev_mpkwh_per_frac)) +     
     (as.numeric(modeled$electric_price)/as.numeric(modeled$hybrid_mpkwh_per_frac))) 
#
# loop five time to get a consistent result
#
for(q in 1:5){
  head(hts_ind$bus_tci)
  hts_ind$bus_tci<-as.numeric(hts_ind$bus_tci_og)*transit_boost
  if(min(hts_ind$bus_tci)<0) {hts_ind[which(hts_ind$bus_tci<0),]$bus_tci<-0.0}
  hts_ind$other_tci<-as.numeric(hts_ind$other_tci_og)*transit_boost
  if(min(hts_ind$other_tci)<0) {hts_ind[which(hts_ind$other_tci<0),]$other_tci<-0.0}
  head(hts_ind$bus_tci)
  modeled$bus_tci<-hts_ind$bus_tci
  modeled$other_tci<-hts_ind$other_tci
  modeled<-predict_model(dep_vars,hts_ind,modeled,5,5)
  #
  # calculate new vmt cost without ruc and transit improvements
  #
  if(q==1){
    modeled$vmt_cost_og<-as.numeric(modeled$vmt_per_hh)*
      as.numeric(modeled$operation_fraction)*
      modeled$vmt_cost_per_mile_og
    modeled$vmt_per_hh_og<-modeled$vmt_per_hh
  }
  #
  # road pricing will be 
  #  low end will increase cost by 25%
  #  high end will increase cost by 50%
  #
  #  assume avg mpg is 23.4 given $.612 per gallon that is .612/23.4 = $0.026/mile 
  #   so if we charge 0.026 * 1.25= 0.0327/mile
  #
  rev_fee<-array(0,dim=6)
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
    as.numeric(modeled$operation_fraction)*modeled$vmt_cost_per_mile)+
    as.numeric(modeled$vmt_per_hh)*price_per_mile
  modeled$vmt_reduction_frac<-1+(((modeled$vmt_cost/modeled$vmt_cost_og)-1)*vmt_elasticity)
  modeled$vmt_per_hh<-modeled$vmt_per_hh*modeled$vmt_reduction_frac
  modeled$vmt_cost<-as.numeric(modeled$vmt_per_hh)*as.numeric(modeled$operation_fraction)*modeled$vmt_cost_per_mile+
    as.numeric(modeled$vmt_per_hh)*price_per_mile
  head(modeled)
  #
  # examine the overall revenue
  #  and store it in an array
  #
  rev<-sum((modeled$vmt_cost-modeled$vmt_cost_og)*modeled$households)
  rev_fee[1]<-round(as.numeric(price_per_mile),4)
  rev_fee[2]<-round(as.numeric(rev))
  sbst<-subset(modeled,!is.na(modeled$vmt_per_hh))
  rev_fee[3]<-round(sum(sbst$vmt_per_hh*sbst$households)/sum(sbst$households))
  rev_fee[4]<-round(sum(sbst$vmt_per_hh_og*sbst$households)/sum(sbst$households))
  rev_fee[5]<-round(sum(sbst$vmt_per_hh*sbst$households))
  rev_fee[6]<-round(sum(sbst$vmt_per_hh_og*sbst$households))
  print('Cost, Revenue, Avg VMT, OG Ave, tot VMT, OG tot')
  print(rev_fee)

  current_op_maint_expend<-6066530217
  add_revenue<-rev_fee[2]
  transit_boost<-1+((.75*add_revenue)/current_op_maint_expend)
  mn<-mean(subset(modeled,!is.na(modeled$vmt_per_hh))$vmt_per_hh)
  mnb<-mean(subset(modeled,!is.na(modeled$bus_tci))$bus_tci)
  mno<-mean(subset(modeled,!is.na(modeled$other_tci))$other_tci)
  d[q,]<-c(transit_boost,mn,add_revenue,mnb,mno)
}
print(d)
modeled<-predict_model(dep_vars,hts_ind,modeled,1,4)
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
alpha_beta<-as.data.frame(wb_read(data_prep,sheet='transit_alpha_beta'))
names(alpha_beta)[1]<-'geoid'
modeled<-as.data.frame(left_join(modeled,alpha_beta,by='geoid'))
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
#
# save scenario in scenarios .xlsx file
#
road_price_scn<-wb_load("./excel_files/road_pricing_scenario.xlsx")

this_sheet<-paste(hh$prefix,"_ht_model_",round(100*price_per_mile,4),'cents',sep='')
if(this_sheet %in% wb_get_sheet_names(road_price_scn)){road_price_scn$remove_worksheet(this_sheet)}
road_price_scn$add_worksheet(this_sheet)
road_price_scn$add_data(paste(this_sheet),as.data.frame(modeled),colNames = TRUE)

wb_save(road_price_scn,file="./excel_files/road_pricing_scenario.xlsx",overwrite = TRUE)

