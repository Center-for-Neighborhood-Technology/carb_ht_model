#
# clear memory
#
rm(list=ls())
#
# load some utilities
#
source("./utilities/manage_packages.R")
source("./utilities/ht_calculations.R")
source("./utilities/get_data_functions.R")
#
# read in the modeling input and output xlsx file
#
carb_input<-wb_load("./excel_files/carb_ht_model.xlsx")
carb_output<-wb_load("./excel_files/carb_ht_outputs.xlsx")
data_prep<-wb_load("./excel_files/data_prep.xlsx")
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
hts_ind<-htd[c('GEOID',ind_vars$variable)]
hts_ind["(Intercept)"]<-1.0
#
# ask user what household to run
#
hhs<-{}
hhs$options <- c("Local Household",
                 "Typical County Household", 
                 "Typical Metro/Micro/County Household", 
                 "Typical State Household",
                 "HUD Specified Household")
hhs$prefix<-c('local',
              'county',
              'ami',
              'state',
              'hud')
choice_option <- select.list(
  choices = hhs$options,
  title = 'Choose the household:',
  multiple = FALSE ,
  graphics = TRUE
)
choice<-which(hhs$options==choice_option)
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
if(choice==1){
  modeled<-hts_ind[c("GEOID",hh_vars[,1])]
} else{
  county_hh_data<-get_county_hh_vars(carb_output,choice,htd,hud_inc,hud_size)
  hts_ind<-overwrite_model_hh_inputs(htd,county_hh_data)
  modeled<-hts_ind
}
#
# we will need fraction of renters to calculate overall housing cost
#
modeled$frac_rent_hu<-htd$frac_rent_hu
#
# Now calculate the model for all rows if possible
#
modeled<-predict_model(dep_vars,hts_ind,modeled,start=1,end=5)
names(modeled)[1]<-'geoid'
mold<-modeled
modeled<-mold
#
# Now run the cost of each transportation component and the H, T and H+T
#
modeled<-calculate_costs_ht(modeled)
#
# save output to excel file 
#
this_sheet<-paste(hhs$prefix[choice],"_hh_model",sep='')
carb_output<-add_xls_tab(carb_output,this_sheet,as.data.frame(modeled))

wb_save(carb_output,file="./excel_files/carb_ht_outputs.xlsx",overwrite = TRUE)

blkgrps<-st_read("./gis/blkgrps.gpkg")
#modeled<-merge(modeled,subset(blkgrps, select = c('GEOID','lacres','geom')))

modeled <- inner_join(modeled,
                      subset(blkgrps, select = c('GEOID','lacres','geom')),
                      by = c("geoid" = "GEOID"))
#
# save shape file 
#
write_sf(modeled, paste("./gis/",this_sheet,".gpkg",sep=''))

