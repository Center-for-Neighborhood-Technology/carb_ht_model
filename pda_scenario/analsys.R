library(openxlsx2)
#library(dplyr)
#library(sf)
#library(DescTools)
#library(rapportools)
#library(tidyr)
#
# load in some utility functions
#
source("./utilities/ht_calculations.R")
source("./utilities/ploting_scripts.R")
#
# Get the model inputs from the xlsx file
#
carb_ht_outputs<-wb_load("./excel_files/carb_ht_outputs.xlsx")
carb_ht_model<-wb_load("./excel_files/carb_ht_model.xlsx")
pda_scenario<-wb_load("./excel_files/pda_scenario.xlsx")
#
# get the scenario's output and get rid of the na's and make sure everything is numeric
#
scn_ht<-wb_read(pda_scenario,sheet='ht_model')
scn_inputs<-wb_read(pda_scenario,sheet='model_inputs')
scn_ht<-merge(scn_ht,subset(scn_inputs,select=c('geoid','households','gross_hh_density','emp_gravity10')))
vs<-names(scn_ht)
for(v in vs){
  print(v)
  if(v != 'geoid'){
    scn_ht<-subset(scn_ht,!(scn_ht[v]=='#N/A'))
    scn_ht[v]<-as.numeric(scn_ht[[v]])
  }
}
#
# now do the same for the base case 
#
base_ht<-wb_read(carb_ht_outputs,sheet='local_hh_model')
names(base_ht)[1]<-'geoid'
base_inputs<-wb_read(carb_ht_model,sheet='fitting_data')
names(base_inputs)[1]<-'geoid'
base_ht<-merge(base_ht,subset(base_inputs,select=c('geoid','households','gross_hh_density','emp_gravity10')))
vs<-names(base_ht)
for(v in vs){
  print(v)
  if(v != 'geoid'){
    base_ht<-subset(base_ht,!(base_ht[v]=='#N/A'))
    base_ht[v]<-as.numeric(base_ht[[v]])
  }
}
#
# now do the same for the flat case 
#
flat_ht<-wb_read(pda_scenario,sheet='ht_flat_model')
names(flat_ht)[1]<-'geoid'
flat_inputs<-wb_read(pda_scenario,sheet='flat_inputs')
flat_ht<-merge(flat_ht,subset(flat_inputs,select=c('geoid','households','gross_hh_density','emp_gravity10')))
vs<-names(flat_ht)
for(v in vs){
  print(v)
  if(v != 'geoid'){
    flat_ht<-subset(flat_ht,!(flat_ht[v]=='#N/A'))
    flat_ht[v]<-as.numeric(flat_ht[[v]])
  }
}
#
# now sync the data frames up
#
base_ht<-subset(base_ht,base_ht$geoid %in% scn_ht$geoid)
flat_ht<-subset(flat_ht,base_ht$geoid %in% scn_ht$geoid)

plot_ht_results(base_ht,scn_ht,'Everywhere','Base')
plot_ht_results(flat_ht,scn_ht,'Everywhere','Flat')
#
# pull out block groups that are at least 90% in a pda
#
aggs<-wb_read(pda_scenario,sheet='agg_projections')
names(aggs)
pdas<-subset(aggs,aggs$type=='pda',select = c('obj_id'))
abg<-wb_read(pda_scenario,sheet='blkgrps_agg_geom_fracs')
abg<-subset(abg,abg$frac_of_blkgrps>0.75 & abg$obj_id %in% pdas$obj_id)
pda_scn_ht<-subset(scn_ht,scn_ht$geoid %in% abg$geoid)

pda_base_ht<-subset(base_ht,base_ht$geoid %in% abg$geoid)
pda_flat_ht<-subset(flat_ht,base_ht$geoid %in% abg$geoid)

plot_ht_results(pda_base_ht,pda_scn_ht,'PDAs','Base')
plot_ht_results(pda_flat_ht,pda_scn_ht,'PDAs','Flat')

non_pda_scn_ht<-subset(scn_ht,!(scn_ht$geoid %in% abg$geoid))
non_pda_base_ht<-subset(base_ht,!(base_ht$geoid %in% abg$geoid))
non_pda_flat_ht<-subset(flat_ht,!(base_ht$geoid %in% abg$geoid))

plot_ht_results(non_pda_base_ht,non_pda_scn_ht,'non PDAs','Base')
plot_ht_results(non_pda_flat_ht,non_pda_scn_ht,'non PDAs','Flat')
#
# HT
#
av<-{}
av$scavht<-mean(scn_ht$ht)
av$scwtavht<-sum(scn_ht$ht*scn_ht$households)/sum(scn_ht$households)

av$basevht<-mean(base_ht$ht)
av$basewtavht<-sum(base_ht$ht*base_ht$households)/sum(base_ht$households)

av$flatavht<-mean(flat_ht$ht)
av$flatwtavht<-sum(flat_ht$ht*flat_ht$households)/sum(flat_ht$households)

av$scavhtpda<-mean(pda_scn_ht$ht)
av$scwtavhtpda<-sum(pda_scn_ht$ht*pda_scn_ht$households)/sum(pda_scn_ht$households)

av$basevhtpda<-mean(pda_base_ht$ht)
av$basewtavhtpda<-sum(pda_base_ht$ht*pda_base_ht$households)/sum(pda_base_ht$households)

av$flatavhtpda<-mean(pda_flat_ht$ht)
av$flatwtavhtpda<-sum(pda_flat_ht$ht*pda_flat_ht$households)/sum(pda_flat_ht$households)

av$scavhtnon<-mean(non_pda_scn_ht$ht)
av$scwtavhtnon<-sum(non_pda_scn_ht$ht*non_pda_scn_ht$households)/sum(non_pda_scn_ht$households)

av$basevhtnon<-mean(non_pda_base_ht$ht)
av$basewtavhtnon<-sum(non_pda_base_ht$ht*non_pda_base_ht$households)/sum(non_pda_base_ht$households)

av$flatavhtnon<-mean(non_pda_flat_ht$ht)
av$flatwtavhtnon<-sum(non_pda_flat_ht$ht*non_pda_flat_ht$households)/sum(non_pda_flat_ht$households)
#
# T
#
av$scavt<-mean(scn_ht$t)
av$scwtavt<-sum(scn_ht$t*scn_ht$households)/sum(scn_ht$households)

av$basevt<-mean(base_ht$t)
av$basewtavt<-sum(base_ht$t*base_ht$households)/sum(base_ht$households)

av$flatavt<-mean(flat_ht$t)
av$flatwtavt<-sum(flat_ht$t*flat_ht$households)/sum(flat_ht$households)

av$scavtpda<-mean(pda_scn_ht$t)
av$scwtavtpda<-sum(pda_scn_ht$t*pda_scn_ht$households)/sum(pda_scn_ht$households)

av$basevtpda<-mean(pda_base_ht$t)
av$basewtavtpda<-sum(pda_base_ht$t*pda_base_ht$households)/sum(pda_base_ht$households)

av$flatavtpda<-mean(pda_flat_ht$t)
av$flatwtavtpda<-sum(pda_flat_ht$t*pda_flat_ht$households)/sum(pda_flat_ht$households)

av$scavtnon<-mean(non_pda_scn_ht$t)
av$scwtavtnon<-sum(non_pda_scn_ht$t*non_pda_scn_ht$households)/sum(non_pda_scn_ht$households)

av$basevtnon<-mean(non_pda_base_ht$t)
av$basewtavtnon<-sum(non_pda_base_ht$t*non_pda_base_ht$households)/sum(non_pda_base_ht$households)

av$flatavtnon<-mean(non_pda_flat_ht$t)
av$flatwtavtnon<-sum(non_pda_flat_ht$t*non_pda_flat_ht$households)/sum(non_pda_flat_ht$households)







av$scavh<-mean(scn_ht$h)
av$scwtavh<-sum(scn_ht$h*scn_ht$households)/sum(scn_ht$households)

av$basevh<-mean(base_ht$h)
av$basewtavh<-sum(base_ht$h*base_ht$households)/sum(base_ht$households)

av$flatavh<-mean(flat_ht$h)
av$flatwtavh<-sum(flat_ht$h*flat_ht$households)/sum(flat_ht$households)

av$scavhpda<-mean(pda_scn_ht$h)
av$scwtavhpda<-sum(pda_scn_ht$h*pda_scn_ht$households)/sum(pda_scn_ht$households)

av$basevhpda<-mean(pda_base_ht$h)
av$basewtavhpda<-sum(pda_base_ht$h*pda_base_ht$households)/sum(pda_base_ht$households)

av$flatavhpda<-mean(pda_flat_ht$h)
av$flatwtavhpda<-sum(pda_flat_ht$h*pda_flat_ht$households)/sum(pda_flat_ht$households)

av$scavhnon<-mean(non_pda_scn_ht$h)
av$scwtavhnon<-sum(non_pda_scn_ht$h*non_pda_scn_ht$households)/sum(non_pda_scn_ht$households)

av$basevhnon<-mean(non_pda_base_ht$h)
av$basewtavhnon<-sum(non_pda_base_ht$h*non_pda_base_ht$households)/sum(non_pda_base_ht$households)

av$flatavhnon<-mean(non_pda_flat_ht$h)
av$flatwtavhnon<-sum(non_pda_flat_ht$h*non_pda_flat_ht$households)/sum(non_pda_flat_ht$households)



as.data.frame(av)

