library(openxlsx2)
library(dplyr)
library(sf)
library(DescTools)
library(rapportools)
library(tidyr)
rm(list=ls())
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
gz_scenario<-wb_load("./excel_files/gz_scenario.xlsx")
#
# get the scenario's output and get rid of the na's and make sure everything is numeric
#
scn_ht<-wb_read(gz_scenario,sheet='ht_model')
scn_inputs<-wb_read(gz_scenario,sheet='model_inputs')
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
flat_ht<-wb_read(gz_scenario,sheet='ht_flat_model')
names(flat_ht)[1]<-'geoid'
flat_inputs<-wb_read(gz_scenario,sheet='model_flat_inputs')
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
# pull out block groups that are at least 90% in a gz
#
aggs<-wb_read(gz_scenario,sheet='blkgp_projections')
names(aggs)
abg<-subset(aggs,aggs$frac>0.75)
gz_scn_ht<-subset(scn_ht,scn_ht$geoid %in% abg$geoid)

gz_base_ht<-subset(base_ht,base_ht$geoid %in% abg$geoid)
gz_flat_ht<-subset(flat_ht,base_ht$geoid %in% abg$geoid)

plot_ht_results(gz_base_ht,gz_scn_ht,'gzs','Base')
plot_ht_results(gz_flat_ht,gz_scn_ht,'gzs','Flat')

non_gz_scn_ht<-subset(scn_ht,!(scn_ht$geoid %in% abg$geoid))
non_gz_base_ht<-subset(base_ht,!(base_ht$geoid %in% abg$geoid))
non_gz_flat_ht<-subset(flat_ht,!(base_ht$geoid %in% abg$geoid))

plot_ht_results(non_gz_base_ht,non_gz_scn_ht,'non gzs','Base')
plot_ht_results(non_gz_flat_ht,non_gz_scn_ht,'non gzs','Flat')
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

av$scavhtgz<-mean(gz_scn_ht$ht)
av$scwtavhtgz<-sum(gz_scn_ht$ht*gz_scn_ht$households)/sum(gz_scn_ht$households)

av$basevhtgz<-mean(gz_base_ht$ht)
av$basewtavhtgz<-sum(gz_base_ht$ht*gz_base_ht$households)/sum(gz_base_ht$households)

av$flatavhtgz<-mean(gz_flat_ht$ht)
av$flatwtavhtgz<-sum(gz_flat_ht$ht*gz_flat_ht$households)/sum(gz_flat_ht$households)

av$scavhtnon<-mean(non_gz_scn_ht$ht)
av$scwtavhtnon<-sum(non_gz_scn_ht$ht*non_gz_scn_ht$households)/sum(non_gz_scn_ht$households)

av$basevhtnon<-mean(non_gz_base_ht$ht)
av$basewtavhtnon<-sum(non_gz_base_ht$ht*non_gz_base_ht$households)/sum(non_gz_base_ht$households)

av$flatavhtnon<-mean(non_gz_flat_ht$ht)
av$flatwtavhtnon<-sum(non_gz_flat_ht$ht*non_gz_flat_ht$households)/sum(non_gz_flat_ht$households)
#
# T
#
av$scavt<-mean(scn_ht$t)
av$scwtavt<-sum(scn_ht$t*scn_ht$households)/sum(scn_ht$households)

av$basevt<-mean(base_ht$t)
av$basewtavt<-sum(base_ht$t*base_ht$households)/sum(base_ht$households)

av$flatavt<-mean(flat_ht$t)
av$flatwtavt<-sum(flat_ht$t*flat_ht$households)/sum(flat_ht$households)

av$scavtgz<-mean(gz_scn_ht$t)
av$scwtavtgz<-sum(gz_scn_ht$t*gz_scn_ht$households)/sum(gz_scn_ht$households)

av$basevtgz<-mean(gz_base_ht$t)
av$basewtavtgz<-sum(gz_base_ht$t*gz_base_ht$households)/sum(gz_base_ht$households)

av$flatavtgz<-mean(gz_flat_ht$t)
av$flatwtavtgz<-sum(gz_flat_ht$t*gz_flat_ht$households)/sum(gz_flat_ht$households)

av$scavtnon<-mean(non_gz_scn_ht$t)
av$scwtavtnon<-sum(non_gz_scn_ht$t*non_gz_scn_ht$households)/sum(non_gz_scn_ht$households)

av$basevtnon<-mean(non_gz_base_ht$t)
av$basewtavtnon<-sum(non_gz_base_ht$t*non_gz_base_ht$households)/sum(non_gz_base_ht$households)

av$flatavtnon<-mean(non_gz_flat_ht$t)
av$flatwtavtnon<-sum(non_gz_flat_ht$t*non_gz_flat_ht$households)/sum(non_gz_flat_ht$households)







av$scavh<-mean(scn_ht$h)
av$scwtavh<-sum(scn_ht$h*scn_ht$households)/sum(scn_ht$households)

av$basevh<-mean(base_ht$h)
av$basewtavh<-sum(base_ht$h*base_ht$households)/sum(base_ht$households)

av$flatavh<-mean(flat_ht$h)
av$flatwtavh<-sum(flat_ht$h*flat_ht$households)/sum(flat_ht$households)

av$scavhgz<-mean(gz_scn_ht$h)
av$scwtavhgz<-sum(gz_scn_ht$h*gz_scn_ht$households)/sum(gz_scn_ht$households)

av$basevhgz<-mean(gz_base_ht$h)
av$basewtavhgz<-sum(gz_base_ht$h*gz_base_ht$households)/sum(gz_base_ht$households)

av$flatavhgz<-mean(gz_flat_ht$h)
av$flatwtavhgz<-sum(gz_flat_ht$h*gz_flat_ht$households)/sum(gz_flat_ht$households)

av$scavhnon<-mean(non_gz_scn_ht$h)
av$scwtavhnon<-sum(non_gz_scn_ht$h*non_gz_scn_ht$households)/sum(non_gz_scn_ht$households)

av$basevhnon<-mean(non_gz_base_ht$h)
av$basewtavhnon<-sum(non_gz_base_ht$h*non_gz_base_ht$households)/sum(non_gz_base_ht$households)

av$flatavhnon<-mean(non_gz_flat_ht$h)
av$flatwtavhnon<-sum(non_gz_flat_ht$h*non_gz_flat_ht$households)/sum(non_gz_flat_ht$households)



as.data.frame(av)

