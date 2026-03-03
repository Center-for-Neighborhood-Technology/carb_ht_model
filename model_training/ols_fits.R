#
# clear all memory before starting
#
source("./utilities/opt_fits.R", echo = TRUE)
source("./utilities/ploting_scripts.R", echo = TRUE)
#
# define some variables
#
fit<-{}
sm<-{}
cf<-{}
v<-{}
#
# read in the whole xlsx file
#
carb_xls<-wb_load("./excel_files/carb_ht_model.xlsx")
#
# get the data and remove block groups with no households
#
htd <- wb_read(carb_xls,sheet='fitting_data')
htd[is.na(htd)]<-as.numeric(NA)

for(i in 2:length(names(htd))){
  suppressWarnings(htd[[names(htd)[i]]]<-as.numeric(htd[[names(htd)[i]]]))
}

htd<-(subset(htd,htd$households>0))
#
# get dependent and independent variable names
#
vars <- as.data.frame(wb_read(carb_xls,sheet='variable_definitions'))
dep_vars<-as.data.frame(subset(vars,vars$var_type == 'dependent',select=c('variable','transformation','fit_type')))
ind_vars <- as.data.frame(subset(vars,vars$var_type == 'independent',select=c('variable','transformation','fit_type')))
hh_vars<- (subset(vars,vars$fit_type == 'hh',select=c('variable')))
transit_vars<- (subset(vars,vars$fit_type == 'transit',select=c('variable')))
#
# build the independent variable set including the intercept.
#
hts_ind<-htd[ind_vars$variable]
hts_ind["(Intercept)"]<-1.0
#
# build data selection criteria
#
sbset<-{}
sbset$autos_per_hh='htd$median_hh_income>2499 & htd$median_hh_income<250000 & htd$households>0'
sbset$frac_transit_j2w=paste(sbset$autos_per_hh,' & !(htd$bus_tci==0 & 
                                                      htd$other_tci==0 & 
                                                      htd$tas_acres==0 & 
                                                      htd$frac_transit_j2w==0)')
sbset$median_gross_rent   =paste(sbset$autos_per_hh,' & htd$median_gross_rent<3500')
sbset$median_smoc_mortgage=paste(sbset$autos_per_hh,' & htd$median_smoc_mortgage<4000')
sbset$vmt_per_hh=sbset$autos_per_hh
#
# plot the histogram of each dependent variable
#
if(make_plots){
  aph<-subset(htd,eval(parse(text=(sbset$autos_per_hh))),select='autos_per_hh')
  hist(aph$autos_per_hh,xlab='Autos/Household',main = 'Histogram of Autos/Houshold')
  tj2w<-subset(htd,eval(parse(text=(sbset$frac_transit_j2w))),select='frac_transit_j2w')
  hist(tj2w$frac_transit_j2w,xlab='Fraction Transit J2W',main = 'Histogram Fraction Transit J2W')
  vmt<-subset(htd,eval(parse(text=(sbset$vmt_per_hh))),select='vmt_per_hh')
  hist(vmt$vmt_per_hh,xlab='Annual VMT/HH',main = 'Histogram Annual VMT/HH')
  smoc<-subset(htd,eval(parse(text=(sbset$median_smoc_mortgage))),select='median_smoc_mortgage')
  hist(smoc$median_smoc_mortgage,xlab='Ownership Cost',main = 'Histogram Ownership Cost')
  gr<-subset(htd,eval(parse(text=(sbset$median_gross_rent))),select='median_gross_rent')
  hist(gr$median_gross_rent,xlab='Gross Rent',main = 'Histogram Gross Rent')
  #
  # histogram a few other things
  #
  btci<-subset(htd,htd$bus_tci>0,select='bus_tci')
  hist(btci$bus_tci,xlab='Bus TCI',main = 'Histogram Bus TCI')
  b<-ggplot(btci,aes(x = bus_tci)) + geom_histogram(binwidth = 1000)
  b + scale_y_log10() + labs(title = "Bus TCI Histogram", x="Bus TCI", y = "Frequency (Log10 Scale)")
  otci<-subset(htd,htd$other_tci>0,select='other_tci')
  hist(otci$other_tci,xlab='Other TCI',main = 'Histogram Other TCI')
  o<-ggplot(otci,aes(x = other_tci)) + geom_histogram(binwidth = 1000)
  o + scale_y_log10() + labs(title = "Other TCI Histogram", x="Other TCI", y = "Frequency (Log10 Scale)")
}
#
# find optimum fit for transit because it is a quasibinomial there can be variables the drive the fit 
#  into a range the does not work.
#
g<-c(ind_vars$variable,'frac_transit_j2w')
this_data<-subset(htd,eval(parse(text=(sbset$frac_transit_j2w))),select=g) |> na.omit()
#
# now do the optimization - keeping the household and transit variables
#
must_haves<-1:8
xq<-opt_quasib('frac_transit_j2w',ind_vars$variable,this_data,must_haves,1)
#
# build and run models
#
exclude<-{}
exclude$autos_per_hh<-c()
exclude$frac_transit_j2w<-ind_vars$variable[xq$bad_ids]
exclude$median_gross_rent<-c()
exclude$median_smoc_mortgage<-c()
exclude$vmt_per_hh<-c()
exlcude_list<-c()
i=1
for(i in 1:length(dep_vars[,1])){
  ed<-dep_vars[i,1]
  print_color(paste('---',ed,'---\n'),'bg_yellow')
  print_color(paste('---',sbset[ed],'---\n'),'bg_yellow')
  cf_bad<-as.data.frame(c(0))
  cf_bad$vn<-'none'
  vif_large<-c(0)
  #
  # loop through the fits and eliminate insignificant and multi-colinear variables
  #
  while(length(cf_bad$vn) > 0 || length(vif_large) > 0){
    g<-c(ind_vars$variable[!ind_vars$variable %in% exclude[[ed]]],ed)
    this_data<-subset(htd,eval(parse(text=(sbset[ed]))),select=g) |> na.omit()
    #
    # build the fit function (f) and run the regression
    #
    f<-as.formula(paste(ed,'~.',sep=' '))
    if(dep_vars$transformation[i]=='log'){
      this_data[[ed]]<-log(this_data[[ed]])
    }
   if(dep_vars$fit_type[i]=='quasibinomial'){
     fit[[ed]] <- glm(f, data = this_data,family='quasibinomial')
     ps<-PseudoR2(fit[[ed]], which = 'Efron')
    } else {
      fit[[ed]] <- lm(f, data = this_data)
    }
    #
    # get the fit summary and the coefficients
    #
    sm[[ed]]<-summary(fit[[ed]])
    cf[[ed]]<-sm[[ed]]$coefficients
    #
    # find the coefficients that do not have the p-value is less that 5% and 
    #   that this is not a transit or household variable, then sort and pick the worst case.
    #
    cf_bad<-as.data.frame(subset(cf[[ed]],
                                    (cf[[ed]][,4]>0.05) & 
                                    !(row.names(cf[[ed]]) %in% hh_vars$variable) & 
                                    !(row.names(cf[[ed]]) %in% transit_vars$variable) & 
                                    !(row.names(cf[[ed]]) == "(Intercept)")))
    cf_bad$vn<-row.names(cf_bad)
    cf_bad<-cf_bad[order(cf_bad[,4],decreasing = TRUE),]
    #
    # now get the multi-covarient ones that har VIF greater thatn 10 and sort and pick the worst case
    #
    v[[ed]]<-vif(fit[[ed]])
    #print(">>>>> VIF <<<<<")
    vif_large<-subset(v[[ed]],
                                    (v[[ed]]>10) & 
                                    !(names(v[[ed]]) %in% hh_vars$variable) &  
                                    !(names(v[[ed]]) == "(Intercept)"))
    vif_large<-(vif_large[order(unlist(vif_large),decreasing = TRUE)])
    #
    # print status of the fitting process
    #
    if(dep_vars$fit_type[i]=='quasibinomial'){
      print(paste("R2=",round(ps*100,2)))
    }
    else {
      print(paste("R2=",round(sm[[ed]]$r.squared*100,2),"adj R2=",round(sm[[ed]]$adj.r.squared*100,2)))
    }
    #
    # load the worst variables into the exclude vector, by first eliminating bad p-values
    #  then once there are not bad p-values eliminate the multi-colinear ones.
    #
    if(length((cf_bad$vn))>0 || length(vif_large)>0){
      if(length((cf_bad$vn))>0){
        ex<-cf_bad$vn
        val<-cf_bad[1,4]
        why<-"p"
        exclude[[ed]]<-c(exclude[[ed]],ex)
      }  
      else if(length(vif_large)>0){
        ex<-names(vif_large)[1]
        vif_large[1]
        why<-"v"
        val<-vif_large[1]
        exclude[[ed]]<-c(exclude[[ed]],ex)
      }     
      # print(paste(ed,'|',ex,'|',why,'|',val))
      exlcude_list[[length(exlcude_list) + 1]]<-c(ed,ex,why,val)
    }
  }
  i=i+1
}
#
# Now calculate the prediction for all rows if possible
#
predicted<-as.data.frame(htd[,1])
names(predicted)<- c('GEOID')
#
# find the block groups that have no transit and where no one uses transit 
#  and force the fit to be zero
#
no_transit<-subset(htd,htd$bus_tci==0 & htd$other_tci==0 & htd$tas_acres==0 & htd$frac_transit_j2w==0)$GEOID
#
# calculate the predicted values, residuals and the quality of the fits
#
stats<-{}
ed_measured<-{}
ed_residual<-{}
i<-1
for(i in 1:length(dep_vars[,1])){
  ed<-dep_vars[i,1]
  ed_measured[ed]=paste(ed,'_measured',sep="")
  ed_residual[ed]=paste(ed,'_residual',sep="")
  
  thcf<-cf[[ed]][,1]
  thhtd<-hts_ind[names(thcf)]
  length(thhtd$avg_commuters)
  j<-1
  for(j in 1:length(thhtd[,1])){
    predicted[j,ed_measured[ed]]=htd[j,ed]
    predicted[j,ed]<-NA
    if(complete.cases(thhtd[j,])){
      x<-unlist(thhtd[j,])
      y<-x%*%thcf
      if(dep_vars$fit_type[i]=='quasibinomial'){
        predicted[j,ed]<-exp(y)
        predicted[j,ed]<-round((predicted[j,ed])/(1+(predicted[j,ed])),4)
      }
      else if(dep_vars$transformation[i]=='log'){
        predicted[j,ed]<-exp(y)
      } 
      else {
        predicted[j,ed]<-y
      }

    }
    if(ed=='frac_transit_j2w' & predicted$GEOID[j] %in% no_transit){predicted[j,ed]<-0.0}
  }
  predicted[,ed_residual[ed]]<-predicted[,ed_measured[ed]]-predicted[,ed]
  gd<-subset(predicted,!is.na(predicted[,ed_residual[ed]]))
  stats$rmse[ed] <- sqrt(mean((gd[,ed_residual[ed]])^2))
  stats$measured_mean[ed] <- mean(gd[,ed_measured[ed]])
  stats$measured_std[ed] <- sd(gd[,ed_measured[ed]])
  stats$modeled_mean[ed] <- mean(gd[,ed])
  stats$modeled_std[ed] <- sd(gd[,ed])
  stats$rsquared[ed] <- 1-(sum((gd[,ed_residual[ed]])^2)/sum((gd[,ed_measured[ed]]-mean(gd[,ed_measured[ed]]))^2))
  print(paste('Calculated ',ed,' predictions. RMSA=', round(stats$rmse[ed],2),' R^2=',round(stats$rsquared[ed],4)))
}
print(' ----- Fit Statistics -----')
print(stats)
#
# get rid of the outlier that is in death valley
#
predicted<-subset(predicted,!(predicted$GEOID=='060270008001'))
if(make_plots){
  #
  # plot the residuals
  #
  hist(predicted$autos_per_hh_residual,breaks = 50)
  hist(predicted$frac_transit_j2w_residual,breaks = 50)
  hist(predicted$median_gross_rent_residual,breaks = 50)
  hist(predicted$median_smoc_mortgage_residual,breaks = 50)
  hist(predicted$vmt_per_hh_residual,breaks = 50)
  #
  # plot the measured vs. predicted
  #
  error_plot(predicted$autos_per_hh,predicted$autos_per_hh_measured,yl='Autos/HH',xl='Autos/HH Predicted')
  error_plot(predicted$frac_transit_j2w,predicted$frac_transit_j2w_measured,yl='Fraction Transit J2W',xl='Fraction Transit J2W Predicted')
  error_plot(predicted$median_gross_rent,predicted$median_gross_rent_measured ,yl='Gross Rent',xl='Gross Rent Predicted')
  error_plot(predicted$median_smoc_mortgage,predicted$median_smoc_mortgage_measured ,yl='Ownership Cost',xl='Ownership Cost Predicted')
  error_plot(predicted$vmt_per_hh,predicted$vmt_per_hh_measured ,yl='Annual VMT/HH',xl='Annual VMT/HH Predicted',intercept=TRUE)
}
#
# update the outputs with the fit parameters and the 
#
for(i in 1:length(dep_vars[,1])){
  #
  # add sheet of coefficients 
  #
  sheet<-paste(dep_vars[i,1],'_coef',sep='')
  if(sheet %in% wb_get_sheet_names(carb_xls)){carb_xls$remove_worksheet(sheet)}
  carb_xls$add_worksheet(sheet)
  carb_xls$add_data(sheet,as.data.frame(cf[dep_vars[i,1]]),colNames = TRUE, rowNames = TRUE,)
  #
  # add sheet of excluded variables for each fit
  #
  sheet<-paste(dep_vars[i,1],'_excluded',sep='')
  if(sheet %in% wb_get_sheet_names(carb_xls)){carb_xls$remove_worksheet(sheet)}
  carb_xls$add_worksheet(sheet)
  carb_xls$add_data(sheet,as.data.frame(exclude[dep_vars[i,1]]),colNames = TRUE, rowNames = TRUE,)
}
#
# add sheet of predicted values
#
sheet="predicted values"
if(sheet %in% wb_get_sheet_names(carb_xls)){carb_xls$remove_worksheet(sheet)}
carb_xls$add_worksheet(sheet)
carb_xls$add_data(sheet,as.data.frame(predicted),colNames = TRUE, rowNames = TRUE,)
#
# add sheet of fit stats
#
sheet="fitting statisticts"
if(sheet %in% wb_get_sheet_names(carb_xls)){carb_xls$remove_worksheet(sheet)}
carb_xls$add_worksheet(sheet)
carb_xls$add_data(sheet,as.data.frame(stats),colNames = TRUE, rowNames = TRUE,)
#
# save the output into the xlsx file
#
wb_save(carb_xls,file="./excel_files/carb_ht_model.xlsx",overwrite = TRUE)

