opt_quasib<-function(dpv,ipvs,dataset,must_haves,verbose=0){
  #
  # this function optimizes a quasibinomial fit - eliminating variables that cause too high a covariance,
  #   and thus to not give a reliable result. It returns a vector of variables to eliminate
  #
  # dpv = dependent variable
  # ipvs = list of possible independent variables
  # dataset = data frame of all variables to fit
  # must_haves = list of variables that cannot be eliminated
  # varbose = if not zero will print a lot of messages.
  #
  good_ids<-c()
  f=paste(dpv,' ~ ')
  plus<-''
  for(i in 1:length(must_haves)){
    f=paste(f,plus,ipvs[must_haves[i]])
    plus<-'+'
        good_ids<-c(good_ids,must_haves[i])
  }
  if(verbose>0){print(paste('Initial function=>',f))}
  fit<-glm(f, data = dataset,family='quasibinomial')
  if(verbose>0){print(summary(fit)$coefficients)}

  library(DescTools)
  bad_ids<-c(0)
  good_fits<-length(good_ids)
  j<-0
  while(length(good_ids)+length(bad_ids)-1 < length(ipvs)){
    j<-j+1
    best_ps<--99999
    for(i in 1:length(ipvs)){
      if(!(i %in% good_ids) & !(i %in% bad_ids)){
        iv<-names(this_data)[i]
        ft<-paste(f,plus,iv)
        fit<-glm(ft, data = this_data,family='quasibinomial')
        ps<-PseudoR2(fit, which = 'Efron')
        dps<-summary(fit)$dispersion
        
        if(ps>best_ps){
          best_ps<-ps
          best_i<-i
          best_dsp<-dps
          best_absp<-summary(fit)$coefficients[good_fits+2,4]
        }
      }
    }
    if(verbose>0){print(paste('j=',j,' i=',best_i,' dps=',round(as.numeric(best_dsp),3),' psr2=',round(as.numeric(best_ps)*100,2),'%',
                ' |p|=',round(as.numeric(best_absp)*100,3),'%',sep=''))}
    if(best_dsp<2 & best_absp<0.60){
      f=paste(f,plus,names(this_data)[best_i])
      good_fits<-good_fits+1
      good_ids<-c(good_ids,best_i)
    }
    else{
      bad_ids<-c(bad_ids,best_i)
      if(verbose>0){print(names(this_data)[best_i])}
    }
  }
  if(verbose>0){print(bad_ids)}
  cf<-summary(fit)$coefficients
  cf_bad<-subset(cf,(cf[,4]>0.05) & (!row.names(cf) %in% hh_vars$variable) & (!row.names(cf) == "(Intercept)"))
  cf_bad<-cf_bad[order(cf_bad[,4],decreasing = TRUE),]
  xx<-{}
  xx$bad_ids<-bad_ids
  xx$bad_cfs<-cf_bad
  xx$psr2<-best_ps
  xx$coefs<-cf
  xx
}
