error_plot<-function(xx,yy,bins=50,sigma=2,pltline=1,xl="x",yl="y",main="",ymin="",ymax="",details=TRUE,intercept=TRUE){
  x<-subset(xx,!is.na(xx) & !is.na(yy))
  y<-subset(yy,!is.na(xx) & !is.na(yy))
  xmx<-max(x)
  xmn<-min(x)
  ymean<-mean(y)
  ysd<-sigma*sd(y)
  bin_size<-(xmx-xmn)/(bins-1)
  pltval<-array(data=NA,dim=c(bins,5),dimnames=list(1:bins,c('xm','ym','yl','yh','ymd')))
  for(i in 1:bins){
    bmin<-xmn+((i-1)*bin_size)
    bmax<-bmin+bin_size
    pltval[i,'xm']<-bmin+(bin_size/2)
    yb<-subset(y,x>=bmin & x<bmax)
    lb<-length(yb)
    if(lb>0){
      my<-mean(yb)
      md<-median(yb)
      pltval[i,'ym']<-my
      pltval[i,'ymd']<-md
      eom<-sd(yb)/sqrt(lb)
      pltval[i,'yl']<-my-(eom/2)
      pltval[i,'yh']<-my+(eom/2)
    }
  }
  if(ymin==""){ymin<-max(ymean-2*ysd,min(y))}
  if(ymax==""){ymax<-min(ymin+4*ysd,max(y))}
  if(sigma==0){plot(x,y,col='grey',ylim=c(as.numeric(ymin),as.numeric(ymax)),pch='.',xlab=xl,ylab=yl,main=main)}
  else{plot(x,y,col='grey',ylim=c(as.numeric(ymin),as.numeric(ymax)),pch='.',xlab=xl,ylab=yl,main=main)}
  points(pltval[,'xm'],pltval[,'ymd'],col='green',pch=21,bg='light green')
  points(pltval[,'xm'],pltval[,'ym'],col='blue',pch=23,bg='light blue')
  points(pltval[,'xm'],pltval[,'yl'],col='blue',pch='-')
  points(pltval[,'xm'],pltval[,'yh'],col='blue',pch='-')
  if(intercept){
    xx<-lm(y~x)
    b=coef(xx)[1]
    m=coef(xx)[2]
    p<-summary(xx)$coefficients[8]
  }
  else{
    xx<-lm(y~0+x)
    b=0
    m=coef(xx)[1]
  }
  if(pltline>0){abline(c(b,m))}
  results<-array(data=NA,dim=9,dimnames=list(c('intercept','slope','r2','min','max','sd','ymin','ymax','p')))
  results[1]<-b
  results[2]<-m
  results[3]<-find_r2(y,residuals(xx))
  results[4]<-min(x)
  results[5]<-max(x)
  results[6]<-sd(x)
  results[7]<-ymin
  results[8]<-ymax
  results[9]<-p
  
  
  if(details){text(x=(xmn+(0.5*(xmx-xmn))),y=(ymin+(0.9*(ymax-ymin))),paste(round(results[1]*10000)/10000,"+",round(results[2]*10000)/10000,"x \n r^2=",round(results[3]*100000)/1000,"% \n n=",length(x)))}
  results
}

find_r2<-function(meas_y,resid){
  r2<-0
  if(length(resid)>1){r2<-(1 - sum((resid)**2)/sum((meas_y-mean(meas_y))**2))}
  r2
}

ua_hist<-function(xplt,bins=50,xl='x',main='Histogram'){
  xmn<-mean(xplt)-sd(xplt)*3
  xmx<-mean(xplt)+sd(xplt)*3
  q<-hist(xplt, breaks=bins,xlab=xl,main=main,xlim=c(xmn,xmx))
				abline(v=(mean(xplt)),col='blue')
				abline(v=(mean(xplt)-sd(xplt)),col='red')
				abline(v=(mean(xplt)+sd(xplt)),col='red')
				abline(v=(mean(xplt)-2*sd(xplt)),col='orange')
				abline(v=(mean(xplt)+2*sd(xplt)),col='orange')
				legend("topleft",c(paste('n =',length(xplt),'<x>=',round(mean(xplt),3),
				                          'SD =',round(sd(xplt),3)),
				                          'Blue = <x> | Red = 1 SD | Orange = 2 SD'))
}


#
# create function to plot comparisons
#
plot_ht_results<-function(df1,df2,sample_name,comp){
  #
  # error plot h t and ht
  #
    error_plot(df1$t,df2$t,
             xl=paste(comp,' T'),yl='Scenario T',main=paste('Scenario T vs ',comp,' T',sample_name))
  error_plot(df1$h,df2$h,
             xl=paste(comp,' H'),yl='Scenario H',main=paste('Scenario H vs ',comp,' H',sample_name))
  error_plot(df1$ht,df2$ht,
             xl=paste(comp,' H+T'),yl='Scenario H+T',main=paste('Scenario H+T vs ',comp,' H+T',sample_name))
  #
  # percent reductions vs value
  #
  error_plot(as.numeric(df1$t),100*(as.numeric(df1$t)-as.numeric(df2$t))/as.numeric(df1$t),
             xl=paste(comp,' T'),yl='% Reduction in T',main=paste('% Reduction in T vs ',comp,' T',sample_name),
             pltline=0,details=FALSE)
  error_plot(as.numeric(df1$h),100*(as.numeric(df1$h)-as.numeric(df2$h))/as.numeric(df1$h),
             xl=paste(comp,' H'),yl='% Reduction in H',main=paste('% Reduction in H vs ',comp,' H',sample_name),
             pltline=0,details=FALSE)
  error_plot(as.numeric(df1$ht),100*(as.numeric(df1$ht)-as.numeric(df2$ht))/as.numeric(df1$ht),
             xl=paste(comp,' H+T'),yl='% Reduction in H+T',main=paste('% Reduction in H+T vs ',comp,' H+T',sample_name),
             pltline=0,details=FALSE)
  #
  # percent reductions vs income
  #
  error_plot(as.numeric(df1$median_hh_income),100*(as.numeric(df1$t)-as.numeric(df2$t))/as.numeric(df1$t),
             xl='Median HH Income',yl='% Reduction in T',main=paste('% Reduction in T vs Median HH Income',sample_name),
             pltline=0,details=FALSE)
  error_plot(as.numeric(df1$median_hh_income),100*(as.numeric(df1$h)-as.numeric(df2$h))/as.numeric(df1$h),
             xl='Median HH Income',yl='% Reduction in H',main=paste('% Reduction in H vs Median HH Income',sample_name,'for',comp),
             pltline=0,details=FALSE)
  error_plot(as.numeric(df1$median_hh_income),100*(as.numeric(df1$ht)-as.numeric(df2$ht))/as.numeric(df1$ht),
             xl='Median HH Income',yl='% Reduction in H+T',main=paste('% Reduction in H+T vs Median HH Income',sample_name,'for',comp),
             pltline=0,details=FALSE)
  #
  # percent reductions vs density
  #
  error_plot(as.numeric(df1$gross_hh_density),100*(as.numeric(df1$t)-as.numeric(df2$t)),
             xl=paste(comp,'Household Density'),yl='Reduction in T',main=paste('Reduction in T vs Median HH Income',sample_name),
             pltline=0,details=FALSE)
  error_plot(as.numeric(df1$gross_hh_density),100*(as.numeric(df1$h)-as.numeric(df2$h)),
             xl=paste(comp,'Household Density'),yl='Reduction in H',main=paste('Reduction in H vs Median HH Income',sample_name,'for',comp),
             pltline=0,details=FALSE)
  error_plot(as.numeric(df1$gross_hh_density),100*(as.numeric(df1$ht)-as.numeric(df2$ht)),
             xl=paste(comp,'Household Density'),yl='Reduction in H+T',main=paste('Reduction in H+T vs Median HH Income',sample_name,'for',comp),
             pltline=0,details=FALSE)
  
  ua_hist(100*(as.numeric(df1$ht)-as.numeric(df2$ht))/as.numeric(df1$ht),bins=300,xl=paste('% Reduction in H+T from',comp),main=sample_name)
  ua_hist(100*(as.numeric(df1$ht)-as.numeric(df2$ht)),bins=200,xl=paste('Reduction in H+T from',comp),main=sample_name)
  
}


