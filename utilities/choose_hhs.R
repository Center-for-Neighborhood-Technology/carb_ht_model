get_household<-function(){
  #
  # ask user what household to run
  #
  hhs<-{}
  hhs$options <- c("Local Household",
                   "Typical County Household", 
                   "Typical Metro/Micro/County Household", 
                   "Typical State Household",
                   "A HUD Household")
  hhs$prefix<-c('local',
                'county',
                'ami',
                'state',
                'hud')
  choice=0
  while(choice< 1 | choice>length(hhs$options)){
    choice <- menu(hhs$options,title='Choose the household:') 
  }
  print(paste("You choose",hhs$options[choice]))
  #
  # if you choose HUD get the type of hud hh you want
  #
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
  hh<-{}
  hh$prefix<-hhs$prefix[choice]
  hh$name<-hhs$options[choice]
  hh$income<-hud_inc
  hh$size<-hud_size
  as.data.frame(hh)
  
  
}