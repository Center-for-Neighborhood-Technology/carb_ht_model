#
# read in the whole xlsx file
#
data_xls<-wb_load("./excel_files/data_prep.xlsx")
blkgrp_acs_2023<- wb_read(data_xls,sheet='blkgrp_acs_2023')
#
# now get the LEHD LODES data
#
lodes_dat <- grab_lodes(state = c("az","ca", "nv", "or"), year =2022, 
                        version = "LODES8",lodes_type = "wac", 
                        job_type = "JT00", segment = "S000", 
                        state_part = "", agg_geo = "bg") 
lodes_dat<-subset(lodes_dat,select=c("w_bg","C000","CNS01","CNS02","CNS03","CNS04",
                                     "CNS05","CNS06","CNS07","CNS08","CNS09","CNS10",
                                     "CNS11","CNS12","CNS13","CNS14","CNS15","CNS16",
                                     "CNS17","CNS18","CNS19","CNS20") )
data_xls$remove_worksheet("lodes_2022")
data_xls$add_worksheet("lodes_2022")
data_xls$add_data("lodes_2022",as.data.frame(lodes_dat),colNames = TRUE, rowNames = TRUE,)
#
# get the shape files
#
ca_blkgrps<-block_groups('CA',year=2023)
az_blkgrps<-block_groups('AZ',year=2023)
nv_blkgrps<-block_groups('NV',year=2023)
or_blkgrps<-block_groups('OR',year=2023)
#
#  merge them 
#
blkgrp_azcanvor<-do.call("rbind", list(az_blkgrps,
                                       ca_blkgrps,
                                       nv_blkgrps,
                                       or_blkgrps))
#
# Assuming tiger shp is in EPSG:4269 - NAD83 (per Tiger Line file CRS) coordinates
#   create centroid points from intptlat/lon and add in lodes and acs vars we want to 
#   measure gravity for.
#
blkgrp_azcanvor <- st_as_sf(blkgrp_azcanvor,crs = 4269, remove = FALSE)
blkgrp_azcanvor_pt<-st_as_sf(as.data.frame(blkgrp_azcanvor),
                             coords = c("INTPTLON","INTPTLAT"),
                             crs = 4269, remove = FALSE)
blkgrp_azcanvor_pt <- left_join(blkgrp_azcanvor_pt,lodes_dat, by=c('GEOID'='w_bg'))
acs_vars<-subset(as.data.frame(blkgrp_acs_2023),select=c('GEOID','households','occupied_hu','housing_units',
                                                         'renter_occupied_hu','hu_1_detached'))
blkgrp_azcanvor_pt <- left_join(blkgrp_azcanvor_pt,acs_vars, by=c('GEOID'))
#
# create gravity data frame and order by GEOID
#
gravity<-subset(as.data.frame(ca_blkgrps),select=c('GEOID','COUNTYFP'))
order_indices <- order(gravity$GEOID)
gravity<-as.data.frame(gravity[order_indices, ])
#
# define vars to calculate gravity for 
#
gravity_vars<-c('households','renter_occupied_hu','hu_1_detached','housing_units',
                "C000","CNS01","CNS02","CNS03","CNS04",
                "CNS05","CNS06","CNS07","CNS08","CNS09","CNS10",
                "CNS11","CNS12","CNS13","CNS14","CNS15","CNS16",
                "CNS17","CNS18","CNS19","CNS20")
#
# do the gravity for 10 miles
#   first make sure everything is a double number, not text
#
for(g in gravity_vars){
  blkgrp_azcanvor_pt[[g]]<-ifelse(is.na(blkgrp_azcanvor_pt[[g]]),
                                             as.double(0),
                                             as.double(blkgrp_azcanvor_pt[[g]]))
  gravity[[g]]<-0
}
cnty=''
i<-1
for(i in 1:length(gravity$GEOID)){
  cnty_old=cnty
  cnty=gravity$COUNTYFP[i]
  if(cnty!=cnty_old){print(paste("Calculating gravity for county = ",cnty))}
  stfid<-gravity$GEOID[i]
  tst_pt<-subset(blkgrp_azcanvor_pt,blkgrp_azcanvor_pt$GEOID==stfid)
  #
  #  fill the distance in miles
  #
  blkgrp_azcanvor_pt$d<-as.double(st_distance( st_transform(tst_pt$geometry,3857),
                                            st_transform(blkgrp_azcanvor_pt$geometry,3857)))/1609.34
  sbset10<-subset(as.data.frame(blkgrp_azcanvor_pt),blkgrp_azcanvor_pt$d<10)
  sbset10$d2<-ifelse(sbset10$d<1,1.0,sbset10$d*sbset10$d)
  for(g in gravity_vars){
    gravity[i,g] <- sum(sbset10[[g]]/sbset10$d2)
  }
}

data_xls$remove_worksheet("gravity_10")
data_xls$add_worksheet("gravity_10")
data_xls$add_data("gravity_10",as.data.frame(gravity),colNames = TRUE, rowNames = TRUE,)


wb_save(data_xls,file="./excel_files/data_prep.xlsx",overwrite = TRUE)
#
# save the California block groups, and the points for the four states
#
write_sf(ca_blkgrps, "./tiger/ca_blkgrps.shp")
write_sf(subset(blkgrp_azcanvor_pt,select=c('GEOID','COUNTYFP','geometry')), "./tiger/blkgrp_azcanvor_pt.shp")
