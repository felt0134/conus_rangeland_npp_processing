#cali code: linking precip and NPP

#######importing of data##########
#setting extents for rasters
ex_cali<-extent(npp_cali_50)
water_year_precip_allyears_cali<-crop(WatYrprecip_stack,ex_cali)
#water_year_temp_allyears_cali<-crop(temp_stack,ex_cali) #growing season soil moisture
#water_year_transp_allyears_cali<-crop(transp_stack,ex_cali) day of 50%transpiration
#hot-dry metric

plot(npp_cali_50)
plot(water_year_precip_allyears_cali)
#plot(water_year_temp_allyears_cali)
#plot(water_year_transp_allyears_cali)
#plot(water_year_temp_allyears_cali)

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_cali_p = rasterToPoints(npp_cali_50); df_npp_cali = data.frame(npp_cali_p)
colnames(df_npp_cali)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cali_2<-df_npp_cali[-c(33,34)] #get rid of 2016, 2017 columns
cali_npp_melted <- melt(df_npp_cali_2, 
                        id.vars = c("x", "y"),
                        variable.name = "year") #melt to long format
cali_npp_melted$npp<-cali_npp_melted$value/10 #change npp value to g/m2 scale
cali_npp_melted_mean<-aggregate(npp ~ x + y,mean,data=cali_npp_melted) #mean water_year npp

#water_year precipitation
water_yearprecip_cali_p = rasterToPoints(water_year_precip_allyears_cali); df_water_yearprecip_cali = data.frame(water_yearprecip_cali_p)
head(df_water_yearprecip_cali)
colnames(df_water_yearprecip_cali)[3:32] <-paste(1986:2015) #rename columns to years
cali_water_yearprecip_melted <- melt(df_water_yearprecip_cali, 
                                     id.vars = c("x", "y"),
                                     variable.name = "year") #melt to long format

cali_water_yearprecip_melted$mm<-cali_water_yearprecip_melted$value*10 #change to mm
cali_water_yearprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=cali_water_yearprecip_melted) #get mean water_year precip values

#water_year temperature
water_yeartemp_cali_p = rasterToPoints(water_year_temp_allyears_cali); df_water_yeartemp_cali = data.frame(water_yeartemp_cali_p)
head(df_water_yeartemp_cali)
colnames(df_water_yeartemp_cali)[3:32] <-paste(1986:2015) #rename columns to years
cali_water_yeartemp_melted <- melt(df_water_yeartemp_cali, 
                                   id.vars = c("x", "y"),
                                   variable.name = "year") 
cali_water_yeartemp_melted$temp<-cali_water_yeartemp_melted$value #rename column temp
cali_water_yeartemp_melted <- cali_water_yeartemp_melted[-4] #get rid of value column
cali_water_yeartemp_melted_mean<-aggregate(temp~x + y,mean,data=cali_water_yeartemp_melted) #mean water_year temp
head(cali_water_yeartemp_melted_mean)

#water_year transpiration
water_yeartransp_cali_p = rasterToPoints(water_year_transp_allyears_cali); df_water_yeartransp_cali = data.frame(water_yeartransp_cali_p)
head(df_water_yeartransp_cali)
colnames(df_water_yeartransp_cali)[3:32] <-paste(1986:2015) #rename columns to years
cali_water_yeartransp_melted <- melt(df_water_yeartransp_cali, 
                                     id.vars = c("x", "y"),
                                     variable.name = "year") 
cali_water_yeartransp_melted$transp<-cali_water_yeartransp_melted$value #rename column transp
cali_water_yeartransp_melted <- cali_water_yeartransp_melted[-4] #get rid of value column
cali_water_yeartransp_melted_mean<-aggregate(transp~x + y,mean,data=cali_water_yeartransp_melted) #mean water_year transp
head(cali_water_yeartransp_melted_mean)

######identifying outliers, odd values to be removed##########
#analyze mean PUE

#merge the mean npp and ppt datasets
merge_cali_npp_water_yearprecip<-merge(cali_npp_melted_mean,cali_water_yearprecip_melted_mean,by=c("x","y"))
head(merge_cali_npp_water_yearprecip)

#get mean pue for each grid cell: use for
merge_cali_npp_water_yearprecip$pue<-merge_cali_npp_water_yearprecip$npp/merge_cali_npp_water_yearprecip$mm
head(merge_cali_npp_water_yearprecip)

#diagnostic raster/plot
npp_mm_pue_resid_raster_cali<-rasterFromXYZ(merge_cali_npp_water_yearprecip)
plot(npp_mm_pue_resid_raster_cali)

#isolating pue: analzing the structure
cali_pue<-merge_cali_npp_water_yearprecip[-c(3,4)]
head(cali_pue)
hist(cali_pue$pue)
sd(cali_pue$pue)
mean(cali_pue$pue)
min(cali_pue$pue)
max(cali_pue$pue)

#use 3 standard deviations as criteria for anamolous mean PUE values
#round to two decimal places
0.80 + (3*0.30) #1.7 as threshold for high values
0.80 - (3*0.30) #-0.1 as threshold for low values, lower than actual minimum

#cali actually seems fine, no extremely odd values...
#round to two decimal places

# isolate values greater than 3sd away
pue_anamolies_cali <- cali_pue %>%
  dplyr::filter(pue > 1.7 | pue < 0) 
summary(pue_anamolies_cali)
pue_anamolies_cali_raster<- rasterFromXYZ(pue_anamolies_cali)
plot(pue_anamolies_cali_raster)

#turn into geotiff for analaysis in google earth engine
crs(pue_anamolies_cali_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(pue_anamolies_cali_raster, "cali_outliers", format = "GTiff",overwrite=TRUE)

#these outlier values are associated with agriculutre (assessed through google earth), 
#remove them and then look at temporal npp-ppt slopes for the next step of outlier detection

#merge npp-ppt datasets all years to for dataset to generate slope
merge_cali_npp_water_yearprecip_allyears<-merge(cali_water_yearprecip_melted,cali_npp_melted ,by=c("x","y","year"))
head(merge_cali_npp_water_yearprecip_allyears)
merge_cali_npp_water_yearprecip_allyears_2<-merge_cali_npp_water_yearprecip_allyears[-c(4,6)]
head(merge_cali_npp_water_yearprecip_allyears_2)
summary(merge_cali_npp_water_yearprecip_allyears_2)
test_cali<-rasterFromXYZ(merge_cali_npp_water_yearprecip_allyears_2) #didn't work...

#for generating slope dataset
slope_spatial_cali <- merge_cali_npp_water_yearprecip_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_cali)
cali_coef_only<- slope_spatial_cali[ -c(3) ] #isolate coefficient so only slope is graphed
head(cali_coef_only) #looks good

#merge this dataset with the mean pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers
cali_pue_slope<-merge(cali_coef_only,cali_pue,by=c('x','y'))
head(cali_pue_slope)
summary(cali_pue_slope)

#mask out odd pue values
library(dplyr)
cali_pue_slope_2 <- dplyr::filter(cali_pue_slope,pue<1.7,pue>0)
#cali_pue_slope_2  <-filter(cali_pue_slope_2, pue > -0.15)
summary(cali_pue_slope_2)
cali_pue_slope_3<-cali_pue_slope_2[-c(4)]
head(cali_pue_slope_3)

#summary stats of pixel slopes
sd(cali_pue_slope_3$coef)
mean(cali_pue_slope_3$coef)
hist(cali_pue_slope_3$coef)
min(cali_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.15 + (3*0.12) #0.51
0.15 - (3*0.12) #-0.21 - change this to a zero threshold

cali_slope_anamolies <- cali_pue_slope_3 %>%
  dplyr::filter(coef > 0.51 | coef < -0.21) #isolate odd slope values
summary(cali_slope_anamolies)

#change to raster
cali_slope_anamolies_raster<- rasterFromXYZ(cali_slope_anamolies)
plot(cali_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(cali_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(cali_slope_anamolies_raster, "cali_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculutre. remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels

cali_slope_final<-filter(cali_pue_slope_3, coef < 0.51,coef > -0.21)
summary(cali_slope_final)

#make a masking raster for slope
cali_slope_masking_raster<- rasterFromXYZ(cali_slope_final)
plot(cali_slope_masking_raster)
#mask npp
npp_slope_mask_cali_extent<-crop(npp_cali_50,cali_slope_masking_raster)
npp_slope_mask_cali_mask<-mask(npp_slope_mask_cali_extent,cali_slope_masking_raster)
plot(npp_slope_mask_cali_mask)
#mask ppt
precip_slope_mask_cali_extent<-crop(water_year_precip_allyears_cali,cali_slope_masking_raster)
precip_slope_mask_cali_mask<-mask(precip_slope_mask_cali_extent,cali_slope_masking_raster)
plot(precip_slope_mask_cali_mask)
#turn into dataframes
#npp
npp_cali_masked_round_one_p = rasterToPoints(npp_slope_mask_cali_mask); df_npp_cali_masked_round_one = data.frame(npp_cali_masked_round_one_p)
colnames(df_npp_cali_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cali_masked_round_one_2<-df_npp_cali_masked_round_one[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_cali_masked_round_one_final <- melt(df_npp_cali_masked_round_one_2, 
                                           id.vars = c("x", "y"),
                                           variable.name = "year") #melt to long format
df_npp_cali_masked_round_one_final$npp<-df_npp_cali_masked_round_one_final$value/10 #change npp value to g/m2 scale
head(df_npp_cali_masked_round_one_final)
df_npp_cali_masked_round_one_final_2<-df_npp_cali_masked_round_one_final[-c(4)]
head(df_npp_cali_masked_round_one_final_2)

#precip
precip_cali_masked_round_one_p = rasterToPoints(precip_slope_mask_cali_mask); df_precip_cali_masked_round_one = data.frame(precip_cali_masked_round_one_p )
colnames(df_precip_cali_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_cali_masked_final_round_one <- melt(df_precip_cali_masked_round_one, 
                                              id.vars = c("x", "y"),
                                              variable.name = "year") #melt to long format

df_precip_cali_masked_final_round_one$mm<-df_precip_cali_masked_final_round_one$value*10
df_precip_cali_masked_final_round_one_2<-df_precip_cali_masked_final_round_one[-c(4)]
head(df_precip_cali_masked_final_round_one_2)

#merge these two
cali_npp_mm_slope_masked<-merge(df_precip_cali_masked_final_round_one_2,df_npp_cali_masked_round_one_final_2,by=c('x','y','year'))

#make a pue column to assess grids with yearly anaomolies
cali_npp_mm_slope_masked$pue<-cali_npp_mm_slope_masked$npp/cali_npp_mm_slope_masked$mm
cali_npp_mm_slope_masked<-cali_npp_mm_slope_masked[-c(4,5)]
head(cali_npp_mm_slope_masked)
summary(cali_npp_mm_slope_masked)
cali_npp_mm_slope_masked_2<-na.omit(cali_npp_mm_slope_masked)
summary(cali_npp_mm_slope_masked_2)

#yearly pue anamolies 
sd(cali_npp_mm_slope_masked_2$pue,na.rm=TRUE)
mean(cali_npp_mm_slope_masked_2$pue,na.rm=TRUE)
min(cali_npp_mm_slope_masked_2$pue)
#3 sd for yearly pue
0.90 + 3*0.47 #2.31
0.90 - 3*0.47 #-0.51 #lower than actual minimum

yearly_pue_anamolies_cali<-filter(cali_npp_mm_slope_masked_2,pue > 2.31)
yearly_pue_anamolies_cali_final<-yearly_pue_anamolies_cali[-c(3)]
summary(yearly_pue_anamolies_cali_final)
odd_pue_values_cali<-rasterFromXYZ(yearly_pue_anamolies_cali_final) #works
plot(odd_pue_values_cali)

#mask out bad pue pixels from slope masking raster
cali_masking_extent<-crop(cali_slope_masking_raster,odd_pue_values_cali)
cali_masking<-mask(cali_masking_extent,odd_pue_values_cali,inverse=TRUE) #mask out pixels with bad pue
plot(cali_masking)

#######producing dataframes#########
#produce a final dataframe with all years for analysis
#net primary production
#mask the slope raster by the npp rasterbrick

npp_extent_crop_cali<-crop(npp_cali_50,cali_masking)
plot(npp_extent_crop_cali)
npp_cali_masked<-mask(npp_extent_crop_cali,cali_masking)
plot(npp_cali_masked)

#turn into dataframe
npp_cali_masked_p = rasterToPoints(npp_cali_masked); df_npp_cali_masked = data.frame(npp_cali_masked_p)
colnames(df_npp_cali_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cali_masked_2<-df_npp_cali_masked[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_cali_masked_final <- melt(df_npp_cali_masked_2, 
                                 id.vars = c("x", "y"),
                                 variable.name = "year") #melt to long format
summary(df_npp_cali_masked_final)
df_npp_cali_masked_final$npp<-df_npp_cali_masked_final$value/10 #change npp value to g/m2 scale
head(df_npp_cali_masked_final)
df_npp_cali_masked_final_2<-df_npp_cali_masked_final[-c(4)]
summary(df_npp_cali_masked_final_2)
hist(df_npp_cali_masked_final_2$npp)

#precipitation
precip_extent_crop_cali<-crop(water_year_precip_allyears_cali,cali_masking)
plot(precip_extent_crop_cali)
precip_cali_masked<-mask(precip_extent_crop_cali,cali_masking)
plot(precip_cali_masked)

#turn into dataframe
precip_cali_masked_p = rasterToPoints(precip_cali_masked); df_precip_cali_masked = data.frame(precip_cali_masked_p)
colnames(df_precip_cali_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_cali_masked_final <- melt(df_precip_cali_masked, 
                                    id.vars = c("x", "y"),
                                    variable.name = "year") #melt to long format
summary(df_precip_cali_masked_final)
df_precip_cali_masked_final$mm<-df_precip_cali_masked_final$value*10
df_precip_cali_masked_final_2<-df_precip_cali_masked_final[-c(4)]
summary(df_precip_cali_masked_final_2)
str(df_precip_cali_masked_final_2)
#merge
df_npp_ppt_cali_masked_final<-merge(df_precip_cali_masked_final_2,df_npp_cali_masked_final_2,by=c('x','y','year'))
head(df_npp_ppt_cali_masked_final)
plot(npp~mm,data=df_npp_ppt_cali_masked_final)

#make pue column
df_npp_ppt_cali_masked_final$pue<-df_npp_ppt_cali_masked_final$npp/df_npp_ppt_cali_masked_final$mm
plot(pue~mm,data=df_npp_ppt_cali_masked_final)
summary(df_npp_ppt_cali_masked_final)


#end
