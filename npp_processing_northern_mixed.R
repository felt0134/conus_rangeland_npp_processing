#northern_mixed code: linking precip and NPP

#######importing of data##########
#setting extents for rasters
ex_northern_mixed<-extent(npp_northern_mixed_50)
water_year_precip_allyears_northern_mixed<-crop(WatYrprecip_stack,ex_northern_mixed)#precipitation
#water_year_temp_allyears_northern_mixed<-crop(temp_stack,ex_northern_mixed) #growing season soil moisture
#water_year_transp_allyears_northern_mixed<-crop(transp_stack,ex_northern_mixed) #50% transpiration
#water_year_transp_allyears_northern_mixed<-crop(transp_stack,ex_northern_mixed)
#hot dry metric

plot(npp_northern_mixed_50)
plot(water_year_precip_allyears_northern_mixed)
#plot(water_year_temp_allyears_northern_mixed)
#plot(water_year_transp_allyears_northern_mixed)
#hot-dry  metric

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_northern_mixed_p = rasterToPoints(npp_northern_mixed_50); df_npp_northern_mixed = data.frame(npp_northern_mixed_p)
colnames(df_npp_northern_mixed)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_northern_mixed_2<-df_npp_northern_mixed[-c(33,34)] #get rid of 2016, 2017 columns
northern_mixed_npp_melted <- melt(df_npp_northern_mixed_2, 
                                  id.vars = c("x", "y"),
                                  variable.name = "year") #melt to long format
summary(northern_mixed_npp_melted)
northern_mixed_npp_melted$npp<-northern_mixed_npp_melted$value/10 #change npp value to g/m2 scale
northern_mixed_npp_melted_mean<-aggregate(npp ~ x + y,mean,data=northern_mixed_npp_melted) #mean water_year npp

#water_year precipitation
water_yearprecip_northern_mixed_p = rasterToPoints(water_year_precip_allyears_northern_mixed); df_water_yearprecip_northern_mixed = data.frame(water_yearprecip_northern_mixed_p)
colnames(df_water_yearprecip_northern_mixed)[3:32] <-paste(1986:2015) #rename columns to years
northern_mixed_water_yearprecip_melted <- melt(df_water_yearprecip_northern_mixed, 
                                               id.vars = c("x", "y"),
                                               variable.name = "year") #melt to long format

northern_mixed_water_yearprecip_melted$mm<-northern_mixed_water_yearprecip_melted$value*10 #change to mm
northern_mixed_water_yearprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=northern_mixed_water_yearprecip_melted) #get mean water_year precip values

#water_year temperature
water_yeartemp_northern_mixed_p = rasterToPoints(water_year_temp_allyears_northern_mixed); df_water_yeartemp_northern_mixed = data.frame(water_yeartemp_northern_mixed_p)
head(df_water_yeartemp_northern_mixed)
colnames(df_water_yeartemp_northern_mixed)[3:32] <-paste(1986:2015) #rename columns to years
northern_mixed_water_yeartemp_melted <- melt(df_water_yeartemp_northern_mixed, 
                                             id.vars = c("x", "y"),
                                             variable.name = "year") 
northern_mixed_water_yeartemp_melted$temp<-northern_mixed_water_yeartemp_melted$value #rename column temp
northern_mixed_water_yeartemp_melted <- northern_mixed_water_yeartemp_melted[-4] #get rid of value column
northern_mixed_water_yeartemp_melted_mean<-aggregate(temp~x + y,mean,data=northern_mixed_water_yeartemp_melted) #mean water_year temp
head(northern_mixed_water_yeartemp_melted_mean)

#water_year transpiration
water_yeartransp_northern_mixed_p = rasterToPoints(water_year_transp_allyears_northern_mixed); df_water_yeartransp_northern_mixed = data.frame(water_yeartransp_northern_mixed_p)
head(df_water_yeartransp_northern_mixed)
colnames(df_water_yeartransp_northern_mixed)[3:32] <-paste(1986:2015) #rename columns to years
northern_mixed_water_yeartransp_melted <- melt(df_water_yeartransp_northern_mixed, 
                                               id.vars = c("x", "y"),
                                               variable.name = "year") 
northern_mixed_water_yeartransp_melted$transp<-northern_mixed_water_yeartransp_melted$value #rename column transp
northern_mixed_water_yeartransp_melted <- northern_mixed_water_yeartransp_melted[-4] #get rid of value column
northern_mixed_water_yeartransp_melted_mean<-aggregate(transp~x + y,mean,data=northern_mixed_water_yeartransp_melted) #mean water_year transp
head(northern_mixed_water_yeartransp_melted_mean)


######identifying outliers, odd values to be removed##########

#merge the mean npp and ppt datasets
merge_northern_mixed_npp_water_yearprecip<-merge(northern_mixed_npp_melted_mean,northern_mixed_water_yearprecip_melted_mean,by=c("x","y"))
head(merge_northern_mixed_npp_water_yearprecip)

#get mean pue for each grid cell: use for identifying odd pixels
merge_northern_mixed_npp_water_yearprecip$pue<-merge_northern_mixed_npp_water_yearprecip$npp/merge_northern_mixed_npp_water_yearprecip$mm
head(merge_northern_mixed_npp_water_yearprecip)

#diagnostic raster/plot
npp_mm_pue_resid_raster_northern_mixed<-rasterFromXYZ(merge_northern_mixed_npp_water_yearprecip)
plot(npp_mm_pue_resid_raster_northern_mixed)

#isolating pue: analzing the structure
northern_mixed_pue<-merge_northern_mixed_npp_water_yearprecip[-c(3,4)]
head(northern_mixed_pue)
hist(northern_mixed_pue$pue)
sd(northern_mixed_pue$pue)
mean(northern_mixed_pue$pue)
min(northern_mixed_pue$pue)
max(northern_mixed_pue$pue)

#use 3 standard deviations as criteria for anamolous mean PUE values
#round to two decimal places for threshold
0.52 + (3*0.12) #0.88 as threshold for high values
0.52 - (3*0.12) #0.16 as threshold for low values

#northern_mixed actually seems fine, no extremely odd values...
#round to two decimal places

# isolate values greater than 3sd away
pue_anamolies_northern_mixed <- northern_mixed_pue %>%
  dplyr::filter(pue > 0.88 | pue < 0.16) 
summary(pue_anamolies_northern_mixed)
pue_anamolies_northern_mixed_raster<- rasterFromXYZ(pue_anamolies_northern_mixed)
plot(pue_anamolies_northern_mixed_raster)

#turn into geotiff for analaysis in google earth engine
crs(pue_anamolies_northern_mixed_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(pue_anamolies_northern_mixed_raster, "northern_mixed_outliers", format = "GTiff",overwrite=TRUE)

#these outlier values are associated with agriculutre (assessed through google earth), 
#remove them and then look at temporal npp-ppt slopes for the next steps

#merge npp-ppt datasets all years to for dataset to generate slope estimates to identifying outlier pixels
merge_northern_mixed_npp_water_yearprecip_allyears<-merge(northern_mixed_water_yearprecip_melted,northern_mixed_npp_melted ,by=c("x","y","year"))
head(merge_northern_mixed_npp_water_yearprecip_allyears)
merge_northern_mixed_npp_water_yearprecip_allyears_2<-merge_northern_mixed_npp_water_yearprecip_allyears[-c(4,6)]
head(merge_northern_mixed_npp_water_yearprecip_allyears_2)
test_northern_mixed<-rasterFromXYZ(merge_northern_mixed_npp_water_yearprecip_allyears_2) #worked
#for generating slope dataset
library(dplyr)
slope_spatial_northern_mixed <- merge_northern_mixed_npp_water_yearprecip_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_northern_mixed)
northern_mixed_coef_only<- slope_spatial_northern_mixed[ -c(3) ] #isolate coefficient so only slope is graphed
head(northern_mixed_coef_only) #looks good

#merge this dataset with the pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers
northern_mixed_pue_slope<-merge(northern_mixed_coef_only,northern_mixed_pue,by=c('x','y'))
head(northern_mixed_pue_slope)
summary(northern_mixed_pue_slope)
#mask out odd pue values
northern_mixed_pue_slope_2 <- filter(northern_mixed_pue_slope,pue < 0.88,pue > 0.16 ) 
summary(northern_mixed_pue_slope_2)
northern_mixed_pue_slope_3<-northern_mixed_pue_slope_2[-c(4)]
head(northern_mixed_pue_slope_3)

#summary stats of pixel slopes
sd(northern_mixed_pue_slope_3$coef)
mean(northern_mixed_pue_slope_3$coef)
hist(northern_mixed_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.25 + (3*0.08) #0.49
0.25 - (3*0.08) #0.01

northern_mixed_slope_anamolies <- northern_mixed_pue_slope_3 %>%
  dplyr::filter(coef > 0.49 | coef < 0.01) #isolate odd slope values
summary(northern_mixed_slope_anamolies)

#change to raster
northern_mixed_slope_anamolies_raster<- rasterFromXYZ(northern_mixed_slope_anamolies)
plot(northern_mixed_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(northern_mixed_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(northern_mixed_slope_anamolies_raster, "northern_mixed_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculture. remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels
northern_mixed_slope_final<-filter(northern_mixed_pue_slope_3, coef < 0.49,coef > 0.01)
summary(northern_mixed_slope_final)

#make a masking raster for slope
northern_mixed_slope_masking_raster<- rasterFromXYZ(northern_mixed_slope_final)
plot(northern_mixed_slope_masking_raster)
#mask npp
npp_slope_mask_northern_mixed_extent<-crop(npp_northern_mixed_50,northern_mixed_slope_masking_raster)
npp_slope_mask_northern_mixed_mask<-mask(npp_slope_mask_northern_mixed_extent,northern_mixed_slope_masking_raster)
plot(npp_slope_mask_northern_mixed_mask)
#mask ppt
precip_slope_mask_northern_mixed_extent<-crop(water_year_precip_allyears_northern_mixed,northern_mixed_slope_masking_raster)
precip_slope_mask_northern_mixed_mask<-mask(precip_slope_mask_northern_mixed_extent,northern_mixed_slope_masking_raster)
plot(precip_slope_mask_northern_mixed_mask)
#turn into dataframes
#npp
npp_northern_mixed_masked_round_one_p = rasterToPoints(npp_slope_mask_northern_mixed_mask); df_npp_northern_mixed_masked_round_one = data.frame(npp_northern_mixed_masked_round_one_p)
colnames(df_npp_northern_mixed_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_northern_mixed_masked_round_one_2<-df_npp_northern_mixed_masked_round_one[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_northern_mixed_masked_round_one_final <- melt(df_npp_northern_mixed_masked_round_one_2, 
                                                     id.vars = c("x", "y"),
                                                     variable.name = "year") #melt to long format
df_npp_northern_mixed_masked_round_one_final$npp<-df_npp_northern_mixed_masked_round_one_final$value/10 #change npp value to g/m2 scale
head(df_npp_northern_mixed_masked_round_one_final)
df_npp_northern_mixed_masked_round_one_final_2<-df_npp_northern_mixed_masked_round_one_final[-c(4)]
head(df_npp_northern_mixed_masked_round_one_final_2)

#precip
precip_northern_mixed_masked_round_one_p = rasterToPoints(precip_slope_mask_northern_mixed_mask); df_precip_northern_mixed_masked_round_one = data.frame(precip_northern_mixed_masked_round_one_p )
colnames(df_precip_northern_mixed_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_northern_mixed_masked_final_round_one <- melt(df_precip_northern_mixed_masked_round_one, 
                                                        id.vars = c("x", "y"),
                                                        variable.name = "year") #melt to long format

df_precip_northern_mixed_masked_final_round_one$mm<-df_precip_northern_mixed_masked_final_round_one$value*10
df_precip_northern_mixed_masked_final_round_one_2<-df_precip_northern_mixed_masked_final_round_one[-c(4)]
head(df_precip_northern_mixed_masked_final_round_one_2)

#merge these two
northern_mixed_npp_mm_slope_masked<-merge(df_precip_northern_mixed_masked_final_round_one_2,df_npp_northern_mixed_masked_round_one_final_2,by=c('x','y','year'))
#make a yearly pue column
northern_mixed_npp_mm_slope_masked$pue<-northern_mixed_npp_mm_slope_masked$npp/northern_mixed_npp_mm_slope_masked$mm
northern_mixed_npp_mm_slope_masked<-northern_mixed_npp_mm_slope_masked[-c(4,5)]
head(northern_mixed_npp_mm_slope_masked)

#yearly pue anamolies to indentify outlier pixels
sd(northern_mixed_npp_mm_slope_masked$pue)
mean(northern_mixed_npp_mm_slope_masked$pue)
min(northern_mixed_npp_mm_slope_masked$pue)
summary(northern_mixed_npp_mm_slope_masked)
#3 sd for yearly pue
0.53 + 3*0.16 #1.01
0.53 - 3*0.16 #0.05

#isolate anamolies
yearly_pue_anamolies_northern_mixed<-dplyr::filter(northern_mixed_npp_mm_slope_masked,pue>1.01 | pue < .05)
summary(yearly_pue_anamolies_northern_mixed)
yearly_pue_anamolies_northern_mixed_final<-yearly_pue_anamolies_northern_mixed[-c(3)]
head(yearly_pue_anamolies_northern_mixed_final)
odd_pue_values_northern_mixed<-rasterFromXYZ(yearly_pue_anamolies_northern_mixed_final)
plot(odd_pue_values_northern_mixed)

#mask out bad pue pixels from slope masking raster
northern_mixed_masking_extent<-crop(northern_mixed_slope_masking_raster,odd_pue_values_northern_mixed)
northern_mixed_masking<-mask(northern_mixed_masking_extent,odd_pue_values_northern_mixed,inverse=TRUE) #mask out pixels with bad pue
plot(northern_mixed_masking)

#######producing dataframes#########
#produce a final dataframe with all years for analysis
#net primary production
#mask the slope raster by the npp rasterbrick

npp_extent_crop_northern_mixed<-crop(npp_northern_mixed_50,northern_mixed_masking)
plot(npp_extent_crop_northern_mixed)
npp_northern_mixed_masked<-mask(npp_extent_crop_northern_mixed,northern_mixed_masking)
plot(npp_northern_mixed_masked)

#turn into dataframe
npp_northern_mixed_masked_p = rasterToPoints(npp_northern_mixed_masked); df_npp_northern_mixed_masked = data.frame(npp_northern_mixed_masked_p)
colnames(df_npp_northern_mixed_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_northern_mixed_masked_2<-df_npp_northern_mixed_masked[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_northern_mixed_masked_final <- melt(df_npp_northern_mixed_masked_2, 
                                           id.vars = c("x", "y"),
                                           variable.name = "year") #melt to long format
summary(df_npp_northern_mixed_masked_final)
df_npp_northern_mixed_masked_final$npp<-df_npp_northern_mixed_masked_final$value/10 #change npp value to g/m2 scale
head(df_npp_northern_mixed_masked_final)
df_npp_northern_mixed_masked_final_2<-df_npp_northern_mixed_masked_final[-c(4)]
summary(df_npp_northern_mixed_masked_final_2)
str(df_npp_northern_mixed_masked_final_2)

#precipitation
precip_extent_crop_northern_mixed<-crop(water_year_precip_allyears_northern_mixed,northern_mixed_masking)
plot(precip_extent_crop_northern_mixed)
precip_northern_mixed_masked<-mask(precip_extent_crop_northern_mixed,northern_mixed_masking)
plot(precip_northern_mixed_masked)

#turn into dataframe
precip_northern_mixed_masked_p = rasterToPoints(precip_northern_mixed_masked); df_precip_northern_mixed_masked = data.frame(precip_northern_mixed_masked_p)
colnames(df_precip_northern_mixed_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_northern_mixed_masked_final <- melt(df_precip_northern_mixed_masked, 
                                              id.vars = c("x", "y"),
                                              variable.name = "year") #melt to long format
summary(df_precip_northern_mixed_masked_final)
df_precip_northern_mixed_masked_final$mm<-df_precip_northern_mixed_masked_final$value*10
df_precip_northern_mixed_masked_final_2<-df_precip_northern_mixed_masked_final[-c(4)]
summary(df_precip_northern_mixed_masked_final_2)
str(df_precip_northern_mixed_masked_final_2)
#merge
df_npp_ppt_northern_mixed_masked_final<-merge(df_precip_northern_mixed_masked_final_2,df_npp_northern_mixed_masked_final_2,by=c('x','y','year'))
head(df_npp_ppt_northern_mixed_masked_final)
plot(npp~mm,data=df_npp_ppt_northern_mixed_masked_final)

#end