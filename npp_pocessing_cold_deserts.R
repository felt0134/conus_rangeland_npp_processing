#cold_deserts code: linking precip and NPP

#######importing of data##########
#setting extents for rasters
ex_cold_deserts<-extent(npp_cold_deserts_50)
water_year_precip_allyears_cold_deserts<-crop(WatYrprecip_stack,ex_cold_deserts) #precip
#water_year_temp_allyears_cold_deserts<-crop(temp_stack,ex_cold_deserts) #growing season soil moisture
#water_year_transp_allyears_cold_deserts<-crop(transp_stack,ex_cold_deserts) #day of 50% transpiration
#hot-dry metric

plot(npp_cold_deserts_50)
plot(water_year_precip_allyears_cold_deserts)
#plot(water_year_temp_allyears_cold_deserts)
#plot(water_year_transp_allyears_cold_deserts)
#hot dry  metric

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_cold_deserts_p = rasterToPoints(npp_cold_deserts_50); df_npp_cold_deserts = data.frame(npp_cold_deserts_p)
colnames(df_npp_cold_deserts)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cold_deserts_2<-df_npp_cold_deserts[-c(33,34)] #get rid of 2016, 2017 columns
cold_deserts_npp_melted <- melt(df_npp_cold_deserts_2, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format
summary(cold_deserts_npp_melted)
cold_deserts_npp_melted$npp<-cold_deserts_npp_melted$value/10 #change npp value to g/m2 scale
cold_deserts_npp_melted_mean<-aggregate(npp ~ x + y,mean,data=cold_deserts_npp_melted) #mean water_year npp

#water_year precipitation
water_yearprecip_cold_deserts_p = rasterToPoints(water_year_precip_allyears_cold_deserts); df_water_yearprecip_cold_deserts = data.frame(water_yearprecip_cold_deserts_p)
head(df_water_yearprecip_cold_deserts)
colnames(df_water_yearprecip_cold_deserts)[3:32] <-paste(1986:2015) #rename columns to years
cold_deserts_water_yearprecip_melted <- melt(df_water_yearprecip_cold_deserts, 
                                             id.vars = c("x", "y"),
                                             variable.name = "year") #melt to long format

cold_deserts_water_yearprecip_melted$mm<-cold_deserts_water_yearprecip_melted$value*10 #change to mm
cold_deserts_water_yearprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=cold_deserts_water_yearprecip_melted) #get mean water_year precip values

#water_year temperature
water_yeartemp_cold_deserts_p = rasterToPoints(water_year_temp_allyears_cold_deserts); df_water_yeartemp_cold_deserts = data.frame(water_yeartemp_cold_deserts_p)
head(df_water_yeartemp_cold_deserts)
colnames(df_water_yeartemp_cold_deserts)[3:32] <-paste(1986:2015) #rename columns to years
cold_deserts_water_yeartemp_melted <- melt(df_water_yeartemp_cold_deserts, 
                                           id.vars = c("x", "y"),
                                           variable.name = "year") 
cold_deserts_water_yeartemp_melted$temp<-cold_deserts_water_yeartemp_melted$value #rename column temp
cold_deserts_water_yeartemp_melted <- cold_deserts_water_yeartemp_melted[-4] #get rid of value column
cold_deserts_water_yeartemp_melted_mean<-aggregate(temp~x + y,mean,data=cold_deserts_water_yeartemp_melted) #mean water_year temp
head(cold_deserts_water_yeartemp_melted_mean)

#water_year transpiration
water_yeartransp_cold_deserts_p = rasterToPoints(water_year_transp_allyears_cold_deserts); df_water_yeartransp_cold_deserts = data.frame(water_yeartransp_cold_deserts_p)
head(df_water_yeartransp_cold_deserts)
colnames(df_water_yeartransp_cold_deserts)[3:32] <-paste(1986:2015) #rename columns to years
cold_deserts_water_yeartransp_melted <- melt(df_water_yeartransp_cold_deserts, 
                                             id.vars = c("x", "y"),
                                             variable.name = "year") 
cold_deserts_water_yeartransp_melted$transp<-cold_deserts_water_yeartransp_melted$value #rename column transp
cold_deserts_water_yeartransp_melted <- cold_deserts_water_yeartransp_melted[-4] #get rid of value column
cold_deserts_water_yeartransp_melted_mean<-aggregate(transp~x + y,mean,data=cold_deserts_water_yeartransp_melted) #mean water_year transp
head(cold_deserts_water_yeartransp_melted_mean)


######identifying outliers, odd values to be removed##########

#merge the mean npp and ppt datasets
merge_cold_deserts_npp_water_yearprecip<-merge(cold_deserts_npp_melted_mean,cold_deserts_water_yearprecip_melted_mean,by=c("x","y"))
head(merge_cold_deserts_npp_water_yearprecip)

#get mean pue for each grid cell to analyze anamolous cell values
merge_cold_deserts_npp_water_yearprecip$pue<-merge_cold_deserts_npp_water_yearprecip$npp/merge_cold_deserts_npp_water_yearprecip$mm
head(merge_cold_deserts_npp_water_yearprecip)

#diagnostic raster/plot
npp_mm_pue_resid_raster_cold_deserts<-rasterFromXYZ(merge_cold_deserts_npp_water_yearprecip)
plot(npp_mm_pue_resid_raster_cold_deserts)

#isolating pue: analyzing the structure
cold_deserts_pue<-merge_cold_deserts_npp_water_yearprecip[-c(3,4)]
head(cold_deserts_pue)
hist(cold_deserts_pue$pue)
sd(cold_deserts_pue$pue)
mean(cold_deserts_pue$pue)
min(cold_deserts_pue$pue)
max(cold_deserts_pue$pue)

#use 3 standard deviations as criteria for anamolous mean PUE values
#round to two decimal places
0.30 + (3*0.16) #0.78 as threshold for high values
0.30 - (3*0.16) # -0.18 as threshold for low values. lower than the actual minimum

# isolate values greater than 3sd away
pue_anamolies_cold_deserts <- cold_deserts_pue %>%
  dplyr::filter(pue > 0.78 | pue < 0) 
summary(pue_anamolies_cold_deserts)
pue_anamolies_cold_deserts_raster<- rasterFromXYZ(pue_anamolies_cold_deserts)
plot(pue_anamolies_cold_deserts_raster)

#turn into geotiff for analaysis in google earth engine
crs(pue_anamolies_cold_deserts_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(pue_anamolies_cold_deserts_raster, "cold_deserts_outliers", format = "GTiff",overwrite=TRUE)

#these outlier values are associated with agriculutre (assessed through google earth) 
#remove them and then look at temporal npp-ppt slopes for the next steps

#merge npp-ppt datasets all years to for dataset to generate slope values to indentify outlier slope pixels
merge_cold_deserts_npp_water_yearprecip_allyears<-merge(cold_deserts_water_yearprecip_melted,cold_deserts_npp_melted ,by=c("x","y","year"))
head(merge_cold_deserts_npp_water_yearprecip_allyears)
merge_cold_deserts_npp_water_yearprecip_allyears_2<-merge_cold_deserts_npp_water_yearprecip_allyears[-c(4,6)]
head(merge_cold_deserts_npp_water_yearprecip_allyears_2)
test_cold_deserts<-rasterFromXYZ(merge_cold_deserts_npp_water_yearprecip_allyears_2) #didn't work
#for generating slope dataset
slope_spatial_cold_deserts <- merge_cold_deserts_npp_water_yearprecip_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_cold_deserts)
cold_deserts_coef_only<- slope_spatial_cold_deserts[ -c(3) ] #isolate coefficient so only slope is graphed
head(cold_deserts_coef_only) #looks good

#merge this dataset with the pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers

cold_deserts_pue_slope<-merge(cold_deserts_coef_only,cold_deserts_pue,by=c('x','y'))
head(cold_deserts_pue_slope)
summary(cold_deserts_pue_slope)

#mask out odd mean pue values
library(dplyr)
cold_deserts_pue_slope_2 <- dplyr::filter(cold_deserts_pue_slope,pue<0.78,pue > 0)
summary(cold_deserts_pue_slope_2)
cold_deserts_pue_slope_3<-cold_deserts_pue_slope_2[-c(4)]
head(cold_deserts_pue_slope_3)

#summary stats of pixel slopes o indeityf outliers pixels
sd(cold_deserts_pue_slope_3$coef)
mean(cold_deserts_pue_slope_3$coef)
min(cold_deserts_pue_slope_3$coef)
hist(cold_deserts_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.14 + (3*0.074) #0.36
0.14 - (3*0.074) #-0.082, change to zero

cold_deserts_slope_anamolies <- cold_deserts_pue_slope_3 %>%
  dplyr::filter(coef > 0.36 | coef < -0.082 ) #isolate odd slope values
summary(cold_deserts_slope_anamolies)

#change to raster
cold_deserts_slope_anamolies_raster<- rasterFromXYZ(cold_deserts_slope_anamolies)
plot(cold_deserts_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(cold_deserts_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(cold_deserts_slope_anamolies_raster, "cold_deserts_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculutre. remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels
cold_deserts_slope_final<-filter(cold_deserts_pue_slope_3, coef < 0.36,coef > -0.082)
summary(cold_deserts_slope_final)

#make a masking raster for slope
cold_deserts_slope_masking_raster<- rasterFromXYZ(cold_deserts_slope_final)
plot(cold_deserts_slope_masking_raster)
#mask npp
npp_slope_mask_cold_deserts_extent<-crop(npp_cold_deserts_50,cold_deserts_slope_masking_raster)
npp_slope_mask_cold_deserts_mask<-mask(npp_slope_mask_cold_deserts_extent,cold_deserts_slope_masking_raster)
plot(npp_slope_mask_cold_deserts_mask)
#mask ppt
precip_slope_mask_cold_deserts_extent<-crop(water_year_precip_allyears_cold_deserts,cold_deserts_slope_masking_raster)
precip_slope_mask_cold_deserts_mask<-mask(precip_slope_mask_cold_deserts_extent,cold_deserts_slope_masking_raster)
plot(precip_slope_mask_cold_deserts_mask)
#turn into dataframes
#npp
npp_cold_deserts_masked_round_one_p = rasterToPoints(npp_slope_mask_cold_deserts_mask); df_npp_cold_deserts_masked_round_one = data.frame(npp_cold_deserts_masked_round_one_p)
colnames(df_npp_cold_deserts_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cold_deserts_masked_round_one_2<-df_npp_cold_deserts_masked_round_one[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_cold_deserts_masked_round_one_final <- melt(df_npp_cold_deserts_masked_round_one_2, 
                                                   id.vars = c("x", "y"),
                                                   variable.name = "year") #melt to long format
df_npp_cold_deserts_masked_round_one_final$npp<-df_npp_cold_deserts_masked_round_one_final$value/10 #change npp value to g/m2 scale
head(df_npp_cold_deserts_masked_round_one_final)
df_npp_cold_deserts_masked_round_one_final_2<-df_npp_cold_deserts_masked_round_one_final[-c(4)]
head(df_npp_cold_deserts_masked_round_one_final_2)

#precip
precip_cold_deserts_masked_round_one_p = rasterToPoints(precip_slope_mask_cold_deserts_mask); df_precip_cold_deserts_masked_round_one = data.frame(precip_cold_deserts_masked_round_one_p )
colnames(df_precip_cold_deserts_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_cold_deserts_masked_final_round_one <- melt(df_precip_cold_deserts_masked_round_one, 
                                                      id.vars = c("x", "y"),
                                                      variable.name = "year") #melt to long format

df_precip_cold_deserts_masked_final_round_one$mm<-df_precip_cold_deserts_masked_final_round_one$value*10
df_precip_cold_deserts_masked_final_round_one_2<-df_precip_cold_deserts_masked_final_round_one[-c(4)]
head(df_precip_cold_deserts_masked_final_round_one_2)

#merge these two
cold_deserts_npp_mm_slope_masked<-merge(df_precip_cold_deserts_masked_final_round_one_2,df_npp_cold_deserts_masked_round_one_final_2,by=c('x','y','year'))

#make a yearly pue column to further identify odd pixel values
cold_deserts_npp_mm_slope_masked$pue<-cold_deserts_npp_mm_slope_masked$npp/cold_deserts_npp_mm_slope_masked$mm
cold_deserts_npp_mm_slope_masked<-cold_deserts_npp_mm_slope_masked[-c(4,5)]
head(cold_deserts_npp_mm_slope_masked)
summary(cold_deserts_npp_mm_slope_masked)
cold_deserts_npp_mm_slope_masked_2<-na.omit(cold_deserts_npp_mm_slope_masked)
summary(cold_deserts_npp_mm_slope_masked_2)
#yearly pue anamolies 
sd(cold_deserts_npp_mm_slope_masked_2$pue,na.rm=TRUE)
mean(cold_deserts_npp_mm_slope_masked_2$pue,na.rm=TRUE)
min(cold_deserts_npp_mm_slope_masked_2$pue)
#3 sd for yearly pue
0.30 + 3*0.16 #0.78
0.30 - 3*0.16 #-0.26 #below the minimum, which is zero

yearly_pue_anamolies_cold_deserts<-filter(cold_deserts_npp_mm_slope_masked_2,pue > 0.78)
yearly_pue_anamolies_cold_deserts_final<-yearly_pue_anamolies_cold_deserts[-c(3)]
summary(yearly_pue_anamolies_cold_deserts_final)
odd_pue_values_cold_deserts<-rasterFromXYZ(yearly_pue_anamolies_cold_deserts_final) #works
plot(odd_pue_values_cold_deserts)

#mask out bad pue pixels from slope masking raster
cold_deserts_masking_extent<-crop(cold_deserts_slope_masking_raster,odd_pue_values_cold_deserts)
cold_deserts_masking<-mask(cold_deserts_masking_extent,odd_pue_values_cold_deserts,inverse=TRUE) #mask out pixels with bad pue
plot(cold_deserts_masking)

#######producing dataframes#########
#produce a final dataframe with all years for analysis
#net primary production
#mask the slope raster by the npp rasterbrick

npp_extent_crop_cold_deserts<-crop(npp_cold_deserts_50,cold_deserts_masking)
plot(npp_extent_crop_cold_deserts)
npp_cold_deserts_masked<-mask(npp_extent_crop_cold_deserts,cold_deserts_masking)
plot(npp_cold_deserts_masked)

#turn into dataframe
npp_cold_deserts_masked_p = rasterToPoints(npp_cold_deserts_masked); df_npp_cold_deserts_masked = data.frame(npp_cold_deserts_masked_p)
colnames(df_npp_cold_deserts_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cold_deserts_masked_2<-df_npp_cold_deserts_masked[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_cold_deserts_masked_final <- melt(df_npp_cold_deserts_masked_2, 
                                         id.vars = c("x", "y"),
                                         variable.name = "year") #melt to long format
summary(df_npp_cold_deserts_masked_final)
df_npp_cold_deserts_masked_final$npp<-df_npp_cold_deserts_masked_final$value/10 #change npp value to g/m2 scale
head(df_npp_cold_deserts_masked_final)
df_npp_cold_deserts_masked_final_2<-df_npp_cold_deserts_masked_final[-c(4)]
summary(df_npp_cold_deserts_masked_final_2)
str(df_npp_cold_deserts_masked_final_2)
#precipitation
precip_extent_crop_cold_deserts<-crop(water_year_precip_allyears_cold_deserts,cold_deserts_masking)
plot(precip_extent_crop_cold_deserts)
precip_cold_deserts_masked<-mask(precip_extent_crop_cold_deserts,cold_deserts_masking)
plot(precip_cold_deserts_masked)

#turn into dataframe
precip_cold_deserts_masked_p = rasterToPoints(precip_cold_deserts_masked); df_precip_cold_deserts_masked = data.frame(precip_cold_deserts_masked_p)
colnames(df_precip_cold_deserts_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_cold_deserts_masked_final <- melt(df_precip_cold_deserts_masked, 
                                            id.vars = c("x", "y"),
                                            variable.name = "year") #melt to long format
summary(df_precip_cold_deserts_masked_final)
df_precip_cold_deserts_masked_final$mm<-df_precip_cold_deserts_masked_final$value*10
df_precip_cold_deserts_masked_final_2<-df_precip_cold_deserts_masked_final[-c(4)]
summary(df_precip_cold_deserts_masked_final_2)
str(df_precip_cold_deserts_masked_final_2)
#merge
df_npp_ppt_cold_deserts_masked_final<-merge(df_precip_cold_deserts_masked_final_2,df_npp_cold_deserts_masked_final_2,by=c('x','y','year'))
head(df_npp_ppt_cold_deserts_masked_final)
plot(npp~mm,data=df_npp_ppt_cold_deserts_masked_final)

#end