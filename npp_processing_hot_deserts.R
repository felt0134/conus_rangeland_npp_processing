#hot_deserts npp-climate integration
##importing data########
#setting extents for rasters
ex_hot_deserts<-extent(npp_hot_deserts_50)
water_year_precip_allyears_hot_deserts<-crop(WatYrprecip_stack,ex_hot_deserts)
#water_year_temp_allyears_hot_deserts<-crop(temp_stack,ex_hot_deserts) #growing season soil moisture
#water_year_transp_allyears_hot_deserts<-crop(transp_stack,ex_hot_deserts) #50% transpiration
# hot dry metric

plot(npp_hot_deserts_50)
plot(water_year_precip_allyears_hot_deserts)
#plot(water_year_temp_allyears_hot_deserts)
#plot(water_year_transp_allyears_hot_deserts)
#hot-dry metric

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_hot_deserts_p = rasterToPoints(npp_hot_deserts_50); df_npp_hot_deserts = data.frame(npp_hot_deserts_p)
colnames(df_npp_hot_deserts)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_hot_deserts_2<-df_npp_hot_deserts[-c(33,34)] #get rid of 2016, 2017 columns
hot_deserts_npp_melted <- melt(df_npp_hot_deserts_2, 
                               id.vars = c("x", "y"),
                               variable.name = "year") #melt to long format

hot_deserts_npp_melted$npp<-hot_deserts_npp_melted$value/10 #change npp value to g/m2 scale
hot_deserts_npp_melted_mean<-aggregate(npp ~ x + y,mean,data=hot_deserts_npp_melted) #mean water_year npp
head(hot_deserts_npp_melted_mean)
summary(hot_deserts_npp_melted)

#water_year precipitation
water_yearprecip_hot_deserts_p = rasterToPoints(water_year_precip_allyears_hot_deserts); df_water_yearprecip_hot_deserts = data.frame(water_yearprecip_hot_deserts_p)
head(df_water_yearprecip_hot_deserts)
colnames(df_water_yearprecip_hot_deserts)[3:32] <-paste(1986:2015) #rename columns to years
hot_deserts_water_yearprecip_melted <- melt(df_water_yearprecip_hot_deserts, 
                                            id.vars = c("x", "y"),
                                            variable.name = "year") #melt to long format

hot_deserts_water_yearprecip_melted$mm<-hot_deserts_water_yearprecip_melted$value*10 #change to mm
hot_deserts_water_yearprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=hot_deserts_water_yearprecip_melted) #get mean water_year precip values
summary(hot_deserts_water_yearprecip_melted)

#water_year temperature
water_yeartemp_hot_deserts_p = rasterToPoints(water_year_temp_allyears_hot_deserts); df_water_yeartemp_hot_deserts = data.frame(water_yeartemp_hot_deserts_p)
head(df_water_yeartemp_hot_deserts)
colnames(df_water_yeartemp_hot_deserts)[3:32] <-paste(1986:2015) #rename columns to years
hot_deserts_water_yeartemp_melted <- melt(df_water_yeartemp_hot_deserts, 
                                          id.vars = c("x", "y"),
                                          variable.name = "year") 
hot_deserts_water_yeartemp_melted$temp<-hot_deserts_water_yeartemp_melted$value #rename column temp
hot_deserts_water_yeartemp_melted <- hot_deserts_water_yeartemp_melted[-4] #get rid of value column
hot_deserts_water_yeartemp_melted_mean<-aggregate(temp~x + y,mean,data=hot_deserts_water_yeartemp_melted) #mean water_year temp
head(hot_deserts_water_yeartemp_melted_mean)

#water_year transpiration
water_yeartransp_hot_deserts_p = rasterToPoints(water_year_transp_allyears_hot_deserts); df_water_yeartransp_hot_deserts = data.frame(water_yeartransp_hot_deserts_p)
head(df_water_yeartransp_hot_deserts)
colnames(df_water_yeartransp_hot_deserts)[3:32] <-paste(1986:2015) #rename columns to years
hot_deserts_water_yeartransp_melted <- melt(df_water_yeartransp_hot_deserts, 
                                            id.vars = c("x", "y"),
                                            variable.name = "year") 
hot_deserts_water_yeartransp_melted$transp<-hot_deserts_water_yeartransp_melted$value #rename column transp
hot_deserts_water_yeartransp_melted <- hot_deserts_water_yeartransp_melted[-4] #get rid of value column
hot_deserts_water_yeartransp_melted_mean<-aggregate(transp~x + y,mean,data=hot_deserts_water_yeartransp_melted) #mean water_year transp
head(hot_deserts_water_yeartransp_melted_mean)

#####identifying odd values to remove####
#merge the mean npp and ppt datasets
merge_hot_deserts_npp_water_yearprecip<-merge(hot_deserts_npp_melted_mean,hot_deserts_water_yearprecip_melted_mean,by=c("x","y"))
head(merge_hot_deserts_npp_water_yearprecip)

#get mean pue for each grid cell: use for identifying odd pixels
merge_hot_deserts_npp_water_yearprecip$pue<-merge_hot_deserts_npp_water_yearprecip$npp/merge_hot_deserts_npp_water_yearprecip$mm
head(merge_hot_deserts_npp_water_yearprecip)

#diagnostic raster/plot
npp_mm_pue_resid_raster_hot_deserts<-rasterFromXYZ(merge_hot_deserts_npp_water_yearprecip)
plot(npp_mm_pue_resid_raster_hot_deserts)

#isolating pue: analzing the structure
hot_deserts_pue<-merge_hot_deserts_npp_water_yearprecip[-c(3,4)]
head(hot_deserts_pue)
hist(hot_deserts_pue$pue)
sd(hot_deserts_pue$pue)
mean(hot_deserts_pue$pue)
min(hot_deserts_pue$pue)
max(hot_deserts_pue$pue)

#use 3 standard deviations as criteria for anamolous mean PUE values
#round to two decimal places
0.17 + (3*0.12) #0.53 as threshold for high values
0.17 - (3*0.12) #-0.19 as threshold for low values, lower than the actual minimum

# isolate values greater than 3sd away
pue_anamolies_hot_deserts <- hot_deserts_pue %>%
  dplyr::filter(pue > 0.53) 
summary(pue_anamolies_hot_deserts)
pue_anamolies_hot_deserts_raster<- rasterFromXYZ(pue_anamolies_hot_deserts)
plot(pue_anamolies_hot_deserts_raster)

#turn into geotiff for analaysis in google earth engine
crs(pue_anamolies_hot_deserts_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(pue_anamolies_hot_deserts_raster, "hot_deserts_outliers", format = "GTiff",overwrite=TRUE)

#these outlier values are associated with agriculutre (assessed through google earth), 
#remove them and then look at temporal npp-ppt slopes for the next steps

#merge npp-ppt datasets all years for dataset to generate slope
merge_hot_deserts_npp_water_yearprecip_allyears<-merge(hot_deserts_water_yearprecip_melted,hot_deserts_npp_melted ,by=c("x","y","year"))
head(merge_hot_deserts_npp_water_yearprecip_allyears)
merge_hot_deserts_npp_water_yearprecip_allyears_2<-merge_hot_deserts_npp_water_yearprecip_allyears[-c(4,6)]
head(merge_hot_deserts_npp_water_yearprecip_allyears_2)
test<-rasterFromXYZ(merge_hot_deserts_npp_water_yearprecip_allyears_2)

#for generating slope dataset to indentify outlier slope pixels
library(dplyr)
slope_spatial_hot_deserts <- merge_hot_deserts_npp_water_yearprecip_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_hot_deserts)
hot_deserts_coef_only<- slope_spatial_hot_deserts[ -c(3) ] #isolate coefficient so only slope is graphed
head(hot_deserts_coef_only) #looks good

#merge this dataset with the pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers
hot_deserts_pue_slope<-merge(hot_deserts_coef_only,hot_deserts_pue,by=c('x','y'))
head(hot_deserts_pue_slope)
summary(hot_deserts_pue_slope)

#mask out odd pue values first
hot_deserts_pue_slope_2 <- filter(hot_deserts_pue_slope,pue < 0.53)
hot_deserts_pue_slope_3<-hot_deserts_pue_slope_2[-c(4)]
head(hot_deserts_pue_slope_3)
summary(hot_deserts_pue_slope_3)
#summary stats of pixel slopes
sd(hot_deserts_pue_slope_3$coef)
mean(hot_deserts_pue_slope_3$coef)
min(hot_deserts_pue_slope_3$coef)
hist(hot_deserts_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.15 + (3*0.085) #0.41
0.15 - (3*0.085) #-0.11 change to zero for threshold

hot_deserts_slope_anamolies <- hot_deserts_pue_slope_3 %>%
  dplyr::filter(coef > 0.41 | coef < -0.11) #isolate odd slope values
summary(hot_deserts_slope_anamolies)

#change to raster
hot_deserts_slope_anamolies_raster<- rasterFromXYZ(hot_deserts_slope_anamolies)
plot(hot_deserts_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(hot_deserts_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(hot_deserts_slope_anamolies_raster, "hot_deserts_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculutre. remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels
# do this individually for each tail so it actually works....
hot_deserts_slope_final <- filter(hot_deserts_pue_slope_3,coef < 0.40, coef > -0.11)
summary(hot_deserts_slope_final)

#make a masking raster for slope
hot_deserts_slope_masking_raster<- rasterFromXYZ(hot_deserts_slope_final)
plot(hot_deserts_slope_masking_raster)
#mask npp
npp_slope_mask_hot_deserts_extent<-crop(npp_hot_deserts_50,hot_deserts_slope_masking_raster)
npp_slope_mask_hot_deserts_mask<-mask(npp_slope_mask_hot_deserts_extent,hot_deserts_slope_masking_raster)
plot(npp_slope_mask_hot_deserts_mask)
#mask ppt
precip_slope_mask_hot_deserts_extent<-crop(water_year_precip_allyears_hot_deserts,hot_deserts_slope_masking_raster)
precip_slope_mask_hot_deserts_mask<-mask(precip_slope_mask_hot_deserts_extent,hot_deserts_slope_masking_raster)
plot(precip_slope_mask_hot_deserts_mask)
#turn into dataframes
#npp
npp_hot_deserts_masked_round_one_p = rasterToPoints(npp_slope_mask_hot_deserts_mask); df_npp_hot_deserts_masked_round_one = data.frame(npp_hot_deserts_masked_round_one_p)
colnames(df_npp_hot_deserts_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_hot_deserts_masked_round_one_2<-df_npp_hot_deserts_masked_round_one[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_hot_deserts_masked_round_one_final <- melt(df_npp_hot_deserts_masked_round_one_2, 
                                                  id.vars = c("x", "y"),
                                                  variable.name = "year") #melt to long format
df_npp_hot_deserts_masked_round_one_final$npp<-df_npp_hot_deserts_masked_round_one_final$value/10 #change npp value to g/m2 scale
head(df_npp_hot_deserts_masked_round_one_final)
df_npp_hot_deserts_masked_round_one_final_2<-df_npp_hot_deserts_masked_round_one_final[-c(4)]
head(df_npp_hot_deserts_masked_round_one_final_2)

#precip
precip_hot_deserts_masked_round_one_p = rasterToPoints(precip_slope_mask_hot_deserts_mask); df_precip_hot_deserts_masked_round_one = data.frame(precip_hot_deserts_masked_round_one_p )
colnames(df_precip_hot_deserts_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_hot_deserts_masked_final_round_one <- melt(df_precip_hot_deserts_masked_round_one, 
                                                     id.vars = c("x", "y"),
                                                     variable.name = "year") #melt to long format

df_precip_hot_deserts_masked_final_round_one$mm<-df_precip_hot_deserts_masked_final_round_one$value*10
df_precip_hot_deserts_masked_final_round_one_2<-df_precip_hot_deserts_masked_final_round_one[-c(4)]
head(df_precip_hot_deserts_masked_final_round_one_2)

#merge these two
hot_deserts_npp_mm_slope_masked<-merge(df_precip_hot_deserts_masked_final_round_one_2,df_npp_hot_deserts_masked_round_one_final_2,by=c('x','y','year'))

#make a yearly pue column to assess outlier pixels again
hot_deserts_npp_mm_slope_masked$pue<-hot_deserts_npp_mm_slope_masked$npp/hot_deserts_npp_mm_slope_masked$mm
hot_deserts_npp_mm_slope_masked<-hot_deserts_npp_mm_slope_masked[-c(4,5)]
head(hot_deserts_npp_mm_slope_masked)

#yearly pue anamolies 
sd(hot_deserts_npp_mm_slope_masked$pue)
mean(hot_deserts_npp_mm_slope_masked$pue)
min(hot_deserts_npp_mm_slope_masked$pue)
max(hot_deserts_npp_mm_slope_masked$pue)
hist(hot_deserts_npp_mm_slope_masked$pue)
#3 sd for yearly pue
0.18 + 3*0.17 # 0.69
0.18 - 3*0.17 # below zero

yearly_pue_anamolies_hot_deserts<-filter(hot_deserts_npp_mm_slope_masked,pue > 0.69)
yearly_pue_anamolies_hot_deserts_final<-yearly_pue_anamolies_hot_deserts[-c(3)]
summary(yearly_pue_anamolies_hot_deserts_final)
head(yearly_pue_anamolies_hot_deserts_final)

odd_pue_values_hot_deserts<-rasterFromXYZ(yearly_pue_anamolies_hot_deserts_final)
plot(odd_pue_values_hot_deserts)

#final masking raster
hot_deserts_masking_extent<-crop(hot_deserts_slope_masking_raster,odd_pue_values_hot_deserts)
hot_deserts_masking<-mask(hot_deserts_masking_extent,odd_pue_values_hot_deserts,inverse=TRUE) #mask out pixels with bad pue
plot(hot_deserts_masking)

#######producing dataframes#########
#produce a final dataframe with all years for analysis
#net primary production
#mask the slope raster by the npp rasterbrick

npp_extent_crop_hot_deserts<-crop(npp_hot_deserts_50,hot_deserts_masking)
plot(npp_extent_crop_hot_deserts)
npp_hot_deserts_masked<-mask(npp_extent_crop_hot_deserts,hot_deserts_masking)
plot(npp_hot_deserts_masked)
library(dplyr)
library(reshape2)

#turn into dataframe
npp_hot_deserts_masked_p = rasterToPoints(npp_hot_deserts_masked); df_npp_hot_deserts_masked = data.frame(npp_hot_deserts_masked_p)
colnames(df_npp_hot_deserts_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_hot_deserts_masked_2<-df_npp_hot_deserts_masked[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_hot_deserts_masked_final <- melt(df_npp_hot_deserts_masked_2, 
                                        id.vars = c("x", "y"),
                                        variable.name = "year") #melt to long format
summary(df_npp_hot_deserts_masked_final)
df_npp_hot_deserts_masked_final$npp<-df_npp_hot_deserts_masked_final$value/10 #change npp value to g/m2 scale
head(df_npp_hot_deserts_masked_final)
df_npp_hot_deserts_masked_final_2<-df_npp_hot_deserts_masked_final[-c(4)]
summary(df_npp_hot_deserts_masked_final_2)
str(df_npp_hot_deserts_masked_final_2)

#precipitation
precip_extent_crop_hot_deserts<-crop(water_year_precip_allyears_hot_deserts,hot_deserts_masking)
plot(precip_extent_crop_hot_deserts)
precip_hot_deserts_masked<-mask(precip_extent_crop_hot_deserts,hot_deserts_masking)
plot(precip_hot_deserts_masked)

#turn into dataframe
precip_hot_deserts_masked_p = rasterToPoints(precip_hot_deserts_masked); df_precip_hot_deserts_masked = data.frame(precip_hot_deserts_masked_p)
colnames(df_precip_hot_deserts_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_hot_deserts_masked_final <- melt(df_precip_hot_deserts_masked, 
                                           id.vars = c("x", "y"),
                                           variable.name = "year") #melt to long format
summary(df_precip_hot_deserts_masked_final)
df_precip_hot_deserts_masked_final$mm<-df_precip_hot_deserts_masked_final$value*10
df_precip_hot_deserts_masked_final_2<-df_precip_hot_deserts_masked_final[-c(4)]
summary(df_precip_hot_deserts_masked_final_2)
str(df_precip_hot_deserts_masked_final_2)
#merge
df_npp_ppt_hot_deserts_masked_final<-merge(df_precip_hot_deserts_masked_final_2,df_npp_hot_deserts_masked_final_2,by=c('x','y','year'))
head(df_npp_ppt_hot_deserts_masked_final)
plot(npp~mm,data=df_npp_ppt_hot_deserts_masked_final)

#make pue column
df_npp_ppt_hot_deserts_masked_final$pue<-df_npp_ppt_hot_deserts_masked_final$npp/df_npp_ppt_hot_deserts_masked_final$mm
plot(pue~mm,data=df_npp_ppt_hot_deserts_masked_final)
summary(df_npp_ppt_hot_deserts_masked_final)

#end