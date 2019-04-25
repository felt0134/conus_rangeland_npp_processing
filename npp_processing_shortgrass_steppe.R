#SGS code: linking precip and NPP

#######importing of data##########
#setting extents for rasters
ex_sgs<-extent(npp_sgs_50)
water_year_precip_allyears_sgs<-crop(WatYrprecip_stack,ex_sgs) #precipitation - water year
#annual_temp_allyears_sgs<-crop(temp_stack,ex_sgs) #dates of 50% transpiration - water year
#annual_transp_allyears_sgs<-crop(transp_stack,ex_sgs) #growing season soil moisture
#hot-dry metric

plot(npp_sgs_50)
plot(water_year_precip_allyears_sgs)
#plot(annual_temp_allyears_sgs)
#plot(annual_transp_allyears_sgs)
#hot-dry metric

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_sgs_p = rasterToPoints(npp_sgs_50); df_npp_sgs = data.frame(npp_sgs_p)
colnames(df_npp_sgs)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_sgs_2<-df_npp_sgs[-c(33,34)] #get rid of 2016, 2017 columns
sgs_npp_melted <- melt(df_npp_sgs_2, 
                       id.vars = c("x", "y"),
                       variable.name = "year") #melt to long format

sgs_npp_melted$npp<-sgs_npp_melted$value/10 #change npp value to g/m2 scale
sgs_npp_melted_mean<-aggregate(npp ~ x + y,mean,data=sgs_npp_melted) #mean annual npp
summary(sgs_npp_melted_mean)

#precipitation
water_year_precip_sgs_p = rasterToPoints(water_year_precip_allyears_sgs); df_water_year_precip_sgs = data.frame(water_year_precip_sgs_p)
colnames(df_water_year_precip_sgs)[3:32] <-paste(1986:2015) #rename columns to years
sgs_water_year_precip_melted <- melt(df_water_year_precip_sgs, 
                                     id.vars = c("x", "y"),
                                     variable.name = "year") #melt to long format

sgs_water_year_precip_melted$mm<-sgs_water_year_precip_melted$value*10 #change to mm
sgs_water_year_precip_melted_mean<-aggregate(mm ~ x + y,mean,data=sgs_water_year_precip_melted) #get mean annual precip values

#annual temperature - will need to change
annualtemp_sgs_p = rasterToPoints(annual_temp_allyears_sgs); df_annualtemp_sgs = data.frame(annualtemp_sgs_p)
head(df_annualtemp_sgs)
colnames(df_annualtemp_sgs)[3:32] <-paste(1986:2015) #rename columns to years
sgs_annualtemp_melted <- melt(df_annualtemp_sgs, 
                              id.vars = c("x", "y"),
                              variable.name = "year") 
sgs_annualtemp_melted$temp<-sgs_annualtemp_melted$value #rename column temp
sgs_annualtemp_melted <- sgs_annualtemp_melted[-4] #get rid of value column
sgs_annualtemp_melted_mean<-aggregate(temp~x + y,mean,data=sgs_annualtemp_melted) #mean annual temp
head(sgs_annualtemp_melted_mean)

#annual transpiration
annualtransp_sgs_p = rasterToPoints(annual_transp_allyears_sgs); df_annualtransp_sgs = data.frame(annualtransp_sgs_p)
head(df_annualtransp_sgs)
colnames(df_annualtransp_sgs)[3:32] <-paste(1986:2015) #rename columns to years
sgs_annualtransp_melted <- melt(df_annualtransp_sgs, 
                                id.vars = c("x", "y"),
                                variable.name = "year") 
sgs_annualtransp_melted$transp<-sgs_annualtransp_melted$value #rename column transp
sgs_annualtransp_melted <- sgs_annualtransp_melted[-4] #get rid of value column
sgs_annualtransp_melted_mean<-aggregate(transp~x + y,mean,data=sgs_annualtransp_melted) #mean annual transp
head(sgs_annualtransp_melted_mean)

######identifying outliers, odd values to be removed##########

#merge the mean npp and ppt datasets
merge_sgs_npp_water_year_precip<-merge(sgs_npp_melted_mean,sgs_water_year_precip_melted_mean,by=c("x","y"))
head(merge_sgs_npp_water_year_precip)

#get mean pue for each grid cell: use for
merge_sgs_npp_water_year_precip$pue<-merge_sgs_npp_water_year_precip$npp/merge_sgs_npp_water_year_precip$mm
head(merge_sgs_npp_water_year_precip)

#isolating mean pue: analzing the structure
head(sgs_pue)
hist(merge_sgs_npp_water_year_precip$pue)
sd(merge_sgs_npp_water_year_precip$pue)
mean(merge_sgs_npp_water_year_precip$pue)
min(merge_sgs_npp_water_year_precip$pue)
max(merge_sgs_npp_water_year_precip$pue)

#use 3 standard deviations as criteria for anamolous mean PUE values
#round to two decimal places
0.46 + (3*0.098)#0.75 as threshold for high values
0.46 - (3*0.098) #0.17 as threshold for low values

#sgs actually seems fine, no extremely odd values...
#round to two decimal places

# isolate values greater than 3sd away
pue_anamolies_sgs <- merge_sgs_npp_water_year_precip %>%
  dplyr::filter(pue > 0.75 | pue < 0.17) #lower than the actual min, indicates right skew
summary(pue_anamolies_sgs)
pue_anamolies_sgs_raster<- rasterFromXYZ(pue_anamolies_sgs)
plot(pue_anamolies_sgs_raster)

#code to turn into geotiff for analaysis in google earth engine
crs(pue_anamolies_sgs_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(pue_anamolies_sgs_raster, "sgs_outliers", format = "GTiff",overwrite=TRUE)

#these outlier values are associated with agriculutre (assessed through google earth), 
#remove them and then look at temporal npp-ppt slopes for the next steps

#merge npp-ppt datasets all years to for dataset to generate slope for identifying outlier pixels
merge_sgs_npp_water_year_precip_allyears<-merge(sgs_water_year_precip_melted,sgs_npp_melted ,by=c("x","y","year"))
head(merge_sgs_npp_water_year_precip_allyears)
merge_sgs_npp_water_year_precip_allyears_2<-merge_sgs_npp_water_year_precip_allyears[-c(4,6)]
head(merge_sgs_npp_water_year_precip_allyears_2)
test<-rasterFromXYZ(merge_sgs_npp_water_year_precip_allyears_2)

#for generating slope dataset
library(dplyr)
slope_spatial_sgs <- merge_sgs_npp_water_year_precip_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_sgs)
sgs_coef_only<- slope_spatial_sgs[ -c(3) ] #isolate coefficient so only slope is graphed
head(sgs_coef_only) #looks good

#merge this dataset with the pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers

sgs_pue_slope<-merge(sgs_coef_only,merge_sgs_npp_water_year_precip,by=c('x','y'))
head(sgs_pue_slope)
#mask out odd pue values
sgs_pue_slope_2 <- filter(sgs_pue_slope,pue < 0.75, pue > 0.17 )
summary(sgs_pue_slope_2)
sgs_pue_slope_3<-sgs_pue_slope_2[-c(4)]
head(sgs_pue_slope_3)
summary(sgs_pue_slope_3)
#summary stats of pixel slopes
sd(sgs_pue_slope_3$coef) #0.083
mean(sgs_pue_slope_3$coef)#0.32
hist(sgs_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.32 + (3*0.083) #0.57
0.32 - (3*0.083) #0.071

sgs_slope_anamolies <- sgs_pue_slope_3 %>%
  dplyr::filter(coef > 0.57 |coef < 0.071) #isolate odd slope values
summary(sgs_slope_anamolies)

#change to raster
sgs_slope_anamolies_raster<- rasterFromXYZ(sgs_slope_anamolies)
plot(sgs_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(sgs_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(sgs_slope_anamolies_raster, "sgs_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculutre. remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels

#remove odd high and low values
library(dplyr)
sgs_slope_final<-dplyr::filter(sgs_pue_slope_3, coef < 0.57, coef > 0.071)
summary(sgs_slope_final)
head(sgs_slope_final)
sgs_slope_final<-sgs_slope_final[,-c(4,5)]
head(sgs_slope_final)
#make a masking raster for slope
sgs_slope_masking_raster<- rasterFromXYZ(sgs_slope_final)
plot(sgs_slope_masking_raster)
#mask npp
npp_slope_mask_sgs_extent<-crop(npp_sgs_50,sgs_slope_masking_raster)
npp_slope_mask_sgs_mask<-mask(npp_slope_mask_sgs_extent,sgs_slope_masking_raster)
plot(npp_slope_mask_sgs_mask)
#mask ppt
precip_slope_mask_sgs_extent<-crop(water_year_precip_allyears_sgs,sgs_slope_masking_raster)
precip_slope_mask_sgs_mask<-mask(precip_slope_mask_sgs_extent,sgs_slope_masking_raster)
plot(npp_slope_mask_sgs_mask)
#turn into dataframes
#npp
npp_sgs_masked_round_one_p = rasterToPoints(npp_slope_mask_sgs_mask); df_npp_sgs_masked_round_one = data.frame(npp_sgs_masked_round_one_p)
colnames(df_npp_sgs_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_sgs_masked_round_one_2<-df_npp_sgs_masked_round_one[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_sgs_masked_round_one_final <- melt(df_npp_sgs_masked_round_one_2, 
                                          id.vars = c("x", "y"),
                                          variable.name = "year") #melt to long format
df_npp_sgs_masked_round_one_final$npp<-df_npp_sgs_masked_round_one_final$value/10 #change npp value to g/m2 scale
head(df_npp_sgs_masked_round_one_final)
df_npp_sgs_masked_round_one_final_2<-df_npp_sgs_masked_round_one_final[-c(4)]
head(df_npp_sgs_masked_round_one_final_2)

#precip
precip_sgs_masked_round_one_p = rasterToPoints(precip_slope_mask_sgs_mask); df_precip_sgs_masked_round_one = data.frame(precip_sgs_masked_round_one_p )
colnames(df_precip_sgs_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_sgs_masked_final_round_one <- melt(df_precip_sgs_masked_round_one, 
                                             id.vars = c("x", "y"),
                                             variable.name = "year") #melt to long format

df_precip_sgs_masked_final_round_one$mm<-df_precip_sgs_masked_final_round_one$value*10
df_precip_sgs_masked_final_round_one_2<-df_precip_sgs_masked_final_round_one[-c(4)]
head(df_precip_sgs_masked_final_round_one_2)

#merge these two
sgs_npp_mm_slope_masked<-merge(df_precip_sgs_masked_final_round_one_2,df_npp_sgs_masked_round_one_final_2,by=c('x','y','year'))
head(sgs_npp_mm_slope_masked)

#make a yearly pue column to identify outlier pixels
sgs_npp_mm_slope_masked$pue<-sgs_npp_mm_slope_masked$npp/sgs_npp_mm_slope_masked$mm
sgs_npp_mm_slope_masked<-sgs_npp_mm_slope_masked[-c(4,5)]
head(sgs_npp_mm_slope_masked)

#yearly pue anamolies (incorporate later)
sd(sgs_npp_mm_slope_masked$pue)
mean(sgs_npp_mm_slope_masked$pue)
min(sgs_npp_mm_slope_masked$pue)
#3 sd for yearly pue
0.47 + (3*0.15)
0.47 - (3*0.15)

yearly_pue_anamolies_sgs<-filter(sgs_npp_mm_slope_masked,pue > 0.92 | pue < 0.02)
yearly_pue_anamolies_sgs_final<-yearly_pue_anamolies_sgs[-c(3)]
summary(yearly_pue_anamolies_sgs_final)
odd_pue_values_sgs<-rasterFromXYZ(yearly_pue_anamolies_sgs_final)
plot(odd_pue_values_sgs)

#mask out bad pue pixels from slope masking raster
sgs_masking_extent<-crop(sgs_slope_masking_raster,odd_pue_values_sgs)
sgs_masking<-mask(sgs_masking_extent,odd_pue_values_sgs,inverse=TRUE) #mask out pixels with bad pue
plot(sgs_masking)

#######producing dataframes#########
#produce a final dataframe with all years for analysis
#net primary production
#mask the slope raster by the npp rasterbrick

npp_extent_crop_sgs<-crop(npp_sgs_50,sgs_masking)
plot(npp_extent_crop_sgs)
npp_sgs_masked<-mask(npp_extent_crop_sgs,sgs_masking)
plot(npp_sgs_masked)

#turn into dataframe
npp_sgs_masked_p = rasterToPoints(npp_sgs_masked); df_npp_sgs_masked = data.frame(npp_sgs_masked_p)
colnames(df_npp_sgs_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_sgs_masked_2<-df_npp_sgs_masked[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_sgs_masked_final <- melt(df_npp_sgs_masked_2, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format
summary(df_npp_sgs_masked_final)
df_npp_sgs_masked_final$npp<-df_npp_sgs_masked_final$value/10 #change npp value to g/m2 scale
head(df_npp_sgs_masked_final)
df_npp_sgs_masked_final_2<-df_npp_sgs_masked_final[-c(4)]
summary(df_npp_sgs_masked_final_2)
str(df_npp_sgs_masked_final_2)
#precipitation
precip_extent_crop_sgs<-crop(water_year_precip_allyears_sgs,sgs_masking)
plot(precip_extent_crop_sgs)
precip_sgs_masked<-mask(precip_extent_crop_sgs,sgs_masking)
plot(precip_sgs_masked)

#turn into dataframe
precip_sgs_masked_p = rasterToPoints(precip_sgs_masked); df_precip_sgs_masked = data.frame(precip_sgs_masked_p)
colnames(df_precip_sgs_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_sgs_masked_final <- melt(df_precip_sgs_masked, 
                                   id.vars = c("x", "y"),
                                   variable.name = "year") #melt to long format
summary(df_precip_sgs_masked_final)
df_precip_sgs_masked_final$mm<-df_precip_sgs_masked_final$value*10
df_precip_sgs_masked_final_2<-df_precip_sgs_masked_final[-c(4)]
summary(df_precip_sgs_masked_final_2)
str(df_precip_sgs_masked_final_2)
#merge
df_npp_ppt_sgs_masked_final<-merge(df_precip_sgs_masked_final_2,df_npp_sgs_masked_final_2,by=c('x','y','year'))
head(df_npp_ppt_sgs_masked_final)
plot(npp~mm,data=df_npp_ppt_sgs_masked_final)

#end