#####code to create the final dataframe##########
#from the individual site-based np-climate integration scripts
#sgs
head(df_npp_ppt_sgs_masked_final)
plot(npp~mm,data=df_npp_ppt_sgs_masked_final)
df_npp_ppt_sgs_masked_final['region']<-'semi-arid_steppe'
#california
head(df_npp_ppt_cali_masked_final)
plot(npp~mm,data=df_npp_ppt_cali_masked_final)
df_npp_ppt_cali_masked_final['region']<-'california_annuals'
#cold deserts
head(df_npp_ppt_cold_deserts_masked_final)
plot(npp~mm,data=df_npp_ppt_cold_deserts_masked_final)
df_npp_ppt_cold_deserts_masked_final['region']<-'cold_deserts'
#hot deserts
head(df_npp_ppt_hot_deserts_masked_final)
plot(npp~mm,data=df_npp_ppt_hot_deserts_masked_final)
df_npp_ppt_hot_deserts_masked_final['region']<-'hot_deserts'
#north mixed prairies
head(df_npp_ppt_northern_mixed_masked_final)
plot(npp~mm,data=df_npp_ppt_northern_mixed_masked_final)
df_npp_ppt_northern_mixed_masked_final['region']<-'northern_mixed_prairies'

#combine datasets
final<-rbind(df_npp_ppt_northern_mixed_masked_final,df_npp_ppt_hot_deserts_masked_final)
final_2<-rbind(final,df_npp_ppt_hot_deserts_masked_final,df_npp_ppt_cold_deserts_masked_final)
final_3<-rbind(final_2,df_npp_ppt_cali_masked_final)
final_4<-rbind(final_3,df_npp_ppt_sgs_masked_final)
head(final_4)

#creating a final_dataframe
final_5<-final_4[-4]
head(final_5)
#as a .csv
write.csv(final_5,file = "conus_rangeland_npp_processed_7km_nozerothreshold.csv")
unique(production_mean$region)
# as a.r
saveRDS(final_5,file = "conus_rangeland_npp_processed_wy_7km.rds")

#######masking precip by processed npp#######
conus_rangeland_npp <- read.csv("/Users/A02296270/Desktop/CONUS_AFRI/CONUS/conus_rangeland_npp_processed_7km_nozerothreshold.csv")
head(conus_rangeland_npp)
#mean.test<-aggregate(npp~ x+ y + region,mean,data=conus_rangeland_npp) # check for repeats
summary(conus_rangeland_npp)
library(tidyr)
library(dplyr)
library(raster)
library(sp)
library(reshape2)
final_6<-conus_rangeland_npp[-c(1,6)]
conus_split<-split(final_6,final_6$year)
conus_split_2<-lapply(conus_split, function(x) { x["year"] <- NULL; x })
colnames <- c("x","y","z") 
conus_split_3<-lapply(conus_split_2, setNames, colnames)
conus_split_4<-lapply(conus_split_3, rasterFromXYZ)
conus_npp_procossed_stack <-stack(conus_split_4)
plot(conus_npp_procossed_stack)

#processing climate data
extent_npp_processed<-extent(conus_npp_procossed_stack)

####water year precipitation####
wy_precip_extent_npp_processed<-crop(WatYrprecip_stack_2,extent_npp_processed)
wy_precip_mask_npp_processed<-mask(wy_precip_extent_npp_processed,conus_npp_procossed_stack)
plot(wy_precip_mask_npp_processed)
#changing this to a dataframe
precip_final_allsites = rasterToPoints(wy_precip_mask_npp_processed); df_precip_final_allsites = data.frame(precip_final_allsites)
colnames(df_precip_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_final_allsites_melted <- melt(df_precip_final_allsites, 
                                        id.vars = c("x", "y"),
                                        variable.name = "year") #melt to long format
df_precip_final_allsites_melted$mm<-df_precip_final_allsites_melted$value*10
df_precip_final_allsites_melted_2<-df_precip_final_allsites_melted[-4]

#merge with npp dataframe
npp_veg_final<-conus_rangeland_npp[-c(1)]
npp_precip_all_sites<-merge(df_precip_final_allsites_melted_2,npp_veg_final,by=c("x","y","year"))
head(npp_precip_all_sites)

###soil moisture january-march####
annualSWA_JanMar_extent_npp_processed<-crop(SWA_JanMar_stack,extent_npp_processed)
annualSWA_JanMar_mask_npp_processed<-mask(annualSWA_JanMar_extent_npp_processed,conus_npp_procossed_stack)
plot(annualSWA_JanMar_mask_npp_processed)
#changing this to a dataframe
annualSWA_JanMar_final_allsites = rasterToPoints(annualSWA_JanMar_mask_npp_processed); df_annualSWA_JanMar_final_allsites = data.frame(annualSWA_JanMar_final_allsites)
colnames(df_annualSWA_JanMar_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_annualSWA_JanMar_final_allsites_melted <- melt(df_annualSWA_JanMar_final_allsites, 
                                                  id.vars = c("x", "y"),
                                                  variable.name = "year") #melt to long format

df_annualSWA_JanMar_final_allsites_melted$jan_march_swc<-df_annualSWA_JanMar_final_allsites_melted$value
df_swc_january_march<-df_annualSWA_JanMar_final_allsites_melted[-4]

#merge with npp dataframe
npp_climate_all_sites_2<-merge(npp_precip_all_sites,df_swc_january_march,by=c("x","y","year"))
head(npp_climate_all_sites_2)


###soil moisture April-June####
annualSWA_AprJun_extent_npp_processed<-crop(SWA_AprJun_stack,extent_npp_processed)
annualSWA_AprJun_mask_npp_processed<-mask(annualSWA_AprJun_extent_npp_processed,conus_npp_procossed_stack)
plot(annualSWA_AprJun_mask_npp_processed)
#changing this to a dataframe
annualSWA_AprJun_final_allsites = rasterToPoints(annualSWA_AprJun_mask_npp_processed); df_annualSWA_AprJun_final_allsites = data.frame(annualSWA_AprJun_final_allsites)
colnames(df_annualSWA_AprJun_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_annualSWA_AprJun_final_allsites_melted <- melt(df_annualSWA_AprJun_final_allsites, 
                                                  id.vars = c("x", "y"),
                                                  variable.name = "year") #melt to long format

df_annualSWA_AprJun_final_allsites_melted$april_june_swc<-df_annualSWA_AprJun_final_allsites_melted$value
df_swc_april_june<-df_annualSWA_AprJun_final_allsites_melted[-4]

#merge with npp dataframe
npp_climate_all_sites_3<-merge(npp_climate_all_sites_2,df_swc_april_june,by=c("x","y","year"))
head(npp_climate_all_sites_3)

###soil moisture July-September####
annualSWA_JulSep_extent_npp_processed<-crop(SWA_JulSep_stack,extent_npp_processed)
annualSWA_JulSep_mask_npp_processed<-mask(annualSWA_JulSep_extent_npp_processed,conus_npp_procossed_stack)
plot(annualSWA_JulSep_mask_npp_processed)
#changing this to a dataframe
annualSWA_JulSep_final_allsites = rasterToPoints(annualSWA_JulSep_mask_npp_processed); df_annualSWA_JulSep_final_allsites = data.frame(annualSWA_JulSep_final_allsites)
colnames(df_annualSWA_JulSep_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_annualSWA_JulSep_final_allsites_melted <- melt(df_annualSWA_JulSep_final_allsites, 
                                                  id.vars = c("x", "y"),
                                                  variable.name = "year") #melt to long format

df_annualSWA_JulSep_final_allsites_melted$july_september_swc<-df_annualSWA_JulSep_final_allsites_melted$value
df_swc_july_september<-df_annualSWA_JulSep_final_allsites_melted[-4]

#merge with npp dataframe
npp_climate_all_sites_4<-merge(npp_climate_all_sites_3,df_swc_july_september,by=c("x","y","year"))
head(npp_climate_all_sites_4)

###soil moisture October-December####
annualSWA_OctDec_extent_npp_processed<-crop(SWA_OctDec_stack,extent_npp_processed)
annualSWA_OctDec_mask_npp_processed<-mask(annualSWA_OctDec_extent_npp_processed,conus_npp_procossed_stack)
plot(annualSWA_OctDec_mask_npp_processed)
#changing this to a dataframe
annualSWA_OctDec_final_allsites = rasterToPoints(annualSWA_OctDec_mask_npp_processed); df_annualSWA_OctDec_final_allsites = data.frame(annualSWA_OctDec_final_allsites)
colnames(df_annualSWA_OctDec_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_annualSWA_OctDec_final_allsites_melted <- melt(df_annualSWA_OctDec_final_allsites, 
                                                  id.vars = c("x", "y"),
                                                  variable.name = "year") #melt to long format

df_annualSWA_OctDec_final_allsites_melted$october_december_swc<-df_annualSWA_OctDec_final_allsites_melted$value
df_swc_october_december<-df_annualSWA_OctDec_final_allsites_melted[-4]

#merge with npp dataframe
npp_climate_all_sites_5<-merge(npp_climate_all_sites_4,df_swc_october_december,by=c("x","y","year"))
head(npp_climate_all_sites_5)


#getting mean npp for spatial models
#spatial model*vegetation
production_mean<-aggregate(npp ~ x + y + region,mean,data=npp_precip_all_sites)
#write.csv(production_mean,file="conus_npp_processed.csv")
head(production_mean)
mm_mean<-aggregate(mm ~ x + y + region,mean,data=npp_precip_all_sites)
mm_production_mean<-merge(mm_mean,production_mean,by=c('x','y','region'))
head(mm_production_mean)
model<-lm(npp~mm + region + mm:region,data=mm_production_mean)
summary(model)
anova(model)
interact_plot(model, pred = "mm", modx = "region",x.label = 'Mean annual precipitation',y.label = "Mean net primary production")

###50% transpiration####
wy_transp_50_extent_npp_processed<-crop(wy_transp_50_stack,extent_npp_processed)
wy_transp_50_mask_npp_processed<-mask(wy_transp_50_extent_npp_processed,conus_npp_procossed_stack)
plot(wy_transp_50_mask_npp_processed)
#changing this to a dataframe
wy_transp_50_final_allsites = rasterToPoints(wy_transp_50_mask_npp_processed); df_wy_transp_50_final_allsites = data.frame(wy_transp_50_final_allsites)
colnames(df_wy_transp_50_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_wy_transp_50_final_allsites_melted <- melt(df_wy_transp_50_final_allsites, 
                                              id.vars = c("x", "y"),
                                              variable.name = "year") #melt to long format

df_wy_transp_50_final_allsites_melted$day_of_50_total_transp<-df_wy_transp_50_final_allsites_melted$value
df_day_of_50_total_transp<-df_wy_transp_50_final_allsites_melted[-4]

#merge with npp dataframe
npp_climate_all_sites_6<-merge(npp_climate_all_sites_5,df_day_of_50_total_transp,by=c("x","y","year"))
head(npp_climate_all_sites_6)

###hot-dry january-march####
annualBelow3DD_JanMar_extent_npp_processed<-crop(annualBelow3DD_JanMar_stack,extent_npp_processed)
annualBelow3DD_JanMar_mask_npp_processed<-mask(annualBelow3DD_JanMar_extent_npp_processed,conus_npp_procossed_stack)
plot(annualBelow3DD_JanMar_mask_npp_processed)
#changing this to a dataframe
annualBelow3DD_JanMar_final_allsites = rasterToPoints(annualBelow3DD_JanMar_mask_npp_processed); df_annualBelow3DD_JanMar_final_allsites = data.frame(annualBelow3DD_JanMar_final_allsites)
colnames(df_annualBelow3DD_JanMar_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_annualBelow3DD_JanMar_final_allsites_melted <- melt(df_annualBelow3DD_JanMar_final_allsites, 
                                                       id.vars = c("x", "y"),
                                                       variable.name = "year") #melt to long format

df_annualBelow3DD_JanMar_final_allsites_melted$hot_dry_jan_march<-df_annualBelow3DD_JanMar_final_allsites_melted$value
df_hot_dry_jan_march<-df_annualBelow3DD_JanMar_final_allsites_melted[-4]

#merge with npp dataframe
npp_climate_all_sites_7<-merge(npp_climate_all_sites_6,df_hot_dry_jan_march,by=c("x","y","year"))
head(npp_climate_all_sites_7)

###hot-dry April-June####
annualBelow3DD_AprJun_extent_npp_processed<-crop(annualBelow3DD_AprJun_stack,extent_npp_processed)
annualBelow3DD_AprJun_mask_npp_processed<-mask(annualBelow3DD_AprJun_extent_npp_processed,conus_npp_procossed_stack)
plot(annualBelow3DD_AprJun_mask_npp_processed)
#changing this to a dataframe
annualBelow3DD_AprJun_final_allsites = rasterToPoints(annualBelow3DD_AprJun_mask_npp_processed); df_annualBelow3DD_AprJun_final_allsites = data.frame(annualBelow3DD_AprJun_final_allsites)
colnames(df_annualBelow3DD_AprJun_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_annualBelow3DD_AprJun_final_allsites_melted <- melt(df_annualBelow3DD_AprJun_final_allsites, 
                                                       id.vars = c("x", "y"),
                                                       variable.name = "year") #melt to long format

df_annualBelow3DD_AprJun_final_allsites_melted$hot_dry_april_june<-df_annualBelow3DD_AprJun_final_allsites_melted$value
df_hot_dry_april_june<-df_annualBelow3DD_AprJun_final_allsites_melted[-4]

#merge with npp dataframe
npp_climate_all_sites_8<-merge(npp_climate_all_sites_7,df_hot_dry_april_june,by=c("x","y","year"))
head(npp_climate_all_sites_8)

###hot-dry July - September####
annualBelow3DD_JulSep_extent_npp_processed<-crop(annualBelow3DD_JulSep_stack,extent_npp_processed)
annualBelow3DD_JulSep_mask_npp_processed<-mask(annualBelow3DD_JulSep_extent_npp_processed,conus_npp_procossed_stack)
plot(annualBelow3DD_JulSep_mask_npp_processed)
#changing this to a dataframe
annualBelow3DD_JulSep_final_allsites = rasterToPoints(annualBelow3DD_JulSep_mask_npp_processed); df_annualBelow3DD_JulSep_final_allsites = data.frame(annualBelow3DD_JulSep_final_allsites)
colnames(df_annualBelow3DD_JulSep_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_annualBelow3DD_JulSep_final_allsites_melted <- melt(df_annualBelow3DD_JulSep_final_allsites, 
                                                       id.vars = c("x", "y"),
                                                       variable.name = "year") #melt to long format

df_annualBelow3DD_JulSep_final_allsites_melted$hot_dry_july_september<-df_annualBelow3DD_JulSep_final_allsites_melted$value
df_hot_dry_july_september<-df_annualBelow3DD_JulSep_final_allsites_melted[-4]

#merge with npp dataframe
npp_climate_all_sites_9<-merge(npp_climate_all_sites_8,df_hot_dry_july_september,by=c("x","y","year"))
head(npp_climate_all_sites_9)

###hot-dry October-December####
annualBelow3DD_OctDec_extent_npp_processed<-crop(annualBelow3DD_OctDec_stack,extent_npp_processed)
annualBelow3DD_OctDec_mask_npp_processed<-mask(annualBelow3DD_OctDec_extent_npp_processed,conus_npp_procossed_stack)
plot(annualBelow3DD_OctDec_mask_npp_processed)
#changing this to a dataframe
annualBelow3DD_OctDec_final_allsites = rasterToPoints(annualBelow3DD_OctDec_mask_npp_processed); df_annualBelow3DD_OctDec_final_allsites = data.frame(annualBelow3DD_OctDec_final_allsites)
colnames(df_annualBelow3DD_OctDec_final_allsites)[3:32] <-paste(1986:2015) #rename coluns to years
df_annualBelow3DD_OctDec_final_allsites_melted <- melt(df_annualBelow3DD_OctDec_final_allsites, 
                                                       id.vars = c("x", "y"),
                                                       variable.name = "year") #melt to long format

df_annualBelow3DD_OctDec_final_allsites_melted$hot_dry_oct_dec<-df_annualBelow3DD_OctDec_final_allsites_melted$value
df_hot_dry_october_december<-df_annualBelow3DD_OctDec_final_allsites_melted[-4]

#merge with npp dataframe
npp_climate_all_sites_10<-merge(npp_climate_all_sites_9,df_hot_dry_october_december,by=c("x","y","year"))
head(npp_climate_all_sites_10)

write.csv(npp_climate_all_sites_10,file = 'npp_climate_rangelands.csv')

