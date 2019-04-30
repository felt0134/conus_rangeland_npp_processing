library(raster)
library(plyr)
library(dplyr)

dir.AFRI_Historical <- "C:/Users/A02296270/Desktop/My Drive/CONUS_rangelands_NPP_Sensitivity/climate_data_for_import/" #set working directory
load(file.path(dir.AFRI_Historical, "aggTABLE_allregions.Rdata")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(WatYrprecip)

###ecoegion raster#####

regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS")

#Specify directory where you have the raster
sites <- "C:/Users/A02296270/Desktop/My Drive/CONUS_rangelands_NPP_Sensitivity/climate_data_for_import/RasterbySiteID3.tif" 
raster_sites<-raster(sites)
plot(raster_sites)

#create a dataframe with values from the raster (so we can link data from the response variables to it)
rastvals <- as.data.frame(values(raster_sites))
names(rastvals) <- "RegionSite"

#View just the values associated with each cell
#Note that the value in the raster includes both the region (in the millions digit; 1 to 5), and the siteID (in the other digits, range 1 to ~20,000 within each region)
values(raster_sites)

#Plot the raster
plot(raster_sites)

#Create a raster for the different regions
raster_sites <- round(raster_sites/1000000)
plot(raster_sites)

#Create a raster for the different regions
raster_sites <- raster_sites - raster_sites*1000000
plot(raster_sites)


#######water year total precipitatio########
dir.AFRI_Historical <- "C:/Users/A02296270/Desktop/My Drive/CONUS_rangelands_NPP_Sensitivity/climate_data_for_import/" #set working directory
load(file.path(dir.AFRI_Historical, "WatYrprecip19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(WatYrprecip)
str(WatYrprecip)
summary(WatYrprecip)
rownames(WatYrprecip)
head(WatYrprecip)

#add label to dataset based on row names
WatYrprecip <- WatYrprecip[,-102]
WatYrprecip$label <- row.names(WatYrprecip)

#if you want to remove the excess sites not in the core 5 rangeland types

#to remove
WatYrprecip$Regionname <- substr(WatYrprecip$label, 8, 9)
WatYrprecip <- dplyr::filter(WatYrprecip, Regionname != "De") #Remove excess site values
unique(WatYrprecip$Regionname)#it works!
WatYrprecip <- WatYrprecip[,-103]

#Creating unique IDs
sitenumENDpos = as.integer(regexpr('_', WatYrprecip$label) )
Site <- as.integer(substr(WatYrprecip$label, 1, sitenumENDpos-1) )
Regionname <- substr(WatYrprecip$label, 8, 9)
Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
WatYrprecip$RegionSite <- Regionnum*1000000 + Site

WatYrprecip_joindat <- join(rastvals, WatYrprecip, by="RegionSite")

#get rid of years that don't relate to NPP data: isolate 1986-2015
WatYrprecip_joindat_2<-WatYrprecip_joindat[,-c(2:71)]

#use joined data to populate values for a raster
WatYrprecip_done <- list()
#1986
WatYrprecip_1986<- raster_sites
values(WatYrprecip_1986) <- WatYrprecip_joindat_2[,"1986"]
WatYrprecip_done[["WatYrprecip_1986"]] <-WatYrprecip_1986
#1987
WatYrprecip_1987<- raster_sites
values(WatYrprecip_1987) <- WatYrprecip_joindat_2[,"1987"]
WatYrprecip_done[["WatYrprecip_1987"]] <-WatYrprecip_1987
#1988
WatYrprecip_1988<- raster_sites
values(WatYrprecip_1988) <- WatYrprecip_joindat_2[,"1988"]
WatYrprecip_done[["WatYrprecip_1988"]] <-WatYrprecip_1988
#1989
WatYrprecip_1989<- raster_sites
values(WatYrprecip_1989) <- WatYrprecip_joindat_2[,"1989"]
WatYrprecip_done[["WatYrprecip_1989"]] <-WatYrprecip_1989
#1990
WatYrprecip_1990<- raster_sites
values(WatYrprecip_1990) <- WatYrprecip_joindat_2[,"1990"]
WatYrprecip_done[["WatYrprecip_1990"]] <-WatYrprecip_1990
#1991
WatYrprecip_1991<- raster_sites
values(WatYrprecip_1991) <- WatYrprecip_joindat_2[,"1991"]
WatYrprecip_done[["WatYrprecip_1991"]] <-WatYrprecip_1991
#1992
WatYrprecip_1992<- raster_sites
values(WatYrprecip_1992) <- WatYrprecip_joindat_2[,"1992"]
WatYrprecip_done[["WatYrprecip_1992"]] <-WatYrprecip_1992
#1993
WatYrprecip_1993<- raster_sites
values(WatYrprecip_1993) <- WatYrprecip_joindat_2[,"1993"]
WatYrprecip_done[["WatYrprecip_1993"]] <-WatYrprecip_1993
#1994
WatYrprecip_1994<- raster_sites
values(WatYrprecip_1994) <- WatYrprecip_joindat_2[,"1994"]
WatYrprecip_done[["WatYrprecip_1994"]] <-WatYrprecip_1994
#1995
WatYrprecip_1995<- raster_sites
values(WatYrprecip_1995) <- WatYrprecip_joindat_2[,"1995"]
WatYrprecip_done[["WatYrprecip_1995"]] <-WatYrprecip_1995
#1996
WatYrprecip_1996<- raster_sites
values(WatYrprecip_1996) <- WatYrprecip_joindat_2[,"1996"]
WatYrprecip_done[["WatYrprecip_1996"]] <-WatYrprecip_1996
#1997
WatYrprecip_1997<- raster_sites
values(WatYrprecip_1997) <- WatYrprecip_joindat_2[,"1997"]
WatYrprecip_done[["WatYrprecip_1997"]] <-WatYrprecip_1997
#1998
WatYrprecip_1998<- raster_sites
values(WatYrprecip_1998) <- WatYrprecip_joindat_2[,"1998"]
WatYrprecip_done[["WatYrprecip_1998"]] <-WatYrprecip_1998
#1999
WatYrprecip_1999<- raster_sites
values(WatYrprecip_1999) <- WatYrprecip_joindat_2[,"1999"]
WatYrprecip_done[["WatYrprecip_1999"]] <-WatYrprecip_1999
#2000
WatYrprecip_2000<- raster_sites
values(WatYrprecip_2000) <- WatYrprecip_joindat_2[,"2000"]
WatYrprecip_done[["WatYrprecip_2000"]] <-WatYrprecip_2000
#2001
WatYrprecip_2001<- raster_sites
values(WatYrprecip_2001) <- WatYrprecip_joindat_2[,"2001"]
WatYrprecip_done[["WatYrprecip_2001"]] <-WatYrprecip_2001
#2002
WatYrprecip_2002<- raster_sites
values(WatYrprecip_2002) <- WatYrprecip_joindat_2[,"2002"]
WatYrprecip_done[["WatYrprecip_2002"]] <-WatYrprecip_2002
#2003
WatYrprecip_2003<- raster_sites
values(WatYrprecip_2003) <- WatYrprecip_joindat_2[,"2003"]
WatYrprecip_done[["WatYrprecip_2003"]] <-WatYrprecip_2003
#2004
WatYrprecip_2004<- raster_sites
values(WatYrprecip_2004) <- WatYrprecip_joindat_2[,"2004"]
WatYrprecip_done[["WatYrprecip_2004"]] <-WatYrprecip_2004
#2005
WatYrprecip_2005<- raster_sites
values(WatYrprecip_2005) <- WatYrprecip_joindat_2[,"2005"]
WatYrprecip_done[["WatYrprecip_2005"]] <-WatYrprecip_2005
#2006
WatYrprecip_2006<- raster_sites
values(WatYrprecip_2006) <- WatYrprecip_joindat_2[,"2006"]
WatYrprecip_done[["WatYrprecip_2006"]] <-WatYrprecip_2006
#2007
WatYrprecip_2007<- raster_sites
values(WatYrprecip_2007) <- WatYrprecip_joindat_2[,"2007"]
WatYrprecip_done[["WatYrprecip_2007"]] <-WatYrprecip_2007
#2008
WatYrprecip_2008<- raster_sites
values(WatYrprecip_2008) <- WatYrprecip_joindat_2[,"2008"]
WatYrprecip_done[["WatYrprecip_2008"]] <-WatYrprecip_2008
#2009
WatYrprecip_2009<- raster_sites
values(WatYrprecip_2009) <- WatYrprecip_joindat_2[,"2009"]
WatYrprecip_done[["WatYrprecip_2009"]] <-WatYrprecip_2009
#2010
WatYrprecip_2010<- raster_sites
values(WatYrprecip_2010) <- WatYrprecip_joindat_2[,"2010"]
WatYrprecip_done[["WatYrprecip_2010"]] <-WatYrprecip_2010
#2011
WatYrprecip_2011<- raster_sites
values(WatYrprecip_2011) <- WatYrprecip_joindat_2[,"2011"]
WatYrprecip_done[["WatYrprecip_2011"]] <-WatYrprecip_2011
#2012
WatYrprecip_2012<- raster_sites
values(WatYrprecip_2012) <- WatYrprecip_joindat_2[,"2012"]
WatYrprecip_done[["WatYrprecip_2012"]] <-WatYrprecip_2012
#2013
WatYrprecip_2013<- raster_sites
values(WatYrprecip_2013) <- WatYrprecip_joindat_2[,"2013"]
WatYrprecip_done[["WatYrprecip_2013"]] <-WatYrprecip_2013
#2014
WatYrprecip_2014<- raster_sites
values(WatYrprecip_2014) <- WatYrprecip_joindat_2[,"2014"]
WatYrprecip_done[["WatYrprecip_2014"]] <-WatYrprecip_2014
#2015
WatYrprecip_2015<- raster_sites
values(WatYrprecip_2015) <- WatYrprecip_joindat_2[,"2015"]
WatYrprecip_done[["WatYrprecip_2015"]] <-WatYrprecip_2015

#all years stacked, ready for cropping for each site
WatYrprecip_stack <-stack(WatYrprecip_done)
plot(WatYrprecip_stack)

#######soil moisture October-December########

load(file.path(dir.AFRI_Historical, "annualSWA_OctDec19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(annualSWA_OctDec)
str(annualSWA_OctDec)
summary(annualSWA_OctDec)
rownames(annualSWA_OctDec)
head(annualSWA_OctDec)

#add label to dataset based on row names
annualSWA_OctDec$label <- row.names(annualSWA_OctDec)

#if you want to remove the excess sites not in the core 5 rangeland types, if not, skip to line 48
annualSWA_OctDec$Regionname <- substr(annualSWA_OctDec$label, 8, 9) #get abbreviations for sites
annualSWA_OctDec <- dplyr::filter(annualSWA_OctDec, Regionname != "De") #Remove excess site values

#code to link to baseline raster
sitenumENDpos = as.integer(regexpr('_', annualSWA_OctDec$label) ) #find starting position for a pattern
Site <- as.integer(substr(annualSWA_OctDec$label, 1, sitenumENDpos-1) ) #the number at the end of the site label, starts at 1
Regionname <- substr(annualSWA_OctDec$label, 8, 9) #take out the 8th and 9th values from each label ID
Regionnum <- unlist(sapply(annualSWA_OctDec$Regionname, FUN= function(x) grep(x, regions)) )
annualSWA_OctDec$RegionSite <- Regionnum*1000000 + Site
annualSWA_OctDec_joindat <- join(rastvals, annualSWA_OctDec, by="RegionSite")

#get rid of years that don't relate to NPP data: isolate 1986-2015
annualSWA_OctDec_joindat_2<-annualSWA_OctDec_joindat[,-c(2:71)]

#use joined data to populate values for a raster
SWA_OctDec_done <- list()
#1986
annualSWA_OctDec_1986<- raster_sites
values(annualSWA_OctDec_1986) <- annualSWA_OctDec_joindat_2[,"1986"]
SWA_OctDec_done[["annualSWA_OctDec_1986"]] <-annualSWA_OctDec_1986
#1987
annualSWA_OctDec_1987<- raster_sites
values(annualSWA_OctDec_1987) <- annualSWA_OctDec_joindat_2[,"1987"]
SWA_OctDec_done[["annualSWA_OctDec_1987"]] <-annualSWA_OctDec_1987
#1988
annualSWA_OctDec_1988<- raster_sites
values(annualSWA_OctDec_1988) <- annualSWA_OctDec_joindat_2[,"1988"]
SWA_OctDec_done[["annualSWA_OctDec_1988"]] <-annualSWA_OctDec_1988
#1989
annualSWA_OctDec_1989<- raster_sites
values(annualSWA_OctDec_1989) <- annualSWA_OctDec_joindat_2[,"1989"]
SWA_OctDec_done[["annualSWA_OctDec_1989"]] <-annualSWA_OctDec_1989
#1990
annualSWA_OctDec_1990<- raster_sites
values(annualSWA_OctDec_1990) <- annualSWA_OctDec_joindat_2[,"1990"]
SWA_OctDec_done[["annualSWA_OctDec_1990"]] <-annualSWA_OctDec_1990
#1991
annualSWA_OctDec_1991<- raster_sites
values(annualSWA_OctDec_1991) <- annualSWA_OctDec_joindat_2[,"1991"]
SWA_OctDec_done[["annualSWA_OctDec_1991"]] <-annualSWA_OctDec_1991
#1992
annualSWA_OctDec_1992<- raster_sites
values(annualSWA_OctDec_1992) <- annualSWA_OctDec_joindat_2[,"1992"]
SWA_OctDec_done[["annualSWA_OctDec_1992"]] <-annualSWA_OctDec_1992
#1993
annualSWA_OctDec_1993<- raster_sites
values(annualSWA_OctDec_1993) <- annualSWA_OctDec_joindat_2[,"1993"]
SWA_OctDec_done[["annualSWA_OctDec_1993"]] <-annualSWA_OctDec_1993
#1994
annualSWA_OctDec_1994<- raster_sites
values(annualSWA_OctDec_1994) <- annualSWA_OctDec_joindat_2[,"1994"]
SWA_OctDec_done[["annualSWA_OctDec_1994"]] <-annualSWA_OctDec_1994
#1995
annualSWA_OctDec_1995<- raster_sites
values(annualSWA_OctDec_1995) <- annualSWA_OctDec_joindat_2[,"1995"]
SWA_OctDec_done[["annualSWA_OctDec_1995"]] <-annualSWA_OctDec_1995
#1996
annualSWA_OctDec_1996<- raster_sites
values(annualSWA_OctDec_1996) <- annualSWA_OctDec_joindat_2[,"1996"]
SWA_OctDec_done[["annualSWA_OctDec_1996"]] <-annualSWA_OctDec_1996
#1997
annualSWA_OctDec_1997<- raster_sites
values(annualSWA_OctDec_1997) <- annualSWA_OctDec_joindat_2[,"1997"]
SWA_OctDec_done[["annualSWA_OctDec_1997"]] <-annualSWA_OctDec_1997
#1998
annualSWA_OctDec_1998<- raster_sites
values(annualSWA_OctDec_1998) <- annualSWA_OctDec_joindat_2[,"1998"]
SWA_OctDec_done[["annualSWA_OctDec_1998"]] <-annualSWA_OctDec_1998
#1999
annualSWA_OctDec_1999<- raster_sites
values(annualSWA_OctDec_1999) <- annualSWA_OctDec_joindat_2[,"1999"]
SWA_OctDec_done[["annualSWA_OctDec_1999"]] <-annualSWA_OctDec_1999
#2000
annualSWA_OctDec_2000<- raster_sites
values(annualSWA_OctDec_2000) <- annualSWA_OctDec_joindat_2[,"2000"]
SWA_OctDec_done[["annualSWA_OctDec_2000"]] <-annualSWA_OctDec_2000
#2001
annualSWA_OctDec_2001<- raster_sites
values(annualSWA_OctDec_2001) <- annualSWA_OctDec_joindat_2[,"2001"]
SWA_OctDec_done[["annualSWA_OctDec_2001"]] <-annualSWA_OctDec_2001
#2002
annualSWA_OctDec_2002<- raster_sites
values(annualSWA_OctDec_2002) <- annualSWA_OctDec_joindat_2[,"2002"]
SWA_OctDec_done[["annualSWA_OctDec_2002"]] <-annualSWA_OctDec_2002
#2003
annualSWA_OctDec_2003<- raster_sites
values(annualSWA_OctDec_2003) <- annualSWA_OctDec_joindat_2[,"2003"]
SWA_OctDec_done[["annualSWA_OctDec_2003"]] <-annualSWA_OctDec_2003
#2004
annualSWA_OctDec_2004<- raster_sites
values(annualSWA_OctDec_2004) <- annualSWA_OctDec_joindat_2[,"2004"]
SWA_OctDec_done[["annualSWA_OctDec_2004"]] <-annualSWA_OctDec_2004
#2005
annualSWA_OctDec_2005<- raster_sites
values(annualSWA_OctDec_2005) <- annualSWA_OctDec_joindat_2[,"2005"]
SWA_OctDec_done[["annualSWA_OctDec_2005"]] <-annualSWA_OctDec_2005
#2006
annualSWA_OctDec_2006<- raster_sites
values(annualSWA_OctDec_2006) <- annualSWA_OctDec_joindat_2[,"2006"]
SWA_OctDec_done[["annualSWA_OctDec_2006"]] <-annualSWA_OctDec_2006
#2007
annualSWA_OctDec_2007<- raster_sites
values(annualSWA_OctDec_2007) <- annualSWA_OctDec_joindat_2[,"2007"]
SWA_OctDec_done[["annualSWA_OctDec_2007"]] <-annualSWA_OctDec_2007
#2008
annualSWA_OctDec_2008<- raster_sites
values(annualSWA_OctDec_2008) <- annualSWA_OctDec_joindat_2[,"2008"]
SWA_OctDec_done[["annualSWA_OctDec_2008"]] <-annualSWA_OctDec_2008
#2009
annualSWA_OctDec_2009<- raster_sites
values(annualSWA_OctDec_2009) <- annualSWA_OctDec_joindat_2[,"2009"]
SWA_OctDec_done[["annualSWA_OctDec_2009"]] <-annualSWA_OctDec_2009
#2010
annualSWA_OctDec_2010<- raster_sites
values(annualSWA_OctDec_2010) <- annualSWA_OctDec_joindat_2[,"2010"]
SWA_OctDec_done[["annualSWA_OctDec_2010"]] <-annualSWA_OctDec_2010
#2011
annualSWA_OctDec_2011<- raster_sites
values(annualSWA_OctDec_2011) <- annualSWA_OctDec_joindat_2[,"2011"]
SWA_OctDec_done[["annualSWA_OctDec_2011"]] <-annualSWA_OctDec_2011
#2012
annualSWA_OctDec_2012<- raster_sites
values(annualSWA_OctDec_2012) <- annualSWA_OctDec_joindat_2[,"2012"]
SWA_OctDec_done[["annualSWA_OctDec_2012"]] <-annualSWA_OctDec_2012
#2013
annualSWA_OctDec_2013<- raster_sites
values(annualSWA_OctDec_2013) <- annualSWA_OctDec_joindat_2[,"2013"]
SWA_OctDec_done[["annualSWA_OctDec_2013"]] <-annualSWA_OctDec_2013
#2014
annualSWA_OctDec_2014<- raster_sites
values(annualSWA_OctDec_2014) <- annualSWA_OctDec_joindat_2[,"2014"]
SWA_OctDec_done[["annualSWA_OctDec_2014"]] <-annualSWA_OctDec_2014
#2015
annualSWA_OctDec_2015<- raster_sites
values(annualSWA_OctDec_2015) <- annualSWA_OctDec_joindat_2[,"2015"]
SWA_OctDec_done[["annualSWA_OctDec_2015"]] <-annualSWA_OctDec_2015

#all years stacked, ready for cropping for each site
SWA_OctDec_stack <-stack(SWA_OctDec_done)
plot(SWA_OctDec_stack)

###soil moisture July-September#######

load(file.path(dir.AFRI_Historical, "annualSWA_JulSep19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
str(annualSWA_JulSep)
summary(annualSWA_JulSep)
rownames(annualSWA_JulSep)
head(annualSWA_JulSep)

#add label to dataset based on row names
annualSWA_JulSep$label <- row.names(annualSWA_JulSep)
annualSWA_JulSep$Regionname <- substr(annualSWA_JulSep$label, 8, 9) #get abbreviations for sites
annualSWA_JulSep <- dplyr::filter(annualSWA_JulSep, Regionname != "De") #Remove excess site values
sitenumENDpos = as.integer(regexpr('_', annualSWA_JulSep$label) ) #find starting position for a pattern
Site <- as.integer(substr(annualSWA_JulSep$label, 1, sitenumENDpos-1) ) #the number at the end of the site label, starts at 1
Regionname <- substr(annualSWA_JulSep$label, 8, 9) #take out the 8th and 9th values from each label ID
Regionnum <- unlist(sapply(annualSWA_JulSep$Regionname, FUN= function(x) grep(x, regions)) )
annualSWA_JulSep$RegionSite <- Regionnum*1000000 + Site
annualSWA_JulSep_joindat <- join(rastvals, annualSWA_JulSep, by="RegionSite")

#get rid of years that don't relate to NPP data
annualSWA_JulSep_joindat_2<-annualSWA_JulSep_joindat[,-c(2:71)]

#use joined data to populate values for a raster
SWA_JulSep_done <- list()
#1986
annualSWA_JulSep_1986<- raster_sites
values(annualSWA_JulSep_1986) <- annualSWA_JulSep_joindat_2[,"1986"]
SWA_JulSep_done[["annualSWA_JulSep_1986"]] <-annualSWA_JulSep_1986
#1987
annualSWA_JulSep_1987<- raster_sites
values(annualSWA_JulSep_1987) <- annualSWA_JulSep_joindat_2[,"1987"]
SWA_JulSep_done[["annualSWA_JulSep_1987"]] <-annualSWA_JulSep_1987
#1988
annualSWA_JulSep_1988<- raster_sites
values(annualSWA_JulSep_1988) <- annualSWA_JulSep_joindat_2[,"1988"]
SWA_JulSep_done[["annualSWA_JulSep_1988"]] <-annualSWA_JulSep_1988
#1989
annualSWA_JulSep_1989<- raster_sites
values(annualSWA_JulSep_1989) <- annualSWA_JulSep_joindat_2[,"1989"]
SWA_JulSep_done[["annualSWA_JulSep_1989"]] <-annualSWA_JulSep_1989
#1990
annualSWA_JulSep_1990<- raster_sites
values(annualSWA_JulSep_1990) <- annualSWA_JulSep_joindat_2[,"1990"]
SWA_JulSep_done[["annualSWA_JulSep_1990"]] <-annualSWA_JulSep_1990
#1991
annualSWA_JulSep_1991<- raster_sites
values(annualSWA_JulSep_1991) <- annualSWA_JulSep_joindat_2[,"1991"]
SWA_JulSep_done[["annualSWA_JulSep_1991"]] <-annualSWA_JulSep_1991
#1992
annualSWA_JulSep_1992<- raster_sites
values(annualSWA_JulSep_1992) <- annualSWA_JulSep_joindat_2[,"1992"]
SWA_JulSep_done[["annualSWA_JulSep_1992"]] <-annualSWA_JulSep_1992
#1993
annualSWA_JulSep_1993<- raster_sites
values(annualSWA_JulSep_1993) <- annualSWA_JulSep_joindat_2[,"1993"]
SWA_JulSep_done[["annualSWA_JulSep_1993"]] <-annualSWA_JulSep_1993
#1994
annualSWA_JulSep_1994<- raster_sites
values(annualSWA_JulSep_1994) <- annualSWA_JulSep_joindat_2[,"1994"]
SWA_JulSep_done[["annualSWA_JulSep_1994"]] <-annualSWA_JulSep_1994
#1995
annualSWA_JulSep_1995<- raster_sites
values(annualSWA_JulSep_1995) <- annualSWA_JulSep_joindat_2[,"1995"]
SWA_JulSep_done[["annualSWA_JulSep_1995"]] <-annualSWA_JulSep_1995
#1996
annualSWA_JulSep_1996<- raster_sites
values(annualSWA_JulSep_1996) <- annualSWA_JulSep_joindat_2[,"1996"]
SWA_JulSep_done[["annualSWA_JulSep_1996"]] <-annualSWA_JulSep_1996
#1997
annualSWA_JulSep_1997<- raster_sites
values(annualSWA_JulSep_1997) <- annualSWA_JulSep_joindat_2[,"1997"]
SWA_JulSep_done[["annualSWA_JulSep_1997"]] <-annualSWA_JulSep_1997
#1998
annualSWA_JulSep_1998<- raster_sites
values(annualSWA_JulSep_1998) <- annualSWA_JulSep_joindat_2[,"1998"]
SWA_JulSep_done[["annualSWA_JulSep_1998"]] <-annualSWA_JulSep_1998
#1999
annualSWA_JulSep_1999<- raster_sites
values(annualSWA_JulSep_1999) <- annualSWA_JulSep_joindat_2[,"1999"]
SWA_JulSep_done[["annualSWA_JulSep_1999"]] <-annualSWA_JulSep_1999
#2000
annualSWA_JulSep_2000<- raster_sites
values(annualSWA_JulSep_2000) <- annualSWA_JulSep_joindat_2[,"2000"]
SWA_JulSep_done[["annualSWA_JulSep_2000"]] <-annualSWA_JulSep_2000
#2001
annualSWA_JulSep_2001<- raster_sites
values(annualSWA_JulSep_2001) <- annualSWA_JulSep_joindat_2[,"2001"]
SWA_JulSep_done[["annualSWA_JulSep_2001"]] <-annualSWA_JulSep_2001
#2002
annualSWA_JulSep_2002<- raster_sites
values(annualSWA_JulSep_2002) <- annualSWA_JulSep_joindat_2[,"2002"]
SWA_JulSep_done[["annualSWA_JulSep_2002"]] <-annualSWA_JulSep_2002
#2003
annualSWA_JulSep_2003<- raster_sites
values(annualSWA_JulSep_2003) <- annualSWA_JulSep_joindat_2[,"2003"]
SWA_JulSep_done[["annualSWA_JulSep_2003"]] <-annualSWA_JulSep_2003
#2004
annualSWA_JulSep_2004<- raster_sites
values(annualSWA_JulSep_2004) <- annualSWA_JulSep_joindat_2[,"2004"]
SWA_JulSep_done[["annualSWA_JulSep_2004"]] <-annualSWA_JulSep_2004
#2005
annualSWA_JulSep_2005<- raster_sites
values(annualSWA_JulSep_2005) <- annualSWA_JulSep_joindat_2[,"2005"]
SWA_JulSep_done[["annualSWA_JulSep_2005"]] <-annualSWA_JulSep_2005
#2006
annualSWA_JulSep_2006<- raster_sites
values(annualSWA_JulSep_2006) <- annualSWA_JulSep_joindat_2[,"2006"]
SWA_JulSep_done[["annualSWA_JulSep_2006"]] <-annualSWA_JulSep_2006
#2007
annualSWA_JulSep_2007<- raster_sites
values(annualSWA_JulSep_2007) <- annualSWA_JulSep_joindat_2[,"2007"]
SWA_JulSep_done[["annualSWA_JulSep_2007"]] <-annualSWA_JulSep_2007
#2008
annualSWA_JulSep_2008<- raster_sites
values(annualSWA_JulSep_2008) <- annualSWA_JulSep_joindat_2[,"2008"]
SWA_JulSep_done[["annualSWA_JulSep_2008"]] <-annualSWA_JulSep_2008
#2009
annualSWA_JulSep_2009<- raster_sites
values(annualSWA_JulSep_2009) <- annualSWA_JulSep_joindat_2[,"2009"]
SWA_JulSep_done[["annualSWA_JulSep_2009"]] <-annualSWA_JulSep_2009
#2010
annualSWA_JulSep_2010<- raster_sites
values(annualSWA_JulSep_2010) <- annualSWA_JulSep_joindat_2[,"2010"]
SWA_JulSep_done[["annualSWA_JulSep_2010"]] <-annualSWA_JulSep_2010
#2011
annualSWA_JulSep_2011<- raster_sites
values(annualSWA_JulSep_2011) <- annualSWA_JulSep_joindat_2[,"2011"]
SWA_JulSep_done[["annualSWA_JulSep_2011"]] <-annualSWA_JulSep_2011
#2012
annualSWA_JulSep_2012<- raster_sites
values(annualSWA_JulSep_2012) <- annualSWA_JulSep_joindat_2[,"2012"]
SWA_JulSep_done[["annualSWA_JulSep_2012"]] <-annualSWA_JulSep_2012
#2013
annualSWA_JulSep_2013<- raster_sites
values(annualSWA_JulSep_2013) <- annualSWA_JulSep_joindat_2[,"2013"]
SWA_JulSep_done[["annualSWA_JulSep_2013"]] <-annualSWA_JulSep_2013
#2014
annualSWA_JulSep_2014<- raster_sites
values(annualSWA_JulSep_2014) <- annualSWA_JulSep_joindat_2[,"2014"]
SWA_JulSep_done[["annualSWA_JulSep_2014"]] <-annualSWA_JulSep_2014
#2015
annualSWA_JulSep_2015<- raster_sites
values(annualSWA_JulSep_2015) <- annualSWA_JulSep_joindat_2[,"2015"]
SWA_JulSep_done[["annualSWA_JulSep_2015"]] <-annualSWA_JulSep_2015

#all years stacked, ready for cropping for each site
SWA_JulSep_stack <-stack(SWA_JulSep_done)
plot(SWA_JulSep_stack)


###soil moisture April-June #######

load(file.path(dir.AFRI_Historical, "annualSWA_AprJun19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
str(annualSWA_AprJun)
summary(annualSWA_AprJun)
rownames(annualSWA_AprJun)
head(annualSWA_AprJun)

#add label to dataset based on row names
annualSWA_AprJun$label <- row.names(annualSWA_AprJun)
annualSWA_AprJun$Regionname <- substr(annualSWA_AprJun$label, 8, 9) #get abbreviations for sites
annualSWA_AprJun <- dplyr::filter(annualSWA_AprJun, Regionname != "De") #Remove excess site values
sitenumENDpos = as.integer(regexpr('_', annualSWA_AprJun$label) ) #find starting position for a pattern
Site <- as.integer(substr(annualSWA_AprJun$label, 1, sitenumENDpos-1) ) #the number at the end of the site label, starts at 1
Regionname <- substr(annualSWA_AprJun$label, 8, 9) #take out the 8th and 9th values from each label ID
Regionnum <- unlist(sapply(annualSWA_AprJun$Regionname, FUN= function(x) grep(x, regions)) )
annualSWA_AprJun$RegionSite <- Regionnum*1000000 + Site
annualSWA_AprJun_joindat <- join(rastvals, annualSWA_AprJun, by="RegionSite")

#get rid of years that don't relate to NPP data
annualSWA_AprJun_joindat_2<-annualSWA_AprJun_joindat[,-c(2:71)]

#use joined data to populate values for a raster
SWA_AprJun_done <- list()
#1986
annualSWA_AprJun_1986<- raster_sites
values(annualSWA_AprJun_1986) <- annualSWA_AprJun_joindat_2[,"1986"]
SWA_AprJun_done[["annualSWA_AprJun_1986"]] <-annualSWA_AprJun_1986
#1987
annualSWA_AprJun_1987<- raster_sites
values(annualSWA_AprJun_1987) <- annualSWA_AprJun_joindat_2[,"1987"]
SWA_AprJun_done[["annualSWA_AprJun_1987"]] <-annualSWA_AprJun_1987
#1988
annualSWA_AprJun_1988<- raster_sites
values(annualSWA_AprJun_1988) <- annualSWA_AprJun_joindat_2[,"1988"]
SWA_AprJun_done[["annualSWA_AprJun_1988"]] <-annualSWA_AprJun_1988
#1989
annualSWA_AprJun_1989<- raster_sites
values(annualSWA_AprJun_1989) <- annualSWA_AprJun_joindat_2[,"1989"]
SWA_AprJun_done[["annualSWA_AprJun_1989"]] <-annualSWA_AprJun_1989
#1990
annualSWA_AprJun_1990<- raster_sites
values(annualSWA_AprJun_1990) <- annualSWA_AprJun_joindat_2[,"1990"]
SWA_AprJun_done[["annualSWA_AprJun_1990"]] <-annualSWA_AprJun_1990
#1991
annualSWA_AprJun_1991<- raster_sites
values(annualSWA_AprJun_1991) <- annualSWA_AprJun_joindat_2[,"1991"]
SWA_AprJun_done[["annualSWA_AprJun_1991"]] <-annualSWA_AprJun_1991
#1992
annualSWA_AprJun_1992<- raster_sites
values(annualSWA_AprJun_1992) <- annualSWA_AprJun_joindat_2[,"1992"]
SWA_AprJun_done[["annualSWA_AprJun_1992"]] <-annualSWA_AprJun_1992
#1993
annualSWA_AprJun_1993<- raster_sites
values(annualSWA_AprJun_1993) <- annualSWA_AprJun_joindat_2[,"1993"]
SWA_AprJun_done[["annualSWA_AprJun_1993"]] <-annualSWA_AprJun_1993
#1994
annualSWA_AprJun_1994<- raster_sites
values(annualSWA_AprJun_1994) <- annualSWA_AprJun_joindat_2[,"1994"]
SWA_AprJun_done[["annualSWA_AprJun_1994"]] <-annualSWA_AprJun_1994
#1995
annualSWA_AprJun_1995<- raster_sites
values(annualSWA_AprJun_1995) <- annualSWA_AprJun_joindat_2[,"1995"]
SWA_AprJun_done[["annualSWA_AprJun_1995"]] <-annualSWA_AprJun_1995
#1996
annualSWA_AprJun_1996<- raster_sites
values(annualSWA_AprJun_1996) <- annualSWA_AprJun_joindat_2[,"1996"]
SWA_AprJun_done[["annualSWA_AprJun_1996"]] <-annualSWA_AprJun_1996
#1997
annualSWA_AprJun_1997<- raster_sites
values(annualSWA_AprJun_1997) <- annualSWA_AprJun_joindat_2[,"1997"]
SWA_AprJun_done[["annualSWA_AprJun_1997"]] <-annualSWA_AprJun_1997
#1998
annualSWA_AprJun_1998<- raster_sites
values(annualSWA_AprJun_1998) <- annualSWA_AprJun_joindat_2[,"1998"]
SWA_AprJun_done[["annualSWA_AprJun_1998"]] <-annualSWA_AprJun_1998
#1999
annualSWA_AprJun_1999<- raster_sites
values(annualSWA_AprJun_1999) <- annualSWA_AprJun_joindat_2[,"1999"]
SWA_AprJun_done[["annualSWA_AprJun_1999"]] <-annualSWA_AprJun_1999
#2000
annualSWA_AprJun_2000<- raster_sites
values(annualSWA_AprJun_2000) <- annualSWA_AprJun_joindat_2[,"2000"]
SWA_AprJun_done[["annualSWA_AprJun_2000"]] <-annualSWA_AprJun_2000
#2001
annualSWA_AprJun_2001<- raster_sites
values(annualSWA_AprJun_2001) <- annualSWA_AprJun_joindat_2[,"2001"]
SWA_AprJun_done[["annualSWA_AprJun_2001"]] <-annualSWA_AprJun_2001
#2002
annualSWA_AprJun_2002<- raster_sites
values(annualSWA_AprJun_2002) <- annualSWA_AprJun_joindat_2[,"2002"]
SWA_AprJun_done[["annualSWA_AprJun_2002"]] <-annualSWA_AprJun_2002
#2003
annualSWA_AprJun_2003<- raster_sites
values(annualSWA_AprJun_2003) <- annualSWA_AprJun_joindat_2[,"2003"]
SWA_AprJun_done[["annualSWA_AprJun_2003"]] <-annualSWA_AprJun_2003
#2004
annualSWA_AprJun_2004<- raster_sites
values(annualSWA_AprJun_2004) <- annualSWA_AprJun_joindat_2[,"2004"]
SWA_AprJun_done[["annualSWA_AprJun_2004"]] <-annualSWA_AprJun_2004
#2005
annualSWA_AprJun_2005<- raster_sites
values(annualSWA_AprJun_2005) <- annualSWA_AprJun_joindat_2[,"2005"]
SWA_AprJun_done[["annualSWA_AprJun_2005"]] <-annualSWA_AprJun_2005
#2006
annualSWA_AprJun_2006<- raster_sites
values(annualSWA_AprJun_2006) <- annualSWA_AprJun_joindat_2[,"2006"]
SWA_AprJun_done[["annualSWA_AprJun_2006"]] <-annualSWA_AprJun_2006
#2007
annualSWA_AprJun_2007<- raster_sites
values(annualSWA_AprJun_2007) <- annualSWA_AprJun_joindat_2[,"2007"]
SWA_AprJun_done[["annualSWA_AprJun_2007"]] <-annualSWA_AprJun_2007
#2008
annualSWA_AprJun_2008<- raster_sites
values(annualSWA_AprJun_2008) <- annualSWA_AprJun_joindat_2[,"2008"]
SWA_AprJun_done[["annualSWA_AprJun_2008"]] <-annualSWA_AprJun_2008
#2009
annualSWA_AprJun_2009<- raster_sites
values(annualSWA_AprJun_2009) <- annualSWA_AprJun_joindat_2[,"2009"]
SWA_AprJun_done[["annualSWA_AprJun_2009"]] <-annualSWA_AprJun_2009
#2010
annualSWA_AprJun_2010<- raster_sites
values(annualSWA_AprJun_2010) <- annualSWA_AprJun_joindat_2[,"2010"]
SWA_AprJun_done[["annualSWA_AprJun_2010"]] <-annualSWA_AprJun_2010
#2011
annualSWA_AprJun_2011<- raster_sites
values(annualSWA_AprJun_2011) <- annualSWA_AprJun_joindat_2[,"2011"]
SWA_AprJun_done[["annualSWA_AprJun_2011"]] <-annualSWA_AprJun_2011
#2012
annualSWA_AprJun_2012<- raster_sites
values(annualSWA_AprJun_2012) <- annualSWA_AprJun_joindat_2[,"2012"]
SWA_AprJun_done[["annualSWA_AprJun_2012"]] <-annualSWA_AprJun_2012
#2013
annualSWA_AprJun_2013<- raster_sites
values(annualSWA_AprJun_2013) <- annualSWA_AprJun_joindat_2[,"2013"]
SWA_AprJun_done[["annualSWA_AprJun_2013"]] <-annualSWA_AprJun_2013
#2014
annualSWA_AprJun_2014<- raster_sites
values(annualSWA_AprJun_2014) <- annualSWA_AprJun_joindat_2[,"2014"]
SWA_AprJun_done[["annualSWA_AprJun_2014"]] <-annualSWA_AprJun_2014
#2015
annualSWA_AprJun_2015<- raster_sites
values(annualSWA_AprJun_2015) <- annualSWA_AprJun_joindat_2[,"2015"]
SWA_AprJun_done[["annualSWA_AprJun_2015"]] <-annualSWA_AprJun_2015

#all years stacked, ready for cropping for each site
SWA_AprJun_stack <-stack(SWA_AprJun_done)
plot(SWA_AprJun_stack)

###soil moisture January-March #######

load(file.path(dir.AFRI_Historical, "annualSWA_JanMar19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
str(annualSWA_JanMar)
summary(annualSWA_JanMar)
rownames(annualSWA_JanMar)
head(annualSWA_JanMar)

#add label to dataset based on row names
annualSWA_JanMar$label <- row.names(annualSWA_JanMar)
annualSWA_JanMar$Regionname <- substr(annualSWA_JanMar$label, 8, 9) #get abbreviations for sites
annualSWA_JanMar <- dplyr::filter(annualSWA_JanMar, Regionname != "De") #Remove excess site values

sitenumENDpos = as.integer(regexpr('_', annualSWA_JanMar$label) ) #find starting position for a pattern
Site <- as.integer(substr(annualSWA_JanMar$label, 1, sitenumENDpos-1) ) #the number at the end of the site label, starts at 1
Regionname <- substr(annualSWA_JanMar$label, 8, 9) #take out the 8th and 9th values from each label ID
Regionnum <- unlist(sapply(annualSWA_JanMar$Regionname, FUN= function(x) grep(x, regions)) )
annualSWA_JanMar$RegionSite <- Regionnum*1000000 + Site
annualSWA_JanMar_joindat <- join(rastvals, annualSWA_JanMar, by="RegionSite")

#get rid of years that don't relate to NPP data
annualSWA_JanMar_joindat_2<-annualSWA_JanMar_joindat[,-c(2:71)]

#use joined data to populate values for a raster
SWA_JanMar_done <- list()
#1986
annualSWA_JanMar_1986<- raster_sites
values(annualSWA_JanMar_1986) <- annualSWA_JanMar_joindat_2[,"1986"]
SWA_JanMar_done[["annualSWA_JanMar_1986"]] <-annualSWA_JanMar_1986
#1987
annualSWA_JanMar_1987<- raster_sites
values(annualSWA_JanMar_1987) <- annualSWA_JanMar_joindat_2[,"1987"]
SWA_JanMar_done[["annualSWA_JanMar_1987"]] <-annualSWA_JanMar_1987
#1988
annualSWA_JanMar_1988<- raster_sites
values(annualSWA_JanMar_1988) <- annualSWA_JanMar_joindat_2[,"1988"]
SWA_JanMar_done[["annualSWA_JanMar_1988"]] <-annualSWA_JanMar_1988
#1989
annualSWA_JanMar_1989<- raster_sites
values(annualSWA_JanMar_1989) <- annualSWA_JanMar_joindat_2[,"1989"]
SWA_JanMar_done[["annualSWA_JanMar_1989"]] <-annualSWA_JanMar_1989
#1990
annualSWA_JanMar_1990<- raster_sites
values(annualSWA_JanMar_1990) <- annualSWA_JanMar_joindat_2[,"1990"]
SWA_JanMar_done[["annualSWA_JanMar_1990"]] <-annualSWA_JanMar_1990
#1991
annualSWA_JanMar_1991<- raster_sites
values(annualSWA_JanMar_1991) <- annualSWA_JanMar_joindat_2[,"1991"]
SWA_JanMar_done[["annualSWA_JanMar_1991"]] <-annualSWA_JanMar_1991
#1992
annualSWA_JanMar_1992<- raster_sites
values(annualSWA_JanMar_1992) <- annualSWA_JanMar_joindat_2[,"1992"]
SWA_JanMar_done[["annualSWA_JanMar_1992"]] <-annualSWA_JanMar_1992
#1993
annualSWA_JanMar_1993<- raster_sites
values(annualSWA_JanMar_1993) <- annualSWA_JanMar_joindat_2[,"1993"]
SWA_JanMar_done[["annualSWA_JanMar_1993"]] <-annualSWA_JanMar_1993
#1994
annualSWA_JanMar_1994<- raster_sites
values(annualSWA_JanMar_1994) <- annualSWA_JanMar_joindat_2[,"1994"]
SWA_JanMar_done[["annualSWA_JanMar_1994"]] <-annualSWA_JanMar_1994
#1995
annualSWA_JanMar_1995<- raster_sites
values(annualSWA_JanMar_1995) <- annualSWA_JanMar_joindat_2[,"1995"]
SWA_JanMar_done[["annualSWA_JanMar_1995"]] <-annualSWA_JanMar_1995
#1996
annualSWA_JanMar_1996<- raster_sites
values(annualSWA_JanMar_1996) <- annualSWA_JanMar_joindat_2[,"1996"]
SWA_JanMar_done[["annualSWA_JanMar_1996"]] <-annualSWA_JanMar_1996
#1997
annualSWA_JanMar_1997<- raster_sites
values(annualSWA_JanMar_1997) <- annualSWA_JanMar_joindat_2[,"1997"]
SWA_JanMar_done[["annualSWA_JanMar_1997"]] <-annualSWA_JanMar_1997
#1998
annualSWA_JanMar_1998<- raster_sites
values(annualSWA_JanMar_1998) <- annualSWA_JanMar_joindat_2[,"1998"]
SWA_JanMar_done[["annualSWA_JanMar_1998"]] <-annualSWA_JanMar_1998
#1999
annualSWA_JanMar_1999<- raster_sites
values(annualSWA_JanMar_1999) <- annualSWA_JanMar_joindat_2[,"1999"]
SWA_JanMar_done[["annualSWA_JanMar_1999"]] <-annualSWA_JanMar_1999
#2000
annualSWA_JanMar_2000<- raster_sites
values(annualSWA_JanMar_2000) <- annualSWA_JanMar_joindat_2[,"2000"]
SWA_JanMar_done[["annualSWA_JanMar_2000"]] <-annualSWA_JanMar_2000
#2001
annualSWA_JanMar_2001<- raster_sites
values(annualSWA_JanMar_2001) <- annualSWA_JanMar_joindat_2[,"2001"]
SWA_JanMar_done[["annualSWA_JanMar_2001"]] <-annualSWA_JanMar_2001
#2002
annualSWA_JanMar_2002<- raster_sites
values(annualSWA_JanMar_2002) <- annualSWA_JanMar_joindat_2[,"2002"]
SWA_JanMar_done[["annualSWA_JanMar_2002"]] <-annualSWA_JanMar_2002
#2003
annualSWA_JanMar_2003<- raster_sites
values(annualSWA_JanMar_2003) <- annualSWA_JanMar_joindat_2[,"2003"]
SWA_JanMar_done[["annualSWA_JanMar_2003"]] <-annualSWA_JanMar_2003
#2004
annualSWA_JanMar_2004<- raster_sites
values(annualSWA_JanMar_2004) <- annualSWA_JanMar_joindat_2[,"2004"]
SWA_JanMar_done[["annualSWA_JanMar_2004"]] <-annualSWA_JanMar_2004
#2005
annualSWA_JanMar_2005<- raster_sites
values(annualSWA_JanMar_2005) <- annualSWA_JanMar_joindat_2[,"2005"]
SWA_JanMar_done[["annualSWA_JanMar_2005"]] <-annualSWA_JanMar_2005
#2006
annualSWA_JanMar_2006<- raster_sites
values(annualSWA_JanMar_2006) <- annualSWA_JanMar_joindat_2[,"2006"]
SWA_JanMar_done[["annualSWA_JanMar_2006"]] <-annualSWA_JanMar_2006
#2007
annualSWA_JanMar_2007<- raster_sites
values(annualSWA_JanMar_2007) <- annualSWA_JanMar_joindat_2[,"2007"]
SWA_JanMar_done[["annualSWA_JanMar_2007"]] <-annualSWA_JanMar_2007
#2008
annualSWA_JanMar_2008<- raster_sites
values(annualSWA_JanMar_2008) <- annualSWA_JanMar_joindat_2[,"2008"]
SWA_JanMar_done[["annualSWA_JanMar_2008"]] <-annualSWA_JanMar_2008
#2009
annualSWA_JanMar_2009<- raster_sites
values(annualSWA_JanMar_2009) <- annualSWA_JanMar_joindat_2[,"2009"]
SWA_JanMar_done[["annualSWA_JanMar_2009"]] <-annualSWA_JanMar_2009
#2010
annualSWA_JanMar_2010<- raster_sites
values(annualSWA_JanMar_2010) <- annualSWA_JanMar_joindat_2[,"2010"]
SWA_JanMar_done[["annualSWA_JanMar_2010"]] <-annualSWA_JanMar_2010
#2011
annualSWA_JanMar_2011<- raster_sites
values(annualSWA_JanMar_2011) <- annualSWA_JanMar_joindat_2[,"2011"]
SWA_JanMar_done[["annualSWA_JanMar_2011"]] <-annualSWA_JanMar_2011
#2012
annualSWA_JanMar_2012<- raster_sites
values(annualSWA_JanMar_2012) <- annualSWA_JanMar_joindat_2[,"2012"]
SWA_JanMar_done[["annualSWA_JanMar_2012"]] <-annualSWA_JanMar_2012
#2013
annualSWA_JanMar_2013<- raster_sites
values(annualSWA_JanMar_2013) <- annualSWA_JanMar_joindat_2[,"2013"]
SWA_JanMar_done[["annualSWA_JanMar_2013"]] <-annualSWA_JanMar_2013
#2014
annualSWA_JanMar_2014<- raster_sites
values(annualSWA_JanMar_2014) <- annualSWA_JanMar_joindat_2[,"2014"]
SWA_JanMar_done[["annualSWA_JanMar_2014"]] <-annualSWA_JanMar_2014
#2015
annualSWA_JanMar_2015<- raster_sites
values(annualSWA_JanMar_2015) <- annualSWA_JanMar_joindat_2[,"2015"]
SWA_JanMar_done[["annualSWA_JanMar_2015"]] <-annualSWA_JanMar_2015

#all years stacked, ready for cropping for each site
SWA_JanMar_stack <-stack(SWA_JanMar_done)
plot(SWA_JanMar_stack)


######50% of total transpiration#########

load(file.path(dir.AFRI_Historical, "WatYrtransp50_19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
wy_transp_50<-WatYrTransp50
as.data.frame(wy_transp_50)
str(wy_transp_50)
summary(wy_transp_50)
rownames(wy_transp_50)
head(wy_transp_50)

#add label to dataset based on row names
wy_transp_50 <- wy_transp_50[,-102]
wy_transp_50$label <- row.names(wy_transp_50)

#if you want to remove the excess sites not in the core 5 rangeland types

#to remove
wy_transp_50$Regionname <- substr(wy_transp_50$label, 8, 9)
wy_transp_50 <- dplyr::filter(wy_transp_50, Regionname != "De") #Remove excess site values
unique(wy_transp_50$Regionname)#it works!
wy_transp_50 <- wy_transp_50[,-103]

#Creating unique IDs
sitenumENDpos = as.integer(regexpr('_', wy_transp_50$label) )
Site <- as.integer(substr(wy_transp_50$label, 1, sitenumENDpos-1) )
Regionname <- substr(wy_transp_50$label, 8, 9)
Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
wy_transp_50$RegionSite <- Regionnum*1000000 + Site

wy_transp_50_joindat <- join(rastvals, wy_transp_50, by="RegionSite")

#get rid of years that don't relate to NPP data: isolate 1986-2015
wy_transp_50_joindat_2<-wy_transp_50_joindat[,-c(2:71)]

#use joined data to populate values for a raster
wy_transp_50_done <- list()
#1986
wy_transp_50_1986<- raster_sites
values(wy_transp_50_1986) <- wy_transp_50_joindat_2[,"1986"]
wy_transp_50_done[["wy_transp_50_1986"]] <-wy_transp_50_1986
#1987
wy_transp_50_1987<- raster_sites
values(wy_transp_50_1987) <- wy_transp_50_joindat_2[,"1987"]
wy_transp_50_done[["wy_transp_50_1987"]] <-wy_transp_50_1987
#1988
wy_transp_50_1988<- raster_sites
values(wy_transp_50_1988) <- wy_transp_50_joindat_2[,"1988"]
wy_transp_50_done[["wy_transp_50_1988"]] <-wy_transp_50_1988
#1989
wy_transp_50_1989<- raster_sites
values(wy_transp_50_1989) <- wy_transp_50_joindat_2[,"1989"]
wy_transp_50_done[["wy_transp_50_1989"]] <-wy_transp_50_1989
#1990
wy_transp_50_1990<- raster_sites
values(wy_transp_50_1990) <- wy_transp_50_joindat_2[,"1990"]
wy_transp_50_done[["wy_transp_50_1990"]] <-wy_transp_50_1990
#1991
wy_transp_50_1991<- raster_sites
values(wy_transp_50_1991) <- wy_transp_50_joindat_2[,"1991"]
wy_transp_50_done[["wy_transp_50_1991"]] <-wy_transp_50_1991
#1992
wy_transp_50_1992<- raster_sites
values(wy_transp_50_1992) <- wy_transp_50_joindat_2[,"1992"]
wy_transp_50_done[["wy_transp_50_1992"]] <-wy_transp_50_1992
#1993
wy_transp_50_1993<- raster_sites
values(wy_transp_50_1993) <- wy_transp_50_joindat_2[,"1993"]
wy_transp_50_done[["wy_transp_50_1993"]] <-wy_transp_50_1993
#1994
wy_transp_50_1994<- raster_sites
values(wy_transp_50_1994) <- wy_transp_50_joindat_2[,"1994"]
wy_transp_50_done[["wy_transp_50_1994"]] <-wy_transp_50_1994
#1995
wy_transp_50_1995<- raster_sites
values(wy_transp_50_1995) <- wy_transp_50_joindat_2[,"1995"]
wy_transp_50_done[["wy_transp_50_1995"]] <-wy_transp_50_1995
#1996
wy_transp_50_1996<- raster_sites
values(wy_transp_50_1996) <- wy_transp_50_joindat_2[,"1996"]
wy_transp_50_done[["wy_transp_50_1996"]] <-wy_transp_50_1996
#1997
wy_transp_50_1997<- raster_sites
values(wy_transp_50_1997) <- wy_transp_50_joindat_2[,"1997"]
wy_transp_50_done[["wy_transp_50_1997"]] <-wy_transp_50_1997
#1998
wy_transp_50_1998<- raster_sites
values(wy_transp_50_1998) <- wy_transp_50_joindat_2[,"1998"]
wy_transp_50_done[["wy_transp_50_1998"]] <-wy_transp_50_1998
#1999
wy_transp_50_1999<- raster_sites
values(wy_transp_50_1999) <- wy_transp_50_joindat_2[,"1999"]
wy_transp_50_done[["wy_transp_50_1999"]] <-wy_transp_50_1999
#2000
wy_transp_50_2000<- raster_sites
values(wy_transp_50_2000) <- wy_transp_50_joindat_2[,"2000"]
wy_transp_50_done[["wy_transp_50_2000"]] <-wy_transp_50_2000
#2001
wy_transp_50_2001<- raster_sites
values(wy_transp_50_2001) <- wy_transp_50_joindat_2[,"2001"]
wy_transp_50_done[["wy_transp_50_2001"]] <-wy_transp_50_2001
#2002
wy_transp_50_2002<- raster_sites
values(wy_transp_50_2002) <- wy_transp_50_joindat_2[,"2002"]
wy_transp_50_done[["wy_transp_50_2002"]] <-wy_transp_50_2002
#2003
wy_transp_50_2003<- raster_sites
values(wy_transp_50_2003) <- wy_transp_50_joindat_2[,"2003"]
wy_transp_50_done[["wy_transp_50_2003"]] <-wy_transp_50_2003
#2004
wy_transp_50_2004<- raster_sites
values(wy_transp_50_2004) <- wy_transp_50_joindat_2[,"2004"]
wy_transp_50_done[["wy_transp_50_2004"]] <-wy_transp_50_2004
#2005
wy_transp_50_2005<- raster_sites
values(wy_transp_50_2005) <- wy_transp_50_joindat_2[,"2005"]
wy_transp_50_done[["wy_transp_50_2005"]] <-wy_transp_50_2005
#2006
wy_transp_50_2006<- raster_sites
values(wy_transp_50_2006) <- wy_transp_50_joindat_2[,"2006"]
wy_transp_50_done[["wy_transp_50_2006"]] <-wy_transp_50_2006
#2007
wy_transp_50_2007<- raster_sites
values(wy_transp_50_2007) <- wy_transp_50_joindat_2[,"2007"]
wy_transp_50_done[["wy_transp_50_2007"]] <-wy_transp_50_2007
#2008
wy_transp_50_2008<- raster_sites
values(wy_transp_50_2008) <- wy_transp_50_joindat_2[,"2008"]
wy_transp_50_done[["wy_transp_50_2008"]] <-wy_transp_50_2008
#2009
wy_transp_50_2009<- raster_sites
values(wy_transp_50_2009) <- wy_transp_50_joindat_2[,"2009"]
wy_transp_50_done[["wy_transp_50_2009"]] <-wy_transp_50_2009
#2010
wy_transp_50_2010<- raster_sites
values(wy_transp_50_2010) <- wy_transp_50_joindat_2[,"2010"]
wy_transp_50_done[["wy_transp_50_2010"]] <-wy_transp_50_2010
#2011
wy_transp_50_2011<- raster_sites
values(wy_transp_50_2011) <- wy_transp_50_joindat_2[,"2011"]
wy_transp_50_done[["wy_transp_50_2011"]] <-wy_transp_50_2011
#2012
wy_transp_50_2012<- raster_sites
values(wy_transp_50_2012) <- wy_transp_50_joindat_2[,"2012"]
wy_transp_50_done[["wy_transp_50_2012"]] <-wy_transp_50_2012
#2013
wy_transp_50_2013<- raster_sites
values(wy_transp_50_2013) <- wy_transp_50_joindat_2[,"2013"]
wy_transp_50_done[["wy_transp_50_2013"]] <-wy_transp_50_2013
#2014
wy_transp_50_2014<- raster_sites
values(wy_transp_50_2014) <- wy_transp_50_joindat_2[,"2014"]
wy_transp_50_done[["wy_transp_50_2014"]] <-wy_transp_50_2014
#2015
wy_transp_50_2015<- raster_sites
values(wy_transp_50_2015) <- wy_transp_50_joindat_2[,"2015"]
wy_transp_50_done[["wy_transp_50_2015"]] <-wy_transp_50_2015

#all years stacked, ready for cropping for each site
wy_transp_50_stack <-stack(wy_transp_50_done)
plot(wy_transp_50_stack)


###hot-dry metric October-December#######

load(file.path(dir.AFRI_Historical, "Below3DD_OctDec19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(annualBelow3DD_OctDec)
str(annualBelow3DD_OctDec)
summary(annualBelow3DD_OctDec)
rownames(annualBelow3DD_OctDec)
head(annualBelow3DD_OctDec)

#add label to dataset based on row names
annualBelow3DD_OctDec$label <- row.names(annualBelow3DD_OctDec)

#if you want to remove the excess sites not in the core 5 rangeland types, if not, skip to line 48
annualBelow3DD_OctDec$Regionname <- substr(annualBelow3DD_OctDec$label, 8, 9) #get abbreviations for sites
unique(annualBelow3DD_OctDec$Regionname)
annualBelow3DD_OctDec <- dplyr::filter(annualBelow3DD_OctDec, Regionname != "De") #Remove excess site values

#code to link to baseline raster
sitenumENDpos = as.integer(regexpr('_', annualBelow3DD_OctDec$label) ) #find starting position for a pattern
Site <- as.integer(substr(annualBelow3DD_OctDec$label, 1, sitenumENDpos-1) ) #the number at the end of the site label, starts at 1
Regionname <- substr(annualBelow3DD_OctDec$label, 8, 9) #take out the 8th and 9th values from each label ID
Regionnum <- unlist(sapply(annualBelow3DD_OctDec$Regionname, FUN= function(x) grep(x, regions)) )
annualBelow3DD_OctDec$RegionSite <- Regionnum*1000000 + Site
annualBelow3DD_OctDec_joindat <- join(rastvals, annualBelow3DD_OctDec, by="RegionSite")

#get rid of years that don't relate to NPP data: isolate 1986-2015
annualBelow3DD_OctDec_joindat_2<-annualBelow3DD_OctDec_joindat[,-c(2:71)]

#use joined data to populate values for a raster
annualBelow3DD_OctDec_done <- list()
#1986
annualBelow3DD_OctDec_1986<- raster_sites
values(annualBelow3DD_OctDec_1986) <- annualBelow3DD_OctDec_joindat_2[,"1986"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1986"]] <-annualBelow3DD_OctDec_1986
#1987
annualBelow3DD_OctDec_1987<- raster_sites
values(annualBelow3DD_OctDec_1987) <- annualBelow3DD_OctDec_joindat_2[,"1987"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1987"]] <-annualBelow3DD_OctDec_1987
#1988
annualBelow3DD_OctDec_1988<- raster_sites
values(annualBelow3DD_OctDec_1988) <- annualBelow3DD_OctDec_joindat_2[,"1988"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1988"]] <-annualBelow3DD_OctDec_1988
#1989
annualBelow3DD_OctDec_1989<- raster_sites
values(annualBelow3DD_OctDec_1989) <- annualBelow3DD_OctDec_joindat_2[,"1989"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1989"]] <-annualBelow3DD_OctDec_1989
#1990
annualBelow3DD_OctDec_1990<- raster_sites
values(annualBelow3DD_OctDec_1990) <- annualBelow3DD_OctDec_joindat_2[,"1990"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1990"]] <-annualBelow3DD_OctDec_1990
#1991
annualBelow3DD_OctDec_1991<- raster_sites
values(annualBelow3DD_OctDec_1991) <- annualBelow3DD_OctDec_joindat_2[,"1991"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1991"]] <-annualBelow3DD_OctDec_1991
#1992
annualBelow3DD_OctDec_1992<- raster_sites
values(annualBelow3DD_OctDec_1992) <- annualBelow3DD_OctDec_joindat_2[,"1992"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1992"]] <-annualBelow3DD_OctDec_1992
#1993
annualBelow3DD_OctDec_1993<- raster_sites
values(annualBelow3DD_OctDec_1993) <- annualBelow3DD_OctDec_joindat_2[,"1993"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1993"]] <-annualBelow3DD_OctDec_1993
#1994
annualBelow3DD_OctDec_1994<- raster_sites
values(annualBelow3DD_OctDec_1994) <- annualBelow3DD_OctDec_joindat_2[,"1994"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1994"]] <-annualBelow3DD_OctDec_1994
#1995
annualBelow3DD_OctDec_1995<- raster_sites
values(annualBelow3DD_OctDec_1995) <- annualBelow3DD_OctDec_joindat_2[,"1995"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1995"]] <-annualBelow3DD_OctDec_1995
#1996
annualBelow3DD_OctDec_1996<- raster_sites
values(annualBelow3DD_OctDec_1996) <- annualBelow3DD_OctDec_joindat_2[,"1996"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1996"]] <-annualBelow3DD_OctDec_1996
#1997
annualBelow3DD_OctDec_1997<- raster_sites
values(annualBelow3DD_OctDec_1997) <- annualBelow3DD_OctDec_joindat_2[,"1997"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1997"]] <-annualBelow3DD_OctDec_1997
#1998
annualBelow3DD_OctDec_1998<- raster_sites
values(annualBelow3DD_OctDec_1998) <- annualBelow3DD_OctDec_joindat_2[,"1998"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1998"]] <-annualBelow3DD_OctDec_1998
#1999
annualBelow3DD_OctDec_1999<- raster_sites
values(annualBelow3DD_OctDec_1999) <- annualBelow3DD_OctDec_joindat_2[,"1999"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_1999"]] <-annualBelow3DD_OctDec_1999
#2000
annualBelow3DD_OctDec_2000<- raster_sites
values(annualBelow3DD_OctDec_2000) <- annualBelow3DD_OctDec_joindat_2[,"2000"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2000"]] <-annualBelow3DD_OctDec_2000
#2001
annualBelow3DD_OctDec_2001<- raster_sites
values(annualBelow3DD_OctDec_2001) <- annualBelow3DD_OctDec_joindat_2[,"2001"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2001"]] <-annualBelow3DD_OctDec_2001
#2002
annualBelow3DD_OctDec_2002<- raster_sites
values(annualBelow3DD_OctDec_2002) <- annualBelow3DD_OctDec_joindat_2[,"2002"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2002"]] <-annualBelow3DD_OctDec_2002
#2003
annualBelow3DD_OctDec_2003<- raster_sites
values(annualBelow3DD_OctDec_2003) <- annualBelow3DD_OctDec_joindat_2[,"2003"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2003"]] <-annualBelow3DD_OctDec_2003
#2004
annualBelow3DD_OctDec_2004<- raster_sites
values(annualBelow3DD_OctDec_2004) <- annualBelow3DD_OctDec_joindat_2[,"2004"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2004"]] <-annualBelow3DD_OctDec_2004
#2005
annualBelow3DD_OctDec_2005<- raster_sites
values(annualBelow3DD_OctDec_2005) <- annualBelow3DD_OctDec_joindat_2[,"2005"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2005"]] <-annualBelow3DD_OctDec_2005
#2006
annualBelow3DD_OctDec_2006<- raster_sites
values(annualBelow3DD_OctDec_2006) <- annualBelow3DD_OctDec_joindat_2[,"2006"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2006"]] <-annualBelow3DD_OctDec_2006
#2007
annualBelow3DD_OctDec_2007<- raster_sites
values(annualBelow3DD_OctDec_2007) <- annualBelow3DD_OctDec_joindat_2[,"2007"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2007"]] <-annualBelow3DD_OctDec_2007
#2008
annualBelow3DD_OctDec_2008<- raster_sites
values(annualBelow3DD_OctDec_2008) <- annualBelow3DD_OctDec_joindat_2[,"2008"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2008"]] <-annualBelow3DD_OctDec_2008
#2009
annualBelow3DD_OctDec_2009<- raster_sites
values(annualBelow3DD_OctDec_2009) <- annualBelow3DD_OctDec_joindat_2[,"2009"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2009"]] <-annualBelow3DD_OctDec_2009
#2010
annualBelow3DD_OctDec_2010<- raster_sites
values(annualBelow3DD_OctDec_2010) <- annualBelow3DD_OctDec_joindat_2[,"2010"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2010"]] <-annualBelow3DD_OctDec_2010
#2011
annualBelow3DD_OctDec_2011<- raster_sites
values(annualBelow3DD_OctDec_2011) <- annualBelow3DD_OctDec_joindat_2[,"2011"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2011"]] <-annualBelow3DD_OctDec_2011
#2012
annualBelow3DD_OctDec_2012<- raster_sites
values(annualBelow3DD_OctDec_2012) <- annualBelow3DD_OctDec_joindat_2[,"2012"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2012"]] <-annualBelow3DD_OctDec_2012
#2013
annualBelow3DD_OctDec_2013<- raster_sites
values(annualBelow3DD_OctDec_2013) <- annualBelow3DD_OctDec_joindat_2[,"2013"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2013"]] <-annualBelow3DD_OctDec_2013
#2014
annualBelow3DD_OctDec_2014<- raster_sites
values(annualBelow3DD_OctDec_2014) <- annualBelow3DD_OctDec_joindat_2[,"2014"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2014"]] <-annualBelow3DD_OctDec_2014
#2015
annualBelow3DD_OctDec_2015<- raster_sites
values(annualBelow3DD_OctDec_2015) <- annualBelow3DD_OctDec_joindat_2[,"2015"]
annualBelow3DD_OctDec_done[["annualBelow3DD_OctDec_2015"]] <-annualBelow3DD_OctDec_2015

#all years stacked, ready for cropping for each site
annualBelow3DD_OctDec_stack <-stack(annualBelow3DD_OctDec_done)
plot(annualBelow3DD_OctDec_stack)

###hot-dry metric July - September#######

load(file.path(dir.AFRI_Historical, "Below3DD_JulSep19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(annualBelow3DD_JulSep)
str(annualBelow3DD_JulSep)
summary(annualBelow3DD_JulSep)
rownames(annualBelow3DD_JulSep)
head(annualBelow3DD_JulSep)

#add label to dataset based on row names
annualBelow3DD_JulSep$label <- row.names(annualBelow3DD_JulSep)

#if you want to remove the excess sites not in the core 5 rangeland types, if not, skip to line 48
annualBelow3DD_JulSep$Regionname <- substr(annualBelow3DD_JulSep$label, 8, 9) #get abbreviations for sites
unique(annualBelow3DD_JulSep$Regionname)
annualBelow3DD_JulSep <- dplyr::filter(annualBelow3DD_JulSep, Regionname != "De") #Remove excess site values

#code to link to baseline raster
sitenumENDpos = as.integer(regexpr('_', annualBelow3DD_JulSep$label) ) #find starting position for a pattern
Site <- as.integer(substr(annualBelow3DD_JulSep$label, 1, sitenumENDpos-1) ) #the number at the end of the site label, starts at 1
Regionname <- substr(annualBelow3DD_JulSep$label, 8, 9) #take out the 8th and 9th values from each label ID
Regionnum <- unlist(sapply(annualBelow3DD_JulSep$Regionname, FUN= function(x) grep(x, regions)) )
annualBelow3DD_JulSep$RegionSite <- Regionnum*1000000 + Site
annualBelow3DD_JulSep_joindat <- join(rastvals, annualBelow3DD_JulSep, by="RegionSite")

#get rid of years that don't relate to NPP data: isolate 1986-2015
annualBelow3DD_JulSep_joindat_2<-annualBelow3DD_JulSep_joindat[,-c(2:71)]

#use joined data to populate values for a raster
annualBelow3DD_JulSep_done <- list()
#1986
annualBelow3DD_JulSep_1986<- raster_sites
values(annualBelow3DD_JulSep_1986) <- annualBelow3DD_JulSep_joindat_2[,"1986"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1986"]] <-annualBelow3DD_JulSep_1986
#1987
annualBelow3DD_JulSep_1987<- raster_sites
values(annualBelow3DD_JulSep_1987) <- annualBelow3DD_JulSep_joindat_2[,"1987"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1987"]] <-annualBelow3DD_JulSep_1987
#1988
annualBelow3DD_JulSep_1988<- raster_sites
values(annualBelow3DD_JulSep_1988) <- annualBelow3DD_JulSep_joindat_2[,"1988"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1988"]] <-annualBelow3DD_JulSep_1988
#1989
annualBelow3DD_JulSep_1989<- raster_sites
values(annualBelow3DD_JulSep_1989) <- annualBelow3DD_JulSep_joindat_2[,"1989"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1989"]] <-annualBelow3DD_JulSep_1989
#1990
annualBelow3DD_JulSep_1990<- raster_sites
values(annualBelow3DD_JulSep_1990) <- annualBelow3DD_JulSep_joindat_2[,"1990"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1990"]] <-annualBelow3DD_JulSep_1990
#1991
annualBelow3DD_JulSep_1991<- raster_sites
values(annualBelow3DD_JulSep_1991) <- annualBelow3DD_JulSep_joindat_2[,"1991"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1991"]] <-annualBelow3DD_JulSep_1991
#1992
annualBelow3DD_JulSep_1992<- raster_sites
values(annualBelow3DD_JulSep_1992) <- annualBelow3DD_JulSep_joindat_2[,"1992"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1992"]] <-annualBelow3DD_JulSep_1992
#1993
annualBelow3DD_JulSep_1993<- raster_sites
values(annualBelow3DD_JulSep_1993) <- annualBelow3DD_JulSep_joindat_2[,"1993"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1993"]] <-annualBelow3DD_JulSep_1993
#1994
annualBelow3DD_JulSep_1994<- raster_sites
values(annualBelow3DD_JulSep_1994) <- annualBelow3DD_JulSep_joindat_2[,"1994"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1994"]] <-annualBelow3DD_JulSep_1994
#1995
annualBelow3DD_JulSep_1995<- raster_sites
values(annualBelow3DD_JulSep_1995) <- annualBelow3DD_JulSep_joindat_2[,"1995"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1995"]] <-annualBelow3DD_JulSep_1995
#1996
annualBelow3DD_JulSep_1996<- raster_sites
values(annualBelow3DD_JulSep_1996) <- annualBelow3DD_JulSep_joindat_2[,"1996"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1996"]] <-annualBelow3DD_JulSep_1996
#1997
annualBelow3DD_JulSep_1997<- raster_sites
values(annualBelow3DD_JulSep_1997) <- annualBelow3DD_JulSep_joindat_2[,"1997"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1997"]] <-annualBelow3DD_JulSep_1997
#1998
annualBelow3DD_JulSep_1998<- raster_sites
values(annualBelow3DD_JulSep_1998) <- annualBelow3DD_JulSep_joindat_2[,"1998"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1998"]] <-annualBelow3DD_JulSep_1998
#1999
annualBelow3DD_JulSep_1999<- raster_sites
values(annualBelow3DD_JulSep_1999) <- annualBelow3DD_JulSep_joindat_2[,"1999"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_1999"]] <-annualBelow3DD_JulSep_1999
#2000
annualBelow3DD_JulSep_2000<- raster_sites
values(annualBelow3DD_JulSep_2000) <- annualBelow3DD_JulSep_joindat_2[,"2000"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2000"]] <-annualBelow3DD_JulSep_2000
#2001
annualBelow3DD_JulSep_2001<- raster_sites
values(annualBelow3DD_JulSep_2001) <- annualBelow3DD_JulSep_joindat_2[,"2001"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2001"]] <-annualBelow3DD_JulSep_2001
#2002
annualBelow3DD_JulSep_2002<- raster_sites
values(annualBelow3DD_JulSep_2002) <- annualBelow3DD_JulSep_joindat_2[,"2002"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2002"]] <-annualBelow3DD_JulSep_2002
#2003
annualBelow3DD_JulSep_2003<- raster_sites
values(annualBelow3DD_JulSep_2003) <- annualBelow3DD_JulSep_joindat_2[,"2003"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2003"]] <-annualBelow3DD_JulSep_2003
#2004
annualBelow3DD_JulSep_2004<- raster_sites
values(annualBelow3DD_JulSep_2004) <- annualBelow3DD_JulSep_joindat_2[,"2004"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2004"]] <-annualBelow3DD_JulSep_2004
#2005
annualBelow3DD_JulSep_2005<- raster_sites
values(annualBelow3DD_JulSep_2005) <- annualBelow3DD_JulSep_joindat_2[,"2005"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2005"]] <-annualBelow3DD_JulSep_2005
#2006
annualBelow3DD_JulSep_2006<- raster_sites
values(annualBelow3DD_JulSep_2006) <- annualBelow3DD_JulSep_joindat_2[,"2006"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2006"]] <-annualBelow3DD_JulSep_2006
#2007
annualBelow3DD_JulSep_2007<- raster_sites
values(annualBelow3DD_JulSep_2007) <- annualBelow3DD_JulSep_joindat_2[,"2007"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2007"]] <-annualBelow3DD_JulSep_2007
#2008
annualBelow3DD_JulSep_2008<- raster_sites
values(annualBelow3DD_JulSep_2008) <- annualBelow3DD_JulSep_joindat_2[,"2008"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2008"]] <-annualBelow3DD_JulSep_2008
#2009
annualBelow3DD_JulSep_2009<- raster_sites
values(annualBelow3DD_JulSep_2009) <- annualBelow3DD_JulSep_joindat_2[,"2009"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2009"]] <-annualBelow3DD_JulSep_2009
#2010
annualBelow3DD_JulSep_2010<- raster_sites
values(annualBelow3DD_JulSep_2010) <- annualBelow3DD_JulSep_joindat_2[,"2010"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2010"]] <-annualBelow3DD_JulSep_2010
#2011
annualBelow3DD_JulSep_2011<- raster_sites
values(annualBelow3DD_JulSep_2011) <- annualBelow3DD_JulSep_joindat_2[,"2011"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2011"]] <-annualBelow3DD_JulSep_2011
#2012
annualBelow3DD_JulSep_2012<- raster_sites
values(annualBelow3DD_JulSep_2012) <- annualBelow3DD_JulSep_joindat_2[,"2012"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2012"]] <-annualBelow3DD_JulSep_2012
#2013
annualBelow3DD_JulSep_2013<- raster_sites
values(annualBelow3DD_JulSep_2013) <- annualBelow3DD_JulSep_joindat_2[,"2013"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2013"]] <-annualBelow3DD_JulSep_2013
#2014
annualBelow3DD_JulSep_2014<- raster_sites
values(annualBelow3DD_JulSep_2014) <- annualBelow3DD_JulSep_joindat_2[,"2014"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2014"]] <-annualBelow3DD_JulSep_2014
#2015
annualBelow3DD_JulSep_2015<- raster_sites
values(annualBelow3DD_JulSep_2015) <- annualBelow3DD_JulSep_joindat_2[,"2015"]
annualBelow3DD_JulSep_done[["annualBelow3DD_JulSep_2015"]] <-annualBelow3DD_JulSep_2015

#all years stacked, ready for cropping for each site
annualBelow3DD_JulSep_stack <-stack(annualBelow3DD_JulSep_done)
plot(annualBelow3DD_JulSep_stack)


###hot-dry metric April - June#######

load(file.path(dir.AFRI_Historical, "Below3DD_AprJun19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(annualBelow3DD_AprJun)
str(annualBelow3DD_AprJun)
summary(annualBelow3DD_AprJun)
rownames(annualBelow3DD_AprJun)
head(annualBelow3DD_AprJun)

#add label to dataset based on row names
annualBelow3DD_AprJun$label <- row.names(annualBelow3DD_AprJun)

#if you want to remove the excess sites not in the core 5 rangeland types, if not, skip to line 48
annualBelow3DD_AprJun$Regionname <- substr(annualBelow3DD_AprJun$label, 8, 9) #get abbreviations for sites
unique(annualBelow3DD_AprJun$Regionname)
annualBelow3DD_AprJun <- dplyr::filter(annualBelow3DD_AprJun, Regionname != "De") #Remove excess site values

#code to link to baseline raster
sitenumENDpos = as.integer(regexpr('_', annualBelow3DD_AprJun$label) ) #find starting position for a pattern
Site <- as.integer(substr(annualBelow3DD_AprJun$label, 1, sitenumENDpos-1) ) #the number at the end of the site label, starts at 1
Regionname <- substr(annualBelow3DD_AprJun$label, 8, 9) #take out the 8th and 9th values from each label ID
Regionnum <- unlist(sapply(annualBelow3DD_AprJun$Regionname, FUN= function(x) grep(x, regions)) )
annualBelow3DD_AprJun$RegionSite <- Regionnum*1000000 + Site
annualBelow3DD_AprJun_joindat <- join(rastvals, annualBelow3DD_AprJun, by="RegionSite")

#get rid of years that don't relate to NPP data: isolate 1986-2015
annualBelow3DD_AprJun_joindat_2<-annualBelow3DD_AprJun_joindat[,-c(2:71)]

#use joined data to populate values for a raster
annualBelow3DD_AprJun_done <- list()
#1986
annualBelow3DD_AprJun_1986<- raster_sites
values(annualBelow3DD_AprJun_1986) <- annualBelow3DD_AprJun_joindat_2[,"1986"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1986"]] <-annualBelow3DD_AprJun_1986
#1987
annualBelow3DD_AprJun_1987<- raster_sites
values(annualBelow3DD_AprJun_1987) <- annualBelow3DD_AprJun_joindat_2[,"1987"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1987"]] <-annualBelow3DD_AprJun_1987
#1988
annualBelow3DD_AprJun_1988<- raster_sites
values(annualBelow3DD_AprJun_1988) <- annualBelow3DD_AprJun_joindat_2[,"1988"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1988"]] <-annualBelow3DD_AprJun_1988
#1989
annualBelow3DD_AprJun_1989<- raster_sites
values(annualBelow3DD_AprJun_1989) <- annualBelow3DD_AprJun_joindat_2[,"1989"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1989"]] <-annualBelow3DD_AprJun_1989
#1990
annualBelow3DD_AprJun_1990<- raster_sites
values(annualBelow3DD_AprJun_1990) <- annualBelow3DD_AprJun_joindat_2[,"1990"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1990"]] <-annualBelow3DD_AprJun_1990
#1991
annualBelow3DD_AprJun_1991<- raster_sites
values(annualBelow3DD_AprJun_1991) <- annualBelow3DD_AprJun_joindat_2[,"1991"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1991"]] <-annualBelow3DD_AprJun_1991
#1992
annualBelow3DD_AprJun_1992<- raster_sites
values(annualBelow3DD_AprJun_1992) <- annualBelow3DD_AprJun_joindat_2[,"1992"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1992"]] <-annualBelow3DD_AprJun_1992
#1993
annualBelow3DD_AprJun_1993<- raster_sites
values(annualBelow3DD_AprJun_1993) <- annualBelow3DD_AprJun_joindat_2[,"1993"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1993"]] <-annualBelow3DD_AprJun_1993
#1994
annualBelow3DD_AprJun_1994<- raster_sites
values(annualBelow3DD_AprJun_1994) <- annualBelow3DD_AprJun_joindat_2[,"1994"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1994"]] <-annualBelow3DD_AprJun_1994
#1995
annualBelow3DD_AprJun_1995<- raster_sites
values(annualBelow3DD_AprJun_1995) <- annualBelow3DD_AprJun_joindat_2[,"1995"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1995"]] <-annualBelow3DD_AprJun_1995
#1996
annualBelow3DD_AprJun_1996<- raster_sites
values(annualBelow3DD_AprJun_1996) <- annualBelow3DD_AprJun_joindat_2[,"1996"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1996"]] <-annualBelow3DD_AprJun_1996
#1997
annualBelow3DD_AprJun_1997<- raster_sites
values(annualBelow3DD_AprJun_1997) <- annualBelow3DD_AprJun_joindat_2[,"1997"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1997"]] <-annualBelow3DD_AprJun_1997
#1998
annualBelow3DD_AprJun_1998<- raster_sites
values(annualBelow3DD_AprJun_1998) <- annualBelow3DD_AprJun_joindat_2[,"1998"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1998"]] <-annualBelow3DD_AprJun_1998
#1999
annualBelow3DD_AprJun_1999<- raster_sites
values(annualBelow3DD_AprJun_1999) <- annualBelow3DD_AprJun_joindat_2[,"1999"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_1999"]] <-annualBelow3DD_AprJun_1999
#2000
annualBelow3DD_AprJun_2000<- raster_sites
values(annualBelow3DD_AprJun_2000) <- annualBelow3DD_AprJun_joindat_2[,"2000"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2000"]] <-annualBelow3DD_AprJun_2000
#2001
annualBelow3DD_AprJun_2001<- raster_sites
values(annualBelow3DD_AprJun_2001) <- annualBelow3DD_AprJun_joindat_2[,"2001"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2001"]] <-annualBelow3DD_AprJun_2001
#2002
annualBelow3DD_AprJun_2002<- raster_sites
values(annualBelow3DD_AprJun_2002) <- annualBelow3DD_AprJun_joindat_2[,"2002"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2002"]] <-annualBelow3DD_AprJun_2002
#2003
annualBelow3DD_AprJun_2003<- raster_sites
values(annualBelow3DD_AprJun_2003) <- annualBelow3DD_AprJun_joindat_2[,"2003"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2003"]] <-annualBelow3DD_AprJun_2003
#2004
annualBelow3DD_AprJun_2004<- raster_sites
values(annualBelow3DD_AprJun_2004) <- annualBelow3DD_AprJun_joindat_2[,"2004"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2004"]] <-annualBelow3DD_AprJun_2004
#2005
annualBelow3DD_AprJun_2005<- raster_sites
values(annualBelow3DD_AprJun_2005) <- annualBelow3DD_AprJun_joindat_2[,"2005"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2005"]] <-annualBelow3DD_AprJun_2005
#2006
annualBelow3DD_AprJun_2006<- raster_sites
values(annualBelow3DD_AprJun_2006) <- annualBelow3DD_AprJun_joindat_2[,"2006"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2006"]] <-annualBelow3DD_AprJun_2006
#2007
annualBelow3DD_AprJun_2007<- raster_sites
values(annualBelow3DD_AprJun_2007) <- annualBelow3DD_AprJun_joindat_2[,"2007"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2007"]] <-annualBelow3DD_AprJun_2007
#2008
annualBelow3DD_AprJun_2008<- raster_sites
values(annualBelow3DD_AprJun_2008) <- annualBelow3DD_AprJun_joindat_2[,"2008"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2008"]] <-annualBelow3DD_AprJun_2008
#2009
annualBelow3DD_AprJun_2009<- raster_sites
values(annualBelow3DD_AprJun_2009) <- annualBelow3DD_AprJun_joindat_2[,"2009"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2009"]] <-annualBelow3DD_AprJun_2009
#2010
annualBelow3DD_AprJun_2010<- raster_sites
values(annualBelow3DD_AprJun_2010) <- annualBelow3DD_AprJun_joindat_2[,"2010"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2010"]] <-annualBelow3DD_AprJun_2010
#2011
annualBelow3DD_AprJun_2011<- raster_sites
values(annualBelow3DD_AprJun_2011) <- annualBelow3DD_AprJun_joindat_2[,"2011"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2011"]] <-annualBelow3DD_AprJun_2011
#2012
annualBelow3DD_AprJun_2012<- raster_sites
values(annualBelow3DD_AprJun_2012) <- annualBelow3DD_AprJun_joindat_2[,"2012"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2012"]] <-annualBelow3DD_AprJun_2012
#2013
annualBelow3DD_AprJun_2013<- raster_sites
values(annualBelow3DD_AprJun_2013) <- annualBelow3DD_AprJun_joindat_2[,"2013"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2013"]] <-annualBelow3DD_AprJun_2013
#2014
annualBelow3DD_AprJun_2014<- raster_sites
values(annualBelow3DD_AprJun_2014) <- annualBelow3DD_AprJun_joindat_2[,"2014"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2014"]] <-annualBelow3DD_AprJun_2014
#2015
annualBelow3DD_AprJun_2015<- raster_sites
values(annualBelow3DD_AprJun_2015) <- annualBelow3DD_AprJun_joindat_2[,"2015"]
annualBelow3DD_AprJun_done[["annualBelow3DD_AprJun_2015"]] <-annualBelow3DD_AprJun_2015

#all years stacked, ready for cropping for each site
annualBelow3DD_AprJun_stack <-stack(annualBelow3DD_AprJun_done)
plot(annualBelow3DD_AprJun_stack)


###hot-dry metric january - March#######

load(file.path(dir.AFRI_Historical, "Below3DD_JanMar19152015.Rdata")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(annualBelow3DD_JanMar)
str(annualBelow3DD_JanMar)
summary(annualBelow3DD_JanMar)
rownames(annualBelow3DD_JanMar)
head(annualBelow3DD_JanMar)

#add label to dataset based on row names
annualBelow3DD_JanMar$label <- row.names(annualBelow3DD_JanMar)

#if you want to remove the excess sites not in the core 5 rangeland types, if not, skip to line 48
annualBelow3DD_JanMar$Regionname <- substr(annualBelow3DD_JanMar$label, 8, 9) #get abbreviations for sites
unique(annualBelow3DD_JanMar$Regionname)
annualBelow3DD_JanMar <- dplyr::filter(annualBelow3DD_JanMar, Regionname != "De") #Remove excess site values

#code to link to baseline raster
sitenumENDpos = as.integer(regexpr('_', annualBelow3DD_JanMar$label) ) #find starting position for a pattern
Site <- as.integer(substr(annualBelow3DD_JanMar$label, 1, sitenumENDpos-1) ) #the number at the end of the site label, starts at 1
Regionname <- substr(annualBelow3DD_JanMar$label, 8, 9) #take out the 8th and 9th values from each label ID
Regionnum <- unlist(sapply(annualBelow3DD_JanMar$Regionname, FUN= function(x) grep(x, regions)) )
annualBelow3DD_JanMar$RegionSite <- Regionnum*1000000 + Site
annualBelow3DD_JanMar_joindat <- join(rastvals, annualBelow3DD_JanMar, by="RegionSite")

#get rid of years that don't relate to NPP data: isolate 1986-2015
annualBelow3DD_JanMar_joindat_2<-annualBelow3DD_JanMar_joindat[,-c(2:71)]

#use joined data to populate values for a raster
annualBelow3DD_JanMar_done <- list()
#1986
annualBelow3DD_JanMar_1986<- raster_sites
values(annualBelow3DD_JanMar_1986) <- annualBelow3DD_JanMar_joindat_2[,"1986"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1986"]] <-annualBelow3DD_JanMar_1986
#1987
annualBelow3DD_JanMar_1987<- raster_sites
values(annualBelow3DD_JanMar_1987) <- annualBelow3DD_JanMar_joindat_2[,"1987"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1987"]] <-annualBelow3DD_JanMar_1987
#1988
annualBelow3DD_JanMar_1988<- raster_sites
values(annualBelow3DD_JanMar_1988) <- annualBelow3DD_JanMar_joindat_2[,"1988"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1988"]] <-annualBelow3DD_JanMar_1988
#1989
annualBelow3DD_JanMar_1989<- raster_sites
values(annualBelow3DD_JanMar_1989) <- annualBelow3DD_JanMar_joindat_2[,"1989"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1989"]] <-annualBelow3DD_JanMar_1989
#1990
annualBelow3DD_JanMar_1990<- raster_sites
values(annualBelow3DD_JanMar_1990) <- annualBelow3DD_JanMar_joindat_2[,"1990"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1990"]] <-annualBelow3DD_JanMar_1990
#1991
annualBelow3DD_JanMar_1991<- raster_sites
values(annualBelow3DD_JanMar_1991) <- annualBelow3DD_JanMar_joindat_2[,"1991"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1991"]] <-annualBelow3DD_JanMar_1991
#1992
annualBelow3DD_JanMar_1992<- raster_sites
values(annualBelow3DD_JanMar_1992) <- annualBelow3DD_JanMar_joindat_2[,"1992"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1992"]] <-annualBelow3DD_JanMar_1992
#1993
annualBelow3DD_JanMar_1993<- raster_sites
values(annualBelow3DD_JanMar_1993) <- annualBelow3DD_JanMar_joindat_2[,"1993"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1993"]] <-annualBelow3DD_JanMar_1993
#1994
annualBelow3DD_JanMar_1994<- raster_sites
values(annualBelow3DD_JanMar_1994) <- annualBelow3DD_JanMar_joindat_2[,"1994"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1994"]] <-annualBelow3DD_JanMar_1994
#1995
annualBelow3DD_JanMar_1995<- raster_sites
values(annualBelow3DD_JanMar_1995) <- annualBelow3DD_JanMar_joindat_2[,"1995"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1995"]] <-annualBelow3DD_JanMar_1995
#1996
annualBelow3DD_JanMar_1996<- raster_sites
values(annualBelow3DD_JanMar_1996) <- annualBelow3DD_JanMar_joindat_2[,"1996"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1996"]] <-annualBelow3DD_JanMar_1996
#1997
annualBelow3DD_JanMar_1997<- raster_sites
values(annualBelow3DD_JanMar_1997) <- annualBelow3DD_JanMar_joindat_2[,"1997"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1997"]] <-annualBelow3DD_JanMar_1997
#1998
annualBelow3DD_JanMar_1998<- raster_sites
values(annualBelow3DD_JanMar_1998) <- annualBelow3DD_JanMar_joindat_2[,"1998"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1998"]] <-annualBelow3DD_JanMar_1998
#1999
annualBelow3DD_JanMar_1999<- raster_sites
values(annualBelow3DD_JanMar_1999) <- annualBelow3DD_JanMar_joindat_2[,"1999"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_1999"]] <-annualBelow3DD_JanMar_1999
#2000
annualBelow3DD_JanMar_2000<- raster_sites
values(annualBelow3DD_JanMar_2000) <- annualBelow3DD_JanMar_joindat_2[,"2000"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2000"]] <-annualBelow3DD_JanMar_2000
#2001
annualBelow3DD_JanMar_2001<- raster_sites
values(annualBelow3DD_JanMar_2001) <- annualBelow3DD_JanMar_joindat_2[,"2001"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2001"]] <-annualBelow3DD_JanMar_2001
#2002
annualBelow3DD_JanMar_2002<- raster_sites
values(annualBelow3DD_JanMar_2002) <- annualBelow3DD_JanMar_joindat_2[,"2002"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2002"]] <-annualBelow3DD_JanMar_2002
#2003
annualBelow3DD_JanMar_2003<- raster_sites
values(annualBelow3DD_JanMar_2003) <- annualBelow3DD_JanMar_joindat_2[,"2003"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2003"]] <-annualBelow3DD_JanMar_2003
#2004
annualBelow3DD_JanMar_2004<- raster_sites
values(annualBelow3DD_JanMar_2004) <- annualBelow3DD_JanMar_joindat_2[,"2004"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2004"]] <-annualBelow3DD_JanMar_2004
#2005
annualBelow3DD_JanMar_2005<- raster_sites
values(annualBelow3DD_JanMar_2005) <- annualBelow3DD_JanMar_joindat_2[,"2005"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2005"]] <-annualBelow3DD_JanMar_2005
#2006
annualBelow3DD_JanMar_2006<- raster_sites
values(annualBelow3DD_JanMar_2006) <- annualBelow3DD_JanMar_joindat_2[,"2006"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2006"]] <-annualBelow3DD_JanMar_2006
#2007
annualBelow3DD_JanMar_2007<- raster_sites
values(annualBelow3DD_JanMar_2007) <- annualBelow3DD_JanMar_joindat_2[,"2007"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2007"]] <-annualBelow3DD_JanMar_2007
#2008
annualBelow3DD_JanMar_2008<- raster_sites
values(annualBelow3DD_JanMar_2008) <- annualBelow3DD_JanMar_joindat_2[,"2008"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2008"]] <-annualBelow3DD_JanMar_2008
#2009
annualBelow3DD_JanMar_2009<- raster_sites
values(annualBelow3DD_JanMar_2009) <- annualBelow3DD_JanMar_joindat_2[,"2009"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2009"]] <-annualBelow3DD_JanMar_2009
#2010
annualBelow3DD_JanMar_2010<- raster_sites
values(annualBelow3DD_JanMar_2010) <- annualBelow3DD_JanMar_joindat_2[,"2010"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2010"]] <-annualBelow3DD_JanMar_2010
#2011
annualBelow3DD_JanMar_2011<- raster_sites
values(annualBelow3DD_JanMar_2011) <- annualBelow3DD_JanMar_joindat_2[,"2011"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2011"]] <-annualBelow3DD_JanMar_2011
#2012
annualBelow3DD_JanMar_2012<- raster_sites
values(annualBelow3DD_JanMar_2012) <- annualBelow3DD_JanMar_joindat_2[,"2012"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2012"]] <-annualBelow3DD_JanMar_2012
#2013
annualBelow3DD_JanMar_2013<- raster_sites
values(annualBelow3DD_JanMar_2013) <- annualBelow3DD_JanMar_joindat_2[,"2013"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2013"]] <-annualBelow3DD_JanMar_2013
#2014
annualBelow3DD_JanMar_2014<- raster_sites
values(annualBelow3DD_JanMar_2014) <- annualBelow3DD_JanMar_joindat_2[,"2014"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2014"]] <-annualBelow3DD_JanMar_2014
#2015
annualBelow3DD_JanMar_2015<- raster_sites
values(annualBelow3DD_JanMar_2015) <- annualBelow3DD_JanMar_joindat_2[,"2015"]
annualBelow3DD_JanMar_done[["annualBelow3DD_JanMar_2015"]] <-annualBelow3DD_JanMar_2015

#all years stacked, ready for cropping for each site
annualBelow3DD_JanMar_stack <-stack(annualBelow3DD_JanMar_done)
plot(annualBelow3DD_JanMar_stack)

