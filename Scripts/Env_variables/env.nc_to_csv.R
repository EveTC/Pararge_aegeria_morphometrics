### Bring in environmental variables as .nc files and make to csv
## Environmental dataset: HadUK-Grid Gridded Climate Observations on a 1km grid over the UK 

### N.B. I can not directly porovide the environmental data. It is available through the CEDA archives and the Met office (for 2018 temp data).


## Follows http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

#### 1. Set up ####
# clear R environment
#rm(list=ls())

# install required libraries
#install.packages("RCurl")
#install.packages("readr")
# install and load packages
#install.packages("chron")
#install.packages("ncdf.tools")
#install.packages("ncdf")

# Load libraries required
library(RCurl)
library(readr)
library(raster)
library(ncdf4)
library(ncdf)
library(chron)
library(lattice)
library(RColorBrewer)
library(ncdf.tools)


#### 2. Mean Monthly air temp 2018 ####
ncpath <- "./Data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_201801-201812"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname, write=T)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname) # This should be a matrix with all NAs

# Change missing value - as this is not present in the file for 2018.

# Find out what missing value is in many decimals 
sprintf("%.100f", tmp_array[1,1,1])

# Set missing value
Mvalue <- 9.969209968386869047442886268468442020e+36

# change missing value in nc file
ncvar_change_missval(ncin, dname, Mvalue)
print(ncin)

# now when we call tmp_array these numbers should be replaced with NA
tmp_array <- ncvar_get(ncin,dname)
# correct

# continue as normal
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")

dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2017

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==Mvalue] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))

colMeans(x=tmp_df_All, na.rm=T)

## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_201801-201812"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Mean air temperature 2017 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_201701-201712"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
fillvalue
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2017

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_201701-201712"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2016 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_201601-201612"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))


## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))
length(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))
length(na.omit(tmp_df_All))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_201601-201612"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2015 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_201501-201512"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_201501-201512"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2014 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_201401-201412"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_201401-201412"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2013 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_201301-201312"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_201301-201312"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2012 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_201201-201212"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_201201-201212"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2011 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_201101-201112"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_201101-201112"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2010 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_201001-201012"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_201001-201012"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2009 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_200901-200912"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_200901-200912"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2008 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_200801-200812"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))


## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_200801-200812"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2007 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_200701-200712"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))


## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))
length(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))
length(na.omit(tmp_df_All))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))

## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_200701-200712"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2006 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_200601-200612"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname, write=T)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname) # this should be all NAs
# its not! There is no missing value number

# Find out what missing value is in many decimals 
sprintf("%.100f", tmp_array[1,1,1])

# Set missing value
Mvalue <- 9969209968386869047442886268468442020

# change missing value in nc file
ncvar_change_missval(ncin, dname, Mvalue)
print(ncin) # missing value has been inserted

# reload and it should work
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))


## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))
length(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))
length(na.omit(tmp_df_All))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))

## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_200601-200612"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temp 2005 ####
ncpath <- "./data/UKCP18/Mean_air_temperature_(tas)/.nc_files/"
ncname <- "tas_hadukgrid_uk_1km_mon_200501-200512"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname, write=T)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname) # this should be all NAs
# its not! There is no missing value number

# Find out what missing value is in many decimals 
sprintf("%.100f", tmp_array[1,1,1])

# Set missing value
Mvalue <- 9969209968386869047442886268468442020

# change missing value in nc file
ncvar_change_missval(ncin, dname, Mvalue)
print(ncin) # missing value has been inserted

# reload and it should work
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2016

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))


## Get a single slice of data
#Jan <- 1
#tmp_slice <- tmp_array[,,Jan]
#dim(tmp_slice)

# plot Jan Temps
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
#grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
#          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
#lonlat <- as.matrix(expand.grid(lon,lat))
#dim(lonlat)
#tmp_vec <- as.vector(tmp_slice)
#length(tmp_vec)

#tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
#names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
#head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))
length(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                       "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))
length(na.omit(tmp_df_All))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))

## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Mean_air_temperature_(tas)/"
csvname <- "tas_hadukgrid_uk_1km_mon_200501-200512"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Rainfall 2017 ####
ncpath <- "./data/UKCP18/Rainfall/"
ncname <- "rainfall_hadukgrid_uk_1km_mon_201701-201712"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "rainfall"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
rain_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(rain_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2017

## Replace netCDF fillvalues with R NAs
rain_array[rain_array==fillvalue$value] <- NA
length(na.omit(as.vector(rain_array[,,1])))

## Get a single slice of data
Jan <- 1
rain_slice <- rain_array[,,Jan]
dim(rain_slice)

# plot Jan Temps
image(lon,lat,rain_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
levelplot(rain_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)
rain_vec <- as.vector(rain_slice)
length(rain_vec)

rain_df01<- data.frame(cbind(lonlat,rain_vec))
names(rain_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
head(na.omit(rain_df01), 10)

### Convert whole array
rain_vec_long <- as.vector(rain_array)
length(rain_vec_long)

rain_mat <- matrix(rain_vec_long, nrow=nlon*nlat, ncol=nt)
dim(rain_mat)
head(na.omit(rain_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
rain_df_All <- data.frame(cbind(lonlat,rain_mat))
names(rain_df_All) <- c("lon", "lat", "rainJan.17", "rainFeb.17","rainMar.17","rainApr.17","rainMay.17","rainJun.17",
                        "rainJul.17","rainAug.17","rainSep.17","rainOct.17","rainNov.17","rainDec.17")
head(na.omit(rain_df_All, 20))

### Calculate 1) annual mean rainfall; 2) MPWA - Mean precip of wettest month; and 3) MPdr - mean precip of driest month
rain_df_All$mPwe <- apply(rain_df_All[3:14],1,max) # mwa
rain_df_All$mpdr <- apply(rain_df_All[3:14],1,min) # mtco
rain_df_All$map <- apply(rain_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(rain_df_All))

## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/Rainfall"
csvname <- "rainfall_hadukgrid_uk_1km_mon_201701-201712"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(rain_df_All), csvfile, row.names = FALSE, sep = ",")

#### Temperature 20yrs monthly average ####
ncpath <- "./data/UKCP18/long-term/"
ncname <- "tas_hadukgrid_uk_1km_mon-20y_198101-200012"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tas"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2017

## Replace netCDF fillvalues with R NAs
tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

## Get a single slice of data
Jan <- 1
tmp_slice <- tmp_array[,,Jan]
dim(tmp_slice)

# plot Jan Temps
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)

tmp_df01<- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
head(na.omit(tmp_df01), 10)

### Convert whole array
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df_All <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df_All) <- c("lon", "lat", "tmpJan", "tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun", "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
head(na.omit(tmp_df_All, 20))

### Calculate 1) annual mean temp; 2) MTWA - Mean temp of warmest month; and 3) MTCO - mean temp of coldest month
tmp_df_All$mtwa <- apply(tmp_df_All[3:14],1,max) # mtwa
tmp_df_All$mtco <- apply(tmp_df_All[3:14],1,min) # mtco
tmp_df_All$mat <- apply(tmp_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df_All))

## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/long-term/"
csvname <- "tas_hadukgrid_uk_1km_mon_201701-201712"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(tmp_df_All), csvfile, row.names = FALSE, sep = ",")

#### Rainfall 20yrs monthly average ####
ncpath <- "./data/UKCP18/long-term/"
ncname <- "rainfall_hadukgrid_uk_1km_mon-20y_198101-200012"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "rainfall"

ncin <- nc_open(ncfname)
print(ncin)

# get corodinate including time variables
lon <- ncvar_get(ncin,"projection_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "projection_y_coordinate")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt
tunits

## get temperature variable and its attributes
rain_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"untis")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(rain_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
short_name <- ncatt_get(ncin,0,"short_name")
collection <- ncatt_get(ncin,0,"collection")
comment <- ncatt_get(ncin,0,"comment")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
Conventions <- ncatt_get(ncin,0,"Conventions")

# close file
ncdf4::nc_close(ncin)

## Reshape from raster to rectangular

# Convert time variable
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time, origin=c(tday,tmonth,tyear)) # should they not all be 2017?

# another method
convertDateNcdf2R(time, units = "hours") # confirmed that all points are from year 2017

## Replace netCDF fillvalues with R NAs
rain_array[rain_array==fillvalue$value] <- NA
length(na.omit(as.vector(rain_array[,,1])))

## Get a single slice of data
Jan <- 1
rain_slice <- rain_array[,,Jan]
dim(rain_slice)

# plot Jan Temps
image(lon,lat,rain_slice, col=rev(brewer.pal(10,"RdBu")))

# alt plot
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-5,0,1,2,3,4,5,6,7,8,9,10)
levelplot(rain_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


## Create dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)
rain_vec <- as.vector(rain_slice)
length(rain_vec)

rain_df01<- data.frame(cbind(lonlat,rain_vec))
names(rain_df01) <- c("lon", "lat", paste(dname, as.character(Jan), sep="_"))
head(na.omit(rain_df01), 10)

### Convert whole array
rain_vec_long <- as.vector(rain_array)
length(rain_vec_long)

rain_mat <- matrix(rain_vec_long, nrow=nlon*nlat, ncol=nt)
dim(rain_mat)
head(na.omit(rain_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
rain_df_All <- data.frame(cbind(lonlat,rain_mat))
names(rain_df_All) <- c("lon", "lat", "rainJan", "rainFeb","rainMar","rainApr","rainMay","rainJun","rainJul","rainAug","rainSep","rainOct","rainNov","rainDec")
head(na.omit(rain_df_All, 20))

### Calculate 1) annual mean rainfall; 2) MPWA - Mean precip of wettest month; and 3) MPdr - mean precip of driest month
rain_df_All$mPwe <- apply(rain_df_All[3:14],1,max) # mwa
rain_df_All$mpdr <- apply(rain_df_All[3:14],1,min) # mtco
rain_df_All$map <- apply(rain_df_All[3:14],1,mean) # annual (i.e. row) means
head(na.omit(rain_df_All))

## data to csv
# read out dataframe to csv
csvpath <- "./data/UKCP18/long-term/"
csvname <- "rainfall_hadukgrid_uk_1km_mon-20y_198101-200012"
csvfile <- paste(csvpath, csvname,".csv", sep="")
write.table(na.omit(rain_df_All), csvfile, row.names = FALSE, sep = ",")