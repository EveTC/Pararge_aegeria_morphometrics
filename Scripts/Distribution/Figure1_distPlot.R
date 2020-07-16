## PA sample site plot per year 10km colonised (first recorded) with recorder effort
## written by Callum Macgregor and Evelyn Tyalor-Cox

## N.B. Distribution data can not be provided but is available from butterfly Condervation upon request

### Clear the current workspace (not loaded libraries)
rm(list=ls())

### install if necessary and then load the libraries you need

j <- c("rstudioapi","devtools","plyr","rnrfa","rgdal","dplyr","raster","RColorBrewer","lme4","ggplot2","lubridate","mgcv","gridExtra", "ggmap", "data.table")

new.packages <- j[!(j %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(j, require, character.only = TRUE)  # loads up any libraries that aren't already loaded

## install "Blighty" for easy plotting of the UK in base R

#install_version("blighty", version = "3.1-4", repos = "http://cran.us.r-project.org")

library(blighty)

# we have data from four sources - BNM, NMRS, UKBMS and RIS

## selecting data to use
# we have different criteria for inclusion in the study for each data type 
# (BNM/NMRS = hectad-level annual presence/absence; UKBMS/RIS = site-level daily count data)

# we need to apply these to select out species to use from the hectad data, and species/site combinations to use from the site data

### BNM data ####
# first let's do it on the hectad data for butterflies

## read in the data - can not be provided from us but is available from Butterfly conservation
BNM_raw <- read.csv("./Data/BC/record_levels/Report - BNM Butterflies 10km 1950-2014.csv", header = TRUE)

summary(BNM_raw)


# tweak the colnames to a standard

colnames(BNM_raw) <- c("SCI_NAME","COMMON_NAME","HECTAD","YEAR")

BNM_raw$fYEAR <- as.factor(BNM_raw$YEAR)

# attach a column to indicate presence

BNM_raw$PRESENCE <- 1

## next, we want to attach eastings and northings to this data so that it can be plotted
# as there are > 1000 repeats of each hectad let's generate a non-redundant list

BNM_hectads <- ddply(BNM_raw, .(HECTAD), summarise, COUNT = sum(PRESENCE))

# at the moment the dataset includes a whole bunch of records from sites in Northern Ireland, and some in the Channel Islands.
# that's nice but I don't want them - they will cause problems elsewhere (esp. when analysing range-margin stuff)

# the question is, how to pick them out and get rid?
# first let's take first two characters of every grid ref: these are the 100km square

BNM_hectads$SQUARE <- as.factor(substr(BNM_hectads$HECTAD, 1,2))

# now, all sites in GB have two letters, sites in NI have one letter and one number
# we can use this to separate them

BNM_hectadsGB <- BNM_hectads[which(!grepl("[[:digit:]]", BNM_hectads$SQUARE)),]

# to get rid of Channel Islands, we need to get rid of squares WA and WV

BNM_hectadsGB <- BNM_hectadsGB[which(!(BNM_hectadsGB$SQUARE %in% c("WA","WV"))), ]



# drop unused levels (i.e. those from NI)
BNM_hectadsGB <- droplevels(BNM_hectadsGB)


# now we are ready to parse the easting/northings

latlon <- osg_parse(BNM_hectadsGB$HECTAD)

BNM_hectadsGB$EASTING <- latlon[[1]]
BNM_hectadsGB$NORTHING <- latlon[[2]]

### we now have eastings and northings! let's do a brief visual check that all's well by plotting them on a map...

# to do this we actually also need these with fewer digits - specifically, in units of 1 km
BNM_hectadsGB$east.km <- BNM_hectadsGB$EASTING/1000
BNM_hectadsGB$north.km <- BNM_hectadsGB$NORTHING/1000

# plot base map to test
blighty()

# plot all records to test
# we need to offset all points slightly for them to be centred in the middle of hectads, rather than the bottom-left corner
points(5+BNM_hectadsGB$east.km,5+BNM_hectadsGB$north.km,pch=16) # this looks a bit messy but looks fine in a zoom window


# great
# we also need to merge these eastings and northings back into the main data
BNM_hectadsGB <- BNM_hectadsGB[,-2]


# we can now use this to trim down the other data to only include these sites
BNM_GB <- merge(BNM_raw,BNM_hectadsGB)



# let's plot an example species to get a distribution
BNM_GB.db <- BNM_GB[which(BNM_GB$COMMON_NAME=="Duke of Burgundy"), ]
blighty()
points((5+BNM_GB.db$east.km),(5+BNM_GB.db$north.km),pch=16)
title(main = "Duke of Burgundy")


## now restrict the data down to fit data-quality filters


### I want to select only 'well-recorded' hectads to take forwards
# this follows Hickling et al 2005, 'common squares' (see Hassall & Thompson 2010)
# the idea of this is that we only use hectads where we are more confident that we're looking at a true absence rather than a lack of recording
# i.e. we check every hectad individually and only use those where a high enough proportion of the regional fauna
# has been recorded at least once in each time window


## first, let's create a non-redundant list of recording in each hectad in each year, dropping the species information

BNM_rec.hectad.years <- ddply(BNM_GB, .(HECTAD, YEAR), numcolwise(mean))

# for inspection, let's plot the total hectad-level recording across all years of interest

years.extended <- 1950:2014


## EVE: this loop won't run until you create a folder for the output images to go into and update the line beginning with png accordingly

for (n in years.extended){
  year <- BNM_rec.hectad.years[which(BNM_rec.hectad.years$YEAR==n), ]
  png(paste0("./output/HectadRecording/per_year/",n,".png"),width = 612, height = 627, units = "px", bg = "white")
  blighty()
  points(year$east.km,year$north.km,pch=16)
  title(main = n)
  dev.off()
}

# we can see that recording was pretty sparse at some points and in some places, especially in the North
# but gets fairly robust especially in most recent years

# let's actually count the number of recorded hectads and the number of species*hectad records in each year 

recording <- data.frame(YEAR = numeric(),
                        HECTADS = numeric(),
                        RECORDS = numeric())

for (n in years.extended){
  year.hec <- BNM_rec.hectad.years[which(BNM_rec.hectad.years$YEAR==n), ]
  HECTADS <- nrow(year.hec)
  
  year.rec <- BNM_GB[which(BNM_GB$YEAR==n), ]
  RECORDS <- nrow(year.rec)
  
  YEAR <- n
  
  out <- data.frame(cbind(YEAR,HECTADS,RECORDS))
  recording <- rbind(recording,out)
  
  
}

summary(recording)


# now plot the number of hectads and records over time
plot(HECTADS ~ YEAR, data = recording)
plot(RECORDS ~ YEAR, data = recording)


# and the number of hectads recorded per year as a percent of the biggest year
total.hecs <- max(recording$HECTADS)

recording$PERC <- recording$HECTADS*100/total.hecs

plot(PERC ~ YEAR, data = recording)



# for butterflies, there is a really obvious jump in recording in 1995, and thereafter it's at a pretty stable level
# for this analysis, our target window is:

interval <- 1965:2014

# now, let's do some data cleaning

## we can restrict to the hectads that have records of a minimum percentage of the regional species richness in each year
# this is going to take quite a complex loop to construct:
# for every hectad in the BNM_GB dataframe, we need to calculate the observed species richness
# then we need to calculate pairwise distances to all other recorded hectads, pick out the nearest 100, 
# and calculate the regional species richness of these 100
# and finally turn this into a percentage of regional SR that is recorded in the hectad

# start with the list of recorded hectads, re-attach the eastings and northings
BNM_good <- ddply(BNM_rec.hectad.years, .(HECTAD,EASTING,NORTHING), summarise,
                  COUNT = sum(PRESENCE))


# now start to construct the loop:
hec.records <- data.frame(YEAR = numeric(),
                          HECTAD = factor(),
                          EASTING = numeric(),
                          NORTHING = numeric(),
                          hec.SR = numeric(),
                          reg.SR = numeric(),
                          hec.perc = numeric())

# make a list of hectads to loop over

BNM_hectads_all <- levels(droplevels(BNM_GB$HECTAD))

z <- 1

for (x in BNM_hectads_all){
  print(z)
  hec <- BNM_good[which(BNM_good$HECTAD == x), ]    # pick out details of focal hectad
  candidates <- BNM_good[which(BNM_good$HECTAD != x), ]   # pick out details of all others
  
  hec_east <- hec[[1,2]]     # easting of target
  hec_north <- hec[[1,3]]    # northing of target
  
  candidates$east_diff <- candidates$EASTING - hec_east             # longitudinal difference
  candidates$north_diff <- candidates$NORTHING - hec_north          # latitudinal difference
  
  candidates$distance <- sqrt((candidates$east_diff^2) + (candidates$north_diff^2))    # absolute difference
  
  candidates <- candidates[order(candidates$distance),] # sort by distance ascending
  
  closest <- candidates[1:100,] # select out closest 100
  
  # now we want to calculate species richness from the hectad and from the region in each year
  for (n in years.extended){
    year.recs <- BNM_GB[which(BNM_GB$YEAR == n), ]
    
    hec.recs <- year.recs[which(year.recs$HECTAD == x), ] # pull out hectad records
    hec.SR <- nlevels(droplevels(hec.recs$COMMON_NAME))       # calculate species richness
    
    reg.recs <- year.recs[which(year.recs$HECTAD %in% droplevels(closest$HECTAD)), ]  # pull out region records
    reg.recs <- rbind(reg.recs,hec.recs)    # add in hectad records (they're part of the regional richness too!)
    reg.SR <- nlevels(droplevels(reg.recs$COMMON_NAME))
    
    hec.perc <- hec.SR*100/reg.SR
    
    out <- cbind(x,n,hec_east,hec_north,hec.SR,reg.SR,hec.perc)
    hec.records <- rbind(hec.records,out)
  }    
  z <- z+1
}

colnames(hec.records) <- c("HECTAD","YEAR","EASTING","NORTHING","HECTAD.SR","REGION.SR","PERCENT.RECORDED")
summary(hec.records)

# make things that should be numeric, numeric
hec.records$YEAR <- as.numeric(as.character(hec.records$YEAR))
hec.records$EASTING <- as.numeric(as.character(hec.records$EASTING))
hec.records$NORTHING <- as.numeric(as.character(hec.records$NORTHING))
hec.records$HECTAD.SR <- as.numeric(as.character(hec.records$HECTAD.SR))
hec.records$REGION.SR <- as.numeric(as.character(hec.records$REGION.SR))
hec.records$PERCENT.RECORDED <- as.numeric(as.character(hec.records$PERCENT.RECORDED))

summary(hec.records)


# the median is 0% recorded which is not too surprising since we included all the early years

# this data takes a while to generate so let's back it up
write.csv(hec.records, "./data/BC/record_levels//RecordingLevels_forEve.csv", row.names = F)

# then we can later return to this point without needing to run anything above...
hec.records <- read.csv("./data/BC/record_levels/RecordingLevels_forEve.csv")
summary(hec.records)


## because we acknowledge that recording was very light in the early years
# but we're mainly interested in the expansion (say, post-1980)
# let's just focus on those years going forward

hec.records.recent <- hec.records[which(hec.records$YEAR > 1979),]

# now we want to generate a single row for each hectad with the minimum recording level in any year
# we'll use this to assign hectads to different recording levels
hec.records.recent$HECTAD <- as.character(hec.records.recent$HECTAD)


hec.records.overall <- ddply(hec.records.recent, .(HECTAD,EASTING,NORTHING), summarise,
                             MIN.PERC = min(PERCENT.RECORDED, na.rm=T),
                             MEAN.PERC = mean(PERCENT.RECORDED, na.rm=T),
                             MED.PERC = median(PERCENT.RECORDED, na.rm=T),
                             LQ.PERC = quantile(PERCENT.RECORDED, na.rm=T)[2])

summary(hec.records.overall)

# now we have a LOT of hectads which aren't even recorded in every year showing up,
# and a large proportion of hectads fail to reach either the well-recorded or heavily-recorded thresholds when using the 'in every year' (i.e. min) criterion

# using the median criterion (i.e. 'in a median year'), we have about a quarter of cells at least well-recorded


# so let's try plotting the geographic spread of recording density a couple of ways
# first, label each hectad according to its maximum recording level
hec.records.overall$RECORDING <- as.factor(ifelse(hec.records.overall$MED.PERC >= 25, "Heavily recorded",
                                                  ifelse(hec.records.overall$MED.PERC >= 10, "Well recorded",
                                                         ifelse(hec.records.overall$MED.PERC >0, "Recorded",
                                                                "Not recorded"))))

summary(hec.records.overall$RECORDING)

blighty()
points(5+(hec.records.overall$EASTING/1000),5+(hec.records.overall$NORTHING/1000),pch=16,col=c("Black","Grey90","Grey70","Red")[hec.records.overall$RECORDING])
legend("topright", col=c("Grey90","Grey70","Red","Black"),pch=16,
       legend=c("Not recorded","Recorded","Well recorded","Heavily recorded"))

# this figure shows the median yearly recording level across the time period  (currently 1980-2014) -
# so e.g. a square plotted in red is well-recorded (or better) in at least half of years,
# and a square plotted in black is heavily-recorded in at least half of years


# next, do a gradient for percentage

cols <- brewer.pal(11, "RdYlBu")
pal <- colorRampPalette(rev(cols))

hec.records.overall$ORDER <- findInterval(hec.records.overall$MED.PERC, sort(hec.records.overall$MED.PERC))

blighty()

points(5+(hec.records.overall$EASTING/1000),5+(hec.records.overall$NORTHING/1000),pch=16,
       col = pal(nrow(hec.records.overall))[hec.records.overall$ORDER])
legend("topleft", col = pal(2), pch=16,
       legend=c("Less recording","More recording"))


# save these plots
png(paste0("./output/HectadRecording/RecordingLevels_forEve.png"), width = 1600, height = 800, units = "px", bg = "white")
par( mfrow = c(1,2), oma = c(0,0,2,0))

blighty()
points(5+(hec.records.overall$EASTING/1000),5+(hec.records.overall$NORTHING/1000),pch=16,col=c("Black","Grey90","Grey70","Red")[hec.records.overall$RECORDING])
legend("topright", col=c("Grey90","Grey70","Red","Black"),pch=16,cex=2,
       legend=c("Not recorded","Recorded","Well recorded","Heavily recorded"))

blighty()
points(5+(hec.records.overall$EASTING/1000),5+(hec.records.overall$NORTHING/1000),pch=16,
       col = pal(nrow(hec.records.overall))[hec.records.overall$ORDER])
legend("topleft", col = pal(2), pch=16,cex=2,
       legend=c("Less recording","More recording"))

dev.off()       


# so - there is a clear bias towards the South
# and also towards some northern rare-butterfly hotspots (e.g. Morecambe Bay, North York Moors/Teesside, Oban)


### now ready to do some analyses:
# so let's pick out the data to use

# so the main dataframe (BNM_GB) is effectively our dataframe for 'recorded'
# let's generate an additional one for each of 'well recorded' and 'heavily recorded'
hec.records.overall$HECTAD <- as.factor(hec.records.overall$HECTAD)

rec.hecs <- hec.records.overall[which(hec.records.overall$MED.PERC > 0), ]
well.hecs <- hec.records.overall[which(hec.records.overall$MED.PERC >= 10), ]
heavy.hecs <- hec.records.overall[which(hec.records.overall$MED.PERC >= 25), ]


# and select out data from these hectads only 
# (at this stage you could probably just do this step on your Speckled Wood data only)
BNM_GB_rec <- BNM_GB[which(BNM_GB$HECTAD %in% droplevels(rec.hecs$HECTAD)), ]
BNM_GB_well <- BNM_GB[which(BNM_GB$HECTAD %in% droplevels(well.hecs$HECTAD)), ]
BNM_GB_heavy <- BNM_GB[which(BNM_GB$HECTAD %in% droplevels(heavy.hecs$HECTAD)), ]

### Import SW dist data 
PA <- read.csv("./data/BC/BC_PA_all_edinR.csv")
head(PA)

## sepearte dates
PA.new <- tidyr::separate(PA, "Date", c("Day", "Month", "Year"), sep="/")
head(PA.new)

# lets remove data before 1966 ie. pre UKBMS (pre1966)
PA.new$yrnum <- as.numeric(PA.new$Year)
PA.filt <- PA.new[which(PA.new$yrnum>=1965), ]
range(PA.filt$yrnum)
# 1965-2015 = good
head(PA.filt)

PA.filt.1 <- aggregate(PA.filt[, c("x.10km", "y.10km", "Year", "yrnum")], by= list(PA.filt$grid10), FUN=min)
head(PA.filt.1)
colnames(PA.filt.1)[1] <- "grid.10"


## Combine year colonised info with hectad recording amount
PA_overall_rec <- merge(PA.filt.1, hec.records.overall, by.x="grid.10", by.y="HECTAD")
head(PA_overall_rec)



### transform data to SPDFs ####
ggmap::register_google(key = "AIzaSyAN6Lao2HxKfw9yj7u4Yha_o4RJ-6WCqwE") # insert your hey here
ggmap::ggmap_show_api_key()

# download basemap
base_map <- get_googlemap(center = c(lon= -2.325278, lat=54.6000000), zoom = 5, style = 'element:labels|visibility:off',color= "bw")

# Britan CRS
Brit.Ng <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +a=6377563 +b=6356256.161 +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894 +units=m +no_defs")
# World Geographic System (lat/long) - mapping
World <- CRS("+proj=longlat +ellps=GRS80 +no_defs") 

# # # # #
## SPDF for BC PA data
# create spatial datafram
PA_overall_rec.spat<-SpatialPointsDataFrame(coords = PA_overall_rec[,c("x.10km", "y.10km")], data = PA_overall_rec, proj4string = Brit.Ng)
#plot(BC.filt_spat)

## put in geographical context
# transform BC data to "world" one
PA_overall_rec.spat_world <- spTransform(PA_overall_rec.spat, World)
# grab world coordinates
world_coords <- coordinates(PA_overall_rec.spat_world)
# append WGS84 coords onto original data
PA_overall_rec$X.new.10 <- world_coords[,1]
PA_overall_rec$Y.new.10 <- world_coords[,2]

### Get sample data ###
samps <- read.csv("./data/logs/Morphometric_sample_log.csv")
head(samps)

## SPDF for sample data
# create spatial datafram
samps_spat<-SpatialPointsDataFrame(coords = samps[,c("X", "Y")], data = samps, proj4string = Brit.Ng)
plot(samps_spat)

## put in geographical context
# transform BC data to "world" one
samps_spat_world <- spTransform(samps_spat, World)
# grab world coordinates
world_coords <- coordinates(samps_spat_world)
# append WGS84 coords onto original data
samps$X.new <- world_coords[,1]
samps$Y.new <- world_coords[,2]

## Plot 1 - level of recording ####

shapes <- c(15,10,1,17)
plot_test <- ggmap(base_map, extent = "panel")+
  geom_point(aes(x=X.new.10, y=Y.new.10, col= yrnum, shape=as.factor(RECORDING)), data = PA_overall_rec, alpha = .8, size =2) +
  geom_point(aes(x=X.new, y=Y.new), data = samps, alpha = 1, size = 4, stroke=2, col= "black", shape=4) +
  scale_x_continuous(limits = c(-7, 3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(49.5, 59), expand = c(0, 0))+
  scale_color_viridis_c(option = "D")+
  scale_shape_manual(values=shapes)+
  labs(color="First \n record", shape="Level of recording")+
  theme_bw()
plot_test

# to export (increase size of points)
plot_test.export <- ggmap(base_map, extent = "panel")+
  geom_point(aes(x=X.new.10, y=Y.new.10, col= yrnum, shape=as.factor(RECORDING)), data = PA_overall_rec, alpha = .8, size =5, stroke=2) +
  geom_point(aes(x=X.new, y=Y.new), data = samps, alpha = 1, size = 10, stroke=5, col= "black", shape=4) +
  scale_x_continuous(limits = c(-7, 3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(49.5, 59), expand = c(0, 0))+
  scale_color_viridis_c(option = "D")+
  scale_shape_manual(values=shapes)+
  labs(color="First \n record", shape="Level of recording")+
  theme_bw()

tiff(filename="./output/HectadRecording/Sample_locs_recLevel.tiff",
     type="cairo",
     units="in",
     width=50,
     pointsize = 35,
     height=35,
     res=96)
plot_test.export + theme(axis.title = element_text(size =40), axis.text = element_text(size = 35), legend.title= element_text(hjust = 0.5, face="bold", size=40), legend.text = element_text(size=35), strip.text.x = element_text(size=40,face="bold"))
dev.off()





### Plot 2 - Colonistaion certainty #####


## For each square - find the 3rd year that Percent.recorded is over 10%.

# Lets filter to only records with 10 percent recorded or more
wellHeav_rec <- hec.records[hec.records$PERCENT.RECORDED>=10,]
wellHeav_rec <- na.omit(wellHeav_rec)

# Now for each hectad order by years and take the 3rd year
rec_10plus_3yr <- by(wellHeav_rec, wellHeav_rec[,"HECTAD"], function (x) if(length(x[,"YEAR"])<3){print(2020)}else sort(x[,"YEAR"], decreasing=FALSE)[3]) # this takes the 3rd year that the square was either well or heavily recorded - come up with NA for those with less than 3 years recorded... does this matter? These may be uncertain anyway. TO force it to be uncertain and not NA - make the year it becomes well recorded not until 2015 - this will make it uncertain in the next steps as no data goes to 2020

# let manually check for some this is doing what we expect
by(wellHeav_rec, wellHeav_rec[,"HECTAD"], function (x) sort(x[,"YEAR"], decreasing=FALSE))


# make df
rec_10plus_3yr <- as.data.frame(cbind(rec_10plus_3yr))
rec_10plus_3yr <- tibble::rownames_to_column(rec_10plus_3yr, "grid.10")
head(rec_10plus_3yr)

# merge with PA data
PA.filt.new <- merge(PA.filt.1, rec_10plus_3yr, by="grid.10", all.x=TRUE)
head(PA.filt.new)

# now create new column for level of certainty of colonisation
PA.filt.new$col.cert <- ifelse(PA.filt.new$Year < 1975, "Core", ifelse(PA.filt.new$rec_10plus_3yr < PA.filt.new$Year, "High", "Low"))

PA.filt.new$col.cert2<- ifelse(PA.filt.new$rec_10plus_3yr < PA.filt.new$Year, "Certain", "Uncertain")

head(PA.filt.new)


## SPDF for BC PA data
# create spatial datafram
PA_rec_cert.spat<-SpatialPointsDataFrame(coords = PA.filt.new[,c("x.10km", "y.10km")], data = PA.filt.new, proj4string = Brit.Ng)
#plot(BC.filt_spat)

## put in geographical context
# transform BC data to "world" one
PA_rec_cert.spat_world <- spTransform(PA_rec_cert.spat, World)
# grab world coordinates
world_coords <- coordinates(PA_rec_cert.spat_world)
# append WGS84 coords onto original data
PA.filt.new$X.new.10 <- world_coords[,1]
PA.filt.new$Y.new.10 <- world_coords[,2]

# replace any NA in PA.filt.new with uncertain - these had % recorded under 10 in all years
PA.filt.new["col.cert"][is.na(PA.filt.new["col.cert"])] <- "Low"
PA.filt.new["col.cert2"][is.na(PA.filt.new["col.cert2"])] <- "Uncertain"

PA.filt.new$col.cert <- as.factor(PA.filt.new$col.cert)
levels(PA.filt.new$col.cert)

# change order
PA.filt.new$col.cert <- factor(PA.filt.new$col.cert, levels= c("Core", "High", "Low"))


shapes <- c(15,19,1)
plot_test <- ggmap(base_map, extent = "panel")+
  geom_point(aes(x=X.new.10, y=Y.new.10, col= yrnum, shape=as.factor(col.cert)), data = PA.filt.new, alpha = .8, size =2) +
  geom_point(aes(x=X.new, y=Y.new), data = samps, alpha = 1, size = 4, stroke=2, col= "black", shape=4) +
  scale_x_continuous(limits = c(-7, 3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(49.5, 59), expand = c(0, 0))+
  scale_color_viridis_c(option = "D")+
  scale_shape_manual(values=shapes)+
  labs(color="First \n record", shape="Certainty of colonisation")+
  theme_bw()
plot_test

plot_test.export <- ggmap(base_map, extent = "panel")+
  geom_point(aes(x=X.new.10, y=Y.new.10, col= yrnum, shape=as.factor(col.cert)), data = PA.filt.new, alpha = .8, size =5, stroke=2) +
  geom_point(aes(x=X.new, y=Y.new), data = samps, alpha = 1, size = 10, stroke=5, col= "black", shape=4) +
  scale_x_continuous(limits = c(-7, 3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(49.5, 59), expand = c(0, 0))+
  scale_color_viridis_c(option = "D")+
  scale_shape_manual(values=shapes)+
  labs(color="First \n record", shape="Colonisation \n confidence")+
  theme_bw()

tiff(filename="./output/HectadRecording/Sample_locs_colCert.tiff",
     type="cairo",
     units="in",
     width=50,
     pointsize = 35,
     height=35,
     res=96)
plot_test.export + theme(axis.title = element_text(size =40), axis.text = element_text(size = 35), legend.title= element_text(hjust = 0.5, face="bold", size=40), legend.text = element_text(size=35), legend.margin = margin(t=3, unit="cm"), legend.box.spacing = unit(3,"cm"), strip.text.x = element_text(size=40,face="bold"))
dev.off()
