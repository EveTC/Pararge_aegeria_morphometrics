## PA sample site plot per year 10km colonised (first recorded)
## written by Eve Taylor-Cox

## Distribution data can not be provided but is availbale directly from Butterfly Conservation upon request

## Libs ####
# install packages (if necessary)
p <- c("raster","rgdal", "ggmap", "rasterVis","rstudioapi","plyr","gridExtra","svglite","scales","grid","gridExtra","svglite", "archdata", "sp", "leaflet", "rgeos", "spatstat", "rworldmap", "RColorBrewer","tidyr")

new.packages <- p[!(p %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(p, require, character.only = TRUE)

## BC data until 2017 ####
## Read in BC data 
# read in data
BC_15 <- read.csv("./data/BC/Speckled Wood data edited.csv")
head(BC_15)

# data from 2016-2017
BC_1617 <- read.csv("./data/BC/GB Speckled Wood records 2016-17.csv")
head(BC_1617)

# remove columns that are not needed
BC_1617 <- BC_1617[,-c(1,8,10,12:21)]
head(BC_1617)

# combine data - rbind
BC <- rbind(BC_15, BC_1617)
dim(BC)
head(BC)


## Add 10km grid ref in - if not already done ####

## Will use grid.10km as grouping for abundance info 

# extract the two letters from grid ref provided and put into new column
BC$grid10<-substring(BC$GridRef,1,3)
head(BC)
class(BC$GridRef)

# extract number portion of gridref
BC$nums <- gsub("[^[:digit:]]", "", BC$GridRef)
head(BC)

# split the the sting into number
BC$split <- strsplit(BC$nums, "")
head(BC)

# if two numbers take second
class(BC$nums)
two <- subset(BC, subset= nchar(BC$nums)=="2")
head(two)
# get 2 element
two$east<-lapply(two$split, '[[', 2)
head(two)

# if 4 take thrid number
quad <- BC[nchar(BC$nums)==4,]
quad <- subset(BC, subset= nchar(BC$nums)=="4")
head(quad)
# get third element
quad$east<-lapply(quad$split, '[[', 3)
head(quad)

# if 6 take 4
six <- BC[nchar(BC$nums)==6,]
six <- subset(BC, subset= nchar(BC$nums)=="6")
head(six)
# get 4 element
six$east<-lapply(six$split, '[[', 4)
head(six)

# if 8 take 5
eight <- BC[nchar(BC$nums)==8,]
eight <- subset(BC, subset= nchar(BC$nums)=="8")
head(eight)
# get 5 element
eight$east<-lapply(eight$split, '[[', 5)
head(eight)

#if 10 take 6
ten <- BC[nchar(BC$nums)==10,]
ten <- subset(BC, subset= nchar(BC$nums)=="10")
head(ten)
# get 6 element
ten$east<-lapply(ten$split, '[[', 6)
head(ten)

# rbing above - should have same number of obvs as original sw
BC.new<-rbind(two,quad,six,eight,ten)
dim(BC)
dim(BC.new)

# join grid10 and east
class(BC.new$grid10) #character
class(BC.new$east) # list needs to be a character
BC.new$grid10 <- paste(BC.new$grid10, as.character(BC.new$east), sep="")
head(BC.new)
# yay!

# remove columns we dont need
BC.new <- BC.new[,-c(10:12)]
head(BC.new)


## write out csv
#write.csv(BC.new, "./data/BC/BC_PA_all_edinR.csv")

## Batch convert tool online used to get 10km grid as X and Y coords

## Read in new csv with x.10km and y.10km for plotting #####
BC.ed <- read.csv("./data/BC/BC_PA_all_edinR.csv")
head(BC.ed)

## sepearte dates
BC.ed.new <- tidyr::separate(BC.ed, "Date", c("Day", "Month", "Year"), sep="/")
head(BC.ed.new)

# lets remove data before 1966 ie. pre UKBMS (pre1966)
BC.ed.new$yrnum <- as.numeric(BC.ed.new$Year)
BC.filt <- BC.ed.new[which(BC.ed.new$yrnum>=1965), ]
range(BC.filt$yrnum)
# 1965-2015 = good
head(BC.filt)

BC.filt.1 <- aggregate(BC.filt[, c("x.10km", "y.10km", "Year", "yrnum")], by= list(BC.filt$grid10), FUN=min)
head(BC.filt.1)
colnames(BC.filt.1)[1] <- "grid.10"


## Input morphometic sample log with locations ####
samps <- read.csv("./data/logs/Morphometric_sample_log.csv")
head(samps)

## Make SPDFs ####
ggmap::register_google(key = "AIzaSyAN6Lao2HxKfw9yj7u4Yha_o4RJ-6WCqwE") # insert your hey here
ggmap::ggmap_show_api_key()

# download basemap
base_map <- get_googlemap(center = c(lon= -2.325278, lat=54.6000000), zoom = 5, style = 'element:labels|visibility:off',color= "bw")

# Britan CRS
Brit.Ng <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +a=6377563 +b=6356256.161 +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894 +units=m +no_defs")
# World Geographic System (lat/long) - mapping
World <- CRS("+proj=longlat +ellps=GRS80 +no_defs") 

# # # # #
## SPDF for BC data
# create spatial datafram
BC.filt.1_spat<-SpatialPointsDataFrame(coords = BC.filt.1[,c("x.10km", "y.10km")], data = BC.filt.1, proj4string = Brit.Ng)
#plot(BC.filt_spat)

## put in geographical context
# transform BC data to "world" one
BC.filt.1_spat_world <- spTransform(BC.filt.1_spat, World)
# grab world coordinates
world_coords <- coordinates(BC.filt.1_spat_world)
# append WGS84 coords onto original data
BC.filt.1$X.new.10 <- world_coords[,1]
BC.filt.1$Y.new.10 <- world_coords[,2]

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

## PLOT ####

samp_plot_test <- ggmap(base_map, extent = "panel")+
  geom_point(aes(x=X.new.10, y=Y.new.10, col= yrnum), data = BC.filt.1, alpha = .8, size =5, stroke=4) +
  geom_point(aes(x=X.new, y=Y.new), data = samps, alpha = 1, size = 10, stroke=5, col= "black", shape=4) +
  scale_x_continuous(limits = c(-7, 3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(49.5, 59), expand = c(0, 0))+
  scale_color_viridis_c(option = "D")+
  labs(color="First \n record")+
  theme_bw()

tiff(filename="./output/Manuscript_figs/Figure_1.0_Sample_locs.tiff",
     type="cairo",
     units="in",
     width=50,
     pointsize = 35,
     height=35,
     res=96)
samp_plot_test + theme(axis.title = element_text(size =40), axis.text = element_text(size = 35), legend.title= element_text(hjust = 0.5, face="bold", size=40), legend.title.align=0.5,legend.text = element_text(size=35), strip.text.x = element_text(size=40,face="bold"))
dev.off()
