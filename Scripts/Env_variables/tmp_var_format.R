### Environmental variables - Morphometric study

### Aim of script: 
# 1) Get 10yr mean annual temp for each individual. If collected in 2017 10yr mat from 2006-2016 and so on.
# 2) Produce emergence plots for year samples collected in each 10km grid. Calculate how many months previous to the 2nd peak samples were collected - into variable. Work out which months they would have developed in? Use these as another exp var in model to look at developmental changes.

### N.B. I can not directly provide the environmental data nor the Butterfly Conservation data. 
# The env. data is available through the CEDA archives and the Met office (for 2018 temp data).
# Pararge aegeria distribution data is available from Butterfly Conservation on request.


# Env data - save/load
#save.image(file="tmp_format.RData")
load("tmp_format.RData")


### Load libraries ####
library(dplyr)
library(mgcv)
library(stringr)
library(data.table)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)

### Read in env. data - 2006 to 2018 ####
tmp_18 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_201801-201812.csv")
tmp_17 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_201701-201712.csv")
tmp_16 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_201601-201612.csv")
tmp_15 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_201501-201512.csv")
tmp_14 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_201401-201412.csv")
tmp_13 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_201301-201312.csv")
tmp_12 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_201201-201212.csv")
tmp_11 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_201101-201112.csv")
tmp_10 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_201001-201012.csv")
tmp_09 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_200901-200912.csv")
tmp_08 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_200801-200812.csv")
tmp_07 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_200701-200712.csv")
tmp_06 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_200601-200612.csv")
tmp_05 <- read.csv("./Data/UKCP18/Mean_air_temperature_(tas)/tas_hadukgrid_uk_1km_mon_200501-200512.csv")


### Read in log info ####
samp_log <- read.csv("./Data/logs/Morphometric_sample_log.csv")
head(samp_log)
dim(samp_log)

##### 1. 10yr mat for selection question #####
### Format env. vars to 06-16 and 07-17 #### 
## set lists for 07-17 and 06-16
tmp_yrs18_08 <- list(tmp_18=tmp_18,tmp_17=tmp_17,tmp_16=tmp_16, tmp_15=tmp_15, tmp_14=tmp_14, tmp_13=tmp_13, tmp_12=tmp_12, tmp_11=tmp_11, tmp_10=tmp_10, tmp_09=tmp_09, tmp_08=tmp_08)

tmp_yrs17_07 <- list(tmp_17=tmp_17,tmp_16=tmp_16, tmp_15=tmp_15, tmp_14=tmp_14, tmp_13=tmp_13, tmp_12=tmp_12, tmp_11=tmp_11, tmp_10=tmp_10, tmp_09=tmp_09, tmp_08=tmp_08, tmp_07=tmp_07)

tmp_yrs16_06 <- list(tmp_16=tmp_16, tmp_15=tmp_15, tmp_14=tmp_14, tmp_13=tmp_13, tmp_12=tmp_12, tmp_11=tmp_11, tmp_10=tmp_10, tmp_09=tmp_09, tmp_08=tmp_08, tmp_07=tmp_07, tmp_06=tmp_06)

tmp_yrs15_05 <- list(tmp_15=tmp_15, tmp_14=tmp_14, tmp_13=tmp_13, tmp_12=tmp_12, tmp_11=tmp_11, tmp_10=tmp_10, tmp_09=tmp_09, tmp_08=tmp_08, tmp_07=tmp_07, tmp_06=tmp_06, tmp_05=tmp_05)

lapply(tmp_yrs17_07, dim)
lapply(tmp_yrs16_06, dim)

## Aggregate tmp dfs on lon&lat, take mean of combined info

# rbind dataframe
rbind_0818 <- do.call(rbind, tmp_yrs18_08)
rbind_0717 <- do.call(rbind,tmp_yrs17_07)
rbind_0616 <- do.call(rbind,tmp_yrs16_06)
rbind_0515 <- do.call(rbind, tmp_yrs15_05)

dim(rbind_0818)
dim(rbind_0717)
dim(rbind_0616)
dim(rbind_0515)

# aggregate on lon and lat taking the mean 0717
tmp_mean_10yr_08_18 <- aggregate(rbind_0818, by=list(rbind_0818$lon, lat=rbind_0818$lat), FUN="mean")
dim(tmp_mean_10yr_08_18)
# N.B some will NOT include 2007 - need to work out which grids do not include 2007
head(tmp_mean_10yr_08_18)

# aggregate on lon and lat taking the mean 0717
tmp_mean_10yr_07_17 <- aggregate(rbind_0717, by=list(rbind_0717$lon, lat=rbind_0717$lat), FUN="mean")
dim(tmp_mean_10yr_07_17)
# N.B some will NOT include 2007 - need to work out which grids do not include 2007
head(tmp_mean_10yr_07_17)

# aggregate on lon and lat taking the mean 0616
tmp_mean_10yr_06_16 <- aggregate(rbind_0616, by=list(rbind_0616$lon, lat=rbind_0616$lat), FUN="mean")
dim(tmp_mean_10yr_06_16)
# N.B some will NOT include 2007 - need to work out which grids do not include 2007
head(tmp_mean_10yr_06_16)

# aggregate on lon and lat taking the mean 0516
tmp_mean_10yr_05_15 <- aggregate(rbind_0515, by=list(rbind_0515$lon, lat=rbind_0515$lat), FUN="mean")
dim(tmp_mean_10yr_05_15)
# N.B some will NOT include 2007 - need to work out which grids do not include 2007
head(tmp_mean_10yr_05_15)


## Remove first 2 cols, mat, mwta and mtco
tmp_mean_10yr_08_18 <- tmp_mean_10yr_08_18[,-c(1:2,17:19)]
head(tmp_mean_10yr_08_18)
dim(tmp_mean_10yr_08_18)

tmp_mean_10yr_07_17 <- tmp_mean_10yr_07_17[,-c(1:2,17:19)]
head(tmp_mean_10yr_07_17)
dim(tmp_mean_10yr_07_17)

tmp_mean_10yr_06_16 <- tmp_mean_10yr_06_16[,-c(1:2,17:19)]
head(tmp_mean_10yr_06_16)
dim(tmp_mean_10yr_06_16)

tmp_mean_10yr_05_15 <- tmp_mean_10yr_05_15[,-c(1:2,17:19)]
head(tmp_mean_10yr_05_15)
dim(tmp_mean_10yr_05_15)

## Recalculate mat, mtwa, mtco
tmp_mean_10yr_08_18$mtwa <- apply(tmp_mean_10yr_08_18[3:14],1,max) # mtwa
tmp_mean_10yr_08_18$mtco <- apply(tmp_mean_10yr_08_18[3:14],1,min) # mtco
tmp_mean_10yr_08_18$mat <- apply(tmp_mean_10yr_08_18[3:14],1,mean) # mat

head(tmp_mean_10yr_08_18)

tmp_mean_10yr_07_17$mtwa <- apply(tmp_mean_10yr_07_17[3:14],1,max) # mtwa
tmp_mean_10yr_07_17$mtco <- apply(tmp_mean_10yr_07_17[3:14],1,min) # mtco
tmp_mean_10yr_07_17$mat <- apply(tmp_mean_10yr_07_17[3:14],1,mean) # mat

head(tmp_mean_10yr_07_17)

tmp_mean_10yr_06_16$mtwa <- apply(tmp_mean_10yr_06_16[3:14],1,max) # mtwa
tmp_mean_10yr_06_16$mtco <- apply(tmp_mean_10yr_06_16[3:14],1,min) # mtco
tmp_mean_10yr_06_16$mat <- apply(tmp_mean_10yr_06_16[3:14],1,mean) # mat

head(tmp_mean_10yr_06_16)

tmp_mean_10yr_05_15$mtwa <- apply(tmp_mean_10yr_05_15[3:14],1,max) # mtwa
tmp_mean_10yr_05_15$mtco <- apply(tmp_mean_10yr_05_15[3:14],1,min) # mtco
tmp_mean_10yr_05_15$mat <- apply(tmp_mean_10yr_05_15[3:14],1,mean) # mat

head(tmp_mean_10yr_05_15)

### Convert co-ordinates in sample log and env vars to bottom left corner ####
# - ensure correct matching of env to location of sites

source("./scripts/functions/mroundOG_fun.R")

# sample log
samp_log$met.x <- mroundOG(samp_log$X, 1000)
samp_log$met.y <- mroundOG(samp_log$Y, 1000)
head(samp_log)
dim(samp_log)

tmp_mean_10yr_08_18$met.x <- mroundOG(tmp_mean_10yr_08_18$lon, 1000)
tmp_mean_10yr_08_18$met.y <- mroundOG(tmp_mean_10yr_08_18$lat, 1000)
head(tmp_mean_10yr_08_18)


tmp_mean_10yr_07_17$met.x <- mroundOG(tmp_mean_10yr_07_17$lon, 1000)
tmp_mean_10yr_07_17$met.y <- mroundOG(tmp_mean_10yr_07_17$lat, 1000)
head(tmp_mean_10yr_07_17)


tmp_mean_10yr_06_16$met.x <- mroundOG(tmp_mean_10yr_06_16$lon, 1000)
tmp_mean_10yr_06_16$met.y <- mroundOG(tmp_mean_10yr_06_16$lat, 1000)
head(tmp_mean_10yr_06_16)


tmp_mean_10yr_05_15$met.x <- mroundOG(tmp_mean_10yr_05_15$lon, 1000)
tmp_mean_10yr_05_15$met.y <- mroundOG(tmp_mean_10yr_05_15$lat, 1000)
head(tmp_mean_10yr_05_15)


## Some site grid squares are in the sea - select next nearest with data
# Garlieston has not merged as no data for grid square 248500,546500 as corner is in the sea!
# nearest grid sqaure with data = 248000,547000
# RF 171500	763500
# nearest 172000 764000 - another valley not mountainy

# change met grid square to these
samp_log <- transform(samp_log, met.y = ifelse(Site.ID=="Garl", "547000", met.y))
samp_log <- transform(samp_log, met.y = ifelse(Site.ID=="RF", "764000", met.y))
head(samp_log)

# merge using met.x and met.y 
### If collected in 2017 use 06_16 tmp vars, 2018 use 07_17 etc. ####

# extract mat, met.x and met.y from tmp dfs
dim(tmp_mean_10yr_08_18)
mat_0818 <- tmp_mean_10yr_08_18[,c(17:19)]
head(mat_0818)

dim(tmp_mean_10yr_07_17)
mat_0717 <- tmp_mean_10yr_07_17[,c(17:19)]
head(mat_0717)

dim(tmp_mean_10yr_06_16)
mat_0616 <- tmp_mean_10yr_06_16[,c(17:19)]
head(mat_0616)

dim(tmp_mean_10yr_05_15)
mat_0515 <- tmp_mean_10yr_05_15[,c(17:19)]
head(mat_0515)


# check what years collected are in samp_log
unique(samp_log$Yr.col)
# some samples collected in 2016 

length(samp_log[samp_log$Yr.col=="2016",])

# write if function to merge with correct 10yr mat depending on year collected
samp_log.new <- map2_dfr(list(mat_0515, mat_0616, mat_0717, mat_0818),
                         2016:2019,
                         function(x,y){merge(samp_log %>% filter(Yr.col == y), x, by=c("met.x", "met.y"))})
# lets remove some columns we dont need to make it more manageable
samp_log.new <- samp_log.new[,c(1:2,4:21,28:40)]
head(samp_log.new)
dim(samp_log.new)
colnames(samp_log.new)[33] <- "mat.10yr"



##### 2. Developmental temp format #####
# Developmental tmp for 2019 samples will have to use 2018 - but should be removed from analysis in future
### Emergence plots ####

## For emergence plots I need lat/lng, calender date and abundance
# We can not get an emergence plot for 2018 as we do not have BC data fro 2018.
# Therefore investigate the average emergence plot over 10years(??) 2007-2017 and use these to suggest months we should take for developmental question.


## Read in BC data 
# read in data
BC_15 <- read.csv("./Data/BC/Speckled Wood data edited.csv")
head(BC_15)

# data from 2016-2017
BC_1617 <- read.csv("./Data/BC/GB Speckled Wood records 2016-17.csv")
head(BC_1617)

# remove columns that are not needed
BC_1617 <- BC_1617[,-c(1,8,10,12:21)]
head(BC_1617)

# combine data - rbind
BC <- rbind(BC_15, BC_1617)
dim(BC)
head(BC)

## Subset BC data to only include years 2007-2017

# split date into day,month,year
setDT(BC)[,paste0(c("Day","Mnth","Yr")) := tstrsplit(BC$Date, "/")]
head(BC)

BC_0717 <- subset(BC, Yr %in% c("2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017"))
dim(BC_0717)
head(BC_0717)

## Add 10km grid ref in ####

## Will use grid.10km as grouping for abundance info 

# extract the two letters from grid ref provided and put into new column
BC_0717$grid10<-substring(BC_0717$GridRef,1,3)
head(BC_0717)
class(BC_0717$GridRef)

# extract number portion of gridref
BC_0717$nums <- gsub("[^[:digit:]]", "", BC_0717$GridRef)
head(BC_0717)

# split the the sting into number
BC_0717$split <- strsplit(BC_0717$nums, "")
head(BC_0717)

# if two numbers take second
class(BC_0717$nums)
two <- subset(BC_0717, subset= nchar(BC_0717$nums)=="2")
head(two)
# get 2 element
two$east<-lapply(two$split, '[[', 2)
head(two)

# if 4 take thrid number
quad <- BC_0717[nchar(BC_0717$nums)==4,]
quad <- subset(BC_0717, subset= nchar(BC_0717$nums)=="4")
head(quad)
# get third element
quad$east<-lapply(quad$split, '[[', 3)
head(quad)

# if 6 take 4
six <- BC_0717[nchar(BC_0717$nums)==6,]
six <- subset(BC_0717, subset= nchar(BC_0717$nums)=="6")
head(six)
# get 4 element
six$east<-lapply(six$split, '[[', 4)
head(six)

# if 8 take 5
eight <- BC_0717[nchar(BC_0717$nums)==8,]
eight <- subset(BC_0717, subset= nchar(BC_0717$nums)=="8")
head(eight)
# get 5 element
eight$east<-lapply(eight$split, '[[', 5)
head(eight)

#if 10 take 6
ten <- BC_0717[nchar(BC_0717$nums)==10,]
ten <- subset(BC_0717, subset= nchar(BC_0717$nums)=="10")
head(ten)
# get 6 element
ten$east<-lapply(ten$split, '[[', 6)
head(ten)

# rbing above - should have same number of obvs as original sw
BC_0717.new<-rbind(two,quad,six,eight,ten)
dim(BC_0717)
dim(BC_0717.new)

# join grid10 and east
class(BC_0717.new$grid10) #character
class(BC_0717.new$east) # list needs to be a character
BC_0717.new$grid10 <- paste(BC_0717.new$grid10, as.character(BC_0717.new$east), sep="")
head(BC_0717.new)
# yay!

# remove columns we dont need
BC_0717.new <- BC_0717.new[,-c(13:15)]
head(BC_0717.new)

## Change Code for number of values into min and max individuals ####
unique(BC_0717.new$Adults)
BC_0717.new$Adults <- as.character(BC_0717.new$Adults)


BC_0717.new = within(BC_0717.new, {
  min.ind = ifelse(Adults=="P", "1", 
                   ifelse(Adults=="A","1",
                          ifelse(Adults=="B","2",
                                 ifelse(Adults=="C","10",
                                        ifelse(Adults=="D", "30",
                                               ifelse(Adults=="E", "100", as.character(BC_0717.new$Adults)))))))
  max.ind = ifelse(Adults=="P", "1", 
                   ifelse(Adults=="A","1",
                          ifelse(Adults=="B","9",
                                 ifelse(Adults=="C","29",
                                        ifelse(Adults=="D", "99",
                                               ifelse(Adults=="E", "100", as.character(BC_0717.new$Adults)))))))
})
# It worked! - Now I have two abundance columns - one "min" number of indivudals and 2nd "max" number of individuals
head(BC_0717.new)
dim(BC_0717.new)



## subset BC data to only info for the grids we are interested in ####

# first on met.x
BC_sub <- BC_0717.new %>%
  filter(grid10 %in% samp_log.new$Grid.10km)
length(unique(BC_sub$grid10))
length(unique(samp_log.new$Grid.10km))

## Code from Callum macgregor - emergence plots 2007 to 2017 data ####
# wrangle the dates

BC_sub$EveDate <- as.Date(BC_sub$Date, format = "%d/%m/%Y")

# set up a reference date
BC_sub$YEAR <- as.numeric(as.character(BC_sub$Yr))
BC_sub$refdate <- dmy(paste0("31-12-",(BC_sub$YEAR-1)))
head(BC_sub)

BC_sub$julianday <- as.numeric(difftime(BC_sub$EveDate, BC_sub$refdate))
head(BC_sub)

# Format data to allow for emergence plotting ####

# make min and max.ind numeric
BC_sub$min.ind <- as.numeric(BC_sub$min.ind)
BC_sub$max.ind <- as.numeric(BC_sub$max.ind)

# remove any data that have NA for julian day
BC_sub <- BC_sub[BC_sub$julianday!="NA",]

# look to see how many rows there are per grid 10km
nrow.per.grid <-data.frame(table(BC_sub$grid10))
nrow.per.grid
# some have a low number of data points so GAM will not be able to run

# filter above to remove those less than 10 data points
grids.chosen <- nrow.per.grid[nrow.per.grid$Freq>10,]
dim(grids.chosen)
# 38 grids have more than 10 data points

# remove grids that have less than 8 points
BC_sub.new <- merge(BC_sub, grids.chosen, by.x="grid10", by.y="Var1")
head(BC_sub.new)
length(unique(BC_sub.new$grid10))
# correct

## Function to find peaks ####
source("./Scripts/Functions/find_peaks_fun.R")
# we will use this code to identify the 2nd generation peak in each instance
# we will also output the number of peaks found - this can be used to see how size changes with voltinism - this is added to the chosen method below 


# CHOSEN METHOD - Plots - make weights=max.ind - may cause line to be 'influenced' more by points with higher count #####
# create empty dataframe to read result into in loop
gen.2.maxInd <- data.frame(matrix(nrow=1,ncol=2))
colnames(gen.2.maxInd) <- c("day", "volt_num")
head(gen.2.maxInd)


# now construct a loop to fit and plot out the GAM 
for (i in unique(BC_sub.new$grid10)){
  BC_grid <- BC_sub.new[BC_sub.new$grid10==i,]
  # set weights = 5 depending on number of data for each grid
  wghts <- BC_grid$max.ind
  # fit the gam: k=15, weights=max.ind
  pheno <- gam(max.ind ~ s(julianday, k=10, bs='cr'), data=BC_grid, family = "poisson", weights = wghts, method = "REML", gamma = 1)
  # plot the gam
  pred <- data.frame(day = c(100:300))
  pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))
  max.ind <- ggplot(pred)+
    geom_point(data = BC_grid[which(BC_grid$max.ind > 0), ], 
               aes(x = julianday, y = max.ind), 
               colour = "grey60")+
    scale_y_continuous(limits = c(0,max(BC_grid$max.ind)))+
    geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
    theme_bw()+
    ggtitle(paste("Emergence plot for", i, "k=10, wghts=max.ind" ,sep=" "))+
    xlab("Julian day") + ylab("Abundance") 
  # print out plot
  print(max.ind)
  # save plot
  ggsave(paste0("./output/Emergence_plots/",i,".png"), plot = max.ind , width = 100, height = 70, units = "mm", limitsize = F)
  # get highest peak to df
  # get day of 2nd gen peak to df
  peaks <- find_peaks(pred$pred, m=8)
  if (length(peaks)==1){
    gen.2.maxInd[i,1] <- pred[peaks[1],]} else if (length(peaks)==2){ 
      gen.2.maxInd[i,1] <- pred[peaks[2],]} else if (length(peaks)==3){
        gen.2.maxInd[i,1] <- pred[peaks[2],]} else if (length(peaks)==4){
          gen.2.maxInd[i,1] <- pred[peaks[3],]} else gen.2.maxInd[i,1]<- length(peaks)
  # get number of peaks (generations) in gam plot into second column
  peaks2 <- find_peaks(pred$pred, m=30)
  gen.2.maxInd[i,2]<- length(peaks2)
}


# this has done what was expected and has exaggerated the peaks but has performed less well for some grids with low data - as only high points have more weight so can pull curve higher ignoring lower points as they have less weight.

head(gen.2.maxInd)

## Confirm 2nd generation identified ####

# Using max.ind plots - make sure the peak identified is the 2nd generation
head(gen.2.maxInd)
View(gen.2.maxInd)

# some peaks have been identified as incorrect - those with potentiall 4 peaks - I took the 3rd but in some cases the 2nd was required

# Replace incorrect assigned peaks ####
# SD38; SD57; SJ43; TM04


SD38 <- subset(BC_sub.new, grid10=="SD38")
pheno <- gam(max.ind ~ s(julianday, k=10, bs='cr'), data=SD38, family = "poisson", weights = SD38$max.ind, method = "REML", gamma = 1)
pred <- data.frame(day = c(100:300))
pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))

max.ind <- ggplot(pred)+
  geom_point(data = SD38[which(SD38$max.ind > 0), ], 
             aes(x = julianday, y = max.ind), 
             colour = "grey60")+
  scale_y_continuous(limits = c(0,max(SD38$max.ind)))+
  geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
  theme_bw()+
  ggtitle(paste("Emergence plot for SD38 k=10, wghts= manual set" ,sep=" "))+
  xlab("Julian day") + ylab("Abundance") 
# print out plot
max.ind

peaks <- find_peaks(pred$pred, m=8)
peaks
SD38.peak <- pred[peaks[1],]
SD38.peak
SD38.volt <- length(peaks)

SD57 <- subset(BC_sub.new, grid10=="SD57")
pheno <- gam(max.ind ~ s(julianday, k=10, bs='cr'), data=SD57, family = "poisson", weights = SD57$max.ind, method = "REML", gamma = 1)
pred <- data.frame(day = c(100:300))
pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))

max.ind <- ggplot(pred)+
  geom_point(data = SD57[which(SD57$max.ind > 0), ], 
             aes(x = julianday, y = max.ind), 
             colour = "grey60")+
  scale_y_continuous(limits = c(0,max(SD57$max.ind)))+
  geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
  theme_bw()+
  ggtitle(paste("Emergence plot for SD57 k=10, wghts= max ind set" ,sep=" "))+
  xlab("Julian day") + ylab("Abundance") 
# print out plot
max.ind

peaks <- find_peaks(pred$pred, m=1)
peaks
SD57.peak <- 231
SD57.peak
SD57.volt <- 2

SJ43 <- subset(BC_sub.new, grid10=="SJ43")
pheno <- gam(max.ind ~ s(julianday, k=10, bs='cr'), data=SJ43, family = "poisson", weights = SJ43$max.ind, method = "REML", gamma = 1)
pred <- data.frame(day = c(100:300))
pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))

max.ind <- ggplot(pred)+
  geom_point(data = SJ43[which(SJ43$max.ind > 0), ], 
             aes(x = julianday, y = max.ind), 
             colour = "grey60")+
  scale_y_continuous(limits = c(0,max(SJ43$max.ind)))+
  geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
  theme_bw()+
  ggtitle(paste("Emergence plot for SJ43 k=10, wghts= manual set" ,sep=" "))+
  xlab("Julian day") + ylab("Abundance") 
# print out plot
max.ind

peaks <- find_peaks(pred$pred, m=1)
peaks
SJ43.peak <- pred[peaks[3],]
SJ43.peak
SJ43.volt <- 2

TM04 <- subset(BC_sub.new, grid10=="TM04")
pheno <- gam(max.ind ~ s(julianday, k=10, bs='cr'), data=TM04, family = "poisson", weights = TM04$max.ind, method = "REML", gamma = 1)
pred <- data.frame(day = c(100:300))
pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))

max.ind <- ggplot(pred)+
  geom_point(data = TM04[which(TM04$max.ind > 0), ], 
             aes(x = julianday, y = max.ind), 
             colour = "grey60")+
  scale_y_continuous(limits = c(0,max(TM04$max.ind)))+
  geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
  theme_bw()+
  ggtitle(paste("Emergence plot for TM04 k=10, wghts= manual set" ,sep=" "))+
  xlab("Julian day") + ylab("Abundance") 
# print out plot
max.ind

peaks <- find_peaks(pred$pred, m=1)
peaks
TM04.peak <- pred[peaks[2],]
TM04.peak
TM04.volt <- 3

# Re-fit GAM NM76; NS20  ####

NM76 <- subset(BC_sub.new, BC_sub.new$grid10=="NM76")
head(NM76)
dim(NM76)
range(NM76$max.ind)
# very low abundance numbers

## NM76
NM76$wghts <- ifelse(NM76$max.ind>=3, "30", 
                     ifelse(NM76$max.ind>=2, "20", 
                            ifelse(NM76$max.ind>=1, "10", "1")))
NM76$wghts <- as.numeric(NM76$wghts)
head(NM76)

# fit the gam: k=15, weights=max.ind
pheno <- gam(max.ind ~ s(julianday, k=10, bs='cr'), data=NM76, family = "poisson", weights = NM76$wghts, method = "REML", gamma = 1)
# plot the gam
pred <- data.frame(day = c(100:300))
pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))
max.ind <- ggplot(pred)+
  geom_point(data = NM76[which(NM76$max.ind > 0), ], 
             aes(x = julianday, y = max.ind), 
             colour = "grey60")+
  scale_y_continuous(limits = c(0,max(NM76$max.ind)))+
  geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
  theme_bw()+
  ggtitle(paste("Emergence plot for NM76 k=10, wghts= manual set" ,sep=" "))+
  xlab("Julian day") + ylab("Abundance") 
# print out plot
max.ind
# save plot
ggsave("./output/Emergence_plots/07to17/Max.ind/sep_plots/NM76_new.png", plot = max.ind , width = 100, height = 70, units = "mm", limitsize = F)

# identify peaks and select 2
peaks <- find_peaks(pred$pred, m=5)
peaks
NM76.peak <- pred[peaks[3],]
NM76.peak
NM76.volt <- 2

# # # # # # 

## NS20 
NS20 <- subset(BC_sub.new, BC_sub.new$grid10=="NS20")
head(NS20)
dim(NS20)
range(NS20$max.ind)

NS20$wghts <- ifelse(NS20$max.ind>=5, "10", 
                     ifelse(NS20$max.ind>=1, "5", "1"))
NS20$wghts <- as.numeric(NS20$wghts)
head(NS20)

# fit the gam: k=15, weights=max.ind
pheno <- gam(max.ind ~ s(julianday, k=10, bs='cr'), data=NS20, family = "poisson", weights = NS20$wghts, method = "REML", gamma = 1)
# plot the gam
pred <- data.frame(day = c(100:300))
pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))
max.ind <- ggplot(pred)+
  geom_point(data = NS20[which(NS20$max.ind > 0), ], 
             aes(x = julianday, y = max.ind), 
             colour = "grey60")+
  scale_y_continuous(limits = c(0,max(NS20$max.ind)))+
  geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
  theme_bw()+
  ggtitle(paste("Emergence plot for NS20 k=10, wghts= manual set" ,sep=" "))+
  xlab("Julian day") + ylab("Abundance") 
# print out plot
max.ind
# save plot
ggsave("./output/Emergence_plots/07to17/Max.ind/sep_plots/NS20_new.png", plot = max.ind , width = 100, height = 70, units = "mm", limitsize = F)

# now identify highest peak = 2nd generation
peaks <- find_peaks(pred$pred, m=5)
peaks
NS20.peak <- pred[peaks[2],]
NS20.peak
NS20.volt <- 2


# Look at grid with less than 10 data points ####
# NJ14; NT16; NX44; NY86 and NN29

## NJ14
NJ14 <- subset(BC_sub, grid10=="NJ14")
head(NJ14)
dim(NJ14)
range(NJ14$max.ind)

# look at plot to get an idea
pheno <- gam(max.ind ~ s(julianday, k=5, bs='cr'), data=NJ14, family = "poisson", weights = NJ14$max.ind, method = "REML", gamma = 1)
# plot the gam
pred <- data.frame(day = c(100:300))
pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))
max.ind <- ggplot(pred)+
  geom_point(data = NJ14[which(NJ14$max.ind > 0), ], 
             aes(x = julianday, y = max.ind), 
             colour = "grey60")+
  scale_y_continuous(limits = c(0,max(NJ14$max.ind)))+
  geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
  theme_bw()+
  ggtitle(paste("Emergence plot for NJ14 k=10, wghts= manual set" ,sep=" "))+
  xlab("Julian day") + ylab("Abundance") 
# print out plot
max.ind

NJ14
NJ14.peak <- 199
NJ14.peak
NJ14.volt <- 2

## NT16
NT16 <- subset(BC_sub, grid10=="NT16")
head(NT16)

# one point at 207 take as 2nd generation?
NT16.peak <- 207
NT16.volt <- 2

## NX44
NX44 <- subset(BC_sub, grid10=="NX44")
head(NX44)

# look at plot to get an idea
pheno <- gam(max.ind ~ s(julianday, k=3, bs='cr'), data=NX44, family = "poisson", weights = NX44$max.ind, method = "REML", gamma = 1)
# plot the gam
pred <- data.frame(day = c(100:300))
pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))
max.ind <- ggplot(pred)+
  geom_point(data = NX44[which(NX44$max.ind > 0), ], 
             aes(x = julianday, y = max.ind), 
             colour = "grey60")+
  scale_y_continuous(limits = c(0,max(NX44$max.ind)))+
  geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
  theme_bw()+
  ggtitle(paste("Emergence plot for NX44 k=10, wghts= manual set" ,sep=" "))+
  xlab("Julian day") + ylab("Abundance") 
# print out plot
max.ind

# hard to tell with very few points - lets take mid point of where it starts to increase. Unlikely to have 3 generations as it is further north but better to take mid point incase.
NX44.peak <- 238
NX44.volt <- 2

## NY86
NY86 <- subset(BC_sub, grid10=="NY86")
head(NY86)

# day 142 seems to early fo 2nd generation but is the only data point. Location is near Haydon Bridge.
# Looked at surrounding grids and nearest with data is NZ13 - this is southern of NY86 and so should be conservative to use as a proxy if we assume that 2nd gen emerges earlier in the season compared to further north

# NZ13 has already been run for another set of samples - 2nd gen emerges at 225

NY86.peak <- 225
NY86.volt <-2


## NN29
NN29 <- subset(BC_sub, grid10=="NN29")
head(NN29)
dim(NN29)
range(NN29$max.ind)

# look at plot to get an idea
pheno <- gam(max.ind ~ s(julianday, k=3, bs='cr'), data=NN29, family = "poisson", weights = NN29$max.ind, method = "REML", gamma = 1)
# plot the gam
pred <- data.frame(day = c(100:300))
pred$pred <- predict(pheno, type="response",newdata=data.frame(julianday=100:300))
max.ind <- ggplot(pred)+
  geom_point(data = NN29[which(NN29$max.ind > 0), ], 
             aes(x = julianday, y = max.ind), 
             colour = "grey60")+
  scale_y_continuous(limits = c(0,max(NN29$max.ind)))+
  geom_line(aes(x = day, y = pred), colour = "royalblue", size = 1.25)+
  theme_bw()+
  ggtitle(paste("Emergence plot for NN29 k=10, wghts= manual set" ,sep=" "))+
  xlab("Julian day") + ylab("Abundance") 
# print out plot
max.ind

# identify peaks and select 2
peaks <- find_peaks(pred$pred, m=5)
peaks
NN29.peak <- pred[peaks[1],]
NN29.peak
NN29.volt <- 2

# input missing grids to df 
# NJ14; NT16; NX44; NY86 and NN29

grids <- c("NJ14", "NT16", "NX44", "NY86", "NN29")
peaks_lowDat <- as.numeric(c(NJ14.peak, NT16.peak, NX44.peak, NY86.peak, NN29.peak[1]))
volt_lowDat <- as.numeric(c(NJ14.volt, NT16.volt, NX44.volt, NY86.volt, NN29.volt))

gen.2.lowDatGrids <- as./Data.frame(cbind(grids, peaks_lowDat, volt_lowDat))
head(gen.2.lowDatGrids)

colnames(gen.2.lowDatGrids)[] <- c("grid10", "gen2Jday", "volt_num")
head(gen.2.lowDatGrids)

# not sure why its got an extra row?? - remove it
gen.2.lowDatGrids <- gen.2.lowDatGrids[-6,]
head(gen.2.lowDatGrids)

## Insert correct peaks for grids #### 
# SD38; SD57; SJ43; TM04; NS20; NM76

setDT(gen.2.maxInd, keep.rownames = TRUE)[]
colnames(gen.2.maxInd)[] <- c("grid10", "gen2Jday", "volt_num")
head(gen.2.maxInd)

gen.2.maxInd <- transform(gen.2.maxInd, gen2Jday = ifelse(grid10=="SD38", SD38.peak[1], gen2Jday))
gen.2.maxInd <- transform(gen.2.maxInd, volt_num = ifelse(grid10=="SD38", SD38.volt, volt_num))

gen.2.maxInd <- transform(gen.2.maxInd, gen2Jday = ifelse(grid10=="SD57", SD57.peak[1], gen2Jday))
gen.2.maxInd <- transform(gen.2.maxInd, volt_num = ifelse(grid10=="SD57", SD57.volt, volt_num))

gen.2.maxInd <- transform(gen.2.maxInd, gen2Jday = ifelse(grid10=="SJ43", SJ43.peak[1], gen2Jday))
gen.2.maxInd <- transform(gen.2.maxInd, volt_num = ifelse(grid10=="SJ43", SJ43.volt, volt_num))

gen.2.maxInd <- transform(gen.2.maxInd, gen2Jday = ifelse(grid10=="TL11", "245", gen2Jday))

gen.2.maxInd <- transform(gen.2.maxInd, gen2Jday = ifelse(grid10=="TM04", TM04.peak[1], gen2Jday))
gen.2.maxInd <- transform(gen.2.maxInd, volt_num = ifelse(grid10=="TM04", TM04.volt, volt_num))

gen.2.maxInd <- transform(gen.2.maxInd, gen2Jday = ifelse(grid10=="NS20", NS20.peak[1], gen2Jday))
gen.2.maxInd <- transform(gen.2.maxInd, volt_num = ifelse(grid10=="NS20", NS20.volt, volt_num))

gen.2.maxInd <- transform(gen.2.maxInd, gen2Jday = ifelse(grid10=="NM76", NM76.peak[1], gen2Jday))
gen.2.maxInd <- transform(gen.2.maxInd, volt_num = ifelse(grid10=="NM76", NM76.volt, volt_num))

head(gen.2.maxInd)
gen.2.maxInd <- gen.2.maxInd[-1,]

# add in the grids that had low number of data
gen.2.maxInd.Final <- rbind(gen.2.maxInd, gen.2.lowDatGrids)
dim(gen.2.maxInd.Final)
head(gen.2.maxInd.Final)

## Check number of generations (voltinism) ####
volt_1_check <- gen.2.maxInd.Final[gen.2.maxInd.Final$volt_num==1,]
dim(volt_1_check) # 6 sites have 1 generation? - check
head(volt_1_check)

# manually look at gam plots and correct any that have more than 1 generations
volt2 <- 2
volt3 <- 3

# replace in df
gen.2.maxInd.Final <- within(gen.2.maxInd.Final, volt_num[grid10 == 'NJ04'] <- 2)
gen.2.maxInd.Final <- within(gen.2.maxInd.Final, volt_num[grid10 == 'NN86'] <- 2)
gen.2.maxInd.Final <- within(gen.2.maxInd.Final, volt_num[grid10 == 'NO79'] <- 2)
gen.2.maxInd.Final <- within(gen.2.maxInd.Final, volt_num[grid10 == 'SE84'] <- 3)
gen.2.maxInd.Final <- within(gen.2.maxInd.Final, volt_num[grid10 == 'SE94'] <- 3)
gen.2.maxInd.Final <- within(gen.2.maxInd.Final, volt_num[grid10 == 'SU13'] <- 2)

volt_1_check2 <- gen.2.maxInd.Final[gen.2.maxInd.Final$volt_num==1,]
dim(volt_1_check2) # only 1 site with 1 generation - lack of data?

## Julian day to norm dates ####

gen.2.maxInd.Final$Date <- as.Date(as.numeric(as.character(gen.2.maxInd.Final$gen2Jday)), origin=as.Date("2007-12-31"))
head(gen.2.maxInd.Final)

# split Date into Yr mnth and day
gen.2.maxInd.Final <- separate(gen.2.maxInd.Final, "Date", c("gen2.yr", "gen2.mnth", "gen2.day"), sep="-")
head(gen.2.maxInd.Final)

## Convert dates colected in samp_log into Julian day ####

head(samp_log.new)

#
samp_log.new$Date <- paste(samp_log.new$Day.col, samp_log.new$Mnth.col, samp_log.new$Yr.col, sep="/")
samp_log.new$EveDate <- as.Date(samp_log.new$Date, format = "%d/%m/%Y")

# set up a reference date
samp_log.new$Yr.col <- as.numeric(as.character(samp_log.new$Yr.col))
samp_log.new$refdate <- dmy(paste0("31-12-",(samp_log.new$Yr.col-1)))
head(samp_log.new)

samp_log.new$Jday.col <- as.numeric(difftime(samp_log.new$EveDate, samp_log.new$refdate))
head(samp_log.new)


# merge gen.2.maxInd.Final with samp_log
head(samp_log.new)
dim(samp_log.new)

head(gen.2.maxInd.Final)

samp_log.new2 <- merge(samp_log.new, gen.2.maxInd.Final, by.x="Grid.10km", by.y="grid10")
head(samp_log.new2)
dim(samp_log.new2)
# good

## Calculate num of day collected prior to peak 2nd gen ####
samp_log.new2$gen2Jday <- as.numeric(as.character(samp_log.new2$gen2Jday))
head(samp_log.new2)
samp_log.new2$Jday.diff <- samp_log.new2$gen2Jday - samp_log.new2$Jday.col
head(samp_log.new2)

# each year temp data to have met.x and met.y #####
tmp_16$met.x <- mroundOG(tmp_16$lon, 1000)
tmp_16$met.y <- mroundOG(tmp_16$lat, 1000)

tmp_17$met.x <- mroundOG(tmp_17$lon, 1000)
tmp_17$met.y <- mroundOG(tmp_17$lat, 1000)

tmp_18$met.x <- mroundOG(tmp_18$lon, 1000)
tmp_18$met.y <- mroundOG(tmp_18$lat, 1000)
head(tmp_18)

## Take mean temp of 3 months prior to peak month of year sampled ####

# remove zeros beofre gen2.mnth
samp_log.new2$gen2.mnth <- str_remove(samp_log.new2$gen2.mnth, "^0+")

samp_log.new2$gen2.mnth <- as.numeric(samp_log.new2$gen2.mnth)
unique(samp_log.new2$gen2.mnth)

# merge sample log with either tmp_16/17/18 depending on Yr.col
samp_log.new3 <- map2_dfr(list(tmp_16,tmp_17,tmp_18,tmp_18),
                          2016:2019,
                          function(x,y){merge(samp_log.new2 %>% filter(Yr.col == y), x, by=c("met.x", "met.y"))})
head(samp_log.new3)

## depending on gen2.mnth take different months for development time.
# if gen2.mnth=6 take April, May, June; =7 take May, June,July; =8 take June, July, Aug; =9 Jult, Aug,Sep

samp_log.Final = within(samp_log.new3, {
  dev.tmp = ifelse(gen2.mnth=="6", apply(samp_log.new3[,c("tmpApr", "tmpMay", "tmpJun")], 1, mean), # columns 
                   ifelse(gen2.mnth=="7", apply(samp_log.new3[,c("tmpMay", "tmpJun", "tmpJul")], 1, mean),
                          ifelse(gen2.mnth=="8", apply(samp_log.new3[,c("tmpJun", "tmpJul", "tmpAug")], 1, mean),
                                 ifelse(gen2.mnth=="9", apply(samp_log.new3[,c("tmpJul", "tmpAug", "tmpSep")], 1, mean),"NA"
                                 ))))})

# let remove columns we do not need in samp_log.Final i.e. all the tmperatures for the year etc.
head(samp_log.Final)
dim(samp_log.Final)
samp_log.Final <- samp_log.Final[,-c(46:60)]
head(samp_log.Final)

### Reorder indivduals to same as landmerked order (Ind.num.cont)
samp_log.Final <- samp_log.Final[order(samp_log.Final$Ind.num.cont),]
head(samp_log.Final)


# lets round up mat.10yr and dev.tmp to 4 dp
samp_log.Final$dev.tmp <- as.numeric(samp_log.Final$dev.tmp)
samp_log.Final <- samp_log.Final %>% mutate_at(vars(mat.10yr, dev.tmp), funs(round(., 5)))

head(samp_log.Final)



### Write out new samp_log.Final ####

write.csv(samp_log.Final, "./Data/logs/Morph_sampLog_env_Final.csv")
