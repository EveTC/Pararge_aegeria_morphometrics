### Morphometrics of Pararge aegeria wings across the range in the UK.
## Evelyn Taylor-Cox
## Supervisor: Ilik Saccheri
# 18/11/19

# Aim of script is to input landmrk data and combine with sample log + env. info for analysis

## Photos.zip needs to be unzipped

#### Load packages and dependencies ####
library(geomorph)
library(StereoMorph)
library(ggplot2)
library(Morpho)
library(vegan)
library(corrplot)
library(lme4)
library(pbkrtest)
library(car)
library(MASS)
library(multcomp)
library(tidyr)
library(shapes)

#### Input landmark data and env data + format FW and HW ####
FW <- readland.tps('./Photos/JPEG/Forewing/All_FW.TPS', specID="imageID")
class(FW)
dim(FW)

HW <- readland.tps('././Photos/JPEG/Hindwing/All_HW.TPS', specID="imageID")
class(HW)
dim(HW)

# making a factor: group variables - inport csv with group i.e. which population.
# N.B. sites have been reordered to same organsiation in landmark tps file
classifier <- read.csv("./Data/Logs/Morph_sampLog_env_Final.csv", header=T)
head(classifier)

# remove first column
classifier <- classifier[,-1]
head(classifier)


## FW
dim(FW)
# 759 inds have been landmarked for FWS
# remove FW not landmarked in classifier
classifier_FW <- classifier[classifier$FW_used!="N",]
dim(classifier_FW)

## HW
dim(HW)
# 683 inds landmarked
classifier_HW <- classifier[classifier$HW_used!="N",]
dim(classifier_HW)

# rownames as column 33 for FW classifiers - photo id FW
head(classifier_FW)
rownames(classifier_FW) <- classifier_FW[,"Photo.FW.D.ID"]

# rownames as column 35 for HW classifiers - photo id HW
head(classifier_HW)
rownames(classifier_HW) <- classifier_HW[,"Photo.HW.D.ID"]

## FW- create dataframe - with variables of interest ####
FW_dataframe <- geomorph.data.frame(landmarks=FW, samp=classifier_FW$Photo.FW.D.ID, Site=classifier_FW$Site.ID, Gird.10km=classifier_FW$Grid.10km, expn=classifier_FW$Expansion, site.lat=as.factor(classifier_FW$Lat),site.lng=as.factor(classifier_FW$Lng), rec.1st.10km = as.factor(classifier_FW$X10km.1st.rec), site.1st.rec=as.factor(classifier_FW$site.1st.rec), yrs.col.10km=as.factor(classifier_FW$years.col.10km), yrs.col.site=as.factor(classifier_FW$years.col.site), met.x=as.factor(classifier_FW$met.x), met.y=as.factor(classifier_FW$met.y), scale_dmg=as.factor(classifier_FW$Scale_dmg), dmg=as.factor(classifier_FW$Dmg), age=as.factor(classifier_FW$Age), Jdiff=as.factor(classifier_FW$Jday.diff), mat.10yr=as.factor(classifier_FW$mat.10yr),Dev.tmp=as.factor(classifier_FW$dev.tmp), volt.num=as.factor(classifier_FW$volt_num))

## HW- create dataframe - with variables of interest ####
HW_dataframe <- geomorph.data.frame(landmarks=HW, samp=classifier_HW$Photo.HW.D.ID, Site=classifier_HW$Site.ID, Gird.10km=classifier_HW$Grid.10km, expn=classifier_HW$Expansion, site.lat=as.factor(classifier_HW$Lat),site.lng=as.factor(classifier_HW$Lng), rec.1st.10km = as.factor(classifier_HW$X10km.1st.rec), site.1st.rec=as.factor(classifier_HW$site.1st.rec), yrs.col.10km=as.factor(classifier_HW$years.col.10km), yrs.col.site=as.factor(classifier_HW$years.col.site), met.x=as.factor(classifier_HW$met.x), met.y=as.factor(classifier_HW$met.y), scale_dmg=as.factor(classifier_HW$Scale_dmg), dmg=as.factor(classifier_HW$Dmg), age=as.factor(classifier_HW$Age), Jdiff=as.factor(classifier_HW$Jday.diff), mat.10yr=as.factor(classifier_HW$mat.10yr),Dev.tmp=as.factor(classifier_HW$dev.tmp), volt.num=as.factor(classifier_HW$volt_num))



#### Generalized Procrutes Analysis - make gpa.df ####

## FW 
# using geomorph
FW_gpa <- gpagen(FW)
par(mfrow=c(1,1))
plot(FW_gpa)

gdf.FW <- geomorph.data.frame(FW_gpa, samp=classifier_FW$Photo.FW.D.ID, Site=classifier_FW$Site.ID, Gird.10km=classifier_FW$Grid.10km, expn=classifier_FW$Expansion, site.lat=as.factor(classifier_FW$Lat), site.lng=as.factor(classifier_FW$Lng) , rec.1st.10km = as.factor(classifier_FW$X10km.1st.rec), site.1st.rec=as.factor(classifier_FW$site.1st.rec), yrs.col.10km=as.factor(classifier_FW$years.col.10km), yrs.col.site=as.factor(classifier_FW$years.col.site), met.x.1km=as.factor(classifier_FW$met.x), met.y.1km=as.factor(classifier_FW$met.y), scale_dmg=as.factor(classifier_FW$Scale_dmg), dmg=as.factor(classifier_FW$Dmg), age=as.factor(classifier_FW$Age),  Jdiff=as.factor(classifier_FW$Jday.diff), mat.10yr=as.factor(classifier_FW$mat.10yr),Dev.tmp=as.factor(classifier_FW$dev.tmp), volt.num=as.factor(classifier_FW$volt_num))

gp <- gdf.FW$Site
shape_pca <- gm.prcomp(gdf.FW$coords)
summary(shape_pca)
plot(shape_pca, col=gp) # good no outliers

## HW 
# using geomorph
HW_gpa <- gpagen(HW)
par(mfrow=c(1,1))
plot(HW_gpa)

gdf.HW <- geomorph.data.frame(HW_gpa, samp=classifier_HW$Photo.HW.D.ID, Site=classifier_HW$Site.ID, Gird.10km=classifier_HW$Grid.10km, expn=classifier_HW$Expansion, site.lat=as.factor(classifier_HW$Lat), site.lng=as.factor(classifier_HW$Lng) , rec.1st.10km = as.factor(classifier_HW$X10km.1st.rec), site.1st.rec=as.factor(classifier_HW$site.1st.rec), yrs.col.10km=as.factor(classifier_HW$years.col.10km), yrs.col.site=as.factor(classifier_HW$years.col.site), met.x.1km=as.factor(classifier_HW$met.x), met.y.1km=as.factor(classifier_HW$met.y), scale_dmg=as.factor(classifier_HW$Scale_dmg), dmg=as.factor(classifier_HW$Dmg), age=as.factor(classifier_HW$Age),  Jdiff=as.factor(classifier_HW$Jday.diff), mat.10yr=as.factor(classifier_HW$mat.10yr),Dev.tmp=as.factor(classifier_HW$dev.tmp), volt.num=as.factor(classifier_HW$volt_num))


gp <- gdf.HW$Site
shape_pca <- gm.prcomp(gdf.HW$coords)
summary(shape_pca)
plot(shape_pca, col=gp) # outliers

names(shape_pca)
# x= PC scores for all specimens
# d= Singluar values for decomposed VCV matrix
# rotation = matrix of variable loadings
# shapes = list with shape coords of extreme ends of all PC axes

## Investigate outliers in HW landmarks ####
# what are these massive outliers? - 2 from Eng; 2 from Scot according to PCA
par(mfrow=c(1,1))
#identify(qqnorm(shape_pca$x[,2], pch=20))
# 97 382 654 661
# 4 points away from main line in PCA

df <- as.data.frame(shape_pca$x)
head(df)
rownames(df[c(97,382,654,661),])
# these are the indivduals that seem to be outlier in PCA
# [1] "PA_7_CA_09_D_calibrated" "PA_24_Tay_10_D_calibrated" "CNM36_D_calibrated" "ENM5_D_calibrated" 

## look at all wierd points
gpa_dat <-gdf.HW[["coords"]]
head(gpa_dat)

# Remove these individuals: 
# 97 382 654 661
#"PA_7_CA_09_D_calibrated" "PA_24_Tay_10_D_calibrated" "CNM36_D_calibrated" "ENM5_D_calibrated" 

HW_filt <- HW[,,-c(97,382,654,661)]
dim(HW_filt)

# has this improved the gpa and PCA??
HW_filt_gpa <- gpagen(HW_filt)
par(mfrow=c(1,1))
plot(HW_filt_gpa)
# so far so good!!

# remove these individuals from classifier
remove <- c("PA_7_CA_09_D_calibrated","PA_24_Tay_10_D_calibrated","CNM36_D_calibrated","ENM5_D_calibrated")
classifier_HW_filt <- classifier_HW[!row.names(classifier_HW)%in%remove,]
dim(classifier_HW_filt)

gdf.HW_filt <- geomorph.data.frame(HW_filt_gpa, samp=classifier_HW_filt$Photo.HW.D.ID, Site=classifier_HW_filt$Site.ID, Gird.10km=classifier_HW_filt$Grid.10km, expn=classifier_HW_filt$Expansion, site.lat=as.factor(classifier_HW_filt$Lat), site.lng=as.factor(classifier_HW_filt$Lng) , rec.1st.10km = as.factor(classifier_HW_filt$X10km.1st.rec), site.1st.rec=as.factor(classifier_HW_filt$site.1st.rec), yrs.col.10km=as.factor(classifier_HW_filt$years.col.10km), yrs.col.site=as.factor(classifier_HW_filt$years.col.site), met.x.1km=as.factor(classifier_HW_filt$met.x), met.y.1km=as.factor(classifier_HW_filt$met.y), scale_dmg=as.factor(classifier_HW_filt$Scale_dmg), dmg=as.factor(classifier_HW_filt$Dmg), age=as.factor(classifier_HW_filt$Age), Jdiff=as.factor(classifier_HW_filt$Jday.diff), mat.10yr=as.factor(classifier_HW_filt$mat.10yr),Dev.tmp=as.factor(classifier_HW_filt$dev.tmp), volt.num=as.factor(classifier_HW_filt$volt_num))

gp <- gdf.HW_filt$Site
shape_pca_filt <- gm.prcomp(gdf.HW_filt$coords)
summary(shape_pca_filt)
plot(shape_pca_filt, col=gp)
# much better!


#### FW Measurements: Length, width and centroid to gpa.df ####

## Can we get length of wings? Landmark 1-13.
lmks_FW <- matrix(c(1,14,7,14), ncol=2, byrow=TRUE, 
                  dimnames = list(c("Lgth", "width"),c("start", "end")))
FW_meas <- FW_dataframe$landmarks

FW_distances <- as.data.frame(interlmkdist(FW_meas, lmks_FW))
head(FW_distances)
dim(FW_distances)

# add population length and width info to classifier - rename measures
FW_measures <- merge(FW_distances,classifier_FW, by="row.names", all=TRUE)
head(FW_measures)
dim(FW_measures)

# Centroid size
Csize <- matrix(FW_gpa$Csize, dimnames=list(names(FW_gpa$Csize))) 

FW_sizeDF <- merge(FW_measures, Csize, by.x="Row.names", by.y="row.names", all=TRUE)
head(FW_sizeDF)
dim(FW_sizeDF)
names(FW_sizeDF)[50] <- "Csize" 

rownames(FW_sizeDF) <- FW_sizeDF[,1]
FW_sizeDF[,1]<-NULL
head(FW_sizeDF)

# order size DF
FW_sizeDF <- FW_sizeDF[order(FW_sizeDF$Ind.num.cont),]

#### HW Measurements: Length, width and centroid to df ####

## Can we get length of wings? Landmark 1-13.
lmks_HW <- matrix(c(1,8,5,11), ncol=2, byrow=TRUE, 
                  dimnames = list(c("Lgth", "width"),c("start", "end")))
HW_meas <- HW_dataframe$landmarks

HW_distances <- as.data.frame(interlmkdist(HW_meas, lmks_HW))
head(HW_distances)
dim(HW_distances)

# add population info to distances df
HW_measures <- merge(HW_distances,classifier_HW_filt, by="row.names", all=TRUE)
head(HW_measures)
dim(HW_measures)

# Centroid size
Csize_HW <- matrix(gdf.HW_filt$Csize, dimnames=list(names(gdf.HW_filt$Csize))) 

# 07to17
HW_sizeDF <- merge(HW_measures, Csize_HW, by.x="Row.names", by.y="row.names", all=TRUE)
head(HW_sizeDF)
dim(HW_sizeDF)
names(HW_sizeDF)[50] <- "Csize" 

rownames(HW_sizeDF) <- HW_sizeDF[,1]
HW_sizeDF[,1]<-NULL
head(HW_sizeDF)

# order size DF
HW_sizeDF <- HW_sizeDF[order(HW_sizeDF$Ind.num.cont),]
head(HW_sizeDF)

#### Order populations ######

## FW
# keep pops in same order
FW_sizeDF$Site.ID <- factor(FW_sizeDF$Site.ID, levels=c('WW','CS_CarvCopse','CS_MarDwn','CS_Snapes','MH','RHCP','ES_HadRail','ES_Wolves','ES_StourW','Ferm','Fox','CC_FennM','CC_BettiM','CC_LlanRocks','CC_LlanHill','CA','RW','WC','HPW','HRC', 'EC_SnuffML','EC_PrioryF','EC_WL','EC_HudWay','EC_HudWayCP','ARW','12-RW','CM','WLW','SodG','L','ACW','Garl','CCP','EM','YC','GB','CN_ShianW','CN_Glasdrum','Tay','AF','RF','SL','EN_Dunph','EN_DivV','EN_DavaW','EN_DrumW','CC','CB','CDho','MW'), ordered=TRUE)

## HW
HW_sizeDF$Site.ID <- factor(HW_sizeDF$Site.ID, levels=c('WW','CS_CarvCopse','CS_MarDwn','CS_Snapes','MH','RHCP','ES_HadRail','ES_Wolves','ES_StourW','Ferm','Fox','CC_FennM','CC_BettiM','CC_LlanRocks','CC_LlanHill','CA','RW','WC','HPW','HRC', 'EC_SnuffML','EC_PrioryF','EC_WL','EC_HudWay','EC_HudWayCP','ARW','12-RW','CM','WLW','SodG','L','ACW','Garl','CCP','EM','YC','GB','CN_ShianW','CN_Glasdrum','Tay','AF','RF','SL','EN_Dunph','EN_DivV','EN_DavaW','EN_DrumW','CC','CB','CDho','MW'), ordered=TRUE)
HW_sizeDF <- HW_sizeDF[!row.names(HW_sizeDF)%in%remove,]

