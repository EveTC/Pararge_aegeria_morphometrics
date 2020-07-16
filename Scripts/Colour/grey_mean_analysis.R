## Lightness using mean grey value - as calculated in imageJ

### Get Libraries ####
library(ggplot2)
library(lme4)
library(sjstats)
library(car)
library(MASS)
library(coefplot2)
library(broom.mixed)
library(dotwhisker)
library(sjPlot)
library(sjmisc)
library(effects)
library(vegan)
library(geomorph)
library(factoextra)
library(numDeriv)
library(tidyr)
library(jtools)
library(ggpubr)
library(dplyr)
library(scales)


##### FW - Dorsal analysis #####
### Get data & format ####
## read in data
FW_D_grey <- read.csv("./ImageJ/Results/Mean_grey/FW_D_mGV_per_3rd.csv")
head(FW_D_grey)

# change label to not have .png at end
# remove .png from Photo.FW.D.ID
FW_D_grey <- cbind(stringr::str_split_fixed(as.character(FW_D_grey$Label), stringr::fixed(".png:"), 2), FW_D_grey)
head(FW_D_grey)

# remove unwanted columns
FW_D_grey <- FW_D_grey[,c(1:2,4:9,14)]
colnames(FW_D_grey)[2]<- "wing_section"
colnames(FW_D_grey)[1]<- "Photo.FW.D.ID"
head(FW_D_grey)

## read in env data
logEnv <- read.csv("./Data/Logs/Morph_sampLog_env_Final.csv")
head(logEnv)

logEnv <- logEnv[,-c(1)]
head(logEnv)
dim(logEnv)

FW_D_grey_allEnv<- merge(FW_D_grey,logEnv, by="Photo.FW.D.ID")
dim(FW_D_grey_allEnv)
head(FW_D_grey_allEnv)

# reorder samples
FW_D_grey_allEnv <- FW_D_grey_allEnv[order(FW_D_grey_allEnv$Ind.num.cont),]
head(FW_D_grey_allEnv)

FW_D_grey_allEnv$Site.ID <- factor(FW_D_grey_allEnv$Site.ID, levels=c('WW','CS_CarvCopse','CS_MarDwn','CS_Snapes','MH','RHCP',"ES_HadRail","ES_Wolves","ES_StourW",'Ferm','Fox','CC_FennM','CC_BettiM','CC_LlanRocks','CC_LlanHill','CA','RW','WC','HPW','HRC', 'EC_SnuffML','EC_PrioryF','EC_WL','EC_HudWay','EC_HudWayCP','ARW','12-RW','CM','WLW','SodG','L','ACW','Garl','CCP','EM','YC','GB','CN_ShianW','CN_Glasdrum','Tay','AF','RF','SL','EN_Dunph','EN_DivV','EN_DavaW','EN_DrumW','CC','CB','Cdho','MW'), ordered=TRUE)


## Filter to sections of wings (whole and bottom 3rd) ####

FW_D_grey_wing <- FW_D_grey_allEnv[which(FW_D_grey_allEnv$wing_section=="whole_wing"),]
head(FW_D_grey_wing)
dim(FW_D_grey_wing)

FW_D_grey_bot3wing <- FW_D_grey_allEnv[which(FW_D_grey_allEnv$wing_section=="bot3rd"),]
head(FW_D_grey_bot3wing)
dim(FW_D_grey_bot3wing)

FW_D_grey_mid3wing <- FW_D_grey_allEnv[which(FW_D_grey_allEnv$wing_section=="mid3rd"),]
head(FW_D_grey_mid3wing)
dim(FW_D_grey_mid3wing)


### Filter out scale damage and replot ####

## remove highly damage individuals (Scale_dmg =4) ####
FW_D_grey_wing.filt4 <- FW_D_grey_wing[which(FW_D_grey_wing$Scale_dmg<4),]
dim(FW_D_grey_wing.filt4)
FW_D_grey_bot3wing.filt4 <- FW_D_grey_bot3wing[which(FW_D_grey_bot3wing$Scale_dmg<4),]
dim(FW_D_grey_bot3wing.filt4)

## remove Scale_dmg of 3 & 4 ####
FW_D_grey_wing.filt3n4 <- subset(FW_D_grey_wing, Scale_dmg!=3 & Scale_dmg!=4)
dim(FW_D_grey_wing.filt3n4)

FW_D_grey_bot3wing.filt3n4 <- subset(FW_D_grey_bot3wing, Scale_dmg!=3 & Scale_dmg!=4)
dim(FW_D_grey_bot3wing.filt3n4)


### GLMMs of FW_D_grey mean value - following set up used for size ####

## Model Csize with variables of interest - filtered for 4  ####

# Remove inds sampled in 2019 - incorecct temperature data for these
FW_D_grey_wing.filt4_new <- subset(FW_D_grey_wing.filt4, Yr.col!="2019")
dim(FW_D_grey_wing.filt4_new)

FW_D_grey_bot3wing.filt4_new <- subset(FW_D_grey_bot3wing.filt4, Yr.col!="2019")
dim(FW_D_grey_bot3wing.filt4_new)

# Remove inds sampled in 2019 for filt 3n4 - incorecct temperature data for these
FW_D_grey_wing.filt3n4_new <- subset(FW_D_grey_wing.filt3n4, Yr.col!="2019")
dim(FW_D_grey_wing.filt3n4_new)

FW_D_grey_bot3wing.filt3n4_new <- subset(FW_D_grey_bot3wing.filt3n4, Yr.col!="2019")
dim(FW_D_grey_bot3wing.filt3n4_new)

# Check correlation of explanatory variables
# look at explan variables
pairs(~Mean+Lat+mat.10yr+dev.tmp, data=FW_D_grey_wing.filt4_new, upper.panel=NULL)

par(mfrow=c(1,1))
M <- cor(FW_D_grey_wing.filt4_new[c(5,19,18,55)], method="pearson") # chack columns used here
corrplot::corrplot(M,method = "number", type="lower")

# normal dist lmmer ####
hist(FW_D_grey_wing.filt4_new$Mean)
hist(FW_D_grey_bot3wing.filt4_new$Mean)

#### FW - Ventral analysis #####
### Get data & format ####
## read in data
FW_V_grey <- read.csv("./imageJ/Results/Mean_grey/FW_V_mGV_per_3rd.csv")
head(FW_V_grey)

# change label to not have .png at end
# remove .png from Photo.FW.D.ID
FW_V_grey <- cbind(stringr::str_split_fixed(as.character(FW_V_grey$Label), stringr::fixed(".png:"), 2), FW_V_grey)
head(FW_V_grey)

# remove unwanted columns
FW_V_grey <- FW_V_grey[,c(1:2,4:9,14)]
colnames(FW_V_grey)[2]<- "wing_section"
colnames(FW_V_grey)[1]<- "Photo.FW.V.ID"
head(FW_V_grey)

FW_V_grey_allEnv<- merge(FW_V_grey, logEnv,by="Photo.FW.V.ID")
dim(FW_V_grey_allEnv)
head(FW_V_grey_allEnv)

# reorder samples
FW_V_grey_allEnv <- FW_V_grey_allEnv[order(FW_V_grey_allEnv$Ind.num.cont),]
head(FW_V_grey_allEnv)

FW_V_grey_allEnv$Site.ID <- factor(FW_V_grey_allEnv$Site.ID, levels=c('WW','CS_CarvCopse','CS_MarDwn','CS_Snapes','MH','RHCP',"ES_HadRail","ES_Wolves","ES_StourW",'Ferm','Fox','CC_FennM','CC_BettiM','CC_LlanRocks','CC_LlanHill','CA','RW','WC','HPW','HRC', 'EC_SnuffML','EC_PrioryF','EC_WL','EC_HudWay','EC_HudWayCP','ARW','12-RW','CM','WLW','SodG','L','ACW','Garl','CCP','EM','YC','GB','CN_ShianW','CN_Glasdrum','Tay','AF','RF','SL','EN_Dunph','EN_DivV','EN_DavaW','EN_DrumW','CC','CB','Cdho','MW'), ordered=TRUE)


## Filter to sections of wings (whole and bottom 3rd) ####

FW_V_grey_wing <- FW_V_grey_allEnv[which(FW_V_grey_allEnv$wing_section=="whole_wing"),]
head(FW_V_grey_wing)
dim(FW_V_grey_wing)

FW_V_grey_bot3wing <- FW_V_grey_allEnv[which(FW_V_grey_allEnv$wing_section=="bot3rd"),]
head(FW_V_grey_bot3wing)
dim(FW_V_grey_bot3wing)

FW_V_grey_mid3wing <- FW_V_grey_allEnv[which(FW_V_grey_allEnv$wing_section=="mid3rd"),]
head(FW_V_grey_mid3wing)
dim(FW_V_grey_mid3wing)


### Filter out scale damage and replot ####

## remove highly damage individuals (Scale_dmg =4) ####
FW_V_grey_wing.filt4 <- FW_V_grey_wing[which(FW_V_grey_wing$Scale_dmg<4),]
dim(FW_V_grey_wing.filt4)
FW_V_grey_bot3wing.filt4 <- FW_V_grey_wing[which(FW_V_grey_bot3wing$Scale_dmg<4),]
dim(FW_V_grey_bot3wing.filt4)

## remove Scale_dmg of 3 & 4 ####
FW_V_grey_wing.filt3n4 <- subset(FW_V_grey_wing, Scale_dmg!=3 & Scale_dmg!=4)
dim(FW_V_grey_wing.filt3n4)

FW_V_grey_bot3wing.filt3n4 <- subset(FW_V_grey_bot3wing, Scale_dmg!=3 & Scale_dmg!=4)
dim(FW_V_grey_bot3wing.filt3n4)

### GLMMs of FW_V_grey mean value - following set up used for size ####

## Model mean grey value with variables of interest  ####

# Remove inds sampled in 2019 - incorecct temperature data for these
FW_V_grey_wing.filt4_new <- subset(FW_V_grey_wing.filt4, Yr.col!="2019")
dim(FW_V_grey_wing.filt4_new)

FW_V_grey_bot3wing.filt4_new <- subset(FW_V_grey_bot3wing.filt4, Yr.col!="2019")
dim(FW_V_grey_bot3wing.filt4_new)

# filt 3n4
FW_V_grey_wing.filt3n4_new <- subset(FW_V_grey_wing.filt3n4, Yr.col!="2019")
dim(FW_V_grey_wing.filt3n4_new)

FW_V_grey_bot3wing.filt3n4_new <- subset(FW_V_grey_bot3wing.filt3n4, Yr.col!="2019")
dim(FW_V_grey_bot3wing.filt3n4_new)

# Check correlation of explanatory variables
# look at explan variables
pairs(~Mean+Lat+mat.10yr+dev.tmp, data=FW_V_grey_wing.filt4_new, upper.panel=NULL)

par(mfrow=c(1,1))
M <- cor(FW_V_grey_wing.filt4_new[c(3,16,38,52)], method="pearson") #check columns used here
corrplot::corrplot(M,method = "number", type="lower")

# normal dist lmmer ####
hist(FW_V_grey_wing.filt4_new$Mean)
hist(FW_V_grey_bot3wing.filt4_new$Mean)


##### HW - Dorsal analysis #####
### Get data & format ####
## read in data
HW_D_grey <- read.csv("./imageJ/Results/Mean_grey/HW_D_mGV_per_3rd.csv")
head(HW_D_grey)

# change label to not have .png at end
# remove .png from Photo.HW.D.ID
HW_D_grey <- cbind(stringr::str_split_fixed(as.character(HW_D_grey$Label), stringr::fixed(".png:"), 2), HW_D_grey)
head(HW_D_grey)

# remove unwanted columns
HW_D_grey <- HW_D_grey[,c(1:2,4:9,14)]
colnames(HW_D_grey)[2]<- "wing_section"
colnames(HW_D_grey)[1]<- "Photo.HW.D.ID"
head(HW_D_grey)

HW_D_grey_allEnv<- merge(HW_D_grey, logEnv,by="Photo.HW.D.ID")
dim(HW_D_grey_allEnv)
head(HW_D_grey_allEnv)

# reorder samples
HW_D_grey_allEnv <- HW_D_grey_allEnv[order(HW_D_grey_allEnv$Ind.num.cont),]
head(HW_D_grey_allEnv)

HW_D_grey_allEnv$Site.ID <- factor(HW_D_grey_allEnv$Site.ID, levels=c('WW','CS_CarvCopse','CS_MarDwn','CS_Snapes','MH','RHCP',"ES_HadRail","ES_Wolves","ES_StourW",'Ferm','Fox','CC_FennM','CC_BettiM','CC_LlanRocks','CC_LlanHill','CA','RW','WC','HPW','HRC', 'EC_SnuffML','EC_PrioryF','EC_WL','EC_HudWay','EC_HudWayCP','ARW','12-RW','CM','WLW','SodG','L','ACW','Garl','CCP','EM','YC','GB','CN_ShianW','CN_Glasdrum','Tay','AF','RF','SL','EN_Dunph','EN_DivV','EN_DavaW','EN_DrumW','CC','CB','Cdho','MW'), ordered=TRUE)


## Filter to sections of wings (whole and bottom 3rd) ####

HW_D_grey_wing <- HW_D_grey_allEnv[which(HW_D_grey_allEnv$wing_section=="whole_wing"),]
head(HW_D_grey_wing)
HW_D_grey_bot3wing <- HW_D_grey_allEnv[which(HW_D_grey_allEnv$wing_section=="bot3rd"),]
head(HW_D_grey_bot3wing)
HW_D_grey_mid3wing <- HW_D_grey_allEnv[which(HW_D_grey_allEnv$wing_section=="mid3rd"),]
head(HW_D_grey_mid3wing)

### Filter out scale damage and replot ####

## remove highly damage individuals (Scale_dmg =4) ####
HW_D_grey_wing.filt4 <- HW_D_grey_wing[which(HW_D_grey_wing$Scale_dmg<4),]
dim(HW_D_grey_wing.filt4)
HW_D_grey_bot3wing.filt4 <- HW_D_grey_wing[which(HW_D_grey_bot3wing$Scale_dmg<4),]
dim(HW_D_grey_bot3wing.filt4)


## remove Scale_dmg of 3 & 4 ####
HW_D_grey_wing.filt3n4 <- subset(HW_D_grey_wing, Scale_dmg!=3 & Scale_dmg!=4)
dim(HW_D_grey_wing.filt3n4)

HW_D_grey_bot3wing.filt3n4 <- subset(HW_D_grey_bot3wing, Scale_dmg!=3 & Scale_dmg!=4)
dim(HW_D_grey_bot3wing.filt3n4)


### GLMMs of HW_D_grey mean value - following set up used for size ####

## Model Csize with variables of interest - filtered for 4  ####

# Remove inds sampled in 2019 - incorecct temperature data for these
HW_D_grey_wing.filt4_new <- subset(HW_D_grey_wing.filt4, Yr.col!="2019")
dim(HW_D_grey_wing.filt4_new)

HW_D_grey_bot3wing.filt4_new <- subset(HW_D_grey_bot3wing.filt4, Yr.col!="2019")
dim(HW_D_grey_bot3wing.filt4_new)

#filt 3n4
HW_D_grey_wing.filt3n4_new <- subset(HW_D_grey_wing.filt3n4, Yr.col!="2019")
dim(HW_D_grey_wing.filt3n4_new)

HW_D_grey_bot3wing.filt3n4_new <- subset(HW_D_grey_bot3wing.filt3n4, Yr.col!="2019")
dim(HW_D_grey_bot3wing.filt3n4_new)


# Check correlation of explanatory variables
# look at explan variables
pairs(~Mean+Lat+mat.10yr+dev.tmp, data=HW_D_grey_wing.filt4_new, upper.panel=NULL)

par(mfrow=c(1,1))
M <- cor(HW_D_grey_wing.filt4_new[c(3,16,38,52)], method="pearson") # check columns used here
corrplot::corrplot(M,method = "number", type="lower")

# normal dist lmmer ####
hist(HW_D_grey_wing.filt4_new$Mean)
hist(HW_D_grey_bot3wing.filt4_new$Mean)


##### HW - Ventral analysis #####
### Get data & format ####
## read in data
HW_V_grey <- read.csv("./imageJ/Results/Mean_grey/HW_V_mGV_per_3rd.csv")
HW_V_grey<-HW_V_grey[,-1]
head(HW_V_grey)

# change label to not have .png at end
# remove .png from Photo.HW.D.ID
HW_V_grey <- cbind(stringr::str_split_fixed(as.character(HW_V_grey$Label), stringr::fixed(".png:"), 2), HW_V_grey)
head(HW_V_grey)

# remove unwanted columns
HW_V_grey <- HW_V_grey[,c(1:2,4:9,13)]
colnames(HW_V_grey)[2]<- "wing_section"
colnames(HW_V_grey)[1]<- "Photo.HW.V.ID"
head(HW_V_grey)

HW_V_grey_allEnv<- merge(HW_V_grey, logEnv,by="Photo.HW.V.ID")
dim(HW_V_grey_allEnv)
head(HW_V_grey_allEnv)

# reorder samples
HW_V_grey_allEnv <- HW_V_grey_allEnv[order(HW_V_grey_allEnv$Ind.num.cont),]
head(HW_V_grey_allEnv)

HW_V_grey_allEnv$Site.ID <- factor(HW_V_grey_allEnv$Site.ID, levels=c('WW','CS_CarvCopse','CS_MarDwn','CS_Snapes','MH','RHCP',"ES_HadRail","ES_Wolves","ES_StourW",'Ferm','Fox','CC_FennM','CC_BettiM','CC_LlanRocks','CC_LlanHill','CA','RW','WC','HPW','HRC', 'EC_SnuffML','EC_PrioryF','EC_WL','EC_HudWay','EC_HudWayCP','ARW','12-RW','CM','WLW','SodG','L','ACW','Garl','CCP','EM','YC','GB','CN_ShianW','CN_Glasdrum','Tay','AF','RF','SL','EN_Dunph','EN_DivV','EN_DavaW','EN_DrumW','CC','CB','Cdho','MW'), ordered=TRUE)


## Filter to sections of wings (whole and bottom 3rd) ####

HW_V_grey_wing <- HW_V_grey_allEnv[which(HW_V_grey_allEnv$wing_section=="whole_wing"),]
head(HW_V_grey_wing)
HW_V_grey_bot3wing <- HW_V_grey_allEnv[which(HW_V_grey_allEnv$wing_section=="bot3rd"),]
head(HW_V_grey_bot3wing)
HW_V_grey_mid3wing <- HW_V_grey_allEnv[which(HW_V_grey_allEnv$wing_section=="mid3rd"),]
head(HW_V_grey_mid3wing)

### Filter out scale damage and replot ####

## remove highly damage individuals (Scale_dmg =4) ####
HW_V_grey_wing.filt4 <- HW_V_grey_wing[which(HW_V_grey_wing$Scale_dmg<4),]
dim(HW_V_grey_wing.filt4)
HW_V_grey_bot3wing.filt4 <- HW_V_grey_wing[which(HW_V_grey_bot3wing$Scale_dmg<4),]
dim(HW_V_grey_bot3wing.filt4)

## remove Scale_dmg of 3 & 4 ####
HW_V_grey_wing.filt3n4 <- subset(HW_V_grey_wing, Scale_dmg!=3 & Scale_dmg!=4)
dim(HW_V_grey_wing.filt3n4)

HW_V_grey_bot3wing.filt3n4 <- subset(HW_V_grey_bot3wing, Scale_dmg!=3 & Scale_dmg!=4)
dim(HW_V_grey_bot3wing.filt3n4)


### GLMMs of HW_V_grey mean value - following set up used for size ####

## Model Csize with variables of interest - filtered for 4  ####

# Remove inds sampled in 2019 - incorecct temperature data for these
HW_V_grey_wing.filt4_new <- subset(HW_V_grey_wing.filt4, Yr.col!="2019")
dim(HW_V_grey_wing.filt4_new)

HW_V_grey_bot3wing.filt4_new <- subset(HW_V_grey_bot3wing.filt4, Yr.col!="2019")
dim(HW_V_grey_bot3wing.filt4_new)

#filt 3n4
HW_V_grey_wing.filt3n4_new <- subset(HW_V_grey_wing.filt3n4, Yr.col!="2019")
dim(HW_V_grey_wing.filt3n4_new)

HW_V_grey_bot3wing.filt3n4_new <- subset(HW_V_grey_bot3wing.filt3n4, Yr.col!="2019")
dim(HW_V_grey_bot3wing.filt3n4_new)


# Check correlation of explanatory variables
# look at explan variables
pairs(~Mean+Lat+mat.10yr+dev.tmp, data=HW_V_grey_wing.filt4_new, upper.panel=NULL)

par(mfrow=c(1,1))
M <- cor(HW_V_grey_wing.filt4_new[c(3,16,38,52)], method="pearson")
corrplot::corrplot(M,method = "number", type="lower")

# normal dist lmmer ####
hist(HW_V_grey_wing.filt4_new$Mean)
hist(HW_V_grey_bot3wing.filt4_new$Mean)


### Run LMMs ####

## FW DORSAL ##
FW_D_grey_wing_lmer_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km++(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=FW_D_grey_wing.filt4_new)
summary(FW_D_grey_wing_lmer_reExpGrid_JDiff)

FW_D_grey_bot3wing_lmer_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km++(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=FW_D_grey_bot3wing.filt4_new)
summary(FW_D_grey_bot3wing_lmer_reExpGrid_JDiff)

## FOREWING VENTRAL ##
FW_V_grey_wing_lmer_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km++(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=FW_V_grey_wing.filt4_new)
summary(FW_V_grey_wing_lmer_reExpGrid_JDiff)

FW_V_grey_bot3wing_lmer_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km++(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=FW_V_grey_bot3wing.filt4_new)
summary(FW_V_grey_bot3wing_lmer_reExpGrid_JDiff)

## HINDWING DORSAL ##
HW_D_grey_wing_lmer_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km++(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=HW_D_grey_wing.filt4_new)
summary(HW_D_grey_wing_lmer_reExpGrid_JDiff)

HW_D_grey_bot3wing_lmer_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km++(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=HW_D_grey_bot3wing.filt4_new)
summary(HW_D_grey_bot3wing_lmer_reExpGrid_JDiff)

## HINDWING VENTRAL ###
HW_V_grey_wing_lmer_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km++(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=HW_V_grey_wing.filt4_new)
# model failed to converge
summary(HW_V_grey_wing_lmer_reExpGrid_JDiff)

HW_V_grey_bot3wing_lmer_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km++(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=HW_V_grey_bot3wing.filt4_new)
# model failed to converge
summary(HW_V_grey_bot3wing_lmer_reExpGrid_JDiff)

# Any optimisers that work across all???? ####

## FOREWING ##
#Dorsal
FW_D_filt4_diff_optims <- allFit(FW_D_grey_wing_lmer_reExpGrid_JDiff)
is.OK <- sapply(FW_D_filt4_diff_optims, is, "merMod")
FW_D_filt4_diff_optims.OK <- FW_D_filt4_diff_optims[is.OK]
lapply(FW_D_filt4_diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
# all ok

FW_D_filt4_botW_diff_optims <- allFit(FW_D_grey_bot3wing_lmer_reExpGrid_JDiff)
is.OK <- sapply(FW_D_filt4_botW_diff_optims, is, "merMod")
FW_D_filt4_botW_diff_optims.OK <- FW_D_filt4_diff_optims[is.OK]
lapply(FW_D_filt4_botW_diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
# all ok

# Ventral
FW_V_filt4_diff_optims <- allFit(FW_V_grey_wing_lmer_reExpGrid_JDiff)
is.OK <- sapply(FW_V_filt4_diff_optims, is, "merMod")
FW_V_filt4_diff_optims.OK <- FW_V_filt4_diff_optims[is.OK]
lapply(FW_V_filt4_diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
# all ok

FW_V_filt4_botW_diff_optims <- allFit(FW_V_grey_bot3wing_lmer_reExpGrid_JDiff)
is.OK <- sapply(FW_V_filt4_botW_diff_optims, is, "merMod")
FW_V_filt4_botW_diff_optims.OK <- FW_V_filt4_diff_optims[is.OK]
lapply(FW_V_filt4_botW_diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
# all ok

## HINDWING ##

#Dorsal
HW_D_filt4_diff_optims <- allFit(HW_D_grey_wing_lmer_reExpGrid_JDiff)
is.OK <- sapply(HW_D_filt4_diff_optims, is, "merMod")
HW_D_filt4_diff_optims.OK <- HW_D_filt4_diff_optims[is.OK]
lapply(HW_D_filt4_diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
# all ok

HW_D_filt4_botW_diff_optims <- allFit(HW_D_grey_bot3wing_lmer_reExpGrid_JDiff)
is.OK <- sapply(HW_D_filt4_botW_diff_optims, is, "merMod")
HW_D_filt4_botW_diff_optims.OK <- HW_D_filt4_diff_optims[is.OK]
lapply(HW_D_filt4_botW_diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
# all ok

# Ventral
HW_V_filt4_diff_optims <- allFit(HW_V_grey_wing_lmer_reExpGrid_JDiff)
is.OK <- sapply(HW_V_filt4_diff_optims, is, "merMod")
HW_V_filt4_diff_optims.OK <- HW_V_filt4_diff_optims[is.OK]
lapply(HW_V_filt4_diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
# singular? - BOB | Nelder_Mead | nlminbwrap 
# ok = nmkbw

HW_V_filt4_botW_diff_optims <- allFit(HW_V_grey_bot3wing_lmer_reExpGrid_JDiff)
is.OK <- sapply(HW_V_filt4_botW_diff_optims, is, "merMod")
HW_V_filt4_botW_diff_optims.OK <- HW_V_filt4_diff_optims[is.OK]
lapply(HW_V_filt4_botW_diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
# singular? - BOB | Nelder_Mead | nlminbwrap 
# ok = nmkbw

### Using nmkbw as optimiser ###

## FW DORSAL ##
FW_D_grey_wing_lmer_nmkbw_reExpGrid_JDiff <- lmer(Mean~mat.10yr+dev.tmp+years.col.10km+(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=FW_D_grey_wing.filt4_new, control = lmerControl(optimizer = "nmkbw"))
summary(FW_D_grey_wing_lmer_nmkbw_reExpGrid_JDiff)

FW_D_grey_bot3wing_lmer_nmkbw_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km+(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=FW_D_grey_bot3wing.filt4_new, control = lmerControl(optimizer = "nmkbw"))
summary(FW_D_grey_bot3wing_lmer_nmkbw_reExpGrid_JDiff)

## FOREWING VENTRAL ##
FW_V_grey_wing_lmer_nmkbw_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km+(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=FW_V_grey_wing.filt4_new, control = lmerControl(optimizer = "nmkbw"))
summary(FW_V_grey_wing_lmer_nmkbw_reExpGrid_JDiff)

FW_V_grey_bot3wing_lmer_nmkbw_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km+(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=FW_V_grey_bot3wing.filt4_new, control = lmerControl(optimizer = "nmkbw"))
summary(FW_V_grey_bot3wing_lmer_nmkbw_reExpGrid_JDiff)

## HINDWING DORSAL ##
HW_D_grey_wing_lmer_nmkbw_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km+(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=HW_D_grey_wing.filt4_new, control = lmerControl(optimizer = "nmkbw"))
summary(HW_D_grey_wing_lmer_nmkbw_reExpGrid_JDiff)

HW_D_grey_bot3wing_lmer_nmkbw_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km+(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=HW_D_grey_bot3wing.filt4_new, control = lmerControl(optimizer = "nmkbw"))
summary(HW_D_grey_bot3wing_lmer_nmkbw_reExpGrid_JDiff)

## HINDWING VENTRAL ###
HW_V_grey_wing_lmer_nmkbw_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km+(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=HW_V_grey_wing.filt4_new, control = lmerControl(optimizer = "nmkbw"))
summary(HW_V_grey_wing_lmer_nmkbw_reExpGrid_JDiff)

HW_V_grey_bot3wing_lmer_nmkbw_reExpGrid_JDiff <- lmer(Mean~Lat+mat.10yr+dev.tmp+years.col.10km+(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=HW_V_grey_bot3wing.filt4_new, control = lmerControl(optimizer = "nmkbw"))
summary(HW_V_grey_bot3wing_lmer_nmkbw_reExpGrid_JDiff)

## Plot forest plots for FW and HW model on same plot ####

## Plot forest plots for models on same plot 

lmer_forest <- plot_summs(FW_D_grey_wing_lmer_BOB_reExpGrid_JDiff,FW_V_grey_wing_lmer_BOB_reExpGrid_JDiff, HW_D_grey_wing_lmer_BOB_reExpGrid_JDiff, HW_V_grey_wing_lmer_BOB_reExpGrid_JDiff, scale = TRUE, plot.distributions = TRUE, coefs = c("Developmental temp"="dev.tmp", "Mean 10yr temp"="mat.10yr", "Latitude"="Lat"), colours=c("orange", "red", "blue", "green"))
lmer_forest

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.text=element_text(size=30),
        axis.title.x = element_text(size=40, face="bold"),
        axis.title.y = element_blank(),
        plot.margin=unit(c(1,1,1,1), "cm"))

lmer_forest + apatheme + theme(legend.position = "none")

tiff(filename = "./output/Manuscript_figs/MmGV_Lmer_forestplot.tiff",
     type="cairo",
     units="in",
     width=25,
     height=15,
     res=96)
lmer_forest + apatheme + theme(legend.position = "none")
dev.off()

## Mean grey value with Latitude boxplot ####
FW_mGv_lat <- FW_D_grey_wing.filt4_new[,c("Mean","Lat", "Expansion")]
head(FW_mGv_lat)
HW_mGv_lat <- HW_D_grey_wing.filt4_new[,c("Mean","Lat", "Expansion")]
head(HW_mGv_lat)


FW_mGV_lat_plot <- ggplot(FW_mGv_lat)+
  geom_boxplot(aes(x=Lat, y=Mean, group=as.factor(Lat), colour=Expansion), position=position_dodge(width=0), width=50/length(FW_mGv_lat$Lat))+
  scale_colour_manual(values = c("#7570B3","#E6AB02" ))+
  expand_limits(x=50.7)+
  labs(x="Latitude", y = "Mean grey value")+
  theme_classic()
FW_mGV_lat_plot

FW_mGV_lat_point <- ggplot(FW_mGv_lat, aes(x=Lat, y=Mean, group=Lat))+
  geom_point(aes(colour=Expansion))+
  scale_colour_manual(values = c("#7570B3","#E6AB02" ))+
  labs(x="Latitude", y = "Mean grey value")+
  theme_classic()
FW_mGV_lat_point

both_mGV <- gdata::combine(FW_mGv_lat,HW_mGv_lat)
colnames(both_mGV)[4] <- "wing"
head(both_mGV)

both_mGV[,2] <- round(both_mGV[,2],4)
head(both_mGV)

both_mGV <- both_mGV %>% 
  mutate(Lat1 = as.factor(Lat),
         Lat1_index = as.integer(Lat1))
head(both_mGV)

breaks <- both_mGV %>% 
  # e.g. plot first, every third and last tick
  mutate(ticks_to_plot = ((Lat1_index - 1) %% 8 == 0) | (Lat1_index == n_distinct(Lat1_index))) %>%
  filter(ticks_to_plot) %>% 
  pull(Lat1)

wing_names <- list(
  'FW_mGv_lat'="Forewing",
  'HW_mGv_lat'="Hindwing")

wing_labeller <- function(variable,value){
  return(wing_names[value])
}

fill <- c("#7570B3","#E6AB02" )

apatheme2=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.text = element_text(size=30),
        axis.text.x=element_text(size=28, hjust = 1),
        axis.title.x = element_text(size=40, face="bold"),
        axis.title.y = element_text(size=40, face="bold"),
        strip.text.x = element_text(size =40, face="bold"),
        plot.margin=unit(c(1,1,1,1), "cm"))

scaleFUN <- function(x) sprintf("%.2f", x)

mGV_lat_plot <- ggplot(both_mGV)+
  geom_boxplot(aes(x=Lat, y=Mean, group=as.factor(Lat), fill=Expansion), position=position_dodge(width=0), width=100/length(FW_mGv_lat$Lat))+
  scale_fill_manual(values=c("#7570B3","#E6AB02" ))+
  #scale_colour_manual(values = c("#7570B3","#E6AB02" ))+
  labs(x="Latitude", y = "Mean grey value")+
  theme_classic()+
  #scale_x_discrete(breaks = breaks)+
  facet_wrap(~wing, labeller = wing_labeller)

mGV_lat_plot + apatheme2 + scale_y_continuous(labels=scaleFUN) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", panel.spacing = unit(1, "inch")) +  scale_y_continuous(breaks = pretty_breaks(n=10))


tiff(filename = "./output/Manuscript_figs/mGV_Lat_boxplot1.tiff",
     type="cairo",
     units="in",
     width=50,
     height=30,
     res=96)
mGV_lat_plot + apatheme2 + scale_y_continuous(labels=scaleFUN) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", panel.spacing = unit(1.5, "inch")) +  scale_y_continuous(breaks = pretty_breaks(n=10))
dev.off()

## Table for publication ####


# FW whole wing
tab_model(FW_D_grey_wing_lmer_BOB_reExpGrid_JDiff,FW_V_grey_wing_lmer_BOB_reExpGrid_JDiff,  p.val = "kr", show.df = TRUE,show.stat = TRUE, show.se=TRUE,show.aic = TRUE,show.aicc=TRUE,digits = 3)

# FW bot wing
tab_model(FW_D_grey_bot3wing_lmer_BOB_reExpGrid_JDiff,FW_V_grey_bot3wing_lmer_BOB_reExpGrid_JDiff,  p.val = "kr", show.df = TRUE,show.stat = TRUE, show.se=TRUE,show.aic = TRUE,show.aicc=TRUE,digits = 3)


# HW
tab_model(HW_D_grey_wing_lmer_BOB_reExpGrid_JDiff,HW_V_grey_wing_lmer_BOB_reExpGrid_JDiff,  p.val = "kr", show.df = TRUE,show.stat = TRUE, show.se=TRUE,show.aic = TRUE,show.aicc=TRUE,digits = 3)

# hW bot wing
tab_model(HW_D_grey_bot3wing_lmer_BOB_reExpGrid_JDiff,HW_V_grey_bot3wing_lmer_BOB_reExpGrid_JDiff,  p.val = "kr", show.df = TRUE,show.stat = TRUE, show.se=TRUE,show.aic = TRUE,show.aicc=TRUE,digits = 3)
