## Investigate colour and brown patches - contrast??

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

#### FW Dorsal and format ####

### Get data ####
FW_D <- read.csv("./ImageJ/Results/Mean_grey/FW_D_bwnVscrm.csv")
head(FW_D)

# seperate name and part of wing

# change label to not have .png at end
# remove .png from Photo.FW.D.ID
FW_D <- cbind(stringr::str_split_fixed(as.character(FW_D$Label), stringr::fixed(".png:"), 2), FW_D)
head(FW_D)

# remove unwanted columns
FW_D <- FW_D[,c(1,2,5:9,16)]
colnames(FW_D)[1]<- "Photo.FW.D.ID"
colnames(FW_D)[2]<- "wing_section"
head(FW_D)

### Input environmental data ####
logEnv <- read.csv("./Data/logs/Morph_sampLog_env_Final.csv")
head(logEnv)

logEnv <- logEnv[,-c(1)]
head(logEnv)
dim(logEnv)

FW_D_allEnv<- merge(FW_D, logEnv,by="Photo.FW.D.ID")
dim(FW_D_allEnv)
head(FW_D_allEnv)

# reorder samples
FW_D_allEnv <- FW_D_allEnv[order(FW_D_allEnv$Ind.num.cont),]
head(FW_D_allEnv)

FW_D_allEnv$Site.ID <- factor(FW_D_allEnv$Site.ID, levels=c('WW','CS_CarvCopse','CS_MarDwn','CS_Snapes','MH','RHCP',"ES_HadRail","ES_Wolves","ES_StourW",'Ferm','Fox','CC_FennM','CC_BettiM','CC_LlanRocks','CC_LlanHill','CA','RW','WC','HPW','HRC', 'EC_SnuffML','EC_PrioryF','EC_WL','EC_HudWay','EC_HudWayCP','ARW','12-RW','CM','WLW','SodG','L','ACW','Garl','CCP','EM','YC','GB','CN_ShianW','CN_Glasdrum','Tay','AF','RF','SL','EN_Dunph','EN_DivV','EN_DavaW','EN_DrumW','CC','CB','Cdho','MW'), ordered=TRUE)

### Subsets of df ####

cream_FW_D <- FW_D_allEnv[FW_D_allEnv$wing_section=="cream",]
brown_FW_D <- FW_D_allEnv[FW_D_allEnv$wing_section=="brown",]
whole_FW_D <- FW_D_allEnv[FW_D_allEnv$wing_section=="wing",]

### Plot to investigate data ####

# Site
ggplot(aes(x=Site.ID, y=as.numeric(Mean), colour=wing_section), data=FW_D_allEnv)+
  geom_boxplot()+
  theme_bw()


# Latitude
ggplot(aes(x=Lat, y=as.numeric(Mean), colour=wing_section), data=FW_D_allEnv)+
  geom_point(aes(colour=wing_section))+
  geom_smooth(method="glm")+
  theme_bw()

# dev.tmp
ggplot(aes(x=dev.tmp, y=as.numeric(Mean), colour=wing_section), data=FW_D_allEnv)+
  geom_point(aes(colour=wing_section))+
  geom_smooth(method="glm")+
  theme_bw()


# mat.10yr
ggplot(aes(x=mat.10yr, y=as.numeric(Mean), colour=wing_section), data=FW_D_allEnv)+
  geom_point(aes(colour=wing_section))+
  geom_smooth(method="glm")+
  theme_bw()


### Standardise areas and replot #####
head(cream_FW_D)
cream_FW_D_area <- cream_FW_D[,c(1:3)]
brown_FW_D_area <- brown_FW_D[,c(1:3)]
wing_FW_D_area <- whole_FW_D[,c(1,3)]

# rename wing area 
colnames(wing_FW_D_area)[2] <- "tot.area"

cream_FW_D_area <- merge(cream_FW_D_area, wing_FW_D_area, by="Photo.FW.D.ID")
head(cream_FW_D_area)
brown_FW_D_area <- merge(brown_FW_D_area, wing_FW_D_area, by="Photo.FW.D.ID")
head(brown_FW_D_area)

# standardise to total wing
cream_FW_D_area$Area.stnd <- cream_FW_D_area$Area/cream_FW_D_area$tot.area
head(cream_FW_D_area)
brown_FW_D_area$Area.stnd <- brown_FW_D_area$Area/brown_FW_D_area$tot.area
head(brown_FW_D_area)

area_CnB <- rbind(cream_FW_D_area, brown_FW_D_area)
head(area_CnB)

# add info to df with cream and brown
CnB_FW_D <- subset(FW_D_allEnv, FW_D_allEnv$wing_section!="wing")
CnB_FW_D_new <- merge(CnB_FW_D, area_CnB, by=c("Photo.FW.D.ID", "wing_section"))
head(CnB_FW_D_new)

## now plot
ggplot(aes(x=Site.ID, y=as.numeric(Area.stnd), colour=wing_section), data=CnB_FW_D_new)+
  geom_boxplot()+
  theme_bw()
ggplot(aes(x=Lat, y=as.numeric(Area.stnd), colour=wing_section), data=CnB_FW_D_new)+
  geom_point()+
  geom_smooth(method="glm")+
  theme_bw()

### Ratio of cream to brown mean grey value? Or cream - brown ####

# get mean grey values
head(cream_FW_D)
cream_FW_D_mean <- cream_FW_D[,c(1,4)]
colnames(cream_FW_D_mean)[2]<- "cream_mean"
head(cream_FW_D_mean)

brown_FW_D_mean <- brown_FW_D[,c(1,4)]
colnames(brown_FW_D_mean)[2] <- "brown_mean"

CnB_FW_D_mean <- merge(cream_FW_D_mean, brown_FW_D_mean, by="Photo.FW.D.ID")
head(CnB_FW_D_mean)

## subtraction method 
CnB_FW_D_mean$subt.BfC <- CnB_FW_D_mean$cream_mean - CnB_FW_D_mean$brown_mean
head(CnB_FW_D_mean)

# add info to df with cream and brown
CnB_FW_D_new <- merge(CnB_FW_D_new, CnB_FW_D_mean, by="Photo.FW.D.ID")
head(CnB_FW_D_new)

## now plot - subtratcion method (only need one set)
cream <- subset(CnB_FW_D_new, wing_section=="cream")

# plot
ggplot(aes(x=Site.ID, y=as.numeric(subt.BfC), colour=Expansion), data=cream)+
  geom_boxplot()+
  theme_bw()
ggplot(aes(x=Lat, y=as.numeric(subt.BfC)), data=cream)+
  geom_point(aes(colour=Expansion))+
  geom_smooth(method="glm")+
  theme_bw()
ggplot(aes(x=dev.tmp, y=as.numeric(subt.BfC)), data=cream)+
  geom_point(aes(colour=Expansion))+
  geom_smooth(method="glm")+
  theme_bw()
ggplot(aes(x=mat.10yr, y=as.numeric(subt.BfC)), data=cream)+
  geom_point(aes(colour=Expansion))+
  geom_smooth(method="glm")+
  theme_bw()


## Filter on scale damage and year sampled ####
CnB_FW_D_filt <- subset(CnB_FW_D_new, Yr.col!="2019")

CnB_FW_D_filt <- subset(CnB_FW_D_filt, Scale_dmg!="4")

cream_filt <- subset(CnB_FW_D_filt, wing_section=="cream")
brown_filt <- subset(CnB_FW_D_filt, wing_section=="brown")

## Simple linear regression and ANCOVA for Area ####

pairs(~Area.stnd+Lat+mat.10yr+dev.tmp, data=cream_filt, upper.panel=NULL)

par(mfrow=c(1,1))
M <- cor(CnB_FW_D_new[c(18,40,54,57)], method="pearson")
corrplot::corrplot(M,method = "number", type="lower")


# Simple linear regression on each colour
cream_reg <- lm(Area.stnd~Lat, data=cream_filt)
summary(cream_reg)

brown_reg <- lm(Area.stnd~Lat, data=brown_filt)
summary(brown_reg)

#plot
plot(Area.stnd~Lat, data=CnB_FW_D_new, type='n')
points(cream_filt$Lat,cream_filt$Area.stnd, pch=20)
points(brown_filt$Lat,brown_filt$Area.stnd, pch=1)
abline(cream_reg, lty=1)
abline(brown_reg,lty=1)
legend("right", c("cream","brown"), lty=c(1,2), pch=c(20,1) )

# lm for both - nested
both_reg <- lm(Area.stnd~wing_section/Lat, data=CnB_FW_D_filt)
summary(both_reg)


## CONTRAST?? #####

subt_lm <- lm(subt.BfC~Lat+mat.10yr+dev.tmp, data=cream_filt)
summary(subt_lm)

subt_lmer <- lme4::lmer(subt.BfC~Lat+mat.10yr+dev.tmp+(1|Expansion/Grid.10km)+(1|Jday.diff), data=cream_filt)
isSingular(subt_lmer) # failed to converge
summary(as_lmerModLmerTest(subt_lmer))

diff_optims <- allFit(subt_lmer)

is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)


# apply nmkbw optimiser
subt_lmer_nmkbw <- lmer(subt.BfC~Lat+mat.10yr+dev.tmp+(1|Expansion/Grid.10km)+(1|Jday.diff), data=cream_filt, control = lmerControl(optimizer = "nmkbw"))
isSingular(subt_lmer_nmkbw) #its ok
summary(subt_lmer_nmkbw)

## Change in brown to cream mean grey value different? ####

BvC_mGV <- ggplot(aes(x=Lat, y=as.numeric(Mean), colour=wing_section), data=CnB_FW_D_filt)+
  geom_point()+
  geom_smooth(method="glm")+
  xlab("Latitude") + 
  ylab("Mean grey value")+
  scale_color_manual(labels = c("Brown", "Cream"), values = c('#E69F00', '#56B4E9'))+
  theme_bw()
BvC_mGV

colour_ancova <- aov(Mean~Lat*as.factor(wing_section), data=CnB_FW_D_filt)
summary(colour_ancova)


brown_mGV_lm <- lm(Mean~Lat, data=brown_filt)
summary(brown_mGV_lm)

cream_mGV_lm <- lm(Mean~Lat, data=cream_filt)
summary(cream_mGV_lm)

## Residuals of brown and cream - lightness linked between the two? ####

brown_resid <- resid(brown_mGV_lm)
head(brown_resid)

cream_resid <- resid(cream_mGV_lm)
head(cream_resid)

both_resid <- as.data.frame(cbind(cream_resid,brown_resid))
head(both_resid)


resid_BvC <- lm(brown_mGV_lm$residuals~cream_mGV_lm$residuals)
summary(resid_BvC)

BvC_resid <- ggplot(aes(x=brown_resid, y=cream_resid), data=both_resid)+
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Brown residuals") + 
  ylab("Cream residuals")+
  scale_color_manual(labels = c("Brown", "Cream"), values = c('#E69F00', '#56B4E9'))+
  theme_bw()
BvC_resid

# simple brown to cream

BvC <- lm(brown_mean~cream_mean, data=CnB_FW_D_filt)
summary(BvC)

BvC_simp <- ggplot(aes(x=brown_mean, y=cream_mean), data=CnB_FW_D_filt)+
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Brown mean grey value") + 
  ylab("Cream mean grey value")+
  theme_bw()
BvC_simp




### Plots for manuscript ####

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.text=element_text(size=30),
        axis.title.x = element_text(size=40, face="bold"),
        axis.title.y = element_text(size=40, face="bold"),
        plot.margin=unit(c(1,1,1,1), "cm"))

## Brown to cream area ####

B_area <- ggplot(aes(x=Lat, y=as.numeric(Area.stnd)), data=brown_filt)+
  geom_point(aes(col=Expansion))+
  geom_smooth(method="glm")+
  xlab("Latitude") + 
  ylab("Relative brown area")+
  theme_bw()
B_area

tiff(filename = "./output/Manuscript_figs/brown_area.tiff",
     type="cairo",
     units="in",
     width=15,
     height=15,
     res=96)
B_area + apatheme  + theme(legend.position = "none")
dev.off()

## Contrast between Brown and Cream ####
ggplot(aes(x=Lat, y=as.numeric(subt.BfC)), data=cream)+
  geom_point(aes(colour=Expansion))+
  geom_smooth(method="glm")+
  theme_bw()

BvC_contrast <- ggplot(aes(x=Lat, y=as.numeric(subt.BfC)), data=cream)+
  geom_point(aes(colour=Expansion, shape=Expansion))+
  scale_colour_manual(values = c("#7570B3","#E6AB02" ))+
  geom_smooth(method="glm", col="black")+
  xlab("Latitude") + 
  ylab("Contrast")+
  theme_bw()
BvC_contrast


tiff(filename = "./output/Manuscript_figs/contrast.tiff",
     type="cairo",
     units="in",
     width=15,
     height=15,
     res=96)
BvC_contrast + apatheme  + theme(legend.position = "none")
dev.off()

## Brown vs cream mean GV ####

tiff(filename = "./output/Manuscript_figs/brownVsCream_mGV.tiff",
     type="cairo",
     units="in",
     width=15,
     height=15,
     res=96)
BvC_mGV + apatheme  + theme(legend.position = "none")
dev.off()

## Brown vs Cream residuals ####
tiff(filename = "./output/Manuscript_figs/resid_BvC_mGV.tiff",
     type="cairo",
     units="in",
     width=15,
     height=15,
     res=96)
BvC_resid + apatheme  + theme(legend.position = "none")
dev.off()

tiff(filename = "./output/Manuscript_figs/simp_BvC_mGV.tiff",
     type="cairo",
     units="in",
     width=15,
     height=15,
     res=96)
BvC_simp + apatheme  + theme(legend.position = "none")
dev.off()
