#### Forewing and hindwing shape analysis for Pararge aegeria
### Eve Taylor-Cox
### 18/11/2019

## Aim of script - Investigate shape changes of FW and HW in PA
# 1. Multivariate multiple regression by marginal effect
# 2. Format data for 2B-PLS analysis in tpsPLS software


### Read in data ####
source("./Scripts/Geometric_morph/Morpho_data_format.R")


### Load libraries ####
library(lme4)
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
library(ggplot2)
library(RColorBrewer)
library(gdata)

### Edit landmark data input - no outlier pops or inds sampled in 2019 ####

# gdf.FW includes sites 12-RW and GB that have been removed from size analysis.
# Removed these sites from TPS file - input new tps file and carry our GPA

## input new landmarks file
FW_new <- readland.tps('./Photos/JPEG/Forewing/All_FW_noOut_no19.TPS', specID="imageID")
class(FW_new)
dim(FW_new)

HW_new <- readland.tps('./Photos/JPEG/Hindwing/All_HW_noOut_no19.TPS', specID="imageID")
class(HW_new)
dim(HW_new)

## Aplly GPA
FW_new_gpa <- gpagen(FW_new)
plot(FW_new_gpa)

HW_new_gpa <- gpagen(HW_new)
plot(HW_new_gpa)

## Match to classifier - remove 12-RW and those sampled in 2019
classifier_FW1 <- classifier_FW[classifier_FW$Site.ID!="12-RW",]
classifier_FW_new <- classifier_FW1[classifier_FW1$Site.ID!="GB",]
classifier_FW_new <- classifier_FW_new[classifier_FW_new$Yr.col!="2019",]

# for HW use the filtered version that does not include the four outliers - for investigation see Morpho_data_format.R
classifier_HW_filt1 <- classifier_HW_filt[classifier_HW_filt$Site.ID!="12-RW",]
classifier_HW_filt_new <- classifier_HW_filt1[classifier_HW_filt1$Site.ID!="GB",]
classifier_HW_filt_new <- classifier_HW_filt_new[classifier_HW_filt_new$Yr.col!="2019",]

## Make new geomorph dataframes
gdf.FW_new <- geomorph.data.frame(FW_new_gpa, samp=classifier_FW_new$Photo.FW.D.ID, Site=classifier_FW_new$Site.ID, Gird.10km=classifier_FW_new$Grid.10km, expn=classifier_FW_new$Expansion, site.lat=as.factor(classifier_FW_new$Lat), site.lng=as.factor(classifier_FW_new$Lng) , rec.1st.10km = as.factor(classifier_FW_new$X10km.1st.rec), site.1st.rec=as.factor(classifier_FW_new$site.1st.rec), yrs.col.10km=as.factor(classifier_FW_new$years.col.10km), yrs.col.site=as.factor(classifier_FW_new$years.col.site), met.x.1km=as.factor(classifier_FW_new$met.x), met.y.1km=as.factor(classifier_FW_new$met.y), scale_dmg=as.factor(classifier_FW_new$Scale_dmg), dmg=as.factor(classifier_FW_new$Dmg), age=as.factor(classifier_FW_new$Age), mat.10yr=as.factor(classifier_FW_new$mat.10yr),dev.tmp=as.factor(classifier_FW_new$dev.tmp))
summary(gdf.FW_new)


gdf.HW_filt_new <- geomorph.data.frame(HW_new_gpa, samp=classifier_HW_filt_new$Photo.HW.D.ID, Site=classifier_HW_filt_new$Site.ID, Gird.10km=classifier_HW_filt_new$Grid.10km, expn=classifier_HW_filt_new$Expansion, site.lat=as.factor(classifier_HW_filt_new$Lat), site.lng=as.factor(classifier_HW_filt_new$Lng) , rec.1st.10km = as.factor(classifier_HW_filt_new$X10km.1st.rec), site.1st.rec=as.factor(classifier_HW_filt_new$site.1st.rec), yrs.col.10km=as.factor(classifier_HW_filt_new$years.col.10km), yrs.col.site=as.factor(classifier_HW_filt_new$years.col.site), met.x.1km=as.factor(classifier_HW_filt_new$met.x), met.y.1km=as.factor(classifier_HW_filt_new$met.y), scale_dmg=as.factor(classifier_HW_filt_new$Scale_dmg), dmg=as.factor(classifier_HW_filt_new$Dmg), age=as.factor(classifier_HW_filt_new$Age), mat.10yr=as.factor(classifier_HW_filt_new$mat),dev.tmp=as.factor(classifier_HW_filt_new$dev.tmp))
summary(gdf.HW_filt_new)

#### EXPLORATORY ####
### FW analysis ####
# PCA on geomorph data ####

# GPA alignment = FW_gpa
gp <- as.factor(paste(classifier_FW_new$Expansion))

# Carry out PCA and plot TPS along axis
col.gp <- rainbow(length(levels(gp)))
names(col.gp)<-levels(gp)

PCA <- plotTangentSpace(FW_new_gpa$coords, verbose=T)
PCA$pc.summary$importance

xlab <- paste("Principle component 1", " (", round(PCA$pc.summary$importance[2,1]*100,1), "%)",sep="")
ylab <- paste("Principle component 2", " (", round(PCA$pc.summary$importance[2,2]*100,1),"%)", sep="")

mat <- matrix(c(4,5,0,1,1,2,1,1,3), 3)
layout(mat, widths=c(1,1,1), heights=c(1,1,0.6))# set the size of the rows and columns
# Item 1 to plot, the graph of PC1 vs PC2
par(mar=c(4, 4, 1, 1)) # sets the margins

plot(PCA$pc.scores[,1], PCA$pc.scores[,2], pch=21, cex=2, bg=col.gp, xlab=xlab, ylab=ylab, asp=T)
legend(0.07, 0.08, legend = unique(gp), pch=19, col=unique(col.gp))

# add TPS to demonstrate shape changes along each axis
ref <- mshape(FW_gpa$coords)

par(mar = c(0,0,0,0)) # sets the margins
plotRefToTarget(ref,PCA$pc.shapes$PC1min, mag=3)
plotRefToTarget(ref, PCA$pc.shapes$PC1max, mag=3)
plotRefToTarget(ref, PCA$pc.shapes$PC2min, mag=3)
plotRefToTarget(ref, PCA$pc.shapes$PC2max, mag=3)


## run diff PCA and extract PCs
gp <- gdf.FW_new$Site
FWshape_pca <- gm.prcomp(gdf.FW_new$coords)
summary(FWshape_pca)
plot(FWshape_pca, col=gp)

names(FWshape_pca)
# x= PC scores for all specimens
# d= Singluar values for decomposed VCV matrix
# rotation = matrix of variable loadings
# shapes = list with shape coords of extreme ends of all PC axes

FWshape_PC10 <- FWshape_pca$x[,1:10]
FWshape_PC10

FWshape_PC6 <- FWshape_pca$x[,1:6]
FWshape_PC6

# Environmental data and PCA ####
summary(gdf.FW_new)
FW_Csize <- gdf.FW_new[["Csize"]]
head(FW_Csize)
FW_sample <- gdf.FW_new[["samp"]]
FW_site <- gdf.FW_new[["Site"]]
FW_grid.10km <- gdf.FW_new[["Grid.10km"]]
FW_year.col.10km <- gdf.FW_new[["yrs.col.10km"]]
FW_dev.tmp <- gdf.FW_new[["dev.tmp"]]
FW_mat.10yr <- gdf.FW_new[["mat.10yr"]]
FW_Lat <- gdf.FW_new[["site.lat"]]
FW_Long <- gdf.FW_new[["site.lng"]]

FW_env_vars <- data.frame(sample=FW_sample, site=FW_site, years.col.10km=as.numeric(FW_year.col.10km), dev.tmp=FW_dev.tmp, lat=FW_Lat, mat.10yr=FW_mat.10yr)
head(FW_env_vars)

dim(FW_env_vars)
dim(classifier_FW_new)

rownames(FW_env_vars)<- FW_env_vars[,1]
FW_env_vars$mat.10yr <- as.numeric(as.character(FW_env_vars$mat.10yr))
FW_env_vars$dev.tmp <- as.numeric(as.character(FW_env_vars$dev.tmp))
FW_env_vars$lat <- as.numeric(as.character(FW_env_vars$lat))
head(FW_env_vars)


# PCA 
FW_env_pca <- prcomp(FW_env_vars[,3:6], scale=TRUE)
summary(FW_env_pca)
par(mfrow=c(1,1))
screeplot(FW_env_pca)

fviz_pca_var(FW_env_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

FWenv_PC1 <- FW_env_pca$x[,1]
FWenv_PC1
FWenv_PC2 <- FW_env_pca$x[,2]
FWenv_PC2


### HW analysis ####
# PCA on geomorph data ####

# GPA alignment = HW_gpa
gp <- as.factor(paste(classifier_HW_filt_new$Expansion))

# Carry out PCA and plogdf.HW_filt_newt TPS along axis
col.gp <- rainbow(length(levels(gp)))
names(col.gp)<-levels(gp)

PCA <- plotTangentSpace(HW_new_gpa$coords, verbose=T)
PCA$pc.summary$importance

xlab <- paste("Principle component 1", " (", round(PCA$pc.summary$importance[2,1]*100,1), "%)",sep="")
ylab <- paste("Principle component 2", " (", round(PCA$pc.summary$importance[2,2]*100,1),"%)", sep="")

mat <- matrix(c(4,5,0,1,1,2,1,1,3), 3)
layout(mat, widths=c(1,1,1), heights=c(1,1,0.6))# set the size of the rows and columns
# Item 1 to plot, the graph of PC1 vs PC2
par(mar=c(4, 4, 1, 1)) # sets the margins

plot(PCA$pc.scores[,1], PCA$pc.scores[,2], pch=21, cex=2, bg=col.gp, xlab=xlab, ylab=ylab, asp=T)
legend(0.07, 0.08, legend = unique(gp), pch=19, col=unique(col.gp))

# add TPS to demonstrate shape changes along each axis
ref <- mshape(HW_gpa$coords)

par(mar = c(0,0,0,0)) # sets the margins
plotRefToTarget(ref,PCA$pc.shapes$PC1min, mag=3)
plotRefToTarget(ref, PCA$pc.shapes$PC1max, mag=3)
plotRefToTarget(ref, PCA$pc.shapes$PC2min, mag=3)
plotRefToTarget(ref, PCA$pc.shapes$PC2max, mag=3)


## run diff PCA and extract PCs
gp <- gdf.HW_filt_new$Site
HWshape_pca <- gm.prcomp(gdf.HW_filt_new$coords)
summary(HWshape_pca)
plot(HWshape_pca, col=gp)

names(HWshape_pca)
# x= PC scores for all specimens
# d= Singluar values for decomposed VCV matrix
# rotation = matrix of variable loadings
# shapes = list with shape coords of extreme ends of all PC axes

HWshape_PC10 <- HWshape_pca$x[,1:10]
HWshape_PC10

HWshape_PC6 <- HWshape_pca$x[,1:6]
HWshape_PC6

# Environmental data and PCA ####
summary(gdf.HW_filt_new)
HW_Csize <- gdf.HW_filt_new[["Csize"]]
head(Csize)
HW_sample <- gdf.HW_filt_new[["samp"]]
HW_site <- gdf.HW_filt_new[["Site"]]
HW_grid.10km <- gdf.HW_filt_new[["Grid.10km"]]
HW_year.col.10km <- gdf.HW_filt_new[["yrs.col.10km"]]
HW_dev.tmp <- gdf.HW_filt_new[["dev.tmp"]]
HW_mat.10yr <- gdf.HW_filt_new[["mat.10yr"]]
HW_Lat <- gdf.HW_filt_new[["site.lat"]]
HW_Long <- gdf.HW_filt_new[["site.lng"]]

HW_env_vars <- data.frame(sample=HW_sample, site=HW_site, years.col.10km=as.numeric(HW_year.col.10km), dev.tmp=HW_dev.tmp, lat=HW_Lat, mat.10yr=HW_mat.10yr)
head(HW_env_vars)

dim(HW_env_vars)
dim(classifier_HW_filt_new)

rownames(HW_env_vars)<- HW_env_vars[,1]
HW_env_vars$mat.10yr <- as.numeric(as.character(HW_env_vars$mat.10yr))
HW_env_vars$dev.tmp <- as.numeric(as.character(HW_env_vars$dev.tmp))
HW_env_vars$lat <- as.numeric(as.character(HW_env_vars$lat))

# PCA 
HW_env_pca <- prcomp(HW_env_vars[,3:6], scale=TRUE)
summary(HW_env_pca)
par(mfrow=c(1,1))
screeplot(HW_env_pca)

fviz_pca_var(HW_env_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

HWenv_PC1 <- HW_env_pca$x[,1]
HWenv_PC1

HWenv_PC2 <- HW_env_pca$x[,2]
HWenv_PC2

##### 2B Partial Least squares anaylysis - data format to input to MorphoJ and tpsPLS32 ######

# Environmental data needs to be in .NTS file format.
# This is basically a matrix format with a line for info.
# second line: 1 (rectangular matrix) n (number of specimens) v (number of variables) 0/1 -999 (complete/missing_value -999)
# in this case FW = 1 706 4 0 ; HW = 1 634 4 0

dim(FW_env_vars)
head(FW_env_vars)
FW_env_vars.new <- FW_env_vars[,-c(1:2)]
head(FW_env_vars.new)

write.table(FW_env_vars.new, "./Data/MorphoJ/FW_Selected_env_data_MorphoJ.txt", sep = "\t", quote=FALSE, row.names = TRUE)
write.table(FW_env_vars.new, "./Data/MorphoJ/FW_Selected_env_data_tpsPLS.txt", sep = "\t", quote=FALSE, row.names = FALSE)

# this is edited in notepad++ to be in the correct nts format and then saved as a nts file for analysis in tpsPLLS

dim(HW_env_vars)
head(HW_env_vars)
HW_env_vars.new <- HW_env_vars[,-c(1:2)]
head(HW_env_vars.new)

write.table(HW_env_vars.new, "./data/HW_Selected_env_data_MorphoJ.txt", sep = "\t", quote=FALSE, row.names = TRUE)
write.table(HW_env_vars.new, "./data/HW_Selected_env_data_tpsPLS.txt", sep = "\t", quote=FALSE, row.names = FALSE)
# this is edited in notepad++ to be in the correct nts format and then saved as a nts file for analysis in tpsPLLS

### Produce plots for analysis in morphoJ ####

## Allometry analysis - plot Regression scores by log Csize ####

## FW
FW_regSc <- read.delim("./MorphoJ/Output_datasets/FW_regression_allom_score.txt")
head(FW_regSc)
FW_CS <- read.delim("./MorphoJ/Output_datasets/FW_Csize.txt")
head(FW_CS)

# two Id / ID columns - remove one from both dfs
FW_regSc <- FW_regSc[,-5]
head(FW_regSc)
FW_CS <- FW_CS[,-5]
head(FW_CS)

# merge df on Id
FW_allom <- merge(FW_regSc, FW_CS, by=c("Id","Site", "Grid.10km", "Expansion"))
head(FW_allom)

# Plot reg score and log centroid size using ggplot
FW_allom_plot <- ggplot(FW_allom, aes(x=Log.Centroid.Size, y=RegScore1))+
  geom_point(aes(col=Expansion, shape=Expansion), size=5, show.legend = TRUE)+
  geom_smooth(method="lm", se=TRUE, col="black")+
  labs(title="Forewing regression score as a function of log centroid size",x="Centroid size (log)", y = "Regression score")+
  scale_colour_manual(values = c("#7570B3","#E6AB02" ))+
  theme_classic()
FW_allom_plot + theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"), axis.title = element_text(size = 14), axis.text = element_text(size = 12), legend.title= element_text(face="bold", size=12), legend.text = element_text(size=12))

## HW
HW_regSc <- read.delim("./morphoJ/Output_datasets/HW_regression_allom_score.txt")
head(HW_regSc)
HW_CS <- read.delim("./morphoJ/Output_datasets/HW_Csize.txt")
head(HW_CS)

# two Id / ID columns - remove one from both dfs
HW_regSc <- HW_regSc[,-5]
head(HW_regSc)
HW_CS <- HW_CS[,-5]
head(HW_CS)

# merge df on Id
HW_allom <- merge(HW_regSc, HW_CS, by=c("Id","Site", "Grid.10km", "Expansion"))
head(HW_allom)

# Plot reg score and log centroid size using ggplot
HW_allom_plot <- ggplot(HW_allom, aes(x=Log.Centroid.Size, y=RegScore1))+
  geom_point(aes(col=Expansion, shape=Expansion), size=5, show.legend = TRUE)+
  geom_smooth(method="lm", se=TRUE, col="black")+
  scale_colour_manual(values = c("#7570B3","#E6AB02" ))+
  labs(title="Hindwing regression score as a function of log centroid size",x="Centroid size (log)", y = "Regression score")+
  theme_classic()
HW_allom_plot + theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"), axis.title = element_text(size = 14), axis.text = element_text(size = 12), legend.title= element_text(face="bold", size=12), legend.text = element_text(size=12))


## Joint plot?

wings_allom<-combine(FW_allom, HW_allom)
head(wings_allom)

wing_names <- c(
  `FW_allom` = "Forewing",
  `HW_allom` = "Hindwing")

col_drk <- c("#7570B3","#E6AB02")
col_lght <- c("#D55E00", "#0072B2")


allom_plot <- ggplot(wings_allom, aes(x=Log.Centroid.Size, y=RegScore1))+
  geom_point(aes(col=Expansion, shape=Expansion), size=5, show.legend = TRUE)+
  scale_colour_manual(values = c("#7570B3","#E6AB02" ))+
  geom_smooth(method="lm", se=TRUE, col="black")+
  labs(x="Centroid size (log)", y = "Regression score")+
  facet_grid(.~source, labeller = as_labeller(wing_names), scales = "free")+
  theme_classic()
allom_plot + theme(axis.title = element_text(size = 30), axis.text = element_text(size = 25), legend.title= element_text(face="bold", size=30), legend.text = element_text(size=25), strip.text.x = element_text(size=30, face="bold"))


apatheme2=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.text=element_text(size=30),
        axis.title.x = element_text(size=40, face="bold"),
        axis.title.y = element_text(size=40, face="bold"),
        strip.text.x = element_text(size =40, face="bold"),
        plot.margin=unit(c(1,1,1,1), "cm"))

scaleFUN <- function(x) sprintf("%.2f", x)


allom_plot + apatheme2 + theme(axis.title = element_text(size = 30), axis.text = element_text(size = 25), legend.position = "none", strip.text.x = element_text(size=30, face="bold"))

# make png
png(filename = "./output/Manuscript_figs/bothWings_allom.png",
    type="cairo",
    units="in",
    width=30,
    height=15,
    res=96)

allom_plot + apatheme2 + theme(axis.title = element_text(size = 30), axis.text = element_text(size = 25), legend.position = "none", strip.text.x = element_text(size=30, face="bold"))

dev.off()

# make tiff
tiff(filename = "./output/Manuscript_figs/bothWings_allom.tiff",
     type="cairo",
     units="in",
     width=30,
     height=15,
     res=96)

allom_plot + apatheme2 + theme(axis.title = element_text(size = 30), axis.text = element_text(size = 25), legend.position = "none", strip.text.x = element_text(size=30, face="bold"))

dev.off()

## Comparison of 2B-PLS analysis of FW and HW ####

## Plot Block1PLS1 - Block2PLS2 NO GROUP ####
PLS_FW_noGr <- read.delim("./MorphoJ/Output_datasets/PLS_FW_allomResid_noGroup scores.txt")
PLS_HW_noGr <- read.delim("./MorphoJ/Output_datasets/PLS_HW_allomResid_noGroup scores.txt")

head(PLS_FW_noGr)
head(PLS_HW_noGr)

PLS_FW_noGr <- PLS_FW_noGr[,-1]
PLS_HW_noGr <- PLS_HW_noGr[,-1]

PLS_noGr <- combine(PLS_FW_noGr, PLS_HW_noGr)
head(PLS_noGr)

## Block1 PLS- Block2 PLS 1  ####
wing_names <- c(
  `PLS_FW_noGr` = "Forewing",
  `PLS_HW_noGr` = "Hindwing")

# colour
library(RColorBrewer)
n <- 41
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n)) 

grid.cols <- col_vector
col_drk <- c("#1B9E77", "#D95F02")
col_lght <- c("#D55E00", "#0072B2")

PLS1_plot <- ggplot(PLS_noGr, aes(x=Block1PLS1, y=Block2PLS1))+
  geom_point(aes(col=Expansion, shape=Expansion), size=5,stroke=5, show.legend = TRUE)+
  geom_smooth(method="glm", se=TRUE, col="black")+
  scale_colour_manual(values = c("#7570B3","#E6AB02" ))+
  labs(x="\n2B-PLS 1 Block 1 (shape)", y = "\n2B-PLS 1 Block 2 (env. vars)")+
  facet_grid(.~source, labeller = as_labeller(wing_names), scales = "free")+
  theme_classic()
PLS1_plot + theme(axis.title = element_text(size = 24), axis.text = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size=22, face="bold"), panel.spacing = unit(2, "lines"))

# make tiff
tiff(filename = "./output/Manuscript_figs/bothWings_PLS1.tiff",
     type="cairo",
     units="in",
     width=50,
     height=35,
     res=96)

PLS1_plot  + apatheme2 + theme(axis.title = element_text(size = 60, face="bold"), axis.text = element_text(size = 50), legend.position = "none", strip.text.x = element_text(size=60, face="bold"), panel.spacing = unit(4, "inch"))

dev.off()
