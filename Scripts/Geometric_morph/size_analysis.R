#### Forewing and hindwing size analysis for Pararge aegeria
### Eve Taylor-Cox
### 18/11/2019

## Aim of script - Investigate size changes of FW and HW in PA

### Read in data ####
source("./Scripts/Geometric_morph/Morpho_data_format.R")

### Load libraries required ####
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

### Forewing analysis ####

## Variables of interest: 1) pop / Grid.10km; 2) Latitude; 3) Age of site; 4) mat (10yrs); 5) mean temp of selected months

# Can not include 2019 data as do not have developmental temps for 2019.

### Model Csize with variables of interest  ####

## Remove GB 2018 inds and 12-RW from analysis as outliers
FW_sizeDF_new <- subset(FW_sizeDF, Site.ID!="12-RW")
FW_sizeDF_new <- subset(FW_sizeDF_new, Site.ID!="GB")

# Remove inds sampled in 2019
FW_sizeDF_new <- subset(FW_sizeDF_new, Yr.col!="2019")
dim(FW_sizeDF_new)

# Check correlation of explanatory variables
# look at explan variables
pairs(~Lat+years.col.10km+mat.10yr+dev.tmp, data=FW_sizeDF_new, upper.panel=NULL, labels = c("Latitude", "Years colonised","T10", "Temp. during \n development"), pch=16)
# export as tiff
tiff(filename = "./output/Manuscript_figs/pairs_plotr.tiff",
     type="cairo",
     units="in",
     width=15,
     height=15,
     res=96)
pairs(~Lat+years.col.10km+mat.10yr+dev.tmp, data=FW_sizeDF_new, upper.panel=NULL, labels = c("Latitude", "Years colonised","T10", "Temp. during \n development"), pch=16, cex.labels=4, cex.axis=2, cex=2)
dev.off()


par(mfrow=c(1,1))
M <- cor(FW_sizeDF_new[c(13,22,36,49)], method="pearson") # ensure these are the correct columns
# edit names for publication
colnames(M) <- c("Latitude", "Years colonised", "T10", "Temp. during \n development")
rownames(M) <- c("Latitude", "Years colonised", "T10", "Temp. during \n development")

corrplot::corrplot(M,method = "number", type="lower")

# export as tiff
tiff(filename = "./output/Manuscript_figs/pear_corr.tiff",
     type="cairo",
     units="in",
     width=15,
     height=15,
     res=96)
par(cex=2)
corrplot::corrplot(M,method = "number", type="lower", tl.col = "black")
dev.off()

# combine these two plots using inkscape for a supplementary figure.


# what probabilty best fits my data? ####

# distribution must be non 0
FW_sizeDF_new$Csize.t <- FW_sizeDF_new$Csize +1

# norm distribtion
qqp(FW_sizeDF_new$Csize, "norm")

# Log normal distribution
qqp(FW_sizeDF_new$Csize, "lnorm") # bad fit

# gamma
gamma <- fitdistr(FW_sizeDF_new$Csize, "gamma")
qqp(FW_sizeDF_new$Csize, "gamma", shape=gamma$estimate[[1]], rate=gamma$estimate[[2]])

# best fit = normal (?)

# normal dist lmmer ####
hist(FW_sizeDF_new$Csize)

# Model Csize~Lat+tmpSelMnths+years.col.10km+volt_num+(1|Expansion/Grid.10km)+(1|Jday.diff)
FW_lmer_reExpGrid.JDiff <- lmer(Csize~Lat+years.col.10km+mat.10yr+dev.tmp+(1|Expansion/Grid.10km)+(1|Jday.diff), REML=TRUE, data=FW_sizeDF_new)
summary(FW_lmer_reExpGrid.JDiff)

FW_lmer_reExpGrid.JDiff_bob <- lmer(Csize~Lat+years.col.10km+mat.10yr+dev.tmp+(1|Expansion/Grid.10km)+(1|Jday.diff),REML=TRUE, data=FW_sizeDF_new, control = lmerControl(optimizer = "bobyqa"))
summary(FW_lmer_reExpGrid.JDiff_bob)


library("lmerTest")
anova(as_lmerModLmerTest(FW_lmer_reExpGrid.JDiff_bob, tol = 1e-08), type="II", ddf= "Satterthwaite")


### Hindwing analysis ####

### Model Csize with variables of interest 2018 env data ####

## Remove GB 2018 inds and 12-RW from analysis
HW_sizeDF_new <- subset(HW_sizeDF, Site.ID!="12-RW")
HW_sizeDF_new <- subset(HW_sizeDF_new, Site.ID!="GB")

# Remove inds sampled in 2019
HW_sizeDF_new <- subset(HW_sizeDF_new, Yr.col!="2019")
dim(HW_sizeDF_new)

# Check correlation of explanatory variables
# look at explan variables
pairs(~Csize+Lat+years.col.10km+mat.10yr+dev.tmp+volt_num, data=HW_sizeDF_new, upper.panel=NULL)

par(mfrow=c(1,1))
M_HW <- cor(HW_sizeDF_new[c(12,21,35,41,49)], method="pearson")
corrplot::corrplot(M_HW,method = "number", type="lower")

# what probabilty best fits my data? ####

# distribution must be non 0
HW_sizeDF_new$Csize.t <- HW_sizeDF_new$Csize +1

# norm distribtion
qqp(HW_sizeDF_new$Csize, "norm")

# Log normal distribution
qqp(HW_sizeDF_new$Csize, "lnorm") # bad fit

# gamma
gamma <- fitdistr(HW_sizeDF_new$Csize, "gamma")
qqp(HW_sizeDF_new$Csize, "gamma", shape=gamma$estimate[[1]], rate=gamma$estimate[[2]])

# best fit = normal (?)

# normal dist lmmer ####
hist(HW_sizeDF_new$Csize)

# Model Csize~Lat+tmpSelMnths+years.col.10km+(1|Expansion/Grid.10km)+(1|Jday.diff)
HW_lmer_reExpGrid.JDiff <- lmer(Csize~Lat+years.col.10km+mat.10yr+dev.tmp+(1|Expansion/Grid.10km)+(1|Jday.diff),REML=TRUE, data=HW_sizeDF_new)
summary(HW_lmer_reExpGrid.JDiff) # fails to converge

# try diff optimiser
diff_optims <- allFit(HW_lmer_reExpGrid.JDiff)

is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)

HW_lmer_reExpGrid.JDiff_bob <- lmer(Csize~Lat+years.col.10km+mat.10yr+dev.tmp+(1|Expansion/Grid.10km)+(1|Jday.diff),REML=TRUE, data=HW_sizeDF_new, control = lmerControl(optimizer = "bobyqa"))
summary(HW_lmer_reExpGrid.JDiff_bob)



## Investigate models and make plots/ tables for publication ####

## Plot forest plots for models on same plot 

lmer_forest <- plot_summs(FW_lmer_reExpGrid.JDiff_bob,HW_lmer_reExpGrid.JDiff_bob, scale = TRUE, plot.distributions = TRUE, coefs = c("Developmental temp"="dev.tmp", "Mean 10yr temp"="mat.10yr","Years colonised"="years.col.10km", "Latitude"="Lat"))

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

tiff(filename = "./output/Manuscript_figs/Lmer_forestplot.tiff",
     type="cairo",
     units="in",
     width=25,
     height=15,
     res=96)
lmer_forest + apatheme + theme(legend.position = "none")
dev.off()

## CSize with latitude ####
FW_lat <- FW_sizeDF_new[,c("Csize","Lat", "Expansion")]
head(FW_lat)
HW_lat <- HW_sizeDF_new[,c("Csize", "Lat", "Expansion")]
head(HW_lat)

both_lat <- gdata::combine(FW_lat, HW_lat)
colnames(both_lat)[4] <- "wing"
head(both_lat)

wing_names <- list(
  'FW_lat'="Forewing",
  'HW_lat'="Hindwing")

wing_labeller <- function(variable,value){
  return(wing_names[value])
}

# plot
lat_plot <- ggplot(both_lat, aes(x=Lat, y=Csize))+
  geom_point(aes(colour=Expansion))+
  geom_smooth(method='glm')+
  labs(x="Latitude", y = "Centroid size")+
  theme_classic()+
  facet_wrap(~wing, labeller = wing_labeller)

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

lat_plot + apatheme2 + scale_y_continuous(labels=scaleFUN) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


tiff(filename = "./output/Manuscript_figs/CSize_Lat.tiff",
     type="cairo",
     units="in",
     width=15,
     height=15,
     res=96)
lat_plot + apatheme2 + scale_y_continuous(labels=scaleFUN) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", panel.spacing = unit(0.5, "inch"))
dev.off()



## Table for both models
tab_model(FW_lmer_reExpGrid.JDiff_bob, HW_lmer_reExpGrid.JDiff_bob,  p.val = "kr", show.df = TRUE, show.se=TRUE,show.aic = TRUE,show.aicc=TRUE,digits = 4) # uses conditional F-tests with Kenward-Roger approximation for the degrees of freedom (using the using the pbkrtest-package)


#### Voltinism on centroid size ####

head(FW_sizeDF_new)

FW_Csize_Volt <- ggplot(FW_sizeDF_new, aes(x=as.factor(volt_num), y=Csize))+
  geom_boxplot(aes(fill=as.factor(volt_num)), width=0.5/length(unique(FW_sizeDF_new$volt_num)))+
  labs(title="Plot of forewing centroid size with different levels of voltinism",x="Number of generations per year", y = "Centroid size")+
  theme_classic()
FW_Csize_Volt + theme(plot.title = element_text(hjust = 0.5), legend.position = "none") + 
  stat_compare_means(method="anova", label.y = 2.9, label.x = 0.55) + scale_fill_brewer(palette = "Dark2")

FW_sizeDF_new$volt_num <- as.factor(FW_sizeDF_new$volt_num)

t.test(FW_sizeDF_new$Csize~as.factor(FW_sizeDF_new$volt_num))
# p=8.9e-07

scaleFUN <- function(x) sprintf("%.2f", x)

HW_Csize_Volt <- ggplot(HW_sizeDF_new, aes(x=as.factor(volt_num), y=Csize))+
  geom_boxplot(aes(fill=as.factor(volt_num)))+
  labs(title="Plot of hindwing centroid size with different levels of voltinism",x="Number of generations", y = "Centroid size")+
  theme_classic()

HW_Csize_Volt + scale_y_continuous(labels=scaleFUN) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") + 
  stat_compare_means(method="anova", label.y = 2.75, label.x = 0.55) + scale_fill_brewer(palette = "Dark2")

HW_sizeDF_new$volt_num <- as.factor(HW_sizeDF_new$volt_num)

t.test(HW_sizeDF_new$Csize~as.factor(HW_sizeDF_new$volt_num))
# p=2.551e-6

## plot HW and FW together for manuscript
library(gdata)

FW_volt <- FW_sizeDF_new[,c("Csize","volt_num")]
head(FW_volt)
HW_volt <- HW_sizeDF_new[,c("Csize", "volt_num")]
head(HW_volt)

both_volt <- gdata::combine(FW_volt, HW_volt)
colnames(both_volt)[3] <- "wing"
head(both_volt)

wing_names <- list(
  'FW_volt'="Forewing",
  'HW_volt'="Hindwing")

wing_labeller <- function(variable,value){
  return(wing_names[value])
}

volt_plot <- ggplot(both_volt, aes(x=as.factor(volt_num), y=Csize))+
  geom_boxplot(aes(fill=volt_num))+
  labs(x="Number of generations per year", y = "Centroid size")+
  theme_classic()+
  facet_wrap(~wing, labeller = wing_labeller)

volt_plot + apatheme + scale_y_continuous(labels=scaleFUN) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +  scale_fill_brewer(palette = "Dark2")


tiff(filename = "./output/Manuscript_figs/volt.tiff",
     type="cairo",
     units="in",
     width=15,
     height=15,
     res=96)
volt_plot + apatheme2 + scale_y_continuous(labels=scaleFUN) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+ scale_fill_brewer(palette = "Dark2")
dev.off()