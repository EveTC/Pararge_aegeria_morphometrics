## Aim of this document is to calibrate photos for further analysis.
# Eve Taylor-Cox

### Pics provided are calibrated and cropped to wings used in analysis. Original photos not provided.

# Pictures are grouped corresponding to the date pictures were taken and should be calibrated together.

##### 1. Load Patternize and dependencies ####

# patternize
library(patternize, lib.loc="C:/Users/etayl/R/win-library/3.6")

# dependecnies
library(devtools)
library(rgdal)
library(abind)
library(raster)
library(sp)
library(RNiftyReg)
library(imager)

#### Calibration group 1 - taken 01/05/19 ####
# make a list of images that we want to calibrate and call it IDlist
IDlist <- c('PA_1_WW_01_D', 'PA_1_WW_02_D', 'PA_1_WW_03_D', 'PA_1_WW_04_D', 'PA_1_WW_05_D', 'PA_1_WW_06_D', 'PA_1_WW_07_D', 'PA_1_WW_08_D', 'PA_1_WW_09_D', 'PA_1_WW_10_D', 'PA_1_WW_11_D', 'PA_1_WW_12_D', 'PA_1_WW_13_D', 'PA_1_WW_14_D', 'PA_1_WW_15_D', 'PA_1_WW_16_D', 'PA_1_WW_17_D', 'PA_1_WW_18_D', 'PA_1_WW_19_D', 'PA_1_WW_20_D', 'PA_1_WW_01_V', 'PA_1_WW_02_V')
IDlist <- c('PA_1_WW_04_V', 'PA_1_WW_05_V', 'PA_1_WW_06_V', 'PA_1_WW_07_V', 'PA_1_WW_08_V', 'PA_1_WW_09_V', 'PA_1_WW_10_V', 'PA_1_WW_11_V', 'PA_1_WW_12_V', 'PA_1_WW_13_V', 'PA_1_WW_14_V', 'PA_1_WW_15_V', 'PA_1_WW_16_V', 'PA_1_WW_17_V', 'PA_1_WW_18_V', 'PA_1_WW_19_V', 'PA_1_WW_20_V', 'PA_3_MH_01_D', 'PA_3_MH_02_D', 'PA_3_MH_03_D', 'PA_3_MH_04_D', 'PA_3_MH_05_D', 'PA_3_MH_06_D', 'PA_3_MH_07_D', 'PA_3_MH_08_D', 'PA_3_MH_09_D', 'PA_3_MH_10_D', 'PA_3_MH_11_D', 'PA_3_MH_12_D', 'PA_3_MH_13_D', 'PA_3_MH_14_D', 'PA_3_MH_15_D', 'PA_3_MH_16_D', 'PA_3_MH_17_D', 'PA_3_MH_18_D', 'PA_3_MH_01_V', 'PA_3_MH_02_V')

IDlist <- c ('PA_3_MH_04_V', 'PA_3_MH_05_V', 'PA_3_MH_06_V', 'PA_3_MH_07_V', 'PA_3_MH_09_V', 'PA_3_MH_10_V', 'PA_3_MH_11_V', 'PA_3_MH_12_V', 'PA_3_MH_13_V', 'PA_3_MH_14_V', 'PA_3_MH_15_V', 'PA_3_MH_16_V', 'PA_3_MH_17_V', 'PA_3_MH_18_V', 'PA_4_RCHP_01_D', 'PA_4_RCHP_02_D', 'PA_4_RCHP_03_D', 'PA_4_RCHP_04_D', 'PA_4_RCHP_05_D', 'PA_4_RCHP_06_D', 'PA_4_RCHP_07_D', 'PA_4_RCHP_08_D', 'PA_4_RCHP_09_D', 'PA_4_RCHP_10_D', 'PA_4_RCHP_11_D', 'PA_4_RCHP_12_D', 'PA_4_RCHP_13_D', 'PA_4_RCHP_14_D', 'PA_4_RCHP_15_D', 'PA_4_RCHP_16_D', 'PA_4_RCHP_17_D', 'PA_4_RCHP_18_D', 'PA_4_RCHP_19_D', 'PA_4_RCHP_20_D', 'PA_4_RCHP_01_V', 'PA_4_RCHP_02_V', 'PA_4_RCHP_03_V', 'PA_4_RCHP_04_V', 'PA_4_RCHP_05_V', 'PA_4_RCHP_06_V', 'PA_4_RCHP_07_V', 'PA_4_RCHP_08_V', 'PA_4_RCHP_09_V', 'PA_4_RCHP_10_V', 'PA_4_RCHP_11_V', 'PA_4_RCHP_12_V', 'PA_4_RCHP_13_V', 'PA_4_RCHP_14_V', 'PA_4_RCHP_15_V', 'PA_4_RCHP_16_V', 'PA_4_RCHP_17_V', 'PA_4_RCHP_18_V', 'PA_4_RCHP_19_V', 'PA_4_RCHP_20_V', 'PA_5_Ferm_01_D', 'PA_5_Ferm_02_D', 'PA_5_Ferm_03_D', 'PA_5_Ferm_04_D', 'PA_5_Ferm_05_D', 'PA_5_Ferm_06_D', 'PA_5_Ferm_07_D', 'PA_5_Ferm_08_D', 'PA_5_Ferm_09_D', 'PA_5_Ferm_10_D', 'PA_5_Ferm_11_D', 'PA_5_Ferm_12_D', 'PA_5_Ferm_13_D', 'PA_5_Ferm_14_D', 'PA_5_Ferm_15_D', 'PA_5_Ferm_16_D', 'PA_5_Ferm_17_D', 'PA_5_Ferm_18_D', 'PA_5_Ferm_19_D', 'PA_5_Ferm_20_D', 'PA_5_Ferm_01_V', 'PA_5_Ferm_02_V', 'PA_5_Ferm_03_V', 'PA_5_Ferm_04_V', 'PA_5_Ferm_05_V', 'PA_5_Ferm_06_V', 'PA_5_Ferm_07_V', 'PA_5_Ferm_08_V', 'PA_5_Ferm_09_V', 'PA_5_Ferm_10_V', 'PA_5_Ferm_11_V', 'PA_5_Ferm_12_V', 'PA_5_Ferm_13_V', 'PA_5_Ferm_14_V', 'PA_5_Ferm_15_V', 'PA_5_Ferm_16_V', 'PA_5_Ferm_17_V', 'PA_5_Ferm_18_V', 'PA_5_Ferm_19_V', 'PA_5_Ferm_20_V')

# assign the path to the images folder into a character 
prepath_group1 <- './photos/GBS pops/RAW_png/Calib.group.1/'

# Calibrate photos - dorsal and ventral
colorChecker(IDlist, prepath_group1, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)

#### Calibration group 2 - taken 10/06/19 ####

# make a list of images that we want to calibrate and call it IDlist
IDlist <- c('PA_1_WW_03_V','PA_3_MH_03_V','PA_6_Fox_01_D','PA_6_Fox_02_D','PA_6_Fox_03_D','PA_6_Fox_04_D','PA_6_Fox_05_D','PA_6_Fox_06_D','PA_6_Fox_07_D','PA_6_Fox_08_D','PA_6_Fox_09_D','PA_6_Fox_10_D','PA_6_Fox_11_D','PA_6_Fox_12_D','PA_6_Fox_13_D','PA_6_Fox_14_D','PA_6_Fox_15_D','PA_6_Fox_16_D','PA_6_Fox_17_D','PA_6_Fox_18_D','PA_6_Fox_19_D','PA_6_Fox_20_D','PA_6_Fox_01_V','PA_6_Fox_02_V','PA_6_Fox_03_V','PA_6_Fox_04_V','PA_6_Fox_05_V','PA_6_Fox_06_V','PA_6_Fox_07_V','PA_6_Fox_08_V','PA_6_Fox_09_V','PA_6_Fox_10_V','PA_6_Fox_11_V','PA_6_Fox_12_V','PA_6_Fox_13_V','PA_6_Fox_14_V','PA_6_Fox_15_V','PA_6_Fox_16_V','PA_6_Fox_17_V','PA_6_Fox_18_V','PA_6_Fox_20_V','PA_7_CA_01_D','PA_7_CA_02_D','PA_7_CA_03_D','PA_7_CA_04_D','PA_7_CA_05_D','PA_7_CA_06_D','PA_7_CA_07_D','PA_7_CA_08_D','PA_7_CA_09_D','PA_7_CA_10_D','PA_7_CA_11_D','PA_7_CA_12_D','PA_7_CA_13_D','PA_7_CA_14_D','PA_7_CA_15_D','PA_7_CA_16_D','PA_7_CA_17_D','PA_7_CA_18_D','PA_7_CA_19_D','PA_7_CA_20_D','PA_7_CA_01_V','PA_7_CA_02_V','PA_7_CA_03_V','PA_7_CA_04_V','PA_7_CA_05_V','PA_7_CA_06_V','PA_7_CA_07_V','PA_7_CA_08_V','PA_7_CA_09_V','PA_7_CA_10_V','PA_7_CA_11_V','PA_7_CA_12_V','PA_7_CA_13_V','PA_7_CA_14_V','PA_7_CA_15_V','PA_7_CA_16_V','PA_7_CA_17_V','PA_7_CA_18_V','PA_7_CA_19_V','PA_7_CA_20_V', 'PA_8_RW_01_D','PA_8_RW_02_D','PA_8_RW_03_D','PA_8_RW_04_D','PA_8_RW_05_D','PA_8_RW_06_D','PA_8_RW_07_D','PA_8_RW_08_D','PA_8_RW_09_D','PA_8_RW_10_D','PA_8_RW_01_V','PA_8_RW_02_V','PA_8_RW_03_V','PA_8_RW_04_V','PA_8_RW_05_V','PA_8_RW_06_V','PA_8_RW_07_V','PA_8_RW_08_V','PA_8_RW_10_D','PA_9_WC_01_D','PA_9_WC_02_D','PA_9_WC_03_D','PA_9_WC_04_D','PA_9_WC_05_D','PA_9_WC_06_D','PA_9_WC_07_D','PA_9_WC_08_D','PA_9_WC_09_D','PA_9_WC_10_D','PA_9_WC_11_D','PA_9_WC_12_D','PA_9_WC_13_D','PA_9_WC_14_D','PA_9_WC_15_D','PA_9_WC_01_V','PA_9_WC_02_V','PA_9_WC_03_V','PA_9_WC_04_V','PA_9_WC_05_V','PA_9_WC_06_V','PA_9_WC_07_V','PA_9_WC_08_V','PA_9_WC_10_V','PA_9_WC_11_V','PA_9_WC_12_V','PA_9_WC_13_V','PA_9_WC_14_V','PA_9_WC_15_V')


# assign the path to the images folder into a character 
prepath_group2 <- './photos/GBS pops/RAW_png/Calib.group.2/'

# Calibrate photos - dorsal and ventral
colorChecker(IDlist, prepath_group2, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)

#### Calibration group 3 - taken 04/06/19 - 05/06/19 ####

# make a list of images that we want to calibrate and call it IDlist
IDlist<-c('PA_9b_HPW_13_D', 'PA_9b_HPW_14_D','PA_9b_HPW_15_D','PA_9b_HPW_13_V', 'PA_9b_HPW_14_V','PA_9b_HPW_15_V')

IDlist <- c('PA_6_Fox_19_V', 'PA_8_RW_09_V', 'PA_9_WC_09_V', 'PA_9b_HPW_01_D', 'PA_9b_HPW_02_D', 'PA_9b_HPW_03_D', 'PA_9b_HPW_04_D', 'PA_9b_HPW_05_D', 'PA_9b_HPW_06_D', 'PA_9b_HPW_07_D','PA_9b_HPW_08_D', 'PA_9b_HPW_09_D', 'PA_9b_HPW_10_D', 'PA_9b_HPW_11_D', 'PA_9b_HPW_12_D','PA_9b_HPW_13_D', 'PA_9b_HPW_14_D','PA_9b_HPW_15_D', 'PA_9b_HPW_01_V', 'PA_9b_HPW_02_V', 'PA_9b_HPW_03_V', 'PA_9b_HPW_04_V', 'PA_9b_HPW_05_V', 'PA_9b_HPW_06_V', 'PA_9b_HPW_07_V', 'PA_9b_HPW_08_V', 'PA_9b_HPW_09_V', 'PA_9b_HPW_10_V', 'PA_9b_HPW_11_V', 'PA_9b_HPW_12_V','PA_9b_HPW_13_V','PA_9b_HPW_14_V','PA_9b_HPW_15_V', 'PA_10_HRC_01_D', 'PA_10_HRC_02_D', 'PA_10_HRC_03_D', 'PA_10_HRC_04_D', 'PA_10_HRC_05_D', 'PA_10_HRC_06_D', 'PA_10_HRC_07_D', 'PA_10_HRC_08_D', 'PA_10_HRC_09_D','PA_10_HRC_10_D', 'PA_10_HRC_11_D', 'PA_10_HRC_12_D', 'PA_10_HRC_13_D', 'PA_10_HRC_14_D', 'PA_10_HRC_15_D', 'PA_10_HRC_16_D', 'PA_10_HRC_17_D', 'PA_10_HRC_18_D', 'PA_10_HRC_19_D', 'PA_10_HRC_20_D', 'PA_10_HRC_01_V', 'PA_10_HRC_02_V', 'PA_10_HRC_03_V', 'PA_10_HRC_04_V', 'PA_10_HRC_05_V', 'PA_10_HRC_06_V', 'PA_10_HRC_07_V', 'PA_10_HRC_08_V', 'PA_10_HRC_09_V', 'PA_10_HRC_10_V','PA_10_HRC_11_V', 'PA_10_HRC_12_V', 'PA_10_HRC_13_V', 'PA_10_HRC_13_V') 

IDlist <- c('PA_10_HRC_14_V', 'PA_10_HRC_15_V', 'PA_10_HRC_16_V', 'PA_10_HRC_17_V', 'PA_10_HRC_18_V', 'PA_10_HRC_19_V', 'PA_10_HRC_20_V', 'PA_11_ARW_01_D', 'PA_11_ARW_02_D', 'PA_11_ARW_03_D', 'PA_11_ARW_04_D', 'PA_11_ARW_05_D', 'PA_11_ARW_06_D', 'PA_11_ARW_07_D', 'PA_11_ARW_08_D', 'PA_11_ARW_09_D', 'PA_11_ARW_10_D','PA_11_ARW_11_D', 'PA_11_ARW_12_D', 'PA_11_ARW_13_D', 'PA_11_ARW_14_D', 'PA_11_ARW_15_D', 'PA_11_ARW_16_D', 'PA_11_ARW_17_D', 'PA_11_ARW_18_D', 'PA_11_ARW_19_D', 'PA_11_ARW_20_D', 'PA_11_ARW_01_V', 'PA_11_ARW_02_V', 'PA_11_ARW_03_V', 'PA_11_ARW_04_V', 'PA_11_ARW_05_V', 'PA_11_ARW_06_V', 'PA_11_ARW_07_V', 'PA_11_ARW_08_V', 'PA_11_ARW_09_V', 'PA_11_ARW_10_V', 'PA_11_ARW_11_V','PA_11_ARW_12_V', 'PA_11_ARW_13_V', 'PA_11_ARW_14_V', 'PA_11_ARW_15_V', 'PA_11_ARW_16_V', 'PA_11_ARW_17_V', 'PA_11_ARW_18_V', 'PA_11_ARW_19_V', 'PA_11_ARW_20_V', 'PA_12_RW_01_D', 'PA_12_RW_02_D', 'PA_12_RW_03_D', 'PA_12_RW_04_D', 'PA_12_RW_05_D', 'PA_12_RW_06_D', 'PA_12_RW_07_D', 'PA_12_RW_08_D', 'PA_12_RW_09_D', 'PA_12_RW_10_D', 'PA_12_RW_11_D', 'PA_12_RW_12_D', 'PA_12_RW_13_D','PA_12_RW_14_D', 'PA_12_RW_15_D', 'PA_12_RW_16_D', 'PA_12_RW_17_D', 'PA_12_RW_18_D', 'PA_12_RW_19_D', 'PA_12_RW_20_D', 'PA_12_RW_01_V', 'PA_12_RW_02_V', 'PA_12_RW_03_V', 'PA_12_RW_04_V','PA_12_RW_05_V','PA_12_RW_06_V', 'PA_12_RW_07_V', 'PA_12_RW_08_V', 'PA_12_RW_09_V', 'PA_12_RW_10_V', 'PA_12_RW_11_V', 'PA_12_RW_12_V', 'PA_12_RW_13_V', 'PA_12_RW_14_V', 'PA_12_RW_15_V','PA_12_RW_16_V', 'PA_12_RW_17_V','PA_12_RW_18_V', 'PA_12_RW_19_V', 'PA_12_RW_20_V', 'PA_13_CM_01_D', 'PA_13_CM_02_D', 'PA_13_CM_03_D', 'PA_13_CM_04_D', 'PA_13_CM_05_D', 'PA_13_CM_06_D','PA_13_CM_07_D', 'PA_13_CM_08_D', 'PA_13_CM_09_D', 'PA_13_CM_10_D', 'PA_13_CM_11_D', 'PA_13_CM_12_D', 'PA_13_CM_13_D', 'PA_13_CM_14_D', 'PA_13_CM_15_D', 'PA_13_CM_16_D', 'PA_13_CM_17_D','PA_13_CM_18_D', 'PA_13_CM_19_D', 'PA_13_CM_20_D', 'PA_13_CM_01_V', 'PA_13_CM_02_V', 'PA_13_CM_03_V', 'PA_13_CM_04_V', 'PA_13_CM_05_V', 'PA_13_CM_06_V', 'PA_13_CM_07_V', 'PA_13_CM_08_V','PA_13_CM_09_V', 'PA_13_CM_08_V','PA_13_CM_09_V', 'PA_13_CM_10_V', 'PA_13_CM_11_V', 'PA_13_CM_12_V', 'PA_13_CM_13_V', 'PA_13_CM_14_V', 'PA_13_CM_15_V', 'PA_13_CM_16_V', 'PA_13_CM_17_V','PA_13_CM_18_V','PA_13_CM_19_V', 'PA_13_CM_20_V', 'PA_13_CM_01_D', 'PA_13_CM_02_D', 'PA_13_CM_03_D', 'PA_13_CM_04_D', 'PA_13_CM_05_D', 'PA_13_CM_06_D', 'PA_13_CM_07_D', 'PA_13_CM_08_D','PA_13_CM_09_D', 'PA_13_CM_10_D', 'PA_13_CM_11_D', 'PA_13_CM_12_D', 'PA_13_CM_13_D', 'PA_13_CM_14_D', 'PA_13_CM_15_D', 'PA_13_CM_16_D', 'PA_13_CM_17_D', 'PA_13_CM_18_D', 'PA_13_CM_19_D','PA_13_CM_20_D', 'PA_13_CM_01_V', 'PA_13_CM_02_V', 'PA_13_CM_03_V', 'PA_13_CM_04_V', 'PA_13_CM_05_V', 'PA_13_CM_06_V', 'PA_13_CM_07_V', 'PA_13_CM_08_V', 'PA_13_CM_09_V', 'PA_13_CM_08_V','PA_13_CM_09_V', 'PA_13_CM_10_V', 'PA_13_CM_11_V', 'PA_13_CM_12_V', 'PA_13_CM_13_V', 'PA_13_CM_14_V', 'PA_13_CM_15_V', 'PA_13_CM_16_V') 


IDlist <- c('PA_13_CM_17_V', 'PA_13_CM_18_V','PA_13_CM_19_V','PA_13_CM_20_V')

# assign the path to the images folder into a character 
prepath_group3 <- './photos/GBS pops/RAW_png/Calib.group.3/'

# Calibrate photos - dorsal and ventral
colorChecker(IDlist, prepath_group3, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = NULL)

#### Calibration group 4 - taken 9 and 11 /07/19 ####
IDList <- c('PA_14_WLW_01_D', 'PA_14_WLW_02_D', 'PA_14_WLW_03_D', 'PA_14_WLW_04_D', 'PA_14_WLW_05_D', 'PA_14_WLW_06_D', 'PA_14_WLW_07_D', 'PA_14_WLW_08_D', 'PA_14_WLW_09_D', 'PA_14_WLW_10_D', 'PA_14_WLW_11_D', 'PA_14_WLW_12_D', 'PA_14_WLW_13_D', 'PA_14_WLW_14_D', 'PA_14_WLW_15_D', 'PA_14_WLW_16_D', 'PA_14_WLW_17_D', 'PA_14_WLW_18_D', 'PA_14_WLW_19_D', 'PA_14_WLW_20_D', 'PA_14_WLW_01_V', 'PA_14_WLW_02_V', 'PA_14_WLW_03_V', 'PA_14_WLW_04_V', 'PA_14_WLW_05_V', 'PA_14_WLW_06_V', 'PA_14_WLW_07_V', 'PA_14_WLW_08_V', 'PA_14_WLW_09_V', 'PA_14_WLW_10_V', 'PA_14_WLW_11_V', 'PA_14_WLW_12_V', 'PA_14_WLW_13_V', 'PA_14_WLW_14_V', 'PA_14_WLW_15_V', 'PA_14_WLW_16_V', 'PA_14_WLW_17_V', 'PA_14_WLW_18_V', 'PA_14_WLW_19_V', 'PA_14_WLW_20_V', 'PA_15_SodG_01_D', 'PA_15_SodG_02_D', 'PA_15_SodG_03_D', 'PA_15_SodG_04_D', 'PA_15_SodG_05_D', 'PA_15_SodG_06_D', 'PA_15_SodG_07_D', 'PA_15_SodG_08_D', 'PA_15_SodG_09_D', 'PA_15_SodG_10_D', 'PA_15_SodG_11_D', 'PA_15_SodG_12_D', 'PA_15_SodG_13_D', 'PA_15_SodG_14_D', 'PA_15_SodG_15_D', 'PA_15_SodG_16_D', 'PA_15_SodG_17_D', 'PA_15_SodG_18_D', 'PA_15_SodG_19_D', 'PA_15_SodG_20_D', 'PA_15_SodG_20_D', 'PA_15_SodG_01_V', 'PA_15_SodG_02_V', 'PA_15_SodG_03_V', 'PA_15_SodG_04_V', 'PA_15_SodG_05_V', 'PA_15_SodG_06_V', 'PA_15_SodG_07_V', 'PA_15_SodG_08_V', 'PA_15_SodG_09_V', 'PA_15_SodG_10_V', 'PA_15_SodG_11_V', 'PA_15_SodG_12_V', 'PA_15_SodG_13_V', 'PA_15_SodG_14_V', 'PA_15_SodG_15_V', 'PA_15_SodG_16_V', 'PA_15_SodG_17_V', 'PA_15_SodG_18_V', 'PA_15_SodG_19_V', 'PA_15_SodG_20_V', 'PA_16_L_01_D', 'PA_16_L_02_D', 'PA_16_L_03_D', 'PA_16_L_04_D', 'PA_16_L_05_D', 'PA_16_L_06_D', 'PA_16_L_07_D', 'PA_16_L_08_D', 'PA_16_L_09_D', 'PA_16_L_10_D', 'PA_16_L_01_V', 'PA_16_L_02_V', 'PA_16_L_03_V', 'PA_16_L_04_V', 'PA_16_L_05_V', 'PA_16_L_06_V', 'PA_16_L_07_V', 'PA_16_L_08_V', 'PA_16_L_09_V', 'PA_16_L_10_V', 'PA_17_ACW_01_D', 'PA_17_ACW_02_D', 'PA_17_ACW_03_D', 'PA_17_ACW_04_D', 'PA_17_ACW_05_D', 'PA_17_ACW_06_D', 'PA_17_ACW_07_D', 'PA_17_ACW_08_D', 'PA_17_ACW_09_D', 'PA_17_ACW_10_D', 'PA_17_ACW_11_D', 'PA_17_ACW_12_D', 'PA_17_ACW_13_D', 'PA_17_ACW_14_D', 'PA_17_ACW_15_D', 'PA_17_ACW_16_D', 'PA_17_ACW_17_D', 'PA_17_ACW_18_D', 'PA_17_ACW_19_D', 'PA_17_ACW_20_D', 'PA_17_ACW_01_V', 'PA_17_ACW_02_V', 'PA_17_ACW_03_V', 'PA_17_ACW_04_V', 'PA_17_ACW_05_V', 'PA_17_ACW_06_V', 'PA_17_ACW_07_V', 'PA_17_ACW_08_V', 'PA_17_ACW_09_V', 'PA_17_ACW_11_V', 'PA_17_ACW_12_V', 'PA_17_ACW_13_V', 'PA_17_ACW_14_V', 'PA_17_ACW_15_V', 'PA_17_ACW_16_V', 'PA_17_ACW_17_V', 'PA_17_ACW_18_V', 'PA_17_ACW_19_V', 'PA_17_ACW_20_V', 'PA_18_Garl_01_D', 'PA_18_Garl_02_D', 'PA_18_Garl_03_D', 'PA_18_Garl_04_D', 'PA_18_Garl_05_D', 'PA_18_Garl_06_D', 'PA_18_Garl_07_D', 'PA_18_Garl_08_D', 'PA_18_Garl_09_D', 'PA_18_Garl_10_D','PA_18_Garl_01_V', 'PA_18_Garl_02_V', 'PA_18_Garl_03_V', 'PA_18_Garl_04_V', 'PA_18_Garl_05_V', 'PA_18_Garl_06_V', 'PA_18_Garl_07_V', 'PA_18_Garl_08_V', 'PA_18_Garl_09_V', 'PA_18_Garl_10_V', 'PA_19_CCP_01_D', 'PA_19_CCP_02_D', 'PA_19_CCP_03_D', 'PA_19_CCP_04_D', 'PA_19_CCP_05_D', 'PA_19_CCP_06_D', 'PA_19_CCP_07_D', 'PA_19_CCP_08_D', 'PA_19_CCP_09_D', 'PA_19_CCP_10_D', 'PA_19_CCP_11_D', 'PA_19_CCP_12_D', 'PA_19_CCP_13_D', 'PA_19_CCP_14_D', 'PA_19_CCP_15_D', 'PA_19_CCP_16_D', 'PA_19_CCP_17_D', 'PA_19_CCP_18_D', 'PA_19_CCP_19_D', 'PA_19_CCP_01_V', 'PA_19_CCP_02_V', 'PA_19_CCP_03_V', 'PA_19_CCP_04_V', 'PA_19_CCP_05_V', 'PA_19_CCP_06_V', 'PA_19_CCP_07_V')

IDList<-c('PA_19_CCP_08_V', 'PA_19_CCP_09_V', 'PA_19_CCP_10_V', 'PA_19_CCP_11_V', 'PA_19_CCP_12_V', 'PA_19_CCP_13_V', 'PA_19_CCP_14_V', 'PA_19_CCP_15_V', 'PA_19_CCP_16_V', 'PA_19_CCP_17_V', 'PA_19_CCP_18_V', 'PA_19_CCP_19_V')


# assign the path to the images folder into a character 
prepath_group4 <- './photos/GBS pops/RAW_png/Calib.group.4/'

# Calibrate photos - dorsal and ventral
colorChecker(IDList, prepath_group4, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)

#### Calibration group 5 - taken 09/07/19 ####
IDList <- c('PA_19_CCP_20_D','PA_19_CCP_20_V', 'PA_20_EM_01_D', 'PA_20_EM_02_D', 'PA_20_EM_03_D', 'PA_20_EM_04_D', 'PA_20_EM_05_D', 'PA_20_EM_06_D', 'PA_20_EM_07_D', 'PA_20_EM_08_D', 'PA_20_EM_09_D', 'PA_20_EM_10_D', 'PA_20_EM_11_D')

IDList<-c('PA_20_EM_12_D', 'PA_20_EM_13_D', 'PA_20_EM_14_D', 'PA_20_EM_15_D', 'PA_20_EM_16_D', 'PA_20_EM_17_D', 'PA_20_EM_18_D', 'PA_20_EM_19_D', 'PA_20_EM_20_D', 'PA_20_EM_01_V', 'PA_20_EM_02_V', 'PA_20_EM_03_V', 'PA_20_EM_04_V', 'PA_20_EM_05_V', 'PA_20_EM_06_V', 'PA_20_EM_07_V', 'PA_20_EM_08_V', 'PA_20_EM_09_V', 'PA_20_EM_10_V', 'PA_20_EM_11_V', 'PA_20_EM_12_V', 'PA_20_EM_13_V', 'PA_20_EM_14_V', 'PA_20_EM_15_V', 'PA_20_EM_16_V', 'PA_20_EM_17_V', 'PA_20_EM_18_V', 'PA_20_EM_19_V', 'PA_20_EM_20_V', 'PA_21_YC_01_D', 'PA_21_YC_02_D', 'PA_21_YC_03_D', 'PA_21_YC_04_D', 'PA_21_YC_05_D', 'PA_21_YC_06_D', 'PA_21_YC_07_D', 'PA_21_YC_08_D', 'PA_21_YC_09_D', 'PA_21_YC_10_D', 'PA_21_YC_11_D','PA_21_YC_12_D', 'PA_21_YC_13_D', 'PA_21_YC_14_D', 'PA_21_YC_15_D', 'PA_21_YC_16_D', 'PA_21_YC_17_D', 'PA_21_YC_18_D', 'PA_21_YC_18_D', 'PA_21_YC_19_D', 'PA_21_YC_20_D', 'PA_21_YC_01_V', 'PA_21_YC_02_V', 'PA_21_YC_03_V')
IDList<-c('PA_21_YC_04_V', 'PA_21_YC_05_V', 'PA_21_YC_06_V', 'PA_21_YC_07_V', 'PA_21_YC_08_V', 'PA_21_YC_09_V', 'PA_21_YC_10_V', 'PA_21_YC_11_V', 'PA_21_YC_12_V', 'PA_21_YC_13_V', 'PA_21_YC_14_V', 'PA_21_YC_15_V', 'PA_21_YC_16_V', 'PA_21_YC_17_V', 'PA_21_YC_18_V', 'PA_21_YC_19_V', 'PA_21_YC_20_V', 'PA_22_GB_01_D', 'PA_22_GB_02_D', 'PA_22_GB_03_D', 'PA_22_GB_04_D', 'PA_22_GB_05_D', 'PA_22_GB_06_D', 'PA_22_GB_07_D', 'PA_22_GB_08_D', 'PA_22_GB_09_D', 'PA_22_GB_10_D', 'PA_22_GB_11_D', 'PA_22_GB_12_D', 'PA_22_GB_13_D', 'PA_22_GB_14_D', 'PA_22_GB_15_D', 'PA_22_GB_16_D', 'PA_22_GB_17_D', 'PA_22_GB_18_D', 'PA_22_GB_19_D', 'PA_22_GB_20_D', 'PA_22_GB_01_V', 'PA_22_GB_02_V', 'PA_22_GB_03_V', 'PA_22_GB_04_V', 'PA_22_GB_05_V', 'PA_22_GB_06_V', 'PA_22_GB_07_V', 'PA_22_GB_08_V', 'PA_22_GB_09_V', 'PA_22_GB_10_V', 'PA_22_GB_11_V', 'PA_22_GB_12_V', 'PA_22_GB_13_V', 'PA_22_GB_14_V', 'PA_22_GB_15_V', 'PA_22_GB_16_V', 'PA_22_GB_17_V','PA_22_GB_18_V', 'PA_22_GB_19_V', 'PA_22_GB_20_V')

# assign the path to the images folder into a character 
prepath_group5 <- './photos/GBS pops/RAW_png/Calib.group.5/'

# Calibrate photos - dorsal and ventral
colorChecker(IDList, prepath_group5, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)

#### Calibrate group 6 - taken /07/19 ####
IDList <- c('PA_24_Tay_01_D', 'PA_24_Tay_02_D', 'PA_24_Tay_03_D', 'PA_24_Tay_04_D', 'PA_24_Tay_05_D', 'PA_24_Tay_06_D', 'PA_24_Tay_07_D', 'PA_24_Tay_08_D', 'PA_24_Tay_09_D', 'PA_24_Tay_10_D', 'PA_24_Tay_11_D', 'PA_24_Tay_12_D', 'PA_24_Tay_13_D', 'PA_24_Tay_14_D', 'PA_24_Tay_15_D', 'PA_24_Tay_16_D', 'PA_24_Tay_17_D', 'PA_24_Tay_18_D', 'PA_24_Tay_19_D', 'PA_24_Tay_20_D', 'PA_24_Tay_01_V', 'PA_24_Tay_02_V', 'PA_24_Tay_03_V', 'PA_24_Tay_04_V', 'PA_24_Tay_05_V', 'PA_24_Tay_06_V', 'PA_24_Tay_07_V','PA_24_Tay_08_V', 'PA_24_Tay_09_V', 'PA_24_Tay_10_V', 'PA_24_Tay_11_V', 'PA_24_Tay_12_V', 'PA_24_Tay_13_V', 'PA_24_Tay_14_V', 'PA_24_Tay_15_V', 'PA_24_Tay_16_V', 'PA_24_Tay_17_V', 'PA_24_Tay_18_V', 'PA_24_Tay_19_V', 'PA_24_Tay_20_V', 'PA_25_AF_01_D', 'PA_25_AF_02_D', 'PA_25_AF_03_D', 'PA_25_AF_04_D', 'PA_25_AF_05_D', 'PA_25_AF_06_D', 'PA_25_AF_07_D', 'PA_25_AF_08_D', 'PA_25_AF_09_D', 'PA_25_AF_10_D', 'PA_25_AF_11_D', 'PA_25_AF_12_D', 'PA_25_AF_13_D', 'PA_25_AF_14_D', 'PA_25_AF_15_D', 'PA_25_AF_16_D', 'PA_25_AF_17_D', 'PA_25_AF_18_D', 'PA_25_AF_19_D', 'PA_25_AF_20_D', 'PA_25_AF_20_D', 'PA_25_AF_01_V', 'PA_25_AF_02_V', 'PA_25_AF_03_V', 'PA_25_AF_04_V', 'PA_25_AF_05_V', 'PA_25_AF_06_V', 'PA_25_AF_07_V', 'PA_25_AF_08_V', 'PA_25_AF_09_V', 'PA_25_AF_10_V', 'PA_25_AF_11_V', 'PA_25_AF_12_V', 'PA_25_AF_13_V', 'PA_25_AF_14_V', 'PA_25_AF_15_V', 'PA_25_AF_16_V', 'PA_25_AF_17_V', 'PA_25_AF_18_V', 'PA_25_AF_19_V', 'PA_25_AF_20_V', 'PA_26_RF_01_D', 'PA_26_RF_02_D', 'PA_26_RF_03_D', 'PA_26_RF_04_D', 'PA_26_RF_05_D', 'PA_26_RF_06_D', 'PA_26_RF_07_D', 'PA_26_RF_01_V', 'PA_26_RF_02_V', 'PA_26_RF_03_V', 'PA_26_RF_04_V', 'PA_26_RF_05_V', 'PA_26_RF_06_V', 'PA_26_RF_07_V', 'PA_27_SL_01_D', 'PA_27_SL_02_D', 'PA_27_SL_03_D', 'PA_27_SL_04_D', 'PA_27_SL_05_D', 'PA_27_SL_06_D', 'PA_27_SL_07_D', 'PA_27_SL_01_V', 'PA_27_SL_02_V', 'PA_27_SL_03_V', 'PA_27_SL_04_V', 'PA_27_SL_05_V', 'PA_27_SL_06_V', 'PA_27_SL_07_V', 'PA_29_CC_01_D', 'PA_29_CC_02_D', 'PA_29_CC_03_D', 'PA_29_CC_04_D', 'PA_29_CC_05_D', 'PA_29_CC_06_D', 'PA_29_CC_07_D', 'PA_29_CC_08_D', 'PA_29_CC_09_D', 'PA_29_CC_10_D', 'PA_29_CC_11_D', 'PA_29_CC_12_D', 'PA_29_CC_13_D', 'PA_29_CC_14_D', 'PA_29_CC_15_D', 'PA_29_CC_16_D', 'PA_29_CC_17_D', 'PA_29_CC_18_D', 'PA_29_CC_19_D', 'PA_29_CC_20_D', 'PA_29_CC_01_V', 'PA_29_CC_02_V', 'PA_29_CC_03_V', 'PA_29_CC_04_V', 'PA_29_CC_05_V', 'PA_29_CC_06_V', 'PA_29_CC_07_V', 'PA_29_CC_08_V', 'PA_29_CC_09_V', 'PA_29_CC_10_V', 'PA_29_CC_11_V', 'PA_29_CC_12_V', 'PA_29_CC_13_V', 'PA_29_CC_14_V', 'PA_29_CC_15_V', 'PA_29_CC_16_V', 'PA_29_CC_17_V', 'PA_29_CC_18_V', 'PA_29_CC_19_V', 'PA_29_CC_20_V', 'PA_30_CB_01_D', 'PA_30_CB_02_D', 'PA_30_CB_03_D', 'PA_30_CB_04_D', 'PA_30_CB_05_D', 'PA_30_CB_06_D', 'PA_30_CB_07_D', 'PA_30_CB_08_D', 'PA_30_CB_09_D', 'PA_30_CB_10_D', 'PA_30_CB_01_V', 'PA_30_CB_02_V', 'PA_30_CB_03_V', 'PA_30_CB_04_V', 'PA_30_CB_05_V', 'PA_30_CB_06_V', 'PA_30_CB_07_V', 'PA_30_CB_08_V', 'PA_30_CB_09_V', 'PA_30_CB_10_V')

# assign the path to the images folder into a character 
prepath_group6 <- './photos/GBS pops/RAW_png/Calib.group.6/'

# Calibrate photos - dorsal and ventral
colorChecker(IDList, prepath_group6, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)

#### Calibrate group 7 - taken /07/19 ####
IDList <- c('PA_31_Cdho_01_D', 'PA_31_Cdho_02_D', 'PA_31_Cdho_03_D', 'PA_31_Cdho_04_D', 'PA_31_Cdho_05_D', 'PA_31_Cdho_06_D', 'PA_31_Cdho_07_D', 'PA_31_Cdho_08_D', 'PA_31_Cdho_09_D', 'PA_31_Cdho_10_D', 'PA_31_Cdho_11_D', 'PA_31_Cdho_12_D', 'PA_31_Cdho_13_D', 'PA_31_Cdho_14_D', 'PA_31_Cdho_15_D', 'PA_31_Cdho_16_D', 'PA_31_Cdho_17_D', 'PA_31_Cdho_18_D', 'PA_31_Cdho_19_D', 'PA_31_Cdho_20_D', 'PA_31_Cdho_01_V', 'PA_31_Cdho_02_V', 'PA_31_Cdho_03_V', 'PA_31_Cdho_04_V', 'PA_31_Cdho_05_V', 'PA_31_Cdho_06_V', 'PA_31_Cdho_07_V', 'PA_31_Cdho_08_V', 'PA_31_Cdho_09_V', 'PA_31_Cdho_10_V', 'PA_31_Cdho_11_V', 'PA_31_Cdho_12_V', 'PA_31_Cdho_13_V', 'PA_31_Cdho_14_V', 'PA_31_Cdho_15_V', 'PA_31_Cdho_16_V', 'PA_31_Cdho_17_V', 'PA_31_Cdho_18_V', 'PA_31_Cdho_19_V', 'PA_31_Cdho_20_V', 'PA_32_MW_01_D', 'PA_32_MW_02_D', 'PA_32_MW_03_D', 'PA_32_MW_04_D', 'PA_32_MW_05_D', 'PA_32_MW_06_D', 'PA_32_MW_07_D', 'PA_32_MW_08_D', 'PA_32_MW_09_D', 'PA_32_MW_10_D', 'PA_32_MW_11_D', 'PA_32_MW_12_D', 'PA_32_MW_13_D', 'PA_32_MW_14_D', 'PA_32_MW_15_D', 'PA_32_MW_16_D', 'PA_32_MW_17_D', 'PA_32_MW_18_D', 'PA_32_MW_19_D', 'PA_32_MW_20_D', 'PA_32_MW_20_D', 'PA_32_MW_01_V', 'PA_32_MW_02_V', 'PA_32_MW_03_V', 'PA_32_MW_04_V', 'PA_32_MW_05_V', 'PA_32_MW_06_V', 'PA_32_MW_07_V', 'PA_32_MW_08_V', 'PA_32_MW_09_V', 'PA_32_MW_10_V', 'PA_32_MW_11_V', 'PA_32_MW_12_V', 'PA_32_MW_13_V', 'PA_32_MW_14_V', 'PA_32_MW_15_V', 'PA_32_MW_16_V', 'PA_32_MW_17_V', 'PA_32_MW_18_V', 'PA_32_MW_19_V', 'PA_32_MW_20_V')

# assign the path to the images folder into a character 
prepath_group7 <- './photos/GBS pops/RAW_png/Calib.group.7/'

# Calibrate photos - dorsal and ventral
colorChecker(IDList, prepath_group7, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)

#### Calibrate group 8 - taken 25/10/19 ####
IDlist <- c('PA_22_GB_21_D','PA_22_GB_22_D','PA_22_GB_23_D','PA_22_GB_24_D','PA_22_GB_25_D','PA_22_GB_26_D','PA_22_GB_27_D','PA_22_GB_28_D','PA_22_GB_29_D','PA_22_GB_30_D','PA_22_GB_21_V','PA_22_GB_22_V','PA_22_GB_23_V','PA_22_GB_24_V','PA_22_GB_25_V','PA_22_GB_26_V','PA_22_GB_27_V','PA_22_GB_28_V','PA_22_GB_29_V','PA_22_GB_30_V','PA_26_RF_08_D','PA_26_RF_09_D','PA_26_RF_10_D','PA_26_RF_08_V','PA_26_RF_09_V','PA_26_RF_10_V')

# assign the path to the images folder into a character 
prepath_group8 <- './photos/GBS pops/RAW_png/Calib.group.8/'

# Calibrate photos - dorsal and ventral
colorChecker(IDlist, prepath_group8, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)

#### Claibrate Velocity pops - Expanding south - taken 25/10/19 #####

IDlist <- c('ESM1_D','ESM2_D','ESM3_D','ESM4_D','ESM5_D','ESM6_D','ESM7_D','ESM8_D','ESM9_D','ESM10_D','ESM11_D','ESM12_D','ESM13_D','ESM14_D','ESM15_D','ESM16_D','ESM17_D','ESM18_D','ESM19_D','ESM20_D','ESM21_D','ESM22_D','ESM23_D','ESM24_D','ESM25_D','ESM26_D','ESM27_D','ESM28_D','ESM29_D','ESM30_D','ESM31_D','ESM32_D','ESM33_D','ESM34_D','ESM35_D','ESM36_D','ESM38_D','ESM39_D','ESM40_D','ESM1_V','ESM2_V','ESM3_V','ESM4_V','ESM5_V','ESM6_V','ESM7_V','ESM8_V','ESM9_V','ESM10_V','ESM11_V','ESM12_V','ESM13_V','ESM14_V','ESM15_V','ESM16_V','ESM17_V','ESM18_V','ESM19_V','ESM20_V','ESM21_V','ESM22_V','ESM23_V','ESM24_V','ESM25_V','ESM26_V','ESM27_V','ESM28_V','ESM29_V','ESM30_V','ESM31_V','ESM32_V','ESM33_V','ESM34_V','ESM35_V','ESM36_V','ESM38_V','ESM39_V','ESM40_V')

# assign the path to the images folder into a character 
prepath_groupVelES <- './photos/Velocity pops/RAW_png/ES/'

# Calibrate photos - dorsal and ventral
colorChecker(IDlist, prepath_groupVelES, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)


#### Claibrate Velocity pops - Core south  #####

IDlist <- c('C19M1_D','C19M2_D','C19M3_D','C19M4_D','C19M5_D','C19M6_D','C19M7_D','C19M8_D','C19M9_D','C19M10_D','C19M11_D','C19M12_D','C19M13_D','C19M14_D','C19M15_D','C19M16_D','C19M17_D','C19M18_D','C19M19_D','C19M20_D','C19M21_D','C19M22_D','C19M23_D','C19M24_D','C19M25_D','C19M27_D','C19M28_D','C19M29_D','C19M30_D','C19M31_D','C19M32_D','C19M33_D','C19M34_D','C19M35_D','C19M36_D','C19M37_D','C19M38_D','C19M39_D','C19M40_D','C19M1_V','C19M2_V','C19M3_V','C19M4_V','C19M5_V','C19M6_V','C19M7_V','C19M8_V','C19M9_V','C19M10_V','C19M11_V','C19M12_V','C19M13_V','C19M14_V','C19M15_V','C19M16_V','C19M17_V','C19M18_V','C19M19_V','C19M20_V','C19M21_V','C19M22_V','C19M23_V','C19M24_V','C19M25_V','C19M27_V','C19M28_V','C19M29_V','C19M30_V','C19M31_V','C19M32_V','C19M33_V','C19M34_V','C19M35_V','C19M36_V','C19M37_V','C19M38_V','C19M39_V','C19M40_V')

# assign the path to the images folder into a character 
prepath_groupVelCS <- './photos/Velocity pops/RAW_png/CS_C19/'

# Calibrate photos - dorsal and ventral
colorChecker(IDlist, prepath_groupVelCS, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)

#### Claibrate Velocity pops - central pops  #####

IDlist <- c('CCM1_D','CCM1_V') # color checker in slightly diff place to below

IDlist<-c('CCM2_D','CCM3_D','CCM4_D','CCM5_D','CCM6_D','CCM7_D','CCM8_D','CCM9_D','CCM10_D','CCM11_D','CCM12_D','CCM13_D','CCM14_D','CCM15_D','CCM16_D','CCM17_D','CCM18_D','CCM19_D','CCM20_D','CCM21_D','CCM22_D','CCM23_D','CCM24_D','CCM25_D','CCM26_D','CCM27_D','CCM28_D','CCM29_D','CCM30_D','CCM31_D','CCM32_D','CCM33_D','CCM34_D','CCM35_D','CCM36_D','CCM37_D','CCM38_D','CCM39_D','CCM40_D','CCM2_V','CCM3_V','CCM4_V','CCM5_V','CCM6_V','CCM7_V','CCM8_V','CCM9_V','CCM10_V','CCM11_V','CCM12_V','CCM13_V','CCM14_V','CCM15_V','CCM16_V','CCM17_V','CCM18_V','CCM19_V','CCM20_V','CCM21_V','CCM22_V','CCM23_V','CCM24_V','CCM25_V','CCM26_V','CCM27_V','CCM28_V','CCM29_V','CCM30_V','CCM31_V','CCM32_V','CCM33_V','CCM34_V','CCM35_V','CCM36_V','CCM37_V','CCM38_V','CCM39_V','CCM40_V','EN19M1_D','EN19M2_D','EN19M3_D','EN19M4_D','EN19M5_D','EN19M6_D','EN19M7_D','EN19M8_D','EN19M9_D','EN19M10_D','EN19M11_D','EN19M12_D','EN19M13_D','EN19M14_D','EN19M15_D','EN19M16_D','EN19M17_D','EN19M18_D','EN19M19_D','EN19M20_D','EN19M21_D','EN19M22_D','EN19M23_D','EN19M24_D','EN19M25_D','EN19M26_D','EN19M27_D','EN19M28_D','EN19M29_D','EN19M30_D','EN19M31_D','EN19M32_D','EN19M33_D','EN19M34_D','EN19M35_D','EN19M36_D','EN19M37_D','EN19M38_D','EN19M39_D','EN19M40_D','EN19M1_V','EN19M2_V','EN19M3_V','EN19M4_V','EN19M5_V','EN19M6_V','EN19M7_V','EN19M8_V','EN19M9_V','EN19M10_V','EN19M11_V','EN19M12_V','EN19M13_V','EN19M14_V','EN19M15_V','EN19M16_V','EN19M17_V','EN19M18_V','EN19M19_V','EN19M20_V','EN19M21_V','EN19M22_V','EN19M23_V','EN19M24_V','EN19M25_V','EN19M26_V','EN19M27_V','EN19M28_V','EN19M29_V','EN19M30_V','EN19M31_V','EN19M32_V','EN19M33_V','EN19M34_V','EN19M35_V','EN19M36_V','EN19M37_V','EN19M38_V','EN19M39_V','EN19M40_V')
# assign the path to the images folder into a character 
prepath_groupVelCS <- './photos/Velocity pops/RAW_png/central_pops/'

# Calibrate photos - dorsal and ventral
colorChecker(IDlist, prepath_groupVelCS, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)

#### Claibrate Velocity pops - scot pops  #####

IDlist <- c('CNM1_D','CNM2_D','CNM3_D','CNM4_D','CNM5_D','CNM6_D','CNM7_D','CNM8_D','CNM9_D','CNM10_D','CNM11_D','CNM12_D','CNM13_D','CNM14_D','CNM15_D','CNM16_D','CNM17_D','CNM18_D','CNM19_D','CNM20_D','CNM21_D','CNM22_D','CNM23_D','CNM24_D','CNM25_D','CNM26_D','CNM27_D','CNM28_D','CNM29_D','CNM30_D','CNM31_D','CNM32_D','CNM33_D','CNM34_D','CNM35_D','CNM36_D','CNM37_D','CNM38_D','CNM39_D','CNM40_D','CNM1_V','CNM2_V','CNM3_V','CNM4_V','CNM5_V','CNM6_V','CNM7_V','CNM8_V','CNM9_V','CNM10_V','CNM11_V','CNM12_V','CNM13_V','CNM14_V','CNM15_V','CNM16_V','CNM17_V','CNM18_V','CNM19_V','CNM20_V','CNM21_V','CNM22_V','CNM23_V','CNM24_V','CNM25_V','CNM26_V','CNM27_V','CNM28_V','CNM29_V','CNM30_V','CNM31_V','CNM32_V','CNM33_V','CNM34_V','CNM35_V','CNM36_V','CNM37_V','CNM38_V','CNM39_V','CNM40_V','ENM1_D','ENM2_D','ENM3_D','ENM4_D','ENM5_D','ENM6_D','ENM7_D','ENM8_D','ENM9_D','ENM10_D','ENM11_D','ENM12_D','ENM13_D','ENM14_D','ENM15_D','ENM16_D','ENM17_D','ENM18_D','ENM19_D','ENM20_D','ENM21_D','ENM22_D','ENM23_D','ENM24_D','ENM25_D','ENM26_D','ENM27_D','ENM28_D','ENM29_D','ENM30_D','ENM31_D','ENM32_D','ENM33_D','ENM34_D','ENM35_D','ENM36_D','ENM37_D','ENM38_D','ENM39_D','ENM40_D','ENM1_V','ENM2_V','ENM3_V','ENM4_V','ENM5_V','ENM6_V','ENM7_V','ENM8_V','ENM9_V','ENM10_V','ENM11_V','ENM12_V','ENM13_V','ENM14_V','ENM15_V','ENM16_V','ENM17_V','ENM18_V','ENM19_V','ENM20_V','ENM21_V','ENM22_V','ENM23_V','ENM24_V','ENM25_V','ENM26_V','ENM27_V','ENM28_V','ENM29_V','ENM30_V','ENM31_V','ENM32_V','ENM33_V','ENM34_V','ENM35_V','ENM36_V','ENM37_V','ENM38_V','ENM39_V','ENM40_V')
# assign the path to the images folder into a character 
prepath_groupVelCS <- './photos/Velocity pops/RAW_png/north_pops/'

# Calibrate photos - dorsal and ventral
colorChecker(IDlist, prepath_groupVelCS, extension = '.png', colorCheckerType = 'ColorGauge Micro Analyzer', fixedCorners = TRUE, resampleFactor = 1)
