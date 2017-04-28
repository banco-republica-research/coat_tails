
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

dnp <- "Data/DNP/Ejecuciones/"
final <- "Results/Descriptives/"
doc <- "Document/Figures/"

###########################################################################################################
########################### Local government funds 1993-2014 ##############################################
###########################################################################################################

# Load ejecuciones
ejecu <- read_dta(paste0(dnp,"Ejecuciones_all.dta"))

