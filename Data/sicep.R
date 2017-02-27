###########################################################################################################
############################################# SICEP DATA ##################################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr")
lapply(packageList,require,character.only=TRUE)

# Directory 
setwd("D:/Users/lbonilme/Dropbox/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

ori_mun <-"CEER/Educacion/Datos/Archivos originales/DNP SICEP/Fuentes/Maria/1993-2010 Municipales/"
ori_dep <-"CEER/Educacion/Datos/Archivos originales/DNP SICEP/Fuentes/Maria/2003-2010 Departamentales/"
data <- "CEER/Educacion/Datos/Archivos originales/DNP SICEP/"

###########################################################################################################
######################################## DBF TO DTA #######################################################
###########################################################################################################

write.dta(read.dbf(paste0(ori_mun,"Eje2000/Tot_a00.dbf")), paste0(data,"2000/municipios/tot_a00.dta"))

