rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","broom","raster", "sp", "rgdal")
lapply(packageList,require,character.only=TRUE)

#Get nightlight data
processing_rasters <- function(layer.list, ext, shape){
  layer.list %>%
    lapply(setExtent, ext) %>%
    lapply(crop, shape) %>%
    stack() %>% 
    mask(shape)
}

# Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
setwd("~/Dropbox/NOAA2")
colombia_municipios <- 
  readOGR(dsn = "Municipios", layer="Municipios") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

list_raster <- list.files("TIFF/") %>%
  str_c("TIFF/", ., sep = "") %>%
  lapply(raster)
rasters_extent <- extent(list_raster[[1]]) #We need to put all rasters into the same extent (all have the same resolution)
rasters_lights <- processing_rasters(list_raster, rasters_extent, colombia_municipios) 
extract_rasters <- raster::extract(rasters_lights, colombia_municipios, fun = sum, na.rm = TRUE, df = T, sp = T)

#Create a raster to measure the change in number of light pixels
rec_matrix <- matrix(c(0, 5, 0, 5, Inf, 1), ncol = 3, byrow = T)
rasters_lights_rec <-  reclassify(rasters_lights, rec_matrix)
extract_rasters_rec <- raster::extract(rasters_lights_rec, colombia_municipios, fun = sum, na.rm = TRUE, df = T, sp = T)
names(extract_rasters_rec) <- names(extract_rasters)

# 5. Average years with two rasters
list_df <- list(extract_rasters@data, extract_rasters_rec@data) %>%
  lapply(., function(x){
    names(x)[10:44] <- lapply(names(x)[10:44], str_sub, 4, 7)
    duplicated_years <- names(x)[duplicated(names(x))]
    duplicated_years2 <- str_c(duplicated_years, "1", sep = ".")
    
    names(x)[10:44] <- make.names(names(x)[10:44], unique = T)
    names(x)[10:44] <- str_sub(names(x)[10:44], 2)
    x <- x[, order(names(x), decreasing  = F)]
    
    for(i in duplicated_years){
      x[, i] <- rowMeans(x[, which(names(x) == i) : which(names(x) == str_c(i, 1, sep = "."))])
    }
    x <- x[, -which(names(x) %in% duplicated_years2)]
    names(x)[1:22] <- str_c("dm", names(x)[1:22])
    return(x)
  })

# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
luces <-"Data/NOAA/"
mapply(function(x,  name){
  write.csv(x, str_c(luces, "nightlights_muni", "_", name, ".csv"))
}, x = list_df, name = c("dm", "pixels"))



