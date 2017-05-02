rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","broom","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
results <- "Results/RD"


###########################################################################################################
###################################### FUNCTION TO EXTRACT INFO FROM RD'S #################################
###########################################################################################################
rd_to_df <- function(list, dataframe){
  rd <- lapply(list, "[", "tabl3.str") %>%
    lapply(as.data.frame) %>%
    lapply( "[", 3 , ) %>%
    ldply() %>% mutate(N_l = unlist(lapply(list, "[", "N_l"))) %>%
    mutate(N_r = unlist(lapply(list, "[", "N_r"))) %>%
    mutate(N = N_l + N_r) %>%
    mutate(bws = unlist(lapply(list, function(x) x$bws[1, 1])))
  
  defo_mean <- mapply(function(x, y){
    y %>%
      filter(abs(dist_disc) <= x$bws[1, 1] & treatment == 0) %>% 
      dplyr::summarize(mean = mean(loss_sum))
  }, x = list , y = dataframe, SIMPLIFY = F) %>% unlist()
  
  df <- rd %>% cbind(., defo_mean) %>% t() %>% 
    as.data.frame() 
  names(df) <- NULL
  names(df) <- c("LineaNegra", "Doble_LN_PNN", "Doble_LN_Resg", "Triple")
  
  # %>% dplyr::rename(LineaNegra = Linea_Negra.mean,
  #                                     Doble_LN_PNN = Proteccion_Doble_LN_&_PNN.mean, Doble_LN_Resg = Proteccion_Doble_LN_&_Resg.mean,
  #                                     Triple = Triple_Proteccion_LN.mean)
  row.names(df) <- c("Territorio","Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right", "N", "bws", "Media control")
  return(df)
}





###########################################################################################################
################################################ LOAD RESULTS #############################################
###########################################################################################################
setwd(results)
list_files <- list.files() %>%
  .[str_detect(., "president")]
president <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "senate")]
senate <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "house")]
house <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)



