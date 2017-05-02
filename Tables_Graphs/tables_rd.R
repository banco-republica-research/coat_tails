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
rd_to_df <- function(list){
  rd <- lapply(list, function(x){
    x$rd %>% .$tabl3.str}) %>%
  lapply(as.data.frame) %>%
  lapply( "[", 3 , ) %>% 
  ldply() %>% mutate(N_l = unlist(lapply(list, function(x) x$rd$N_h_l))) %>%
  mutate(N_r = unlist(lapply(list, function(x) x$rd$N_h_r))) %>%
  mutate(N = as.numeric(N_l) + as.numeric(N_r)) %>%
  mutate(bws = unlist(lapply(list, function(x) x$rd$bws[1,1]))) %>%
  mutate(mean_bw = unlist(lapply(list, function(x) x$mean)))

  df <- rd %>% t() %>% as.data.frame()
  row.names(df) <- c("Eleccion","Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right", "N", "bws", "Media control")
  # colnames(df) <- df$Eleccion
  return(df)
}

###########################################################################################################
################################################ LOAD RESULTS #############################################
################################################## BY PANEL ############################################### 
###########################################################################################################
setwd(results)
list_files <- list.files() %>%
  .[str_detect(., "party")]
party <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "1_coalition")]
coalition_1 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "2_coalition")]
coalition_2 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)


###########################################################################################################
################################################ LOAD RESULTS #############################################
################################################# BY ELECTION ############################################# 
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


###########################################################################################################
############################################  CREATE TABLES  ##############################################
###########################################################################################################

a <- rd_to_df(party) %>% .[c(2, 3, 4, 1)] %>% stargazer(., summary = FALSE)
b <- rd_to_df(coalition_1) %>% .[c(2, 3, 1)] %>% stargazer(., summary = FALSE)
c <- rd_to_df(coalition_2) %>% .[c(2, 3, 1)] %>% stargazer(., summary = FALSE)




