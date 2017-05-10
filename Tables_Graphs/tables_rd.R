rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","stargazer","tidyr","broom","cluster", "rdrobust")
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

list_files <- list.files() %>%
  .[str_detect(., "final")]
final <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)


###########################################################################################################
################################################ LOAD RESULTS #############################################
################################################# BY ELECTION ############################################# 
###########################################################################################################

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

#Table 1 
a <- rd_to_df(party) %>% .[c(5, 1, 4, 2, 3)] %>% stargazer(., summary = FALSE)
#Table 2

b <- rd_to_df(coalition_1) %>% .[c(4, 1, 3, 2)] %>% stargazer(., summary = FALSE)
c <- rd_to_df(final)
d <- rd_to_df(coalition_2) %>% cbind(., c) %>% .[c(7, 5, 6, 2)] %>% stargazer(., summary = FALSE)
#Table 3: Robustness



###########################################################################################################
###################################### FUNCTION TO EXTRACT INFO FROM RD'S #################################
################################################# INVESTMENT ##############################################
###########################################################################################################
rd_to_df_2 <- function(list){
  rd <- lapply(list, "[", "tabl3.str") %>%
    lapply(as.data.frame) %>%
    lapply( "[", 3 , ) %>%
    ldply() %>% mutate(N_l = unlist(lapply(list, "[", "N_h_l"))) %>%
    mutate(N_r = unlist(lapply(list, "[", "N_h_r"))) %>%
    mutate(N = N_l + N_r) %>%
    mutate(bws = unlist(lapply(list, function(x) x$bws[1, 1])))
  
  df <- rd %>% t() %>% as.data.frame()
  row.names(df) <- c("Type","Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right", "N", "bws")
  # colnames(df) <- df$Eleccion
  return(df)
}


###########################################################################################################
###################################### LOAD INVESTMENT RESULTS ############################################
############################################# BY PANEL #################################################### 
###########################################################################################################
setwd(results)

out_investment <- c("log_D_pc","log_D1000_pc", "log_D2000_pc", "log_D3000_pc")
out_roads <- c("log_vias_pc","log_f_SGPp_pc","log_f_regalias_pc", "log_f_trans_nac_pc")
outcomes <- c(out_investment, out_roads)

list_files <- list.files() %>%
  .[str_detect(., "before")]
before <- lapply(list_files, readRDS) %>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)
  # setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "after_current")]
after_current <- lapply(list_files, readRDS) %>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes) 

list_files <- list.files() %>%
  .[str_detect(., "after_next")]
after_next <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)


###########################################################################################################
############################################  CREATE TABLES  ##############################################
###########################################################################################################

#Table 3 (2000, 1000, 3000)
a <- rd_to_df_2(before) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE)

#Table 4
b <- rd_to_df_2(after_current) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE)
c <- rd_to_df_2(after_next) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE)
#Table: Robustness
d <- rd_to_df(final) %>% .[c(3, 2, 1)] %>% stargazer(., summary = FALSE)



