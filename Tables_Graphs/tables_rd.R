
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","stargazer","tidyr","broom","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
 setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
results <- "Results/RD/"


###########################################################################################################
################################################ ELECTIONS ################################################
################################################# BY PANEL ################################################ 
###########################################################################################################

#############
# Function to read results

rd_to_df <- function(list){
  rd <- lapply(list, function(x){
    x$rd %>% .$tabl3.str}) %>%
  lapply(as.data.frame)  %>%
  lapply( "[", 3 , ) %>%
  ldply() %>% mutate(N_l = unlist(lapply(list, function(x) x$rd$N_h_l))) %>%
  mutate(N_r = unlist(lapply(list, function(x) x$rd$N_h_r))) %>%
  mutate(N = as.numeric(N_l) + as.numeric(N_r)) %>%
  mutate(bws = unlist(lapply(list, function(x) x$rd$bws[1,1]))) %>%
  mutate(mean_bw = unlist(lapply(list, function(x) x$mean)))%>%
  mutate(dens_pvalue = unlist(lapply(list, function(x) x$d)))

  df <- rd %>% t() %>% as.data.frame()
  row.names(df) <- c("Eleccion","Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right", "N", "bws", "Media control", "p-value_dens")
  colnames(df) <- df$Eleccion
  return(df)
}

#############
# Read results
setwd(results)

list_files <- list.files() %>%
  .[str_detect(., "_party.")]
  party <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "_1_coalition")]
coalition_1 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "_2_coalition")]
coalition_2 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "_final_coalition")] 
final <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "current1_coalition")]
current_1 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "current2_coalition")]
current_2 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "currentfinal_coalition")]
current_final <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

#############
# Tables 

# Incoming
a <- rd_to_df(party) %>% .[c(5, 1, 4, 2, 3)] %>% stargazer(., summary = FALSE, out= "Tables/elec_party.tex")
b <- rd_to_df(coalition_1) %>% .[c(4, 1, 3, 2)] %>% stargazer(., summary = FALSE, out= "Tables/elec_coalition_1.tex")
c <- rd_to_df(final)
d <- rd_to_df(coalition_2) %>% cbind(., c) %>% .[c(7, 5, 6, 2)] %>% stargazer(., summary = FALSE, out= "Tables/elec_coalition_2.tex")

# Current and incoming
e <- rd_to_df(current_1) %>% .[c(4, 1, 3, 2)] %>% stargazer(., summary = FALSE, out= "Tables/elec_coalition_current1.tex")
f <- rd_to_df(current_final)
g <- rd_to_df(current_2) %>% cbind(., f) %>% .[c(4, 2, 3, 1)] %>% stargazer(., summary = FALSE, out= "Tables/elec_coalition_current2.tex")



###########################################################################################################
###################################### FUNCTION TO EXTRACT INFO FROM RD'S #################################
################################################# INVESTMENT ##############################################
###########################################################################################################

rd_to_df_2 <-  function(list){
  rd <- lapply(list, function(x){
    x$rd %>% .$tabl3.str}) %>%
    lapply(as.data.frame)  %>%
    lapply( "[", 3 , ) %>%
    ldply() %>% mutate(N_l = unlist(lapply(list, function(x) x$rd$N_h_l))) %>%
    mutate(N_r = unlist(lapply(list, function(x) x$rd$N_h_r))) %>%
    mutate(N = as.numeric(N_l) + as.numeric(N_r)) %>%
    mutate(bws = unlist(lapply(list, function(x) x$rd$bws[1,1]))) %>%
    # mutate(mean_bw = unlist(lapply(list, function(x) x$mean)))%>%
    mutate(dens_pvalue = unlist(lapply(list, function(x) x$d))) 
  
  df <- rd %>% t() %>% as.data.frame()
  row.names(df) <- c("Type","Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right", "N", "bws", "p-value_dens")
  # colnames(df) <- df$Eleccion
  return(df)
}

###########################################################################################################
###################################### LOAD INVESTMENT RESULTS ############################################
############################################# BY PANEL #################################################### 
###########################################################################################################
setwd(results)

out_investment <- c("log_D","log_D2000", "log_D1000", "log_D3000")
out_roads <- c("log_vias","log_f_SGPp","log_f_regalias", "log_f_trans_nac")
outcomes <- c(out_investment, out_roads)

list_files <- list.files() %>%
  .[str_detect(., "total_current")]
total <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)





list_files <- list.files() %>%
  .[str_detect(., "before_current")]
before_current <- lapply(list_files, readRDS) %>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)
# setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "after_current")]
after_current <- lapply(list_files, readRDS) %>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes) 



list_files <- list.files() %>%
  .[str_detect(., "before_next")]
before_next <- lapply(list_files, readRDS) %>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)
# setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "after_next")]
after_next <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)


list_files <- list.files() %>%
  .[str_detect(., "before_s2011_next")]
before_s2011_next <- lapply(list_files, readRDS) %>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)
# setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "after_s2011_next")]
after_s2011_next <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

###########################################################################################################
############################################  CREATE TABLES  ##############################################
###########################################################################################################

#Table 3 (2000, 1000, 3000)
a <- rd_to_df_2(before_current) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE, out= "Tables/inv_before_current.tex")
b <- rd_to_df_2(after_current) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE, out= "Tables/inv_after_current.tex")

c1 <- rd_to_df_2(before_next) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE, out= "Tables/inv_before_next.tex")
d1 <- rd_to_df_2(after_next) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE, out= "Tables/inv_after_next.tex")

c2 <- rd_to_df_2(before_s2011_next) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE, out= "Tables/inv_before_s2011_next.tex")
d2 <- rd_to_df_2(after_s2011_next) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE, out= "Tables/inv_after_s2011_next.tex")

#Table: Robustness
e <- rd_to_df_2(total) %>% .[c(3, 2, 4, 6, 7, 8)] %>% stargazer(., summary = FALSE, out= "Tables/inv_total_current.tex")




###########################################################################################################
################################################ LOAD RESULTS #############################################
################################################# AFTERMATH ############################################### 
###########################################################################################################

setwd(results)

out_growth <- c("log_light_pix","log_light_dm","log_ba_tot_vr", "log_ba_peq_vr")
out_institutions <- c("desemp_fisc","desemp_int", "alcalde", "alcalde_guilty", "top", "top_guilty")
out_publicgoods <- c("tasa_m", "cob_pri", "cob_sec", "matematicas_s","lenguaje_s","fert_19_10_p","hom_tasa")
outcomes <- c(out_growth, out_institutions, out_publicgoods)
out_l <- list(out_growth, out_institutions, out_publicgoods) %>%
  sapply(length)

list_files <- list.files() %>%
  .[str_detect(., "aftermath")]
aftermath <- lapply(list_files, readRDS) %>%
  unlist(recursive = FALSE) %>%  
  setNames(., outcomes)

###########################################################################################################
############################################  CREATE TABLES  ##############################################
###########################################################################################################

a <- rd_to_df(aftermath) %>% .[, 1:4]  %>% stargazer(., summary = FALSE, out= "Tables/aftermath_a.tex")
b <- rd_to_df(aftermath) %>% .[, 5:10]  %>% stargazer(., summary = FALSE, out= "Tables/aftermath_b.tex")
c <- rd_to_df(aftermath) %>% .[, 11:length(.)]  %>% stargazer(., summary = FALSE, out= "Tables/aftermath_c.tex")

