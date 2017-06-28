
###########################################################################################################
############################################# COAT-TAILS ##################################################
############################################## RD TABLES ##################################################
###########################################################################################################


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
    mutate(mean_bw = unlist(lapply(list, function(x) x$mean)))%>%
    mutate(bws = unlist(lapply(list, function(x) x$rd$bws[1,1]))) %>%
    mutate(N = as.numeric(N_l) + as.numeric(N_r)) %>%
    mutate(dens_pvalue = unlist(lapply(list, function(x) x$d)))
  
  df <- rd %>% t() %>% as.data.frame()
  row.names(df) <- c("Type","Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right",  "Average outcome","Bandwidth","Observations",  "p-value_dens")
  colnames(df) <- df$Eleccion
  return(df)
}

setwd(results)


#############
# Tables 

# Incoming
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


a <- rd_to_df(party) %>% .[c(5, 1, 4, 2, 3)] %>% stargazer(., summary = FALSE, out= "Tables/elec/elec_party.tex")
b <- rd_to_df(coalition_1) %>% .[c(4, 1, 3, 2)] %>% stargazer(., summary = FALSE, out= "Tables/elec/elec_coalition_1.tex")
c <- rd_to_df(final)
d <- rd_to_df(coalition_2) %>% cbind(., c) %>% .[c(7, 5, 6, 2)] %>% stargazer(., summary = FALSE, out= "Tables/elec/elec_coalition_2.tex")

# Current and incoming
list_files <- list.files() %>%
  .[str_detect(., "_current1_coalition")]
current_1 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "_current2_coalition")]
current_2 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "_currentfinal_coalition")]
current_final <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

e <- rd_to_df(current_1) %>% .[c(4, 1, 3, 2)] %>% stargazer(., summary = FALSE, out= "Tables/elec/elec_coalition_current1.tex")
f <- rd_to_df(current_final)
g <- rd_to_df(current_2) %>% cbind(., f) %>% .[c(4, 2, 3, 1)] %>% stargazer(., summary = FALSE, out= "Tables/elec/elec_coalition_current2.tex")

# Incoming but no current
list_files <- list.files() %>%
  .[str_detect(., "_nocurrent1_coalition")]
nocurrent_1 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "_nocurrent2_coalition")]
nocurrent_2 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "_nocurrentfinal_coalition")]
nocurrent_final <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

ne <- rd_to_df(nocurrent_1) %>% .[c(4, 1, 3, 2)] %>% stargazer(., summary = FALSE, out= "Tables/elec/elec_coalition_nocurrent1.tex")
nf <- rd_to_df(nocurrent_final)
ng <- rd_to_df(nocurrent_2) %>% cbind(., nf) %>% .[c(4, 2, 3, 1)] %>% stargazer(., summary = FALSE, out= "Tables/elec/elec_coalition_nocurrent2.tex")


###########################################################################################################
################################################# INVESTMENT ##############################################
###########################################################################################################

#############
# Function to Read results

rd_to_df_2 <-  function(list){
  rd <- lapply(list, function(x){
    x$rd %>% .$tabl3.str}) %>%
    lapply(as.data.frame)  %>%
    lapply( "[", 3 , ) %>%
    ldply() %>% mutate(N_l = unlist(lapply(list, function(x) x$rd$N_h_l))) %>%
    mutate(N_r = unlist(lapply(list, function(x) x$rd$N_h_r))) %>%
    mutate(mean_bw = unlist(lapply(list, function(x) x$mean)))%>%
    mutate(bws = unlist(lapply(list, function(x) x$rd$bws[1,1]))) %>%
    mutate(N = as.numeric(N_l) + as.numeric(N_r)) %>%
    mutate(dens_pvalue = unlist(lapply(list, function(x) x$d)))
  
  df <- rd %>% t() %>% as.data.frame()
  row.names(df) <- c("Type","Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right",  "Average outcome","Bandwidth","Observations",  "p-value_dens")
  # colnames(df) <- df$Eleccion
  return(df)
}

#############
# Read results

setwd(results)

# out_investment <- c("log_D","log_D4000","log_D2000", "log_D1000", "log_D3000")
# out_roads <- c("log_vias","log_f_propios","log_f_SGPp","log_f_regalias", "log_f_trans_nac")

out_investment <- c("log_D","log_D2000", "log_D1000", "log_D3000")
out_roads <- c("log_vias","log_f_SGPp","log_f_regalias", "log_f_trans_nac")
outcomes <- c(out_investment, out_roads)


#############
# Tables

# Total current: roads 
list_files <- list.files() %>%
  .[str_detect(., "total_current")]
total_current <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "before_current")]
before_current <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "after_current")]
after_current <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

inv_t <- rd_to_df_2(total_current) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_total_current.tex")
inv_b <- rd_to_df_2(before_current) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_before_current.tex")
inv_a <- rd_to_df_2(after_current) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_after_current.tex")

# Total current: roads (sin 2011)
list_files <- list.files() %>%
  .[str_detect(., "total_s2011_current")]
total_s2011_current <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "before_s2011_current")]
before_s2011_current <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "after_s2011_current")]
after_s2011_current <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

inv_t <- rd_to_df_2(total_s2011_current) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_total_s2011_current.tex")
inv_b <- rd_to_df_2(before_s2011_current) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_before_s2011_current.tex")
inv_a <- rd_to_df_2(after_s2011_current) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_after_s2011_current.tex")

# percentage effect
100*(exp(0.779)-1)
100*(exp(1.3909)-1)
100*(exp(0.406)-1)


# Total next: roads 

list_files <- list.files() %>%
  .[str_detect(., "total_next")]
total_next <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "before_next")]
before_next <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "after_next")]
after_next <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

inv_t <- rd_to_df_2(total_next) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_total_next.tex")
inv_b <- rd_to_df_2(before_next) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_before_next.tex")
inv_a <- rd_to_df_2(after_next) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_after_next.tex")

# Total next: roads  (sin 2011)
list_files <- list.files() %>%
  .[str_detect(., "total_s2011_next")]
total_s2011_next <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "before_s2011_next")]
before_s2011_next <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "after_s2011_next")]
after_s2011_next <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)
inv_t <- rd_to_df_2(total_s2011_next) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_total_s2011_next.tex")
inv_b <- rd_to_df_2(before_s2011_next) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_before_s2011_next.tex")
inv_a <- rd_to_df_2(after_s2011_next) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_after_s2011_next.tex")


# Total curnext: roads 
list_files <- list.files() %>%
  .[str_detect(., "total_curnext")]
total_curnext <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "before_curnext")]
before_curnext <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "after_curnext")]
after_curnext <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

inv_t <- rd_to_df_2(total_curnext) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_total_curnext.tex")
inv_b <- rd_to_df_2(before_curnext) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_before_curnext.tex")
inv_a <- rd_to_df_2(after_curnext) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_after_curnext.tex")

# Total curnext: roads (sin 2011)
list_files <- list.files() %>%
  .[str_detect(., "total_s2011_curnext")]
total_s2011_curnext <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "before_s2011_curnext")]
before_s2011_curnext <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

list_files <- list.files() %>%
  .[str_detect(., "after_s2011_curnext")]
after_s2011_curnext <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

inv_t <- rd_to_df_2(total_s2011_curnext) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_total_s2011_curnext.tex")
inv_b <- rd_to_df_2(before_s2011_curnext) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_before_s2011_curnext.tex")
inv_a <- rd_to_df_2(after_s2011_curnext) %>% .[c(5:8)] %>% stargazer(., summary = FALSE, out= "Tables/inv/inv_after_s2011_curnext.tex")



###########################################################################################################
################################################ LOAD RESULTS #############################################
################################################# AFTERMATH ############################################### 
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
    mutate(mean_bw = unlist(lapply(list, function(x) x$mean)))%>%
    mutate(bws = unlist(lapply(list, function(x) x$rd$bws[1,1]))) %>%
    mutate(N = as.numeric(N_l) + as.numeric(N_r)) %>%
    mutate(dens_pvalue = unlist(lapply(list, function(x) x$d)))
  
  df <- rd %>% t() %>% as.data.frame()
  row.names(df) <- c("Type","Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right",  "Average outcome","Bandwidth","Observations",  "p-value_dens")
  colnames(df) <- df$Eleccion
  return(df)
}

#############
# read results and create tables

setwd(results)

setwd(results)

out_growth <- c("log_light_pix","log_light_dm","log_ba_tot_vr", "log_ba_peq_vr")
out_institutions <- c("desemp_fisc","desemp_int", "alcalde", "alcalde_guilty", "top", "top_guilty","hom_tasa")
out_publicgoods <- c("tasa_m", "cob_pri", "cob_sec", "matematicas_s","lenguaje_s","fert_19_10_p")
outcomes <- c(out_growth, out_institutions, out_publicgoods)

out_l <- list(out_growth, out_institutions, out_publicgoods) %>%
  sapply(length)

list_files <- list.files() %>%
  .[str_detect(., "aftermath")]
aftermath <- lapply(list_files, readRDS) %>%
  unlist(recursive = FALSE) %>%  
  setNames(., outcomes)

# Tables

a <- rd_to_df(aftermath) %>% .[, 1:4]  %>% stargazer(., summary = FALSE, out= "Tables/after/aftermath_growth.tex")
b <- rd_to_df(aftermath) %>% .[, 5:11]  %>% stargazer(., summary = FALSE, out= "Tables/after/aftermath_institutions.tex")
c <- rd_to_df(aftermath) %>% .[, 12:length(.)]  %>% stargazer(., summary = FALSE, out= "Tables/after/aftermath_publicgoods.tex")



