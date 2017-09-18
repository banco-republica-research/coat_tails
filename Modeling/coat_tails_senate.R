###########################################################################################################
########################################### COAT TAILS SENATE: RD #########################################
###########################################################################################################

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
doc <- "Results/RD/Graphs/RD/"

###########################################################################################################
############################################# LOAD DATA ###################################################
###########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

# Load party codes and municipal covariates
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

cede <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))
controls <- cede %>%
  dplyr::select(coddepto, codmpio, municipio, ano, nbi) %>%
  filter(ano == 1993) %>%
  merge(., cede, by.x = c("codmpio"), by.y = c("codmpio"), all = T)


###########################################################################################################
#################################### Estimation Function  #################################################
###########################################################################################################

# Regressions for list of outcomes
l_f <- function(o){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                vce = "nn")
  mean <- l %>% filter(margin_prop_2 <= 0 + r$bws[1] &
                         margin_prop_2 >= 0 - r$bws[1])
  mean <- mean(l[,o], na.rm = T)
  
  dens <- rddensity::rddensity(X = l$margin_prop_2, h = r$bws[1], c = 0) 
  dens <- dens$test$p_jk
  return(list(rd = r, mean = mean, d = dens))
}

#BW sensibility function

l_f_sens <- function(o, bw){
  r <- rdrobust(y = l[,o],
                x = l$margin_prop_2,
                covs = cbind(l$pobl_tot, l$altura, l$disbogota, l$discapital, l$nbi.x),
                c = 0,
                all = T,
                h = bw,
                vce = "nn")
  return(r)
}



###########################################################################################################
######################################## COAT TAILS HOUSE BY PARTY: RD ####################################
###########################################################################################################

#Senate representatives by party (previous house data bases were arranged by coalition of first or second round)
senado <- readRDS(paste0(res,"senado_merge.rds")) 

# Parties by election
sen_cand <- senado %>% filter(codpartido!=98 |codpartido!=99) %>%
  dplyr::select(ano,codpartido) %>%
  unique(.) 

# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  arrange(codmpio, ano, codpartido) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) %>% 
  mutate(year = fct_recode(ano,
                           "1998" = "1997",
                           "2002" = "2000",
                           "2006" = "2003",
                           "2010" = "2007",
                           "2014" = "2011")) %>%  mutate(year = as.character(year)) 

table(alcaldes_merge_r2$ano, alcaldes_merge_r2$year)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Merge elections by their party code (codpartido)
# Only parties with candidates running in each department/year

alcaldes_rd <- alcaldes_merge_r2 %>%
  merge(., sen_cand,  by.x = c("year", "codpartido"), by.y = c("ano","codpartido"), 
        suffixes = c("_t", "_t1")) %>%  
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  group_by(ano, codmpio, codpartido) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  merge(., senado,  by.x = c("year", "codmpio", "codpartido"), by.y = c("ano", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1")) %>%
  filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  #filter(is.na(prop_votes_c2) == F | prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# All 
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")

r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/coat_tails_senate_party.rds"))
r 

###########################################################################################################
##################################### COAT TAILS SENATE + COALITION: RD ###################################
########################################  COALITION FIRST ROUND ###########################################
###########################################################################################################

# coalition FIRST roundS
coalitions_long <- readRDS(paste0(res,"coalitions_primera_new.rds"))  %>% 
  dplyr::select(codpartido,ano,year,codmpio,coalition_new)  %>% 
  group_by(codpartido, ano, codmpio, year) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_coalition <- readRDS(paste0(res,"senate_coalition_primera_merge.rds"))

# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., senado_coalition,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  #filter(is.na(prop_votes_total_t1) == F) %>%
  filter(is.na(prop_votes_c2) == F | prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# All 
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# outcomes
out <- c("prop_votes_total_t1")


r <-lapply(out, l_f) 
saveRDS(r, str_c(results, "/coat_tails_senate_1_coalition.rds"))
r


###########################################################################################################
##################################### COAT TAILS SENATE + COALITION: RD ###################################
#######################################  COALITION SECOND ROUND ###########################################
###########################################################################################################

# coalition FINAL roundS
coalitions_long <- readRDS(paste0(res,"coalitions_segunda_new.rds"))  %>% 
  dplyr::select(codpartido,ano,year,codmpio,coalition_new)  %>% 
  group_by(codpartido, ano, codmpio, year) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_coalition <- readRDS(paste0(res,"senate_coalition_segunda_merge.rds"))

# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., senado_coalition,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# All 
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# outcomes
out <- c("prop_votes_total_t1")


r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/coat_tails_senate_2_coalition.rds"))
r


###########################################################################################################
##################################### COAT TAILS SENATE + COALITION: RD ###################################
#######################################  COALITION FINAL ROUND ############################################
###########################################################################################################

# coalition FINAL roundS
coalitions_long <- readRDS(paste0(res,"coalitions_new.rds")) %>% 
  dplyr::select(codpartido,ano,year,codmpio,coalition_new)  %>% 
  group_by(codpartido, ano, codmpio, year) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_coalition <- readRDS(paste0(res,"senate_coalition_merge.rds"))
table(coalitions_long$ano,coalitions_long$year)
table(senado_coalition$ano)

# Top 2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., senado_coalition,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# All 
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")


r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/coat_tails_senate_final_coalition.rds"))
r

# Figure doc
pdf(str_c(doc, "/RD_senate_final.pdf"), height=6, width=12)
rdplot(y=l2$prop_votes_total_t1, x=l2$margin_prop_2, c = 0,
       y.lim = c(0.0, 0.4),
       # x.lim = c(0.45, 0.55),
       title = " ",
       x.label = "Victory Margin",
       y.label = "Vote share (subsequent Election)",
       binselect="es", nbins= 10, kernel="triangular", p=3, ci=95
)
dev.off()

###############################################################################
################################ PLACEBO TESTS ################################
###############################################################################

bw_sensibility <- c(seq(0.01, 0.5, by = 0.01), r[[1]]$rd$bws[1, 1]) %>%
  .[sort.list(.)] %>% as.list()

r_sensibility <- mapply(l_f_sens, o = out, bw = bw_sensibility, SIMPLIFY = F)
saveRDS(r_sensibility, str_c(results, "/Placebos", "/coat_tails_senate_final_coalition_placebo.rds"))




###########################################################################################################
###################################### COAT TAILS SENATE + COALITION: RD ##################################
#######################################  CURRENT and FIRST COALITION ######################################
###########################################################################################################

# coalition FIRST round
coalitions_long <- readRDS(paste0(res,"coalitions_current_primera.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new, starts_with("year"))  %>% 
  group_by(codpartido, ano, codmpio, year_first, year_current) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

# Elections t+1
senado_coalition <- readRDS(paste0(res,"senate_coalition_current_primera_merge.rds"))

# Top 2 and drop municipality if at least one of the top2 is 98 or 99
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>%
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T)


# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>%
  merge(., senado_coalition,  by.x = c("year_first", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions

# All
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")


r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_senate_current1_coalition.rds"))
r

###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
#######################################  CURRENT and FINAL COALITION ######################################
###########################################################################################################

# coalition FIRST round
coalitions_long <- readRDS(paste0(res,"coalitions_current_final.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new, starts_with("year"))  %>% 
  group_by(codpartido, ano, codmpio, year_final) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

# Elections t+1
senado_coalition <- readRDS(paste0(res,"senate_coalition_current_final_merge.rds"))
table(senado_coalition$ano)


# Top 2 and drop municipality if at least one of the top2 is 98 or 99
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>%
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T)


# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>%
  merge(., senado_coalition,  by.x = c("year_final", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions

# All
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")


r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_senate_currentfinal_coalition.rds"))
r



###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
#######################################  FIRST  and no CURRENT COALITION ##################################
###########################################################################################################

# coalition FIRST round
coalitions_long <- readRDS(paste0(res,"coalitions_nocurrent_primera.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new, starts_with("year"))  %>% 
  group_by(codpartido, ano, codmpio, year_first, year_current) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))


# Elections t+1
senado_coalition <- readRDS(paste0(res,"senate_coalition_nocurrent_primera_merge.rds"))
table(senado_coalition$ano)


# Top 2 and drop municipality if at least one of the top2 is 98 or 99
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>%
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T)


# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>%
  merge(., senado_coalition,  by.x = c("year_first", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions

# All
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_senate_nocurrent1_coalition.rds"))
r

###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
#######################################  FINAL and no CURRENT COALITION ###################################
###########################################################################################################

# coalition FIRST round
coalitions_long <- readRDS(paste0(res,"coalitions_nocurrent_final.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio,coalition_new, starts_with("year"))  %>% 
  group_by(codpartido, ano, codmpio, year_final, year_current) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))


# Elections t+1
senado_coalition <- readRDS(paste0(res,"senate_coalition_nocurrent_final_merge.rds"))
table(senado_coalition$ano)


# Top 2 and drop municipality if at least one of the top2 is 98 or 99
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>%
  merge(., coalitions_long, by.x = c("codpartido","ano", "codmpio") , by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == F & coalition_new != 98 & coalition_new != 99) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T)


# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>%
  merge(., senado_coalition,  by.x = c("year_final", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions

# All
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")


r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_senate_nocurrentfinal_coalition.rds"))
r





