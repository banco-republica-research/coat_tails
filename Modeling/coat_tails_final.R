###########################################################################################################
############################################# COAT-TAILS RD ###############################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","broom","gtools","TraMineR","cluster", "rdrobust", "rddensity")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
results <- "Results/RD/"
doc <- "Results/RD/Graphs/RD/"

###########################################################################################################
######################################## ELECTIONS DATA ###################################################
###########################################################################################################

# Load data
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

#NOT FINAL ELECTIONS ONLY SECOND ROUND

# Load party codes and municipal covariates
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

cede <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))
controls <- cede %>%
  dplyr::select(coddepto, codmpio, municipio, ano, nbi) %>%
  filter(ano == 1993) %>%
  merge(., cede, by.x = c("codmpio"), by.y = c("codmpio"), all = T)


# Load presidential for t+1
# Only Parties with candidate in final round! 

win_apellido <- c("PASTRANA", "URIBE", "SANTOS")
win_nom <- c("ANDRES", "ALVARO", "JUAN MANUEL")
runner_apellido <- c("SERPA","MOCKUS","ZULUAGA") 

president_all <- readRDS(paste0(res, "presidentes_segunda_merge.rds"))


president <- president_all %>%  mutate(coalition = ifelse(primer_apellido %in% win_apellido & nombre %in% win_nom , 1, 0))
table(president$primer_apellido, president$ano)
table(president$codpartido, president$ano)

pres_cand <- president %>% 
  filter(ano != 2002 & ano != 2006) %>%
  filter(primer_apellido %in% win_apellido | primer_apellido %in% runner_apellido) %>%
  dplyr::select(ano,codpartido,primer_apellido) %>%
  unique(.) 

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
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
############################################## BY PARTY ###################################################
############################################# FINAL ROUNDS ################################################
###########################################################################################################

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

# Use the same data base but merge with between party codes instead of coalition
# Only parties with candidates running to presidency

alcaldes_rd <- alcaldes_merge_r2 %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  group_by(ano, codmpio, codpartido) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  merge(., president_all,  by.x = c("year", "codmpio", "codpartido"), by.y = c("ano", "codmpio", "codpartido"), 
        suffixes = c("_t", "_t1"), all.x = F) %>%   
  filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  #filter(is.na(prop_votes_c2) == F | prop_votes_c2 != 0.5) %>%
  arrange(codmpio, ano)

############################
# RD and OLS regressions 

# outcomes
out <- c("prop_votes_total_t1")

# Second rounds only
l <- alcaldes_rd 
# %>% filter(ano > 1997)  # Solo Santos
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_pressec_final_party.rds"))
r


###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################### PRESIDENT FINAL ROUND ###########################################
###########################################################################################################

# coalition SECOND roundS
# coalitions_long <- readRDS(paste0(res,"coalitions_new.rds")) %>% 
#   dplyr::select(codpartido,ano,year, codmpio,coalition_new) %>%
#   unique(.)

coalitions_long <- readRDS(paste0(res,"coalitions_new.rds")) %>% 
  filter(coalition_new == 0 | coalition_new == 1) %>%
  group_by(codpartido,ano,year, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

table(coalitions_long$ano,coalitions_long$year)

# top2 and drop municipality if at least one of the top2 is 98 or 99 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>% 
  merge(., coalitions_long, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99)  %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn==2) %>%
  dplyr::select(-c(codep,n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) 

table(alcaldes_merge_r2$ano)
dim(alcaldes_merge_r2)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
  filter(party_2 == 1) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., president,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

dim(alcaldes_rd)

############################
# RD and OLS regressions 

# outcomes
out <- c("prop_votes_total_t1")

# Second rounds only
l <- alcaldes_rd  
# %>% filter(ano > 1997)  # Solo Santos
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_pressec_final_coalition.rds"))
r

pdf(str_c(doc, "RD_pressec_final.pdf"), height=6, width=12)
rdplot(y=l2$prop_votes_total_t1, x=l2$margin_prop_2, c = 0,
       y.lim = c(0.3, 0.8),
       # x.lim = c(0.45, 0.55),
       title = " ",
       x.label = "Victory Margin",
       y.label = "Vote share (subsequent Election)",
       binselect="es", nbins= 10, kernel="triangular", p=3, ci=95
)
dev.off()

#################
# Reelection

# before
l <- alcaldes_rd %>% filter(ano <= 2002)
r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_pressec_final_coalition_before_reelection.rds"))
r

# after
l <- alcaldes_rd %>% filter(ano >= 2002)
r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_pressec_final_coalition_after_reelection.rds"))
r

###############################################################################
################################ PLACEBO TESTS ################################
###############################################################################

bw_sensibility <- c(seq(0.01, 0.5, by = 0.01), r[[1]]$rd$bws[1, 1]) %>%
  .[sort.list(.)] %>% as.list()

r_sensibility <- mapply(l_f_sens, o = out, bw = bw_sensibility, SIMPLIFY = F)
saveRDS(r_sensibility, str_c(results, "Placebos", "/coat_tails_pressec_2_coalition_placebo.rds"))

###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################## CURRENT and FINAL COALITION ######################################
###########################################################################################################

# Load coalitions:
coalitions_long <- readRDS(paste0(res,"coalitions_current_final.rds")) %>% 
  filter(coalition_new == 0 | coalition_new == 1) %>%
  group_by(codpartido,ano,year_final, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

table(coalitions_long$ano,coalitions_long$year_final)
table(coalitions_long$coalition_new)

# top2 and drop municipality if at least one of the top2 is 98 or 99
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>%
  merge(., coalitions_long, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99)  %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn==2) %>%
  dplyr::select(-c(codep,n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T)

dim(alcaldes_merge_r2)

table(alcaldes_merge_r2$ano, alcaldes_merge_r2$year_final)
table(president$ano)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>%
  merge(., president,  by.x = c("year_final", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  # dplyr::select(codmpio, pobl_tot, coddepto, ano, year, codpartido_t, win_t,
  # votos_t, votos_t1, starts_with("prop")) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)


############################
# RD and OLS regressions

# Second rounds only
l <- alcaldes_rd
# %>% filter(ano > 1997)  # Solo Santos
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# outcomes
out <- c("prop_votes_total_t1")


r <- lapply(out, l_f)

saveRDS(r, str_c(results, "/coat_tails_pressec_currentfinal_coalition.rds"))
r


###########################################################################################################
##################################### RD: REVERSE COAT-TAILS EFFECT #######################################
######################################## FINAL but no CURRENT COALITION ###################################
###########################################################################################################

# Load coalitions:
coalitions_long <- readRDS(paste0(res,"coalitions_nocurrent_final.rds")) %>% 
  filter(coalition_new == 0 | coalition_new == 1) %>%
  group_by(codpartido,ano,year_final, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

table(coalitions_long$ano,coalitions_long$year_final)
table(coalitions_long$coalition_new)

# top2 and drop municipality if at least one of the top2 is 98 or 99
alcaldes_merge_r2 <- alcaldes_merge %>%
  filter(ano != 2015) %>%
  filter(cand==1) %>%
  filter(rank <= 2) %>%
  merge(., coalitions_long, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
  arrange(codmpio, ano, codpartido) %>%
  filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99)  %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn==2) %>%
  dplyr::select(-c(codep,n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T)

dim(alcaldes_merge_r2)

table(alcaldes_merge_r2$ano, alcaldes_merge_r2$year_final)
table(president$ano)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>%
  merge(., president,  by.x = c("year_final", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  # dplyr::select(codmpio, pobl_tot, coddepto, ano, year, codpartido_t, win_t,
  # votos_t, votos_t1, starts_with("prop")) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)


############################
# RD and OLS regressions

# Second rounds only
l <- alcaldes_rd
# %>% filter(ano > 1997)  # Solo Santos
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# outcomes
out <- c("prop_votes_total_t1")


r <- lapply(out, l_f)

saveRDS(r, str_c(results, "/coat_tails_pressec_nocurrentfinal_coalition.rds"))
r



