###########################################################################################################
############################################# TURNOUT HOUSE: RD ###########################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr",
               "broom","gtools","cluster", "rdrobust", "haven")

lapply(packageList,library,character.only=TRUE)

# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
#setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
results <- "Results/RD/"
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

population_age <- read_dta(paste0("Data/Poblacion/", "Proyeccion_Poblacion_DANE_2005_2020.dta"))
population_groups <- read_dta(paste0("Data/Poblacion/", "Edades_Simples_1985_2020.dta"))

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
  mean <-   mean(l[,o], na.rm = T)
  
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
#################################### NAIVE INTERPOLATION OF POPULATION ####################################
###########################################################################################################
group_ages_strings = str_c("Total", c(15:19)) 

#Calculate proportion of people over and equal to 18 within the age group
#15-19 using as reference the projections per age from 2005 to 2020.

pop_over_18 <- population_age %>%
  dplyr::select(DPNOM:Total19) %>%
  mutate(total_group_15_19 = rowSums(.[, group_ages_strings])) %>%
  mutate(prop_18_19 = (Total18 + Total19) / total_group_15_19) %>%
  filter(ano == 2005) %>%
  dplyr::select(-c(ano, starts_with("Total"), DPNOM, MPIO))

#Naive interpolation of adults (>18 yo.) inside the population age groups
#from 1985 to 2020.

pop_groups_long <- population_groups %>%
  dplyr::select(codigo, g_edad, ends_with("_T")) %>%
  gather(., year, population_total, a1985_T:a2020_T) %>%
  mutate(year = str_extract(year, "[0-9][0-9][0-9][0-9]"),
         g_edad = as.character(g_edad)) %>%
  filter(codigo != 100)

pop_18_19 <- pop_groups_long %>%
  filter(g_edad == "15-19") %>% 
  merge(., pop_over_18, by.x = c("codigo"), by.y = c("cod_muni"), all.x = T) %>%
  mutate(population_total = round(population_total * prop_18_19, 0)) %>%
  dplyr::select(codigo, year, population_total) %>%
  mutate(g_edad = "18-19")


adults_group_ages <- c("18-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                       "45-49", "50-54", "55-59", "60-64", "65-69", "70-74")
  

adult_pop <- pop_groups_long %>%
  rbind(., pop_18_19) %>%
  dplyr::arrange(codigo, year, g_edad) %>%
  filter(g_edad %in% adults_group_ages ) %>%
  group_by(codigo, year) %>%
  summarize(total_adult_pop = sum(population_total, na.rm = T)) %>%
  ungroup() %>%
  mutate(codigo = as.numeric(codigo))

rm(population_age, population_groups, pop_groups_long)

###########################################################################################################
###################################### TURNOUT HOUSE + COALITION: RD ######################################
########################################  COALITION FIRST ROUND ###########################################
###########################################################################################################

# coalition FIRST round
coalitions_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% 
  dplyr::select(codpartido,ano,year,codmpio,coalition_new) %>% 
  group_by(codpartido, ano, codmpio, year) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))


representantes_coalition <- readRDS(paste0(res,"representantes_coalition_primera_merge.rds"))
table(representantes_coalition$ano)

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
  merge(., representantes_coalition,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

#Add voter population data

alcaldes_rd <- alcaldes_rd %>%
  merge(., adult_pop, by.x = c("ano", "codmpio"), by.y = c("year", "codigo")) %>%
  mutate(., turnout_total = votos_t / total_adult_pop,
            turnout_cand = votos_cand_t / total_adult_pop,
            turnout_dif = votos_cand_t / )

############################
# RD and OLS regressions 

# All 
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

out <- c("turnout_total", "turnout_cand", "turnout_dif")

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/turnout_house_1_coalition.rds"))
r


###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
#######################################  COALITION SECOND ROUND ###########################################
###########################################################################################################


# coalition second rounds
coalitions_long <- readRDS(paste0(res,"coalitions_segunda_new.rds")) %>% 
  dplyr::select(codpartido,ano,year,codmpio,coalition_new)  %>% 
  group_by(codpartido, ano, codmpio, year) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

representantes_coalition <- readRDS(paste0(res,"representantes_coalition_segunda_merge.rds"))
table(coalitions_long$ano,coalitions_long$year)
table(representantes_coalition$ano)

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

#Add voter population data
alcaldes_rd <- alcaldes_rd %>%
  merge(., adult_pop, by.x = c("ano", "codmpio"), by.y = c("year", "codigo")) %>%
  mutate(., turnout = votos_t / total_adult_pop)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., representantes_coalition,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"), 
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
saveRDS(r, str_c(results, "/coat_tails_house_2_coalition.rds"))
r



###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
########################################  COALITION FINAL ROUND ###########################################
###########################################################################################################

# coalition FINAL roundS
coalitions_long <- readRDS(paste0(res,"coalitions_new.rds")) %>% 
  dplyr::select(codpartido,ano,year,codmpio,coalition_new) %>%
  unique(.)

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

# Representatant for t+1
representantes_coalition <- readRDS(paste0(res,"representantes_coalition_merge.rds"))
table(representantes_coalition$ano)

# For a specific party (or group of parties), merge RD in t to outcomes in t+1
# Drop elections where party is both 1 and 2 in t

alcaldes_rd <- alcaldes_merge_r2 %>%
  filter(coalition_new == 1) %>%
  group_by(ano, codmpio) %>%
  mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition
  filter(party_2 == 1) %>%
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
  merge(., representantes_coalition,  by.x = c("year", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"), 
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

table(alcaldes_rd$ano, alcaldes_rd$year)

############################
# RD and OLS regressions 

# All 
l <- alcaldes_rd 
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)

# outcomes
out <- c("prop_votes_total_t1")


r <- lapply(out, l_f) 
saveRDS(r, str_c(results, "/coat_tails_house_final_coalition.rds"))
r

# Figure doc
pdf(str_c(doc,"/RD_house_final.pdf"), height=6, width=12)
rdplot(y=l2$prop_votes_total_t1, x=l2$margin_prop_2, c = 0,
       y.lim = c(0.1, 0.4),
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
saveRDS(r_sensibility, str_c(results, "Placebos", "/coat_tails_house_final_coalition_placebo.rds"))



###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
#######################################  CURRENT and FIRST COALITION ######################################
###########################################################################################################

# coalition CURRENT AND FINAL round at t 
coalitions_long <- readRDS(paste0(res,"coalitions_current_primera.rds")) %>% 
  dplyr::select(codpartido,ano, codmpio, coalition_new,year_first) %>%
  unique(.)
table(coalitions_long$ano,coalitions_long$year_first)


# Elections t+1
representantes_coalition <- readRDS(paste0(res,"representantes_coalition_current_primera_merge.rds")) 
table(representantes_coalition$ano)


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
  merge(., representantes_coalition,  by.x = c("year_first", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

table(alcaldes_rd$ano, alcaldes_rd$year_first)


############################
# RD and OLS regressions

# All
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")


r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_house_current1_coalition.rds"))
r




###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
#######################################  CURRENT and FINAL COALITION ######################################
###########################################################################################################

# coalition CURRENT AND FINAL round at t 
coalitions_long <- readRDS(paste0(res,"coalitions_current_final.rds")) %>% 
  dplyr::select(codpartido,ano, codmpio, coalition_new,year_final) %>%
  unique(.)
table(coalitions_long$ano,coalitions_long$year_final)


# Elections t+1
representantes_coalition <- readRDS(paste0(res,"representantes_coalition_current_final_merge.rds")) 
table(representantes_coalition$ano)


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
  merge(., representantes_coalition,  by.x = c("year_final", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

table(alcaldes_rd$ano, alcaldes_rd$year_final)


############################
# RD and OLS regressions

# All
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_house_currentfinal_coalition.rds"))
r


###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
#######################################  FIRST but no CURRENT COALITION ###################################
###########################################################################################################

# coalition CURRENT AND FINAL round at t 
coalitions_long <- readRDS(paste0(res,"coalitions_nocurrent_primera.rds")) %>% 
  dplyr::select(codpartido,ano, codmpio, coalition_new,year_first) %>%
  unique(.)
table(coalitions_long$ano,coalitions_long$year_first)


# Elections t+1
representantes_coalition <- readRDS(paste0(res,"representantes_coalition_nocurrent_primera_merge.rds")) 
table(representantes_coalition$ano)


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
  merge(., representantes_coalition,  by.x = c("year_first", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

table(alcaldes_rd$ano, alcaldes_rd$year_first)


############################
# RD and OLS regressions

# All
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_house_nocurrent1_coalition.rds"))
r


###########################################################################################################
###################################### COAT TAILS HOUSE + COALITION: RD ###################################
####################################### FINAL but no CURRENT COALITION ####################################
###########################################################################################################

# coalition CURRENT AND FINAL round at t 
coalitions_long <- readRDS(paste0(res,"coalitions_nocurrent_final.rds")) %>% 
  dplyr::select(codpartido,ano, codmpio, coalition_new,year_final) %>%
  unique(.)
table(coalitions_long$ano,coalitions_long$year_final)


# Elections t+1
representantes_coalition <- readRDS(paste0(res,"representantes_coalition_nocurrent_final_merge.rds")) 
table(representantes_coalition$ano)


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
  merge(., representantes_coalition,  by.x = c("year_final", "codmpio", "coalition_new"), by.y = c("ano", "codmpio", "coalition_new"),
        suffixes = c("_t", "_t1"), all.x = T) %>%
  mutate(run_t1=ifelse(is.na(prop_votes_total_t1), 0,1)) %>%
  mutate(prop_votes_total_t1= ifelse(run_t1 == 1, prop_votes_total_t1, 0)) %>%
  filter(is.na(prop_votes_c2) == F) %>%
  arrange(codmpio, ano)

table(alcaldes_rd$ano, alcaldes_rd$year_final)


############################
# RD and OLS regressions

# All
l <- alcaldes_rd
l2 <- l %>% filter(prop_votes_c2 <= 0.6 & prop_votes_c2 >= 0.4)


# outcomes
out <- c("prop_votes_total_t1")

r <- lapply(out, l_f)
saveRDS(r, str_c(results, "/coat_tails_house_nocurrentfinal_coalition.rds"))
r



