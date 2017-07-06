###########################################################################################################
############################################# COAT-TAILS ##################################################
############################################ DESCRIPTIVES #################################################
###########################################################################################################

rm(list=ls())
packageList<-c("data.table","foreign","haven","plyr","dplyr","stargazer", "broom", "knitr","tidyr")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
dnp_e <- "Data/DNP/Ejecuciones/"
dnp_d <- "Data/DNP/Desempeno/"
invias <- "Data/invias/"
pgn <- "Data/PGN/"
violencia <- "Data/Violencia/"
agro <- "Data/Agro/"
edu <- "Data/Educacion/"
vitales <- "Data/Vitales/"
noaa <- "Data/NOAA/"

results <- "Results/Descriptives/"

# COntrols
cede <- read_dta(paste0(res, "PanelCEDE/PANEL_CARACTERISTICAS_GENERALES.dta"))
controls <- cede %>%
  dplyr::select(coddepto, codmpio, municipio, ano, nbi) %>%
  filter(ano == 1993) %>%
  merge(., cede, by.x = c("codmpio"), by.y = c("codmpio"), all = T)

# Load ejecuciones
ejecu_all <- read_dta(paste0(dnp_e,"Ejecuciones_all.dta"))
vias_all <- read_dta(paste0(dnp_e,"Vias_SICEP_all.dta"))
invias_all <- read_dta(paste0(invias,"invias_all.dta"))

# Load outcomes
desempeno <- read_dta(paste0(dnp_d,"desempeno_last.dta"))
pgn <- read_dta(paste0(pgn,"PGN_all.dta"))
hom <- read_dta(paste0(violencia,"homicidios_all.dta"))
agro <- read_dta(paste0(agro,"agro_all.dta"))
cobertura <- read_dta(paste0(edu,"cobertura_all.dta"))
icfes <- read_dta(paste0(edu,"icfes_all.dta"))
teen <- read_dta(paste0(vitales,"fert_all.dta"))
mort <- read_dta(paste0(vitales,"tasa_mort_all.dta"))
nightlights <- read_dta(paste0(noaa,"nightlights_all.dta"))

# Load maire data
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))


# Subsequent elections
alcaldes_t1 <-  readRDS(paste0(res,"alcaldes_t1.rds"))
representantes <- readRDS(paste0(res,"representantes_merge.rds")) 
senado <- readRDS(paste0(res,"senado_merge.rds")) 
president_1 <- readRDS(paste0(res, "presidentes_primera_merge.rds")) 
president_2 <- readRDS(paste0(res, "presidentes_segunda_merge.rds")) 



###########################################################################################################
#########################################  MAYORAL ########################################################
######################################### ELECTIONS #######################################################
###########################################################################################################

# Number of elections in sample 
elections <- alcaldes_merge %>% filter(cand==1) %>% mutate(n=1) %>% group_by(ano, codmpio) %>% summarize(cand = sum(n))  %>% 
  ungroup() %>% mutate(n=1) %>% summarize(n = sum(n), cand = sum(cand))

# Parties 
parties <- alcaldes_merge %>% filter(cand==1) %>% 
  filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>%
  mutate(n=1) %>%  group_by(codpartido) %>% summarize(part = sum(n))  
dim(parties)

# Parties by year
parties_y <- alcaldes_merge %>% filter(cand==1) %>% 
  filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>% 
  mutate(n=1) %>%  group_by(codpartido, ano) %>% summarize(part = sum(n))  %>% 
  mutate(y = 1) %>% group_by(ano) %>% summarize(y = sum(y))

# Parties with more than 1 year 
parties_s <- alcaldes_merge %>% filter(cand==1) %>% filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>% mutate(n=1) %>%  group_by(codpartido, ano) %>% summarize(part = sum(n))  %>% 
  mutate(y = 1) %>% group_by(codpartido) %>% summarize(y = sum(y)) %>% 
  mutate(yy = 1) %>% group_by(y) %>% summarize(yy = sum(yy))


# Candidates and vote share 
des_ele_c <- alcaldes_merge %>% filter(ano != 2015) %>% 
  filter(cand==1) %>% 
  filter(!is.finite(prop_votes_total)==F) %>% filter(prop_votes_total <= 1) %>% 
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn != 1) %>%
  summarize(cand = n()) %>% 
  ungroup() %>% dplyr::select(cand) %>% as.data.frame(.)

stargazer(des_ele_c, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

# Vote share (all candidates)
des_ele <- alcaldes_merge %>% filter(ano != 2015) %>% 
  filter(cand==1) %>% 
  filter(!is.finite(prop_votes_total)==F) %>% filter(prop_votes_total <= 1) %>% 
  dplyr::select(prop_votes_total)
stargazer(des_ele, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

# Vote share top2
des_ele2 <- alcaldes_merge_r2 %>% filter(!is.finite(prop_votes_total)==F) %>% filter(prop_votes_total <= 1) %>% 
  group_by(ano, codmpio) %>% summarize(vs2 = sum(prop_votes_total)) %>% filter(vs2 <= 1) %>% ungroup() %>%
  dplyr::select(vs2) %>% as.data.frame(.)
stargazer(des_ele2, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

# Winner vote share and Victory Margin
des_ele <- alcaldes_merge_r2 %>% filter(cand==1) %>% filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>% 
  filter(rank==1) %>% dplyr::select(prop_votes_total, prop_votes_total, margin_prop_2) 
stargazer(des_ele, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)


###########################################################################################################
#########################################  NATIONAL #######################################################
######################################### ELECTIONS #######################################################
###########################################################################################################

#############
# Need to balance panels first: parties by year for all municipalities
#############

panel <- function(x){
  e <- unique(x$ano)
  p <- lapply(e, function(a){
    xx <- x %>% filter(ano == a) 
    data.table(expand.grid(unique(xx$codmpio) , unique(xx$codpartido))) %>% 
    rename(codmpio = Var1,codpartido = Var2) %>%
    mutate(ano = a)
    }) %>%
    ldply() %>%
    merge(., x,  by = c("ano", "codmpio","codpartido"), all.x = T) %>%
    mutate(prop_votes_total = ifelse(is.na(prop_votes_total)==T,0,prop_votes_total)) 
    return(p)
}


# House 

des_rep_c <-panel(representantes) %>%
  group_by(codmpio, ano) %>%
  group_by(ano, codmpio) %>% summarize(cand = n()) %>% 
  ungroup() %>% dplyr::select(cand) %>% as.data.frame(.)
stargazer(des_rep_c, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

des_rep <- panel(representantes) %>% 
  dplyr::select(prop_votes_total)
stargazer(des_rep, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)


# Senate 
des_sen_c <- panel(senado) %>%
  group_by(codmpio, ano) %>%
  group_by(ano, codmpio) %>% summarize(cand = n()) %>% 
  ungroup() %>% dplyr::select(cand) %>% as.data.frame(.)
stargazer(des_sen_c, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

des_sen <- panel(senado) %>% 
  dplyr::select(prop_votes_total)
stargazer(des_sen, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)


# President: first round
des_pres1_c <- panel(president_1) %>%
  group_by(codmpio, ano) %>%
  group_by(ano, codmpio) %>% summarize(cand = n()) %>% 
  ungroup() %>% dplyr::select(cand) %>% as.data.frame(.)
stargazer(des_pres1_c, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

des_pres1 <- panel(president_1) %>% 
  dplyr::select(prop_votes_total)
stargazer(des_pres1, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)


# President: second round
cand_apellido <- c("SERPA","PASTRANA", "MOCKUS","ZULUAGA", "SANTOS")

des_pres2_c <- president_2 %>% filter(ano != 2002 & ano != 2006) %>% filter(primer_apellido %in% cand_apellido) %>% # table(des_pres2$ano, des_pres2$primer_apellido)
  group_by(codmpio, ano) %>%
  group_by(ano, codmpio) %>% summarize(cand = n()) %>% 
  ungroup() %>% dplyr::select(cand) %>% as.data.frame(.)
stargazer(des_pres2_c, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

des_pres2 <- president_2 %>% filter(ano != 2002 & ano != 2006) %>% filter(primer_apellido %in% cand_apellido) %>% # table(des_pres2$ano, des_pres2$primer_apellido) 
  dplyr::select(prop_votes_total)
stargazer(des_pres2, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2)

###########################################################################################################
########################################## Outcomes #######################################################
###########################################################################################################

# Dataset: incumbency regression 

alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(ano != 2015) %>%
  filter(rank <= 2) %>% 
  arrange(codmpio, ano, codpartido) %>%
  mutate(ano = as.character(ano)) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  dplyr::select(-c(n,nn)) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) %>% 
  merge(., ejecu_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., vias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., invias_all,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., desempeno,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., pgn,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., hom,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., agro,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., cobertura,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., icfes,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., teen,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., mort,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T) %>%
  merge(., nightlights,  by.x = c("ano", "codmpio"), by.y = c("per", "codmpio"), all.x = T)

# Descriptive statistics: Characteristics and outcomes

# des <- alcaldes_merge_r2 %>% dplyr::select(vias, f_propios, f_sgp_reg, f_SGPp, f_regalias, f_trans_nac, light_pix,light_dm, ba_tot_vr, ba_peq_vr,  desemp_fisc,desemp_int, alcalde, alcalde_guilty, top, top_guilty, pobl_tot.x, altura,discapital, disbogota, nbi.x)
 des <- alcaldes_merge_r2 %>% dplyr::select(vias, f_propios, f_sgp_reg, f_SGPp, f_regalias, f_trans_nac, D, D4000, sgp_reg, D2000, D1000, D3000, tasa_m, cob_pri, cob_sec, matematicas_s,lenguaje_s,fert_19_10_p,hom_tasa, desemp_fisc,desemp_int, alcalde, alcalde_guilty, top, top_guilty, light_pix,light_dm, ba_tot_vr, ba_peq_vr, pobl_tot.x, altura,discapital, disbogota, nbi.x)
# des <- alcaldes_merge_r2 %>% dplyr::select(log_vias, log_f_SGPp, log_f_regalias, log_f_trans_nac, log_D, log_D2000, log_D1000, log_D3000, tasa_m, cob_pri, cob_sec, matematicas_s,lenguaje_s,fert_19_10_p,hom_tasa, desemp_fisc,desemp_int, alcalde, alcalde_guilty, top, top_guilty, log_light_pix, log_light_dm, log_ba_tot_vr, log_ba_peq_vr, pobl_tot.x, altura,discapital, disbogota, nbi.x)

setwd(results)
stargazer(des, summary.stat = c("mean", "sd", "median", "min", "max"), type = "latex", digits = 2, out= "descriptives.tex")



###########################################################################################################
#################################### MEAN DIFFERENCE ######################################################
##################################### by coalition ########################################################
###########################################################################################################

# Computes groups' mean, sd, and t-test of all variables in b
# runner-up first!

table_test <- function(b){
  out <- c(2:dim(b)[2])
  tt <- lapply(out, function(x){
    t1 <- b %>% mutate(v= b[,x]) %>% filter(is.na(v)==FALSE) %>%
      group_by(win_t) %>% summarise(mean = mean(v), sd = sd(v)) %>%
      gather(., Var, Val,  mean:sd) %>% arrange(win_t) %>% dplyr::select(Val) %>% t(.)
    t2 <- tidy(t.test(b[,x]~win_t, data=b)) %>% dplyr::select(p.value)
    t <- cbind(t1,t2)
  }) %>% ldply()
  return(tt)
}

######################
# Party

base <- alcaldes_merge %>% 
  filter(rank<=2) %>% 
  filter(cand==1) %>% 
  filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  group_by(ano, codmpio, codpartido) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  dplyr::select(-c(n,nn, party_2)) %>% 
  mutate(win_t = ifelse(rank == 1, 1, 0)) %>%
  filter(abs(margin_prop_2)<0.1) %>%
  merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) %>%
  dplyr::select(win_t, pobl_tot, altura, discapital, disbogota, nbi.x)

tab_party <- table_test(base)
tab_party
kable(tab_party, format = "latex", row.names=T, digits=2)

######################
# Coalitions

coalitions_primera <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 
coalitions_segunda <- readRDS(paste0(res,"coalitions_segunda_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 
coalitions_final <- readRDS(paste0(res,"coalitions_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 
coalitions_current <- readRDS(paste0(res,"coalitions_current.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new)
coalitions_current_final <- readRDS(paste0(res,"coalitions_current_final.rds")) %>% dplyr::select(codpartido,ano,codmpio, coalition_new)

coalitions_list <- list(coalitions_primera, coalitions_segunda, coalitions_final, coalitions_current, coalitions_current_final)

table_coal <- lapply(coalitions_list, function(c){
    base <- alcaldes_merge %>% filter(ano != 2015) %>%
    filter(cand==1) %>%
    filter(rank <= 2) %>% 
    merge(., c, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
    arrange(codmpio, ano, codpartido) %>%
    filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99)  %>%
    group_by(codmpio, ano) %>%
    mutate(n = 1, nn = sum(n)) %>%
    filter(nn==2) %>%
    filter(coalition_new == 1) %>%
    group_by(ano, codmpio) %>%
    mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
    filter(party_2 == 1) %>% 
    dplyr::select(-c(codep,n,nn,party_2)) %>%
    mutate(win_t = ifelse(rank == 1, 1, 0)) %>% 
      filter(abs(margin_prop_2)<0.1) %>%
      merge(., controls[, c("pobl_tot", "coddepto.x", "ano.y", "codmpio", "altura", "discapital", "disbogota", "nbi.x")], by.x = c("codmpio", "ano"), by.y = c("codmpio", "ano.y"), all.x = T) %>% 
    dplyr::select(win_t, pobl_tot, altura, discapital, disbogota, nbi.x)
    tab <- table_test(base)
    return(tab)
  })  

#%>% ldply()

lapply(c(1:length(coalitions_list)), function(c){
  kable(table_coal[[c]], format = "latex", row.names=T, digits=2)
  }) 



###########################################################################################################
################################### DIFFERENCES BETWEEN 1 AND 2 ###########################################
###########################################################################################################

# Differences between winner and best runner up 

# Only rank <= 2 and drop if total votes == 0 
alcaldes_merge_r2 <- alcaldes_merge %>% filter(rank <= 2)
#  %>%  filter(prop_votes_candidates < 1) #drop elections with only one candidate

# Diagnistics
hist(alcaldes_merge_r2$prop_votes_c2)
alcaldes_merge_r2 %>% filter(is.na(prop_votes_c2)==0) %>%
  group_by(rank) %>% summarize(mean=mean(prop_votes_c2),sd=sd(prop_votes_c2),median=median(prop_votes_c2),min=min(prop_votes_c2),max=max(prop_votes_c2))

# Calculate difference between first and second
alcaldes_difference <- alcaldes_merge_r2 %>%
  arrange(codmpio, ano, desc(rank)) %>%
  group_by(codmpio, ano) %>% #Calculate difference
  mutate(diff =  ave(prop_votes_c2, factor(codmpio), factor(ano), FUN = function(x) c(0, diff(x)))) %>%
  mutate(diff = ifelse(diff==0 & rank==1 & prop_votes_c2 == 1, 1, diff))

#Collapse by codmpio and year, and create categorical variables
alcaldes_difference <- alcaldes_difference %>%
  dplyr::group_by(codmpio, ano) %>%
  dplyr::summarize(votes_tot = sum(votos), parties = mean(parties),parties_ef = mean(parties_ef), difference = sum(diff)) 

alcaldes_difference$dif_q <- quantcut(alcaldes_difference$difference, labels=c(1,2,3,4))
saveRDS(alcaldes_difference,paste0(res,"alcaldes_difference.rds"))

# check quantcut
# alcaldes_difference %>% group_by(dif_q) %>% summarize(mean=mean(difference),sd=sd(difference),min=min(difference),max=max(difference))
