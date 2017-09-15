
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "tidyr", "forcats", "stringr", "openxlsx")
lapply(packageList,library,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

  data <-"Data/CEDE/Microdatos/"
  coal <-"Data/CEDE/coaliciones/"
  res <-"Data/CEDE/Bases/"
  results <- "Results/RD"

# Paper gender  
women <- "D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones_Mujeres/Data/Elections/"  
  
###########################################################################################################
############################ Winners and loosers since 1997  ##############################################
###########################################################################################################

# Get party-code list: 
##################################### ERROR: CÓDIGOS DE PARTIDO  ##########################################
# Error en codigos de CEDE excel que está corrido de 1 a partir de 434 con respecto a PDF (diccionario)   #
# de donde viene el .dta. En bases de datos, correcto para todos los años excepto 2011 en que se toman    #
# codigos de xls. Para corregir esto se toma nombre de partido en 2011, y se hace merge para recuperar    #
# partidos.                                                                                               #
# Big parties with different codes: Cambio Radical (41, but is 165), PIN (19, buy 302), AICO (13, but 159)#
###########################################################################################################

# Load party dictionary
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

# Get mayor's election data (Winners and loosers since 1997). 

list_files <- list.files(path=data) %>%
  .[. %in% c("1997", "2000", "2003", "2007", "2011", "2015")]%>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Alcal")]}) %>% 
  str_c(data, c("1997", "2000", "2003", "2007", "2011", "2015"),"/", ., sep = "")

#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
alcaldes <- lapply(list_files, read_dta) 

# Party codes in 2003: # correct POLO that changed name
alcaldes[[3]] <- alcaldes[[3]] %>%
  mutate(codpartido = as.factor(codpartido)) %>%
  mutate(codpartido = fct_recode(codpartido, "194" = "164")) %>% 
  mutate(codpartido = as.character(codpartido)) 
table(alcaldes[[3]]$codpartido)

# Party codes in 2011: merged from party name
# correct parties that changed name
alcaldes[[5]] <- alcaldes[[5]] %>%
  mutate(partido_1 = as.factor(partido_1)) %>%
  mutate(partido_1 = fct_recode(partido_1,
                                "MOVIMIENTO ALIANZA SOC INDIGENA ASI" = "PARTIDO ALIANZA SOCIAL INDEPENDIENTE",
                                "PARTIDO DE INTEGRACION NACIONAL PIN" = "PARTIDO DE INTEGRACION NACIONAL" )) %>% 
  mutate(partido_1 = as.character(partido_1)) %>%
  mutate(nombre = "") %>%
  stringdist_left_join(party_code, by = c(partido_1 = "name_party"), distance_col = "distance", max_dist = 2) %>% #Correct party_codes using the dictionary
  plyr::rename(., c("party_code" = "codpartido"))


# Identify mismatch (run before running the fct_recode line above)
# c(setdiff(unique(alcaldes_aggregate[[5]]$codigo_partido_1), unique(alcaldes_aggregate[[5]]$party_code)))
# a <- alcaldes_aggregate[[5]] %>% filter(partido_1 == name_party & codigo_partido_1 != party_code) %>%
#   plyr::rename(., c("party_code" = "codpartido"))

# Party codes in 2015: correct ASI that changed name
alcaldes[[6]] <- alcaldes[[6]] %>%
  mutate(codpartido = as.factor(codpartido_1))%>%
  mutate(codpartido = fct_recode(codpartido, "15" = "654")) %>% 
  mutate(codpartido = as.character(codpartido)) 
# table(alcaldes[[6]]$codpartido)


#Check for not-joined data
# not_joined <- alcaldes[[5]] %>% filter(is.na(codpartido))

#Aggregate totals for each year and clean non-candidate data

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")

alcaldes_aggregate <- alcaldes %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
      filter(is.na(votos)==0) %>% 
      mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
      mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>% 
      group_by(codmpio, ano) %>%
      arrange(desc(votos)) %>%
      mutate(rank = row_number(desc(votos))) %>% 
      mutate(prop_votes_total = votos / sum(votos)) %>%
      mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>%
      mutate(prop_votes_cand = votos / sum(votos_cand)) %>%
      mutate(votos_r2 = ifelse(rank <= 2, votos ,0)) %>% 
      mutate(prop_votes_c2 = votos / sum(votos_r2)) %>% 
      # mutate(diff = votos - lag(votos, default=first(votos))) %>%
      # mutate(margin_prop = diff / sum(votos_r2)) %>%
      mutate(parties = sum(cand)) %>%
      mutate(margin_prop_2 = 2*(prop_votes_c2) -1) %>%
      mutate(party_ef = ifelse(prop_votes_cand > 0.1, 1,0)) %>%
      mutate(parties_ef = sum(party_ef)) %>% 
      filter(is.na(prop_votes_total)==0) 
  })

# Test
# a <- alcaldes_aggregate[[5]] %>% filter(partido_1 == name_party & codigo_partido_1 != codpartido)


# Reshape data in a long format
alcaldes_merge <- alcaldes_aggregate %>%
  ldply() %>%  
  dplyr::select(c(ano, codmpio, codep, municipio, parties, parties_ef, rank, primer_apellido, nombre, codpartido, cand, votos, votos_cand, votos_r2,
                  prop_votes_total, prop_votes_cand, prop_votes_c2, margin_prop_2)) %>%
  arrange(codmpio, ano, rank) %>%
  mutate(ano = as.integer(ano))


#Prueba
alcaldes_merge_prueba <- alcaldes_merge %>%
  filter(rank <= 2) %>%
  group_by(rank) %>%
  summarise(min_prop = min(prop_votes_c2, na.rm = T),
            max_prop = max(prop_votes_c2, na.rm = T),
            mean_prop = mean(prop_votes_c2, na.rm = T))


saveRDS(alcaldes_merge,paste0(res,"alcaldes_merge.rds"))

# Save for elections women
# saveRDS(alcaldes_merge,paste0(women,"alcaldes_merge.rds"))
write_dta(alcaldes_merge,paste0(women,"alcaldes_merge.dta"))




###########################################################################################################
#################################### Only winners 1988-1994  ##############################################
###########################################################################################################

list_files_old <- list.files(path=data) %>%
  .[. %in% c("1988", "1990", "1992", "1994")]%>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Alcal")]}) %>% 
  str_c(data, c("1988", "1990", "1992", "1994"),"/", ., sep = "")

alcaldes_old <- lapply(list_files_old, read_dta) 

non_candidate_votes <- c("RESTO DE VOTACION","VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")

alcaldes_aggregate_old <- alcaldes_old %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
      filter(is.na(votos)==0) %>% 
      mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
      mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>% 
      group_by(codmpio, ano) %>%
      mutate(prop_votes_total = votos / sum(votos)) %>%
      mutate(n = n())     %>%
      mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>% 
      mutate(rank = row_number(desc(votos_cand))) %>% 
      filter(rank==1 & no_cand==0)   %>%
      filter(is.na(prop_votes_total)==0) 
  })

alcaldes_merge_old <- alcaldes_aggregate_old %>%
  ldply() %>%
  arrange(codmpio, ano, desc(rank)) %>%
  dplyr::select(c(ano, codmpio, municipio, primer_apellido, nombre, codpartido, votos, prop_votes_total)) 

hist(alcaldes_merge_old$prop_votes_total)
saveRDS(alcaldes_merge_old,paste0(res,"alcaldes_merge_old.rds"))




###########################################################################################################
################################# COALITIONS NEXT PRIMERA #################################################
###########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

# Coalitions in the Consevador party for 2011: year of extreme division between the party  
muni_cons_2011 <- read.xlsx(str_c(coal, "Elecciones Alcaldía.xlsx"), sheet = "munp_conser_2011") %>%
  mutate(year = 2011, party_code = 2) %>%
  dplyr::select(muni_code = COD_MUN, municipio = MUNI, name = CANDIDATO, party_code, slant = CORRIENTE, coalition = COALICIÓN, year) %>% 
  mutate(coalition = ifelse(coalition == 99, 0,coalition))

saveRDS(muni_cons_2011,paste0(res,"coalitions_cons.rds"))

#Other parties and movements identified by municipality and coalition
### A warning may be appear caused by different levels in the factor variables of coalition 
other_parties_coal <- read.xlsx(str_c(coal, "Elecciones Alcaldía.xlsx"), sheet = "98 y 99") %>%
  dplyr::select(party_code = code, muni_code = muni, position = posici_muni, year, `1998`, `2002`,`2006`, `2010`) %>%
  gather(year_2, coalition, 5:8) %>% mutate(year_2 = fct_recode(year_2,
                                                                "1998" = "X1998",
                                                                "2002" = "X2002",
                                                                "2006" = "X2006",
                                                                "2010" = "X2010")) %>%
  mutate(year_2 = as.numeric(as.character(year_2))) %>%
  filter(year == 2000 & year_2 == 2002 |
           year == 2003 & year_2 == 2006 |
           year == 2007 & year_2 == 2010)

saveRDS(other_parties_coal, paste0(res,"coalitions_other.rds"))

# Coalition by party (First round): Hand-made based on oficial data, campaign reports and press 
coalitions_primera <- read.xlsx(str_c(coal, "Elecciones Alcaldía.xlsx"), sheet = "COALICION_1ra_VUELTA") %>%
  filter(is.na(party_code)==0) %>%
  mutate_all(funs(as.character(.))) %>%
  rename(name_party = Nombre.Partido) %>%
  gather("year", "coalition",  3:7) %>% mutate(year = as.factor(year)) %>%
  mutate(year = fct_recode(year,
                           "1998" = "X1998",
                           "2002" = "X2002",
                           "2006" = "X2006",
                           "2010" = "X2010", 
                           "2014" = "X2014")) %>%
  mutate(year_lag_presidencial = fct_recode(year,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  # mutate_at(c("party_code", "year", "coalition", "year_lag_presidencial") ,funs(as.character(.) %>% as.numeric(.))) %>%
  mutate(name_party = as.character(name_party)) %>%
  filter(party_code != 164) %>% # Fix Polo
  mutate(coalition = ifelse(party_code==15 & year == 2014, 1, coalition)) %>% 
  filter(party_code != 654) %>% # Fix ASI 
  mutate(coalition = ifelse(coalition == "na", NA, coalition))

saveRDS(coalitions_primera,paste0(res,"coalitions_primera.rds"))


#######################
# Long format (by codmun)

coalitions <- readRDS(paste0(res,"coalitions_primera.rds"))
table(coalitions$year ,coalitions$coalition)
muni_cons_2011 <- readRDS(paste0(res,"coalitions_cons.rds")) 
other_parties_coal <- readRDS(paste0(res,"coalitions_other.rds"))

#Create a df with coalition dummies by municipalities, party and year of election
coalitions_long <- alcaldes_merge %>%
  dplyr::select(ano:cand) %>% filter(cand == 1 & is.na(codpartido) == F & ano != 2015) %>%
  dplyr::select(codmpio, codep, ano, municipio, codpartido, primer_apellido, nombre, rank) %>%
  merge(., coalitions, by.x = c("codpartido", "ano"), by.y = c("party_code", "year_lag_presidencial"), all.x = T) %>%
  rename(coalition_old = coalition) %>% 
  merge(., other_parties_coal, by.x = c("codpartido", "ano", "codmpio"), 
        by.y = c("party_code", "year", "muni_code"), all.x = T) %>%
  rename(coalition_other = coalition) %>%
  merge(., muni_cons_2011, by.x = c("codpartido", "codmpio", "ano"), by.y = c("party_code", "muni_code", "year"), all.x = T) %>%
  rename(coalition_cons = coalition) %>% 
  mutate(coalition_new = coalition_old) %>%
  mutate(coalition_new = ifelse(coalition_new == 99 & coalition_other != 99, coalition_other,coalition_new)) %>% 
  mutate(coalition_new = ifelse(codpartido == 2 & ano == 2011 & coalition_new != coalition_cons, coalition_cons, coalition_new))  %>%
  mutate(year = as.numeric(as.character(year))) 

#  mutate(coalition_new = ifelse(coalition_old == 99 & coalition_other != 99, coalition_other,
#                         ifelse(codpartido == 2 & ano == 2011 & coalition_old != coalition_cons, coalition_cons, coalition_old)))

saveRDS(coalitions_long, paste0(res, "coalitions_primera_new.rds"))


###########################################################################################################
################################### COALITIONS NEXT SEGUNDA ###############################################
###########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
muni_cons_2011 <- readRDS(paste0(res,"coalitions_cons.rds")) 
other_parties_coal <- readRDS(paste0(res,"coalitions_other.rds"))

# Coalition by party (Second round): Hand-made based on oficial data, campaign reports and press 
coalitions_segunda <- openxlsx::read.xlsx(str_c(coal, "Elecciones Alcaldía.xlsx"), sheet = "COALICION_2da_VUELTA") %>%
  filter(is.na(party_code)==0) %>%
  mutate_all(funs(as.character(.))) %>%
  rename(name_party = Nombre.Partido) %>%
  gather("year", "coalition", 3:6) %>% mutate(year = as.factor(year)) %>%
  mutate(year = fct_recode(year,
                           "1994" = "X1994",
                           "1998" = "X1998",
                           "2010" = "X2010", 
                           "2014" = "X2014")) %>%
  mutate(year_lag_presidencial = fct_recode(year,
                                            "1994" = "1994",
                                            "1997" = "1998",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  # mutate_at(c("party_code", "year", "coalition", "year_lag_presidencial") ,funs(as.character(.) %>% as.numeric(.))) %>%
  mutate(name_party = as.character(name_party)) %>%
  filter(party_code != 164) %>% # Fix Polo
  mutate(coalition = ifelse(party_code==15 & year == 2014, 1, coalition)) %>% 
  filter(party_code != 654) %>% # Fix ASI 
  mutate(coalition = ifelse(coalition == "na", NA, coalition))

saveRDS(coalitions_segunda,paste0(res,"coalitions_segunda.rds"))

#######################
# Long format (by codmun) 

coalitions <- readRDS(paste0(res,"coalitions_segunda.rds"))
table(coalitions$year ,coalitions$year_lag_presidencial)
muni_cons_2011 <- readRDS(paste0(res,"coalitions_cons.rds")) 
other_parties_coal <- readRDS(paste0(res,"coalitions_other.rds"))

#Create a df with coalition dummies by municipalities, party and year of election
coalitions_long <- alcaldes_merge %>%
  dplyr::select(ano:cand) %>% filter(cand == 1 & is.na(codpartido) == F & ano != 2015) %>%
  dplyr::select(codmpio, codep, ano, municipio, codpartido, primer_apellido, nombre, rank) %>%
  merge(., coalitions, by.x = c("codpartido", "ano"), by.y = c("party_code", "year_lag_presidencial"), all.x = T) %>%
  rename(coalition_old = coalition) %>% 
  merge(., other_parties_coal, by.x = c("codpartido", "ano", "codmpio"), 
        by.y = c("party_code", "year", "muni_code"), all.x = T) %>%
  rename(coalition_other = coalition) %>%
  merge(., muni_cons_2011, by.x = c("codpartido", "codmpio", "ano"), by.y = c("party_code", "muni_code", "year"), all.x = T) %>%
  rename(coalition_cons = coalition) %>% 
  mutate(coalition_new = coalition_old) %>%
  mutate(coalition_new = ifelse(coalition_new == 99 & coalition_other != 99, coalition_other,coalition_new)) %>% 
  mutate(coalition_new = ifelse(codpartido == 2 & ano == 2011 & coalition_new != coalition_cons, coalition_cons, coalition_new)) %>%
  mutate(year = as.numeric(as.character(year))) 

#  mutate(coalition_new = ifelse(coalition_old == 99 & coalition_other != 99, coalition_other,
#                         ifelse(codpartido == 2 & ano == 2011 & coalition_old != coalition_cons, coalition_cons, coalition_old)))

saveRDS(coalitions_long, paste0(res, "coalitions_segunda_new.rds"))


###########################################################################################################
################################## COALITIONS NEXT FINAL ROUND ############################################
###########################################################################################################

# Primera vuelta para Uribe
coalitions_primera_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) 
coalitions_segunda_long <- readRDS(paste0(res,"coalitions_segunda_new.rds")) 
coalitions_long <- coalitions_primera_long %>% 
  filter(year==2002 | year== 2006) %>%
  rbind(.,coalitions_segunda_long) 


table(coalitions_long$ano ,coalitions_long$year)
saveRDS(coalitions_long, paste0(res, "coalitions_new.rds"))


###########################################################################################################
######################################## COALITIONS WRT ###################################################
###################################### CURRENT PRESIDENT  #################################################
###########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
muni_cons_2011 <- readRDS(paste0(res,"coalitions_cons.rds")) 
other_parties_coal <- readRDS(paste0(res,"coalitions_other.rds"))

# First:leads
coalitions_primera <- readRDS(paste0(res,"coalitions_primera.rds")) %>% 
  mutate(year_lead = fct_recode(year, 
                                "1997" = "1994",
                                "2000" = "1998",
                                "2003" = "2002",
                                "2007" = "2006",
                                "2011" = "2010",
                                "2015" = "2014")) %>%
  dplyr::select(-c(year_lag_presidencial))
table(coalitions_primera$year, coalitions_primera$year_lead)

# Second:leads
coalitions_segunda <- readRDS(paste0(res,"coalitions_segunda.rds")) %>%  
  mutate(year_lead = fct_recode(year, 
                                "1997" = "1994",
                                "2000" = "1998",
                                "2011" = "2010",
                                "2015" = "2014")) %>% 
  dplyr::select(-c(year_lag_presidencial))
table(coalitions_segunda$year, coalitions_segunda$year_lead)

# Other: leads
muni_cons <- readRDS(paste0(res,"coalitions_cons.rds")) 
other_parties_coal <- readRDS(paste0(res,"coalitions_other.rds"))


#######################
# Last round 

coalitions <- coalitions_primera %>% 
  filter(year==2002 | year== 2006) %>%
  rbind(.,coalitions_segunda)

table(coalitions$year, coalitions$year_lead)

#######################
# Long format (by codmun) 

coalitions_long <- alcaldes_merge %>%
  dplyr::select(ano:cand) %>% filter(cand == 1 & is.na(codpartido) == F) %>%
  dplyr::select(codmpio, codep, ano, municipio, codpartido, primer_apellido, nombre, rank) %>%
  merge(., coalitions, by.x = c("codpartido", "ano"), by.y = c("party_code", "year_lead"), all.x = T) %>%
  rename(coalition_old = coalition) %>% 
  merge(., other_parties_coal, by.x = c("codpartido", "ano", "codmpio"), 
        by.y = c("party_code", "year", "muni_code"), all.x = T) %>%
  rename(coalition_other = coalition) %>%
  merge(., muni_cons_2011, by.x = c("codpartido", "codmpio", "ano"), by.y = c("party_code", "muni_code", "year"), all.x = T) %>%
  rename(coalition_cons = coalition) %>% 
  mutate(coalition_new = coalition_old) %>%
  mutate(coalition_new = ifelse(coalition_new == 99 & coalition_other != 99, coalition_other,coalition_new)) %>% 
  mutate(coalition_new = ifelse(codpartido == 2 & ano == 2011 & coalition_new != coalition_cons, coalition_cons, coalition_new)) %>%
  mutate(year = as.numeric(as.character(year))) 

table(coalitions_long$ano, coalitions_long$year)

saveRDS(coalitions_long, paste0(res, "coalitions_current.rds"))



###########################################################################################################
######################################## COALITIONS WRT ###################################################
################################ CURRENT and INCOMING PRESIDENT  ##########################################
###########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
muni_cons_2011 <- readRDS(paste0(res,"coalitions_cons.rds")) 
other_parties_coal <- readRDS(paste0(res,"coalitions_other.rds"))

# Coalition_new: is 1 if both coalition and 0 if both 0 (missing in any of the coalition is missing)

# Load data
coalitions_primera_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% 
  dplyr::select(codpartido, ano, codmpio,year, coalition_new)
coalitions_segunda_long <- readRDS(paste0(res,"coalitions_segunda_new.rds")) %>% 
  dplyr::select(codpartido, ano, codmpio,year, coalition_new)
coalitions_final_long <- readRDS(paste0(res,"coalitions_new.rds")) %>% 
  dplyr::select(codpartido, ano, codmpio,year, coalition_new)
coalitions_current_long <- readRDS(paste0(res,"coalitions_current.rds")) %>% 
  dplyr::select(codpartido, ano, codmpio,year, coalition_new)

###################
# Current + First
table(coalitions_primera_long$ano, coalitions_primera_long$year)
table(coalitions_current_long$ano, coalitions_current_long$year)

coalitions_current_primera <- coalitions_current_long %>% filter(ano != 2015) %>% 
  rename(year_current = year, coalition_current = coalition_new) %>%
  merge(., coalitions_primera_long, by.x = c("codpartido", "ano", "codmpio"), 
        by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>% 
  rename(year_first = year, coalition_first = coalition_new) %>% 
  mutate(coalition_new = ifelse(coalition_current==1 & coalition_first==1, 1,NA)) %>% 
  mutate(coalition_new = ifelse(coalition_current==0 & coalition_first==0, 0,coalition_new)) 
  # mutate(coalition_new = ifelse(coalition_current==1 & coalition_first==0, 0,coalition_new)) %>% 
  # mutate(coalition_new = ifelse(coalition_current==0 & coalition_first==1, 0,coalition_new)) 

table(coalitions_current_primera$coalition_current, coalitions_current_primera$coalition_first)
table(coalitions_current_primera$coalition_new)

saveRDS(coalitions_current_primera, paste0(res, "coalitions_current_primera.rds"))


###################
# First but not current
coalitions_nocurrent_primera <- coalitions_current_long %>% filter(ano != 2015) %>% 
  rename(year_current = year, coalition_current = coalition_new) %>%
  merge(., coalitions_primera_long, by.x = c("codpartido", "ano", "codmpio"), 
        by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>% 
  rename(year_first = year, coalition_first = coalition_new) %>% 
  mutate(coalition_new = ifelse(coalition_current==0 & coalition_first==1, 1,NA)) %>% 
  mutate(coalition_new = ifelse(coalition_current==0 & coalition_first==0, 0,coalition_new))  
  # mutate(coalition_new = ifelse(coalition_current==1 & coalition_first==1, 0,coalition_new)) %>% 
  # mutate(coalition_new = ifelse(coalition_current==1 & coalition_first==0, 0,coalition_new)) 


table(coalitions_nocurrent_primera$coalition_current, coalitions_current_primera$coalition_first)
table(coalitions_nocurrent_primera$coalition_new)

saveRDS(coalitions_nocurrent_primera, paste0(res, "coalitions_nocurrent_primera.rds"))

###################
# Current + Second
table(coalitions_segunda_long$ano, coalitions_segunda_long$year)
table(coalitions_current_long$ano, coalitions_current_long$year)

coalitions_current_segunda <- coalitions_current_long %>% filter(ano != 2015) %>% 
  rename(year_current = year, coalition_current = coalition_new) %>%
  merge(., coalitions_segunda_long, by.x = c("codpartido", "ano", "codmpio"), 
        by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>% 
  rename(year_second = year, coalition_second = coalition_new) %>% 
  mutate(coalition_new = ifelse(coalition_current==1 & coalition_second==1, 1,NA)) %>% 
  mutate(coalition_new = ifelse(coalition_current==0 & coalition_second==0, 0,coalition_new)) 
  # mutate(coalition_new = ifelse(coalition_current==1 & coalition_second==0, 0,coalition_new)) %>% 
  # mutate(coalition_new = ifelse(coalition_current==0 & coalition_second==1, 0,coalition_new)) 

table(coalitions_current_segunda$coalition_current, coalitions_current_segunda$coalition_second)
table(coalitions_current_segunda$coalition_new)

saveRDS(coalitions_current_segunda, paste0(res, "coalitions_current_segunda.rds"))


###################
# Second but not current
coalitions_nocurrent_segunda <- coalitions_current_long %>% filter(ano != 2015) %>% 
  rename(year_current = year, coalition_current = coalition_new) %>%
  merge(., coalitions_segunda_long, by.x = c("codpartido", "ano", "codmpio"), 
        by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>% 
  rename(year_second = year, coalition_second = coalition_new) %>% 
  mutate(coalition_new = ifelse(coalition_current==0 & coalition_second==1, 1,NA)) %>% 
  mutate(coalition_new = ifelse(coalition_current==0 & coalition_second==0, 0,coalition_new))  
  # mutate(coalition_new = ifelse(coalition_current==1 & coalition_second==1, 0,coalition_new)) %>% 
  # mutate(coalition_new = ifelse(coalition_current==1 & coalition_second==0, 0,coalition_new)) 

table(coalitions_nocurrent_segunda$coalition_current, coalitions_nocurrent_segunda$coalition_second)
table(coalitions_nocurrent_segunda$coalition_new)

saveRDS(coalitions_nocurrent_segunda, paste0(res, "coalitions_nocurrent_segunda.rds"))


###################
# Current + final
table(coalitions_final_long$ano, coalitions_final_long$year)
table(coalitions_current_long$ano, coalitions_current_long$year)

coalitions_current_final <- coalitions_current_long %>% filter(ano != 2015) %>% 
  rename(year_current = year, coalition_current = coalition_new) %>%
  merge(., coalitions_final_long, by.x = c("codpartido", "ano", "codmpio"), 
        by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>% 
  rename(year_final = year, coalition_final = coalition_new) %>% 
  mutate(coalition_new = ifelse(coalition_current==1 & coalition_final==1, 1,NA)) %>% 
  mutate(coalition_new = ifelse(coalition_current==0 & coalition_final==0, 0,coalition_new)) 
  # mutate(coalition_new = ifelse(coalition_current==1 & coalition_final==0, 0,coalition_new))  
  # mutate(coalition_new = ifelse(coalition_current==0 & coalition_final==1, 0,coalition_new)) 

table(coalitions_current_final$coalition_current, coalitions_current_final$coalition_final)
table(coalitions_current_final$coalition_new)

saveRDS(coalitions_current_final, paste0(res, "coalitions_current_final.rds"))


# # transitions of coaltions over years
# 
# a <- unique(coalitions_current_final$ano)
# 
# coal <- lapply(a, function(x){
#   b <- coalitions_current_final %>% filter(ano==x)
#   b <- table(b$coalition_current, b$coalition_final)[1:2,1:2] 
#   return(b)
# }) %>% ldply() 
   

###################
# final but not current

coalitions_nocurrent_final <- coalitions_current_long %>% filter(ano != 2015) %>% 
  rename(year_current = year, coalition_current = coalition_new) %>%
  merge(., coalitions_final_long, by.x = c("codpartido", "ano", "codmpio"), 
        by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>% 
  rename(year_final = year, coalition_final = coalition_new) %>% 
  mutate(coalition_new = ifelse(coalition_current==0 & coalition_final==1, 1,NA)) %>% 
  mutate(coalition_new = ifelse(coalition_current==0 & coalition_final==0, 0,coalition_new)) 
  # mutate(coalition_new = ifelse(coalition_current==1 & coalition_final==1, 0,coalition_new)) %>% 
  # mutate(coalition_new = ifelse(coalition_current==1 & coalition_final==0, 0,coalition_new)) 

table(coalitions_nocurrent_final$coalition_current, coalitions_nocurrent_final$coalition_final)
table(coalitions_nocurrent_final$coalition_new)

saveRDS(coalitions_nocurrent_final, paste0(res, "coalitions_nocurrent_final.rds"))



#########################################################################################################
##################################### MAYORS IN T + 1 - INCUMBENCY ######################################
#########################################################################################################

# Elections at t+1 Create lagged year and collapse by party (or group of parties) for t+1 outcome  
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

alcaldes_merge_t1 <- alcaldes_merge %>%
  filter(cand == 1) %>%
  filter(ano!=1997) %>% 
  mutate(ano_lag = as.factor(ano)) %>%
  mutate(ano_lag = fct_recode(ano_lag,
                              "1997" = "2000",
                              "2000" = "2003",
                              "2003" = "2007",
                              "2007" = "2011",
                              "2011" = "2015")) %>%
  mutate(ano_lag = as.character(ano_lag)) %>%
  rename(ano_t1 = ano) %>% 
  group_by(codmpio, ano_lag, ano_t1, codpartido, parties, parties_ef) %>%
  summarize(votos = sum(votos), 
            prop_votes_cand = sum(prop_votes_cand),
            prop_votes_total = sum(prop_votes_total),
            rank = min(rank))

saveRDS(alcaldes_merge_t1 ,paste0(res,"alcaldes_t1.rds"))


#########################################################################################################
############################ MAYORS IN T + 1 BY INCOMING COALITION - INCUMBENCY #########################
#########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

coalitions_primera <- readRDS(paste0(res,"coalitions_primera_new.rds")) 
coalitions_segunda <- readRDS(paste0(res,"coalitions_segunda_new.rds"))
coalitions_final <- readRDS(paste0(res,"coalitions_new.rds"))

list_coalitions <- list(coalitions_primera, coalitions_segunda, coalitions_final)

election_coalitions <- function(x, y){ 
  
  coalition <- x %>% filter(coalition_new == 0 | coalition_new == 1) %>% 
    group_by(codpartido, ano, codmpio) %>%
    mutate(coalition_new = as.numeric(coalition_new)) %>%
    summarize(coalition_new = max(coalition_new)) 
      
  
  df_merge <- y %>%
    filter(.,cand == 1) %>%
    filter(ano!=1997) %>% 
    mutate(ano_lag = as.factor(ano)) %>%
    mutate(ano_lag = fct_recode(ano_lag,
                                "1997" = "2000",
                                "2000" = "2003",
                                "2003" = "2007",
                                "2007" = "2011",
                                "2011" = "2015")) %>%
    mutate(ano_lag = as.character(ano_lag)) %>%
    merge(., coalition, by.x = c("codpartido", "ano_lag", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
    rename(ano_t1 = ano) %>%
    group_by(codmpio, ano_lag, ano_t1, coalition_new) %>%
    summarize(., votos = sum(votos),
              prop_votes_cand = sum(prop_votes_cand),
              prop_votes_total = sum(prop_votes_total))
  return(df_merge)
} 

alcaldes_merge_t1 <- lapply(list_coalitions, election_coalitions, y = alcaldes_merge)

saveRDS(alcaldes_merge_t1 ,paste0(res,"alcaldes_t1_coalition.rds"))


#########################################################################################################
############################ MAYORS IN T + 1 BY COALITION - INCUMBENCY ##################################
###################################### CURRENT AND INCOMING #############################################
#########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

coalitions_current <- readRDS(paste0(res,"coalitions_current.rds")) 
coalitions_current_primera <- readRDS(paste0(res,"coalitions_current_primera.rds")) 
coalitions_current_segunda <- readRDS(paste0(res,"coalitions_current_segunda.rds")) 
coalitions_current_final <- readRDS(paste0(res,"coalitions_current_final.rds")) 

list_coalitions <- list(coalitions_current,coalitions_current_primera, coalitions_current_segunda, coalitions_current_final)

election_coalitions <- function(x, y){
  
  coalition <- x %>% filter(coalition_new == 0 | coalition_new == 1) %>% 
    group_by(codpartido, ano, codmpio) %>%
    mutate(coalition_new = as.numeric(coalition_new)) %>%
    summarize(coalition_new = max(coalition_new)) 
  
  
  df_merge <- y %>%
    filter(.,cand == 1) %>%
    filter(ano!=1997) %>% 
    mutate(ano_lag = as.factor(ano)) %>%
    mutate(ano_lag = fct_recode(ano_lag,
                                "1997" = "2000",
                                "2000" = "2003",
                                "2003" = "2007",
                                "2007" = "2011",
                                "2011" = "2015")) %>%
    mutate(ano_lag = as.character(ano_lag)) %>%
    merge(., coalition, by.x = c("codpartido", "ano_lag", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
    rename(ano_t1 = ano) %>%
    group_by(codmpio, ano_lag, ano_t1, coalition_new) %>%
    summarize(., votos = sum(votos),
              prop_votes_cand = sum(prop_votes_cand),
              prop_votes_total = sum(prop_votes_total))
  return(df_merge)
} 

alcaldes_merge_t1 <- lapply(list_coalitions, election_coalitions, y = alcaldes_merge)

saveRDS(alcaldes_merge_t1 ,paste0(res,"alcaldes_t1_coalition_current.rds"))

#########################################################################################################
############################ MAYORS IN T + 1 BY COALITION - INCUMBENCY ##################################
################################## INCOMING but not CURRENT #############################################
#########################################################################################################

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

coalitions_nocurrent_primera <- readRDS(paste0(res,"coalitions_nocurrent_primera.rds")) 
coalitions_nocurrent_segunda <- readRDS(paste0(res,"coalitions_nocurrent_segunda.rds")) 
coalitions_nocurrent_final <- readRDS(paste0(res,"coalitions_nocurrent_final.rds")) 

list_coalitions <- list(coalitions_nocurrent_primera, coalitions_nocurrent_segunda, coalitions_nocurrent_final)

election_coalitions <- function(x, y){

  coalition <- x %>% filter(coalition_new == 0 | coalition_new == 1) %>% 
    group_by(codpartido, ano, codmpio) %>%
    mutate(coalition_new = as.numeric(coalition_new)) %>%
    summarize(coalition_new = max(coalition_new)) 

  df_merge <- y %>%
    filter(.,cand == 1) %>%
    filter(ano!=1997) %>% 
    mutate(ano_lag = as.factor(ano)) %>%
    mutate(ano_lag = fct_recode(ano_lag,
                                "1997" = "2000",
                                "2000" = "2003",
                                "2003" = "2007",
                                "2007" = "2011",
                                "2011" = "2015")) %>%
    mutate(ano_lag = as.character(ano_lag)) %>%
    merge(., coalition, by.x = c("codpartido", "ano_lag", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
    rename(ano_t1 = ano) %>%
    group_by(codmpio, ano_lag, ano_t1, coalition_new) %>%
    summarize(., votos = sum(votos),
              prop_votes_cand = sum(prop_votes_cand),
              prop_votes_total = sum(prop_votes_total))
  return(df_merge)
} 

alcaldes_merge_t1 <- lapply(list_coalitions, election_coalitions, y = alcaldes_merge)
saveRDS(alcaldes_merge_t1 ,paste0(res,"alcaldes_t1_coalition_nocurrent.rds"))

