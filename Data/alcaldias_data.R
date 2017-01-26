
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
 setwd("~/Dropbox/BANREP/Elecciones/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"

###########################################################################################################
######################################## COALITIONS  ######################################################
###########################################################################################################

coalitions <- read.csv(str_c(data, "coaliciones.csv"), sep = ";")%>%
  mutate(X2006 = as.character(X2006)) %>%
  gather("year", "coalition", starts_with("X")) %>% mutate(year = as.factor(year)) %>%
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
  mutate_all(funs(as.character(.)))

saveRDS(coalitions,paste0(res,"coalitions.rds"))

###########################################################################################################
############################ Winners and loosers since 1997  ##############################################
###########################################################################################################

# Get party-code list: 
##################################### ERROR: CÓDIGOS DE PARTIDO  ##########################################
# Error en codigos de CEDE excel que está corrido de 1 a partir de 434 con respecto a PDF (diccionario)   #
# de donde viene el .dta. En bases de datos, correcto para todos los años excepto 2011 en que se toman    #
# codigos de xls. Para corregir esto se toma nombre de partido en 2011, y se hace merge para recuperar    #
# partidos.                                                                                               #
###########################################################################################################
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

# party_code_2 <- read.csv(paste0(data,"partidos.csv"))
# party_code_v <- merge(party_code, party_code_2, by = "party_code",, all = T)

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
# table(alcaldes[[3]]$codpartido)

# Party codes in 2011: merged from party name
# correct parties that changed name
alcaldes[[5]] <- alcaldes[[5]] %>%
  mutate(nombre = "") %>%
  mutate(partido_1 = as.factor(partido_1)) %>%
  mutate(partido_1 = fct_recode(partido_1,
                              "MOVIMIENTO ALIANZA SOC INDIGENA ASI" = "PARTIDO ALIANZA SOCIAL INDEPENDIENTE",
                              "PARTIDO DE INTEGRACION NACIONAL PIN" = "PARTIDO DE INTEGRACION NACIONAL" )) %>% 
  mutate(partido_1 = as.character(partido_1)) %>%
  stringdist_left_join(party_code, by = c(partido_1 = "name_party"), distance_col = "distance", max_dist = 2) %>% #Correct party_codes using the dictionary
  plyr::rename(., c("party_code" = "codpartido"))

# #Identify mismatch (run before running the fct_recode line above)
# c(setdiff(unique(alcaldes_aggregate[[5]]$codigo_partido_1), unique(alcaldes_aggregate[[5]]$party_code)))
 a <- alcaldes_aggregate[[5]] %>% filter(partido_1 == name_party & codigo_partido_1 != party_code) %>%
#   plyr::rename(., c("party_code" = "codpartido"))

# Party codes in 2015: codpartido_1 
# correct ASI that changed name
alcaldes[[6]] <- alcaldes[[6]] %>%
  mutate(codpartido = as.factor(codpartido_1)) %>%
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
    mutate(rank = row_number(desc(votos))) %>% 
    mutate(prop_votes_total = votos / sum(votos)) %>%
    mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>%
    mutate(prop_votes_cand = votos / sum(votos_cand)) %>%
    mutate(votos_r2 = ifelse(rank <= 2, votos,0)) %>% 
    mutate(prop_votes_c2 = votos / sum(votos_r2)) %>% 
    mutate(parties = sum(cand)) %>%
    mutate(party_ef = ifelse(prop_votes_cand > 0.1, 1,0)) %>%
    mutate(parties_ef = sum(party_ef)) %>% 
    filter(is.na(prop_votes_total)==0) 
  })

a <- alcaldes_aggregate[[5]] %>% 
  filter(partido_1 == name_party & codigo_partido_1 != codpartido)

#Arrange data in a long format
alcaldes_merge <- alcaldes_aggregate %>%
  ldply() %>%
  arrange(codmpio, ano, desc(rank)) %>%
  dplyr::select(c(ano, codmpio, codep, municipio, parties, parties_ef, rank, primer_apellido, nombre, codpartido, cand, votos, votos_cand, votos_r2,
  prop_votes_total, prop_votes_cand, prop_votes_c2)) %>%
  merge(., coalitions, by.x = c("codpartido","ano") , by.y = c("party_code", "year_lag_presidencial"), all = T) %>%
  mutate(ano = as.integer(ano))

saveRDS(alcaldes_merge,paste0(res,"alcaldes_merge.rds"))


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


