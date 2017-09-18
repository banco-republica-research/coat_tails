###########################################################################################################
#################################### DATA NATIONAL ELECTIONS ##############################################
###########################################################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
#setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
coal <-"Data/CEDE/coaliciones/"

###########################################################################################################
###############################  PRESIDENTIAL ELECTIONS: CANDIDATES  ######################################
###########################################################################################################
# Get presidents election data (Winners and loosers since 1997). 

list_files <- list.files(path = data) %>%
  .[. %in% c("1998", "2002", "2006", "2010", "2014")] %>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Pres")]}) %>% 
  lapply(function(x){ 
    if(length(x) > 1){
      x[str_detect(x, "Primera")]
    } else {
      x
    }
  }) %>%
  str_c(data, c("1998", "2002", "2006", "2010", "2014"),"/", ., sep = "")

#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
presidentes_primera <- lapply(list_files, read_dta) 

#Make all names the same (the 1998 data has english name variables)
names(presidentes_primera[[1]]) <- names(presidentes_primera[[2]])

candidates <- presidentes_primera %>% ldply() %>%
  group_by(ano, primer_apellido, segundo_apellido, nombre, codpartido) %>%
  summarize(votos_totales = sum(votos,na.rm = TRUE)) %>%
  filter(!codpartido == "NaN") %>%
  mutate(codpartido = ifelse(primer_apellido == "GAVIRIA", 194, codpartido)) %>%
  mutate(codpartido = ifelse(primer_apellido == "PASTRANA" & ano == 1998, 2, codpartido)) %>%
  mutate(codpartido = as.factor(codpartido)) %>%
  mutate(codpartido = fct_recode(codpartido, "194" = "47", "165" = "164")) 

saveRDS(candidates ,paste0(res, "candidates_primera_vuelta.rds"))


###########################################################################################################
################################ FIRST ROUND PRESIDENTIAL ELECTIONS  ######################################
###########################################################################################################
#Aggregate totals for each year and clean non-candidate data

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS", "TARJETAS N0 MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")

invalid_places <- c(NaN, 96, 97, 99) 
# (Nan: Consulate, 96, 87 and 99 are totals) #


# Get presidents election data (Winners and loosers since 1997). 

list_files <- list.files(path = data) %>%
  .[. %in% c("1990", "1994", "1998", "2002", "2006", "2010", "2014")] %>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Pres")]}) %>% 
  lapply(function(x){ 
    if(length(x) > 1){
      x[str_detect(x, "Primera")]
    } else {
      x
    }
  }) %>%
  str_c(data, c("1990", "1994", "1998", "2002", "2006", "2010", "2014"),"/", ., sep = "")


# Get party-code list
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
presidentes_primera <- lapply(list_files, read_dta) 

#Make all names the same (the 1998 data has english name variables)

names(presidentes_primera[[3]]) <- names(presidentes_primera[[2]])

#Aggregate totals for each year and clean non-candidate data

presidentes_aggregate_primera <- presidentes_primera %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
      filter(is.na(votos)==0) %>% 
      filter(!codmpio %in% invalid_places) %>% #Remove votes from consulates, embassies and totals 
      mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
      mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>% 
      group_by(codmpio, ano) %>%
      mutate(prop_votes_total = votos / sum(votos)) %>%
      mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>%
      mutate(prop_votes_cand = votos / sum(votos_cand)) %>%
      filter(is.na(prop_votes_total)==0)
  })

#Arrange data in a long format
presidentes_merge_primera <- presidentes_aggregate_primera %>%
  ldply() %>%
  arrange(codmpio, ano) %>%
  dplyr::select(c(ano, codmpio, codep, municipio, primer_apellido, nombre, codpartido, cand, votos, votos_cand,
                  prop_votes_total, prop_votes_cand)) %>%
  mutate(codpartido = ifelse(primer_apellido == "GAVIRIA", 194, codpartido)) %>%
  mutate(codpartido = ifelse(primer_apellido == "PASTRANA" & ano == 1998, 2, codpartido)) %>%
  mutate(codpartido = as.factor(codpartido)) %>%
  mutate(codpartido = fct_recode(codpartido,"194" = "47","165" = "164")) 


saveRDS(presidentes_merge_primera,paste0(res,"presidentes_primera_merge.rds"))



###########################################################################################################
################################ SECOND ROUND PRESIDENTIAL ELECTIONS  #####################################
###########################################################################################################

# Get presidents election data (Winners and loosers since 1997). 

list_files <- list.files(path = data) %>%
  .[. %in% c("1998", "2002", "2006", "2010", "2014")] %>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Pres")]}) %>% 
  lapply(function(x){ 
    if(length(x) > 1){
      x[str_detect(x, "Segunda")]
    } else {
      x
    }
  }) %>%
  str_c(data, c("1998", "2002", "2006", "2010", "2014"),"/", ., sep = "")


#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
presidentes_segunda <- lapply(list_files, read_dta) 

#Make all names the same (the 1998 data has english name variables)

names(presidentes_segunda[[1]]) <- names(presidentes_segunda[[2]])

#Aggregate totals for each year and clean non-candidate data

presidentes_aggregate_segunda <- presidentes_segunda %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
      filter(is.na(votos)==0) %>% 
      filter(!codmpio %in% invalid_places) %>% #Remove votes from consulates, embassies and totals 
      mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
      mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>% 
      group_by(codmpio, ano) %>%
      mutate(prop_votes_total = votos / sum(votos)) %>%
      mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>%
      mutate(prop_votes_cand = votos / sum(votos_cand)) %>%
      filter(is.na(prop_votes_total)==0) 
  })

#Arrange data in a long format
presidentes_merge_segunda <- presidentes_aggregate_segunda %>%
  ldply() %>%
  arrange(codmpio, ano) %>%
  dplyr::select(c(ano, codmpio, codep, municipio, primer_apellido, nombre, codpartido, cand, votos, votos_cand, 
                  prop_votes_total, prop_votes_cand)) %>%
  mutate(codpartido = ifelse(primer_apellido == "PASTRANA" & ano == 1998, 2, codpartido)) 
  

saveRDS(presidentes_merge_segunda, paste0(res,"presidentes_segunda_merge.rds"))




###########################################################################################################
#######################################  HOUSE OF REPRESENTATIVES   #######################################
###########################################################################################################

# Get House election data (Winners and loosers since 1997). 

list_files <- list.files(path = data) %>%
  .[. %in% c("1998", "2002", "2006", "2010", "2014")] %>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Cam")]}) %>% 
  str_c(data, c("1998", "2002", "2006", "2010", "2014"),"/", ., sep = "")


#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
representantes <- lapply(list_files, read_dta) %>%
  lapply(., dplyr::select, ano:curules)

#Aggregate totals for each year and clean non-candidate data

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS", "TARJETAS N0 MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")
non_candidate_votes_p <- c(996,997,998)

invalid_places <- c(NaN, 96, 97, 99)
# (Nan: Consulate, 96, 87 and 99 are totals) #

representantes_aggregate <- representantes %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
      # filter(is.na(votos) == 0) %>% 
      filter(!codmpio %in% invalid_places) %>% #Remove totals
      mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes | codpartido %in% non_candidate_votes_p, 1, 0 )) %>% 
      mutate(codpartido = ifelse(no_cand == 0, codpartido, NA)) %>%      
      mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>%
      mutate(ano = as.numeric(ano)) %>%
      group_by(codmpio, ano) %>%
      mutate(rank = row_number(desc(votos))) %>%
      mutate(total_votos = sum(votos)) %>%
      mutate(prop_votes_total = votos / total_votos) %>%
      mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>%
      mutate(total_votos_cand = sum(votos_cand)) %>%
      mutate(prop_votes_cand = votos / total_votos_cand) %>%
      mutate(parties = sum(cand)) %>%
      mutate(party_ef = ifelse(prop_votes_cand > 0.1, 1,0)) %>%
      mutate(parties_ef = sum(party_ef)) %>% 
      filter(is.na(prop_votes_total)==0) 
  })

saveRDS(representantes_aggregate,paste0(res,"representantes_aggregate.rds"))


##########################################  (Group by party)   ##########################################
#Collapse candidates by party (remember that the number of delegates depends on the district magnitude)

# Load Electoral results
representantes_aggregate <- readRDS(paste0(res,"representantes_aggregate.rds"))

representantes_collapse <- representantes_aggregate %>%
  ldply() %>%
    filter(cand == 1) %>%
      group_by(ano, codmpio, codpartido) %>%
      summarise(votos=sum(votos),prop_votes_total=sum(prop_votes_total), prop_votes_cand=sum(prop_votes_cand)) %>%
      group_by(ano, codmpio) %>%
      mutate(rank = row_number(desc(votos))) %>%
      ungroup()

representantes_merge <- representantes_collapse %>%
  filter(is.na(codpartido)==0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  mutate_at(vars(matches("cod")), as.character)

saveRDS(representantes_merge,paste0(res,"representantes_merge.rds"))


##########################################  (Group by FIRST ROUND coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
representantes_merge <- readRDS(paste0(res,"representantes_merge.rds"))
representantes_aggregate <- readRDS(paste0(res,"representantes_aggregate.rds"))

# load first stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

representantes_collapse <- representantes_merge %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(coalition_new, ano, codmpio) %>%
  summarise(votos=sum(votos),prop_votes_total=sum(prop_votes_total), prop_votes_cand=sum(prop_votes_cand)) %>%
  group_by(ano, codmpio) %>%
  mutate(rank = row_number(desc(votos)))


representantes_collapse <- representantes_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio"), all.x = T) %>%
  # mutate(ano = as.character(ano)) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(coalition_new, year, codmpio) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(representantes_collapse ,paste0(res,"representantes_coalition_primera_merge.rds"))

##########################################  (Group by SECOND ROUND coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
representantes_merge <- readRDS(paste0(res,"representantes_merge.rds"))
representantes_aggregate <- readRDS(paste0(res,"representantes_aggregate.rds"))

# load first stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_segunda_new.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))
  

representantes_collapse <- representantes_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(representantes_collapse ,paste0(res,"representantes_coalition_segunda_merge.rds"))


##########################################  (Group by FINAL ROUND coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
representantes_merge <- readRDS(paste0(res,"representantes_merge.rds"))
representantes_aggregate <- readRDS(paste0(res,"representantes_aggregate.rds"))

# load final stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_new.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

representantes_collapse <- representantes_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(representantes_collapse ,paste0(res,"representantes_coalition_merge.rds"))


##########################################  (Group by CURRENT and FIRST coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
representantes_merge <- readRDS(paste0(res,"representantes_merge.rds"))
representantes_aggregate <- readRDS(paste0(res,"representantes_aggregate.rds"))

# load final stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current_primera.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio, coalition_new, year_current, year_first) %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

representantes_collapse <- representantes_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(representantes_collapse ,paste0(res,"representantes_coalition_current_primera_merge.rds"))


##########################################  (Group by FIRST but not CURRENT coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
representantes_merge <- readRDS(paste0(res,"representantes_merge.rds"))
representantes_aggregate <- readRDS(paste0(res,"representantes_aggregate.rds"))

# load final stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_nocurrent_primera.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio, coalition_new, year_current, year_first)  %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

representantes_collapse <- representantes_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(representantes_collapse ,paste0(res,"representantes_coalition_nocurrent_primera_merge.rds"))


##########################################  (Group by CURRENT and FINAL coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
representantes_merge <- readRDS(paste0(res,"representantes_merge.rds"))
representantes_aggregate <- readRDS(paste0(res,"representantes_aggregate.rds"))

# load final stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current_final.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio, coalition_new, year_current, year_final) %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

representantes_collapse <- representantes_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(representantes_collapse ,paste0(res,"representantes_coalition_current_final_merge.rds"))

##########################################  (Group by FINAL but not CURRENT coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
representantes_merge <- readRDS(paste0(res,"representantes_merge.rds"))
representantes_aggregate <- readRDS(paste0(res,"representantes_aggregate.rds"))

# load final stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_nocurrent_final.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio, coalition_new, year_current, year_final)  %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

representantes_collapse <- representantes_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(representantes_collapse ,paste0(res,"representantes_coalition_nocurrent_final_merge.rds"))





###########################################################################################################
##############################################  SENATE  ###################################################
###########################################################################################################

# Get senate election data (Winners and loosers since 1997). 

list_files <- list.files(path = data) %>%
  .[. %in% c("1998", "2002", "2006", "2010", "2014")] %>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Sen")]}) %>% 
  str_c(data, c("1998", "2002", "2006", "2010", "2014"),"/", ., sep = "")


#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
senado <- lapply(list_files, read_dta)
senado[[5]]$partido <- NULL

#Aggregate totals for each year and clean non-candidate data

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS", "TARJETAS N0 MARCADAS", 
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")
non_candidate_votes_p <- c(996,997,998)

invalid_places <- c(NaN, 96, 97, 99)
# (Nan: Consulate, 96, 87 and 99 are totals) #

senado_aggregate <- senado %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
      filter(is.na(votos) == 0) %>% 
      filter(!codmpio %in% invalid_places) %>% #Remove totals
      mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes | codpartido %in% non_candidate_votes_p, 1, 0)) %>% 
      mutate(codpartido = ifelse(no_cand == 0, codpartido, NA)) %>%      
      mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>% 
      group_by(codmpio, ano) %>%
      mutate(rank = row_number(desc(votos))) %>% 
      mutate(prop_votes_total = votos / sum(votos)) %>%
      mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>%
      mutate(prop_votes_cand = votos / sum(votos_cand)) %>%
      mutate(parties = sum(cand)) %>%
      mutate(party_ef = ifelse(prop_votes_cand > 0.1, 1,0)) %>%
      mutate(parties_ef = sum(party_ef)) %>% 
      filter(is.na(prop_votes_total)==0) 
  })

saveRDS(senado_aggregate,paste0(res,"senado_aggregate.rds"))


##########################################  (Group by party)   ##########################################
#Collapse candidates by party (remember that the number of delegates depends on the district magnitude)

# Load Electoral results
senado_aggregate <- readRDS(paste0(res,"senado_aggregate.rds"))

senado_collapse <- senado_aggregate %>%
  lapply(., function(x){
    filter(x, cand == 1) %>%
      group_by(ano, codep, codmpio, codpartido) %>%
      summarise_at(vars(matches("vot")), sum) %>%
      group_by(ano, codep, codmpio) %>%
      mutate(rank = row_number(desc(votos)))
  })

senado_merge <- senado_collapse %>%
  ldply() %>% filter(is.na(codpartido)==0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  mutate_at(vars(matches("cod")), as.character)

saveRDS(senado_merge,paste0(res,"senado_merge.rds"))


##########################################  (Group by FIRST ROUND coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
senado_merge <- readRDS(paste0(res,"senado_merge.rds"))
senado_aggregate <- readRDS(paste0(res,"senado_aggregate.rds"))

# load first stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_collapse <- senado_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(senado_collapse ,paste0(res,"senate_coalition_primera_merge.rds"))


##########################################  (Group by SECOND ROUND coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
senado_merge <- readRDS(paste0(res,"senado_merge.rds"))
senado_aggregate <- readRDS(paste0(res,"senado_aggregate.rds"))

# load first stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_segunda_new.rds")) %>% 
  dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_collapse <- senado_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(senado_collapse ,paste0(res,"senate_coalition_segunda_merge.rds"))


##########################################  (Group by FINAL ROUND coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
senado_merge <- readRDS(paste0(res,"senado_merge.rds"))
senado_aggregate <- readRDS(paste0(res,"senado_aggregate.rds"))

# load final stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_new.rds"))  %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_collapse <- senado_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(senado_collapse ,paste0(res,"senate_coalition_merge.rds"))


##########################################  (Group by CURRENT and FIRST coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
senado_merge <- readRDS(paste0(res,"senado_merge.rds"))
senado_aggregate <- readRDS(paste0(res,"senado_aggregate.rds"))

# load current stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current_primera.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio, coalition_new, year_current, year_first)  %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_collapse <- senado_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(senado_collapse ,paste0(res,"senate_coalition_current_primera_merge.rds"))


##########################################  (Group by FIRST and not CURRENT coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
senado_merge <- readRDS(paste0(res,"senado_merge.rds"))
senado_aggregate <- readRDS(paste0(res,"senado_aggregate.rds"))

# load current stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_nocurrent_primera.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio, coalition_new, year_current, year_first)  %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_collapse <- senado_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(senado_collapse ,paste0(res,"senate_coalition_nocurrent_primera_merge.rds"))


##########################################  (Group by CURRENT and FINAL coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
senado_merge <- readRDS(paste0(res,"senado_merge.rds"))
senado_aggregate <- readRDS(paste0(res,"senado_aggregate.rds"))

# load current stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_current_final.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio, coalition_new, year_current, year_final)  %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_collapse <- senado_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(senado_collapse ,paste0(res,"senate_coalition_current_final_merge.rds"))

##########################################  (Group by FINAL but not CURRENT coalition)   ############################################
# Collapse candidates by coalition (identified mannually)

# Load Electoral results
senado_merge <- readRDS(paste0(res,"senado_merge.rds"))
senado_aggregate <- readRDS(paste0(res,"senado_aggregate.rds"))

# load current stage coalition
coalitions_long <- readRDS(paste0(res,"coalitions_nocurrent_final.rds")) %>% 
  dplyr::select(codpartido,ano,codmpio, coalition_new, year_current, year_final)  %>%
  group_by(codpartido, ano, codmpio) %>%
  mutate(coalition_new = as.numeric(coalition_new)) %>%
  summarize(coalition_new = max(coalition_new))

senado_collapse <- senado_aggregate %>%
  ldply() %>%
  filter(cand == 1) %>%
  filter(is.na(codpartido) == 0) %>%
  arrange(codmpio, ano, rank) %>% mutate(ano = as.factor(ano)) %>%
  mutate(year_lag_presidencial = fct_recode(ano,
                                            "1997" = "1998",
                                            "2000" = "2002",
                                            "2003" = "2006",
                                            "2007" = "2010",
                                            "2011" = "2014"
  )) %>%
  mutate(year_lag_presidencial = as.character(year_lag_presidencial)) %>%
  merge(., coalitions_long, by.x = c("codpartido", "year_lag_presidencial", "codmpio"), by.y = c("codpartido", "ano", "codmpio")) %>%
  #This merge ignore the un-matched observations, because they represent non-existent voting. 
  group_by(ano, codep, codmpio, coalition_new) %>%
  summarise_at(vars(matches("vot")), sum) %>%
  group_by(ano, codep, codmpio) %>%
  mutate(rank = row_number(desc(votos)))

saveRDS(senado_collapse ,paste0(res,"senate_coalition_nocurrent_final_merge.rds"))
