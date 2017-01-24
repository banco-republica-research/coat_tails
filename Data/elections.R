###########################################################################################################
#################################### DATA ELECTIONS  ######################################################
###########################################################################################################

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
##################################  PRESIDENTIAL ELECTIONS: WINNERS  ######################################
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
non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")

invalid_places <- c(NaN, 96, 97, 99) 
# (Nan: Consulate, 96, 87 and 99 are totals) #


winners <- presidentes_segunda %>% 
  lapply(., function(x){
    filter(x, !codmpio %in% invalid_places) %>% #Remove votes from consulates, embassies and totals 
      # mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
      # mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>%
      group_by(ano, nombre, primer_apellido, segundo_apellido, codpartido) %>%
      summarize(total_votos = sum(votos)) %>%
      mutate(rank = dense_rank(total_votos)) 
    # %>% 
    #   filter(rank == 1) %>%
    #   ldply()
})


###########################################################################################################
################################ FIRST ROUND PRESIDENTIAL ELECTIONS  ######################################
###########################################################################################################

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
                  prop_votes_total, prop_votes_cand)) 

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
                  prop_votes_total, prop_votes_cand)) 

saveRDS(presidentes_merge_segunda, paste0(res,"presidentes_segunda_merge.rds"))

###########################################################################################################
#######################################  HOUSE OF REPRESENTATIVES   #######################################
###########################################################################################################


# Get presidents election data (Winners and loosers since 1997). 

list_files <- list.files(path = data) %>%
  .[. %in% c("1998", "2002", "2006", "2010", "2014")] %>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Cam")]}) %>% 
  str_c(data, c("1998", "2002", "2006", "2010", "2014"),"/", ., sep = "")


#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
representantes <- lapply(list_files, read_dta) %>%
  lapply(., dplyr::select, ano:curules)

#Aggregate totals for each year and clean non-candidate data

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")

invalid_places <- c(NaN, 96, 97, 99)
# (Nan: Consulate, 96, 87 and 99 are totals) #

representantes_aggregate <- representantes %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
      filter(is.na(votos) == 0) %>% 
      filter(!codmpio %in% invalid_places) %>% #Remove totals
      mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
      mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>% 
      group_by(codmpio, ano) %>%
      mutate(rank = row_number(desc(votos))) %>% 
      mutate(prop_votes_total = votos / sum(votos)) %>%
      mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>%
      mutate(prop_votes_cand = votos / sum(votos_cand)) %>%
      # mutate(votos_r2 = ifelse(rank <= 2, votos,0)) %>% 
      # mutate(prop_votes_c2 = votos / sum(votos_r2)) %>% 
      mutate(parties = sum(cand)) %>%
      mutate(party_ef = ifelse(prop_votes_cand > 0.1, 1,0)) %>%
      mutate(parties_ef = sum(party_ef)) %>% 
      filter(is.na(prop_votes_total)==0) 
  })

#Collapse candidates by party (remember that the number of delegates depends on the district magnitude)

representantes_collapse <- representantes_aggregate %>%
  lapply(., function(x){
    filter(x, cand == 1) %>%
    group_by(ano, codep, codmpio, codpartido) %>%
    summarise_at(vars(matches("vot")), sum) %>%
    group_by(ano, codep, codmpio) %>%
    mutate(rank = row_number(desc(votos)))
  })

representantes_merge <- representantes_collapse %>%
  ldply() %>%
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

###########################################################################################################
##############################################  SENATE  ###################################################
###########################################################################################################


# Get presidents election data (Winners and loosers since 1997). 

list_files <- list.files(path = data) %>%
  .[. %in% c("1998", "2002", "2006", "2010", "2014")] %>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Sen")]}) %>% 
  str_c(data, c("1998", "2002", "2006", "2010", "2014"),"/", ., sep = "")


#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
senado <- lapply(list_files, read_dta)
senado[[5]]$partido <- NULL

#Aggregate totals for each year and clean non-candidate data

non_candidate_votes <- c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS",
                         "Votos en blanco", "Votos nulos", "Tarjetas no marcadas",
                         "Votos no marcados","COMITE PROMOTOR VOTO EN BLANCO","RETIRADO (A)", "TARJETAS NO MARCADOS")

invalid_places <- c(NaN, 96, 97, 99)
# (Nan: Consulate, 96, 87 and 99 are totals) #

senado_aggregate <- senado %>%
  lapply(., function(x){
    arrange(x, codmpio, ano) %>%
      filter(is.na(votos) == 0) %>% 
      filter(!codmpio %in% invalid_places) %>% #Remove totals
      mutate(no_cand = ifelse(primer_apellido %in% non_candidate_votes | nombre %in% non_candidate_votes, 1, 0)) %>% 
      mutate(cand = ifelse(no_cand == 0 & is.na(primer_apellido) == 0, 1, 0)) %>% 
      group_by(codmpio, ano) %>%
      mutate(rank = row_number(desc(votos))) %>% 
      mutate(prop_votes_total = votos / sum(votos)) %>%
      mutate(votos_cand = ifelse(cand == 1, votos, 0)) %>%
      mutate(prop_votes_cand = votos / sum(votos_cand)) %>%
      # mutate(votos_r2 = ifelse(rank <= 2, votos,0)) %>% 
      # mutate(prop_votes_c2 = votos / sum(votos_r2)) %>% 
      mutate(parties = sum(cand)) %>%
      mutate(party_ef = ifelse(prop_votes_cand > 0.1, 1,0)) %>%
      mutate(parties_ef = sum(party_ef)) %>% 
      filter(is.na(prop_votes_total)==0) 
  })

#Collapse candidates by party (remember that the number of delegates depends on the district magnitude)

senado_collapse <- senado_aggregate %>%
  lapply(., function(x){
    filter(x, cand == 1) %>%
      group_by(ano, codep, codmpio, codpartido) %>%
      summarise_at(vars(matches("vot")), sum) %>%
      group_by(ano, codep, codmpio) %>%
      mutate(rank = row_number(desc(votos)))
  })

senado_merge <- senado_collapse %>%
  ldply() %>%
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



