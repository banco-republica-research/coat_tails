
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","ggplot2","tidyr","rgeos","rgdal","raster","kml","broom","gtools","TraMineR","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"

###########################################################################################################
################################### LOAD AND ARRANGE DATA I  ##############################################
###########################################################################################################

##########################
# Get mayor's election data (Winners and loosers since 1997). 

list_files <- list.files(path=data) %>%
  .[. %in% c("1997", "2000", "2003", "2007", "2011", "2015")]%>%
  str_c(data, ., sep = "") %>%
  lapply(list.files) %>% lapply(function(x){x[str_detect(x, "Alcal")]}) %>% 
  str_c(data, c("1997", "2000", "2003", "2007", "2011", "2015"),"/", ., sep = "")

# Get party-code list
party_code <- read_dta(paste0(data,"codigos_partidos.dta"))

#Open dta files into a list (and add party codes to 2011 and 2015 electoral data)
alcaldes <- lapply(list_files, read_dta) 
alcaldes[[5]] <- alcaldes[[5]] %>%
  mutate(nombre = "") %>%
  mutate(partido_1 = as.factor(partido_1)) %>%
  mutate(partido_1 = fct_recode(partido_1,
                              "PARTIDO ALIANZA SOCIAL INDEPENDIENTE 'ASI'" = "PARTIDO ALIANZA SOCIAL INDEPENDIENTE",
                              "PARTIDO DE INTEGRACION NACIONAL PIN" = "PARTIDO DE INTEGRACION NACIONAL"
  )) %>% mutate(partido_1 = as.character(partido_1)) %>%
  stringdist_left_join(party_code, by = c(partido_1 = "name_party"), distance_col = "distance", max_dist = 2) %>%
  rename(codpartido = party_code)

alcaldes[[6]] <- alcaldes[[6]] %>%
  rename(codpartido = codpartido_1)

#Check for not-joined data
not_joined <- alcaldes[[5]] %>%
  filter(is.na(codpartido))

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

#Arrange data in a long format
alcaldes_merge <- alcaldes_aggregate %>%
  ldply() %>%
  arrange(codmpio, ano, desc(rank)) %>%
  dplyr::select(c(ano, codmpio, codep, municipio, parties, parties_ef, rank, primer_apellido, nombre, codpartido, cand, votos, votos_cand, votos_r2,
  prop_votes_total, prop_votes_cand, prop_votes_c2)) 

saveRDS(alcaldes_merge,paste0(res,"alcaldes_merge.rds"))

# Only rank <= 2 and drop if total votes == 0 
alcaldes_merge_r2 <- alcaldes_merge %>% filter(rank <= 2)
    #  %>%  filter(prop_votes_candidates < 1) #Eliminate elections with only one candidate

# Diagnistics
hist(alcaldes_merge_r2$prop_votes_c2)
alcaldes_merge_r2 %>% filter(is.na(prop_votes_c2)==0) %>%
  group_by(rank) %>% summarize(mean=mean(prop_votes_c2),sd=sd(prop_votes_c2),median=median(prop_votes_c2),min=min(prop_votes_c2),max=max(prop_votes_c2))


##########################
# Differences between winner and best runner up 

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

alcaldes_difference %>% 
  group_by(dif_q) %>% summarize(mean=mean(difference),sd=sd(difference),min=min(difference),max=max(difference))


##########################
# Only winners 1988-1994 

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
###################################### WINNING PARTIES ####################################################
###########################################################################################################

# Winning parties 1997-2015
alcaldes_merge_new <- alcaldes_merge %>% filter(rank == 1) %>% 
  dplyr::select(ano, codmpio, municipio, primer_apellido, nombre,codpartido, votos, prop_votes_total) 

# Append old and new 
alcaldes_win <- rbind(alcaldes_merge_old, alcaldes_merge_new) %>% arrange(codmpio, ano)

# Density vote share by year (interactive!)
d <- ggplot(alcaldes_win, aes(prop_votes_total, colour = factor(ano))) + geom_density() + 
  labs(color="Año", y= "Densidad", x = "votes winner") + theme_bw()
ggplotly(d)

# Party win by year
parties_win <- alcaldes_win %>% group_by(ano, codpartido) %>% summarize(win = n()) %>% 
  merge(party_code,  by.x = c("codpartido"), by.y = c("party_code"), all.x = T) %>% 
  arrange(ano, desc(win)) 

# Big parties: >x win in at least one year
big_parties <- parties_win %>% filter(win >= 20) %>% filter(codpartido!= 98 & codpartido!= 99) %>% 
  group_by(codpartido, name_party) %>% summarize(win = sum(win))%>% 
  arrange(desc(win)) 

# Keep only big parties and collapse others
parties_win_big <- parties_win %>% 
  mutate(party_big = ifelse(codpartido %in% big_parties$codpartido, codpartido, 9999)) %>% 
  mutate(party = ifelse(codpartido %in% big_parties$codpartido, name_party, "Otros")) %>% 
  group_by(ano, party_big, party) %>% summarize(win = sum(win)) %>% 
  group_by(ano) %>% 
  mutate(win_share = win/sum(win)) %>% 
  arrange(ano, party_big) 
  
# Keep liberal/conservador 
parties_win_lc <- parties_win %>% 
  mutate(party_lc = ifelse(codpartido %in% c(1,2), codpartido, 9999)) %>% 
  mutate(party = ifelse(codpartido %in% c(1,2), name_party, "Otros")) %>% 
  group_by(ano, party_lc, party) %>% summarize(win = sum(win)) %>% 
  group_by(ano) %>% 
  mutate(win_share = win/sum(win)) %>% 
  arrange(ano, party_lc) 


ggplot(parties_win_lc, aes(x = ano, y = win_share, fill = party)) + geom_bar(stat = "identity") + 
  labs(x = "Año") + scale_x_continuous(breaks = round(seq(min(parties_win_big$ano), max(parties_win_big$ano), by = 1),1)) + 
  theme_bw() + scale_fill_manual(values=c("grey","blue","red"), name = "", labels = c("Otros","Conservador","Liberal")) 


###########################################################################################################
################################ COMPETITION AND NO PARTIES ###############################################
###########################################################################################################


# Density by year (interactive!)
d <- ggplot(alcaldes_difference, aes(difference, colour = factor(ano))) + geom_density() + 
  labs(color="Año", y= "Densidad", x = "Competencia") + theme_bw()
ggplotly(d)

p <- ggplot(alcaldes_difference, aes(parties, colour = factor(ano))) + geom_density() + 
  labs(color="Año", y= "Densidad", x = "Partidos") + theme_bw()
ggplotly(p)

p_ef <- ggplot(alcaldes_difference, aes(parties_ef, colour = factor(ano))) + geom_density() + 
  labs(color="Año", y= "Densidad", x = "Partidos efectivos") + theme_bw()
ggplotly(p_ef)

# Number of parties and political competition 

s <- ggplot(alcaldes_difference, aes(parties, difference)) + geom_point(aes(colour = factor(ano), size=votes_tot)) + 
  labs(shape = "", color="Año", y= "Competencia", x = "Partidos") + theme_bw() 

ggplotly(s)

s_ef <- ggplot(alcaldes_difference, aes(parties_ef, difference)) + geom_point(aes(colour = factor(ano), size=votes_tot)) +
  labs(shape = "", color="Año", y= "Competencia", x = "Partidos efectivos") + theme_bw() 

ggplotly(s_ef)



