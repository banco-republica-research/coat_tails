###########################################################################################################
###############@@############## RD: DENSITY PLOTS: MCCRARY DENSITY TESTS ##################################
###########################################################################################################
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","ggplot2","tidyr","broom","cluster", "rdrobust", "rdd")
lapply(packageList,require,character.only=TRUE)

# setwd("~/GitHub/coat_tails/Tables_Graphs/")
setwd("C:/Users/lbonilme/GitHub/coat_tails/Tables_Graphs/")
source("ggplot_density.R")


# Directory 
# setwd("~/Dropbox/BANREP/Elecciones/")
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
results <- "Results/RD"

alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))

###########################################################################################################
############################################ PARTY ########################################################
###########################################################################################################

#Graph for only party (no coalition) 
alcaldes_merge_r2 <- alcaldes_merge %>% 
  filter(rank<=2) %>% 
  filter(cand==1) %>% 
  filter(codpartido!=98 & codpartido!=99 & is.na(codpartido)==0) %>%
  group_by(codmpio, ano) %>%
  mutate(n = 1, nn = sum(n)) %>%
  filter(nn == 2) %>%
  group_by(ano, codmpio, codpartido) %>%
  mutate(party_2 = n()) %>%
  filter(party_2 == 1) %>% 
  dplyr::select(-c(n,nn, party_2)) 

a <- dcdensity_ggplot(alcaldes_merge_r2$margin_prop_2, cutpoint = 0, plot = T, ext.out  = T)
a
a.l <- a$data[[1]]
a.r <- a$data[[2]]

g <- ggplot() + geom_point(data = a.l, aes(x = cellmp, y = cellval), size = 0.6)
g <- g + geom_point(data = a.r, aes(x = cellmp, y = cellval), size = 0.6)
g <- g + geom_line(data = a.l, aes(x = cellmp, y = est))
g <- g + geom_line(data = a.r, aes(x = cellmp, y = est))
g <- g + scale_x_continuous(limits = c(-1, 1))
g <- g + geom_line(data = a.l, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.l, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.r, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.r, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
g <- g + geom_vline(xintercept = 0, colour="gray", linetype = 1)
g <- g + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g <- g + labs(x = "Victory Margin", y = "Density")
g <- g + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
# g
ggsave(str_c(results, "/Graphs/Density_tests/", "mani_party", ".pdf"), width=30, height=20, units="cm")


###########################################################################################################
######################### RD: IMCUMBENCY EFFECT - TRADITIONAL PARTIES #####################################
###########################################################################################################

#################
# Traditional

alcaldes_merge_r2_b <- alcaldes_merge_r2 %>% 
  filter(codpartido == 1 | codpartido == 2) 
  
a <- dcdensity_ggplot(alcaldes_merge_r2_b$margin_prop_2, cutpoint = 0, plot = T, ext.out  = T)
# a
a.l <- a$data[[1]]
a.r <- a$data[[2]]

g <- ggplot() + geom_point(data = a.l, aes(x = cellmp, y = cellval), size = 0.6)
g <- g + geom_point(data = a.r, aes(x = cellmp, y = cellval), size = 0.6)
g <- g + geom_line(data = a.l, aes(x = cellmp, y = est))
g <- g + geom_line(data = a.r, aes(x = cellmp, y = est))
g <- g + scale_x_continuous(limits = c(-1, 1))
g <- g + geom_line(data = a.l, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.l, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.r, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.r, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
g <- g + geom_vline(xintercept = 0, colour="gray", linetype = 1)
g <- g + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g <- g + labs(x = "Victory Margin", y = "Density")
g <- g + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
# g
ggsave(str_c(results, "/Graphs/Density_tests/mani_party2.pdf"), width=30, height=20, units="cm")


#################
# Non Traditional

alcaldes_merge_r2_b <- alcaldes_merge_r2 %>% 
  filter(codpartido != 1 & codpartido != 2)

a <- dcdensity_ggplot(alcaldes_merge_r2$margin_prop_2, cutpoint = 0, plot = T, ext.out  = T)
# a
a.l <- a$data[[1]]
a.r <- a$data[[2]]

g <- ggplot() + geom_point(data = a.l, aes(x = cellmp, y = cellval), size = 0.6)
g <- g + geom_point(data = a.r, aes(x = cellmp, y = cellval), size = 0.6)
g <- g + geom_line(data = a.l, aes(x = cellmp, y = est))
g <- g + geom_line(data = a.r, aes(x = cellmp, y = est))
g <- g + scale_x_continuous(limits = c(-1, 1))
g <- g + geom_line(data = a.l, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.l, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.r, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.r, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
g <- g + geom_vline(xintercept = 0, colour="gray", linetype = 1)
g <- g + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g <- g + labs(x = "Victory Margin", y = "Density")
g <- g + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
# g
ggsave(str_c(results, "/Graphs/Density_tests/mani_party2n.pdf"), width=30, height=20, units="cm")



###########################################################################################################
######################################### COALITIONS ######################################################
###########################################################################################################

# Load maire and coalition data
alcaldes_merge <- readRDS(paste0(res,"alcaldes_merge.rds"))
coalitions_primera <- readRDS(paste0(res,"coalitions_primera_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 
coalitions_segunda <- readRDS(paste0(res,"coalitions_segunda_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 
coalitions_final <- readRDS(paste0(res,"coalitions_new.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new) 
coalitions_current <- readRDS(paste0(res,"coalitions_current.rds")) %>% dplyr::select(codpartido,ano,year, codmpio,coalition_old, coalition_new)
coalitions_list <- list(coalitions_primera, coalitions_segunda, coalitions_final, coalitions_current)

#Merge coalition with top 2 of mayor elections

density_tests <- lapply(coalitions_list, function(x){
  alcaldes_merge %>% 
    filter(ano != 2015) %>%
    filter(cand==1) %>%
    filter(rank <= 2) %>% 
    merge(., x, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
    arrange(codmpio, ano, codpartido) %>%
    filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99)  %>%
    group_by(codmpio, ano) %>%
    mutate(n = 1, nn = sum(n)) %>%
    filter(nn==2) %>%
    filter(coalition_new == 1) %>%
    group_by(ano, codmpio) %>%
    mutate(party_2 = n()) %>% #Drop if two candidates are on the coalition 
    filter(party_2 == 1) %>% 
    dplyr::select(-c(codep,n,nn,party_2)) 
  
}) %>%
  lapply(function(x){
    dens <- dcdensity_ggplot(x$margin_prop_2, cutpoint = 0, verbose = TRUE, plot = TRUE, bw = 0.1, ext.out = T)
    return(dens)
  })


mapply(function(x, type){
  a.l <- x$data[[1]]
  a.r <- x$data[[2]]

  g <- ggplot() + geom_point(data = a.l, aes(x = cellmp, y = cellval), size = 0.6)
  g <- g + geom_point(data = a.r, aes(x = cellmp, y = cellval), size = 0.6)
  g <- g + geom_line(data = a.l, aes(x = cellmp, y = est))
  g <- g + geom_line(data = a.r, aes(x = cellmp, y = est))
  g <- g + scale_x_continuous(limits = c(-1, 1))
  g <- g + scale_y_continuous(limits = c(0, 5))
  g <- g + geom_line(data = a.l, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
  g <- g + geom_line(data = a.l, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
  g <- g + geom_line(data = a.r, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
  g <- g + geom_line(data = a.r, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
  g <- g + geom_vline(xintercept = 0, colour="gray", linetype = 1)
  g <- g + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  g <- g + labs(x = "Victory Margin", y = "Density")
  g <- g + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  g
  
  ggsave(str_c(results, "/Graphs/Density_tests/", "mani_", type, ".pdf"), width=30, height=20, units="cm")
}, x = density_tests, type = c("primera", "segunda", "final", "current"))













