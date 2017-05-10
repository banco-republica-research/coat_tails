###########################################################################################################
###############@@############## RD: DENSITY PLOTS: MCCRARY DENSITY TESTS ##################################
###########################################################################################################
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","ggplot2","tidyr","broom","cluster", "rdrobust", "rdd")
lapply(packageList,require,character.only=TRUE)

# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
results <- "Results/RD"

###########################################################################################################
######################################## ELECTIONS DATA ###################################################
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
    filter(rank <= 2) %>% 
    merge(., x, by.x = c("codpartido","ano","codmpio") , by.y = c("codpartido","ano","codmpio"), all.x = T) %>%
    arrange(codmpio, ano, codpartido) %>%
    filter(is.na(coalition_new) == 0 & coalition_new != 98 & coalition_new != 99)  %>%
    group_by(codmpio, ano) %>%
    mutate(n = 1, nn = sum(n)) %>%
    filter(nn==2) %>%
    dplyr::select(-c(codep,n,nn))
}) %>%
  lapply(function(x){
    dens <- DCdensity(x$prop_votes_c2, cutpoint = 0.5, verbose = TRUE, plot = TRUE, bw = 0.1, ext.out = T)
    return(dens)
  })


mapply(function(x, type){
  d <- x$data %>%
    mutate(post = ifelse(cellmp > 0.5, 1, 0))
  
  g <- ggplot(d, aes(y = cellval, x = cellmp, colour = as.factor(post))) 
  g <- g + stat_smooth(method = "auto", se = F, span = 0.23, colour = "black")
  g <- g + geom_ribbon(stat='smooth', method = "auto", span = 0.23, se = TRUE, linetype = "dashed",
                       fill = NA, colour = "grey40")
  g <- g + geom_point(colour = "black", size = 1)
  g <- g + scale_y_continuous(limits = c(0, 6))
  g <- g + labs(x = "Vote share at t", y = "Density")
  g <- g + geom_vline(xintercept = 0.5, colour="grey", linetype = "longdash")
  g <- g + guides(colour = FALSE)
  g <- g + theme_bw()
  g
  ggsave(str_c(results, "/Graphs/Density_tests/", "RD_", type, ".pdf"), width=30, height=20, units="cm")
}, x = density_tests, type = c("primera", "segunda", "final", "current"))

dcdensity_ggplot(alcaldes_merge$prop_votes_c2, cutpoint = 0.5, plot = T, verbose = T)





