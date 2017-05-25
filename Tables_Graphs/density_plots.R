###########################################################################################################
###############@@############## RD: DENSITY PLOTS: MCCRARY DENSITY TESTS ##################################
###########################################################################################################
rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","ggplot2","tidyr","broom","cluster", "rdrobust", "rdd")
lapply(packageList,require,character.only=TRUE)

setwd("~/GitHub/coat_tails/Tables_Graphs/")
source("ggplot_density.R")


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
    dens <- dcdensity_ggplot(x$prop_votes_c2, cutpoint = 0.5, verbose = TRUE, plot = TRUE, bw = 0.1, ext.out = T)
    return(dens)
  })


mapply(function(x, type){
  a.l <- x$data[[1]]
  a.r <- x$data[[2]]

  g <- ggplot() + geom_point(data = a.l, aes(x = cellmp, y = cellval), size = 0.6)
  g <- g + geom_point(data = a.r, aes(x = cellmp, y = cellval), size = 0.6)
  g <- g + geom_line(data = a.l, aes(x = cellmp, y = est))
  g <- g + geom_line(data = a.r, aes(x = cellmp, y = est))
  g <- g + scale_x_continuous(limits = c(0, 1))
  g <- g + scale_y_continuous(limits = c(0, 5))
  g <- g + geom_line(data = a.l, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
  g <- g + geom_line(data = a.l, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
  g <- g + geom_line(data = a.r, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
  g <- g + geom_line(data = a.r, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
  g <- g + geom_vline(xintercept = 0.5, colour="gray", linetype = 1)
  g <- g + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  g <- g + labs(x = "Vote share at t", y = "Density")
  g <- g + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  g
  
  ggsave(str_c(results, "/Graphs/Density_tests/", "RD_", type, ".pdf"), width=30, height=20, units="cm")
}, x = density_tests, type = c("primera", "segunda", "final", "current"))


#Graph for only party (no coalition) 
a <- dcdensity_ggplot(alcaldes_merge$prop_votes_c2, cutpoint = 0.5, plot = T, ext.out  = T)
a.l <- a$data[[1]]
a.r <- a$data[[2]]

g <- ggplot() + geom_point(data = a.l, aes(x = cellmp, y = cellval), size = 0.6)
g <- g + geom_point(data = a.r, aes(x = cellmp, y = cellval), size = 0.6)
g <- g + geom_line(data = a.l, aes(x = cellmp, y = est))
g <- g + geom_line(data = a.r, aes(x = cellmp, y = est))
g <- g + scale_x_continuous(limits = c(0, 1))
g <- g + scale_y_continuous(limits = c(0, 2))
g <- g + geom_line(data = a.l, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.l, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.r, aes(x = cellmp, y = lwr), linetype = 2, colour = "grey40")
g <- g + geom_line(data = a.r, aes(x = cellmp, y = upr), linetype = 2, colour = "grey40")
g <- g + geom_vline(xintercept = 0.5, colour="gray", linetype = 1)
g <- g + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g <- g + labs(x = "Vote share at t", y = "Density")
g <- g + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
g
ggsave(str_c(results, "/Graphs/Density_tests/", "RD_party", ".pdf"), width=30, height=20, units="cm")












