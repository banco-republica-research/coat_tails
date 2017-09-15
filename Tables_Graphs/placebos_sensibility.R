###############################################################################
################################ PLACEBO TESTS ################################
###############################################################################

rm(list=ls())
packageList<-c("foreign","plyr","dplyr","haven","fuzzyjoin", "forcats", "stringr","plotly","stargazer","tidyr","broom","cluster", "rdrobust")
lapply(packageList,require,character.only=TRUE)

# Directory 
setwd("~/Dropbox/BANREP/Elecciones/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Elecciones/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Elecciones/")

data <-"Data/CEDE/Microdatos/"
res <-"Data/CEDE/Bases/"
results <- "Results/RD/"

#############
# Function to read results

rd_to_df <- function(list, name){
  rd <- lapply(list, "[", "tabl3.str") %>%
    lapply(as.data.frame) %>%
    lapply( "[", 1 , ) %>% 
    ldply() %>% mutate(N_l = unlist(lapply(list, "[", "N_h_l"))) %>%
    mutate(N_r = unlist(lapply(list, "[", "N_h_r"))) %>%
    mutate(N = N_l + N_r) %>%
    mutate(bws = unlist(lapply(list, function(x) x$bws[1, 1]))) %>%
    as.data.frame() %>%
    rename(Treatment = tabl3.str.Coef, SE = tabl3.str.Std..Err., z = tabl3.str.z, p_value = tabl3.str.P..z., CI_l = tabl3.str.CI.Lower, 
           CI_u = tabl3.str.CI.Upper, N_left = N_l, N_right = N_r, N = N, Bandwidth = bws ) %>%
  mutate_all(funs(as.character)) %>% mutate_all(funs(as.numeric)) %>% mutate(.id = name)
  return(rd)
}


setwd(results)

######################## RDROBUST OBJ TO DATAFRAME ###########################
############################# LOAD SAVED OBJECTS #############################

list_files <- list.files("Placebos", full.names = T) %>% .[1:4]
sens_test <- lapply(list_files, readRDS) %>%
  setNames(., list_files)

list_files <- list.files("Placebos", full.names = T) %>%
  .[str_detect(.,"roads")]
sens_test_inv <- lapply(list_files, readRDS) %>%
  setNames(., list_files) %>% .[[1]]

list_files <- list.files() %>%
  .[str_detect(., "_2_coalition")]
coalition_2 <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "_final_coalition")] 
final <- lapply(list_files, readRDS) %>%
  lapply(., `[[`, 1) %>%
  setNames(., list_files)

list_files <- list.files() %>%
  .[str_detect(., "total_current")]
total_current <- lapply(list_files, readRDS)%>%
  unlist(recursive = FALSE) %>%
  setNames(., outcomes)

########################### GET OPTIMAL BW'S #############################

#ELECTIONS
optimal_bws_elec <- c(coalition_2, final) %>%
 lapply(., function(x) x$rd$bws[1,1])


#INVESTMENT
optimal_bws_inv <- c(total_current[5:8]) %>%
  lapply(., function(x) x$rd$bws[1,1])


###################### SENSIBILITY TESTS TO DF ##############################

#ELECTIONS
sens_df_elec <- mapply(rd_to_df, list = sens_test, 
                  name = list("House", "Presidential (Second Round)", "Senate", "Mayor"), 
                  SIMPLIFY = F) %>%
  ldply() %>% 
  mutate(optimal_bw = ifelse(Bandwidth %in% optimal_bws_elec, 1, 0)) %>%
  mutate(ci_10_h = Treatment + 1.645 * SE) %>%
  mutate(ci_10_l = Treatment - 1.645 * SE) %>%
  subset(Bandwidth <= 0.25) %>%
  mutate(.id = factor(.$.id, levels = c('Mayor', 'House', 'Senate', 'Presidential (Second Round)')))
  
#INVESTMENT
sens_df_inv <- mapply(rd_to_df, list = sens_test_inv, 
                  name = list("Total", "SGP", "Royalties", "Co-financing Funds"), 
                  SIMPLIFY = F) %>%
  ldply() %>% 
  mutate(optimal_bw = ifelse(Bandwidth %in% optimal_bws_inv, 1, 0)) %>%
  mutate(ci_10_h = Treatment + 1.645 * SE) %>%
  mutate(ci_10_l = Treatment - 1.645 * SE) %>%
  subset(Bandwidth <= 0.25) %>%
  mutate(.id = factor(.$.id, levels = c('Total', 'SGP', 'Royalties', 'Co-financing Funds')))


############################# PLOT SENSIBILITY TEST ####################
setwd("~/Dropbox/BANREP/Elecciones/Results/RD/Graphs/Sens_tests/")
g <- ggplot(sens_df_elec, aes(y = Treatment, x = Bandwidth)) 
g <- g + facet_wrap( ~ .id, ncol=1, scales = "fixed")
g <- g + geom_line()
# g <- g + scale_y_continuous(lim = c(-0.12, 0.2))
g <- g + coord_cartesian(xlim = c(0, 0.25))
g <- g + geom_ribbon(aes(ymin = ci_10_l, ymax = ci_10_h), alpha = 0.2)
# g <- g + geom_vline(xintercept = 0, linetype = 2) 
g <- g + geom_vline(data = sens_df_elec[sens_df_elec$optimal_bw == 1, ], aes(xintercept = Bandwidth), colour="red")
g <- g + geom_hline(yintercept = 0, linetype = 2, colour = "grey")
g <- g + labs(y = "Estimated effect")
g <- g + theme_bw()
g
ggsave("RDggplot_sens_test.pdf", width=30, height=20, units="cm")


############################# PLOT SENSIBILITY TEST ####################
setwd("~/Dropbox/BANREP/Elecciones/Results/RD/Graphs/Sens_tests/")
g <- ggplot(sens_df_inv, aes(y = Treatment, x = Bandwidth)) 
g <- g + facet_wrap( ~ .id, ncol=1, scales = "fixed")
g <- g + geom_line()
# g <- g + scale_y_continuous(lim = c(-0.12, 0.2))
g <- g + coord_cartesian(xlim = c(0, 0.25))
g <- g + geom_ribbon(aes(ymin = ci_10_l, ymax = ci_10_h), alpha = 0.2)
# g <- g + geom_vline(xintercept = 0, linetype = 2) 
g <- g + geom_vline(data = sens_df_inv[sens_df_inv$optimal_bw == 1, ], aes(xintercept = Bandwidth), colour="red")
g <- g + geom_hline(yintercept = 0, linetype = 2, colour = "grey")
g <- g + labs(y = "Estimated effect")
g <- g + theme_bw()
g
ggsave("RDggplot_sens_test_inv.pdf", width=30, height=20, units="cm")





