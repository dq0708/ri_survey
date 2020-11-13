rm(list=ls())

library(data.table)
library(survey)
library(sp)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(rgeos)
library(maptools)
library(latticeExtra)
library(xtable)
library(Hmisc)

options(survey.lonely.psu = "adjust")

load("all_chil_6m.RData")

##################################
# create storage tables
##################################

state_dir_est_temp <- NULL

#######
# DHS
#######

for (year in c(2003, 2008, 2013, 2018)){
  b_6m_vt <- sort(unique(all_chil_dt_final[svy_year == year, ]$b_6m))
  
  for (i in 1:length(b_6m_vt)){
    b_6m_i <- b_6m_vt[i]
    
    svydata <- all_chil_dt_final[b_6m == b_6m_i & svy_year == year, ]
    
    # design object
    svydes <- svydesign(data = svydata, 
                        id = ~cluster+hh, 
                        weights = ~sampwgt, 
                        strata = ~strata, 
                        nest = T)
    
    # state-level mcv estimates
    mcv_state_dt <- as.data.table(svyby(formula = ~mcv, by = ~StateName, design = svydes, FUN = svymean))
    mcv_state_dt$b_6m <- unique(svydata$b_6m)
    mcv_state_dt$t_6m <- unique(svydata$t_6m)
    mcv_state_dt$svy_year <- unique(svydata$svy_year)
    mcv_state_dt$svy_type <- unique(svydata$svy_type)
    
    state_dir_est_temp <- rbind(state_dir_est_temp, mcv_state_dt)
    
    print(b_6m_i)
  }
}

################
# MICS & SMART
################

for (year in c(2007, 2011, 2014, 2015, 2016)){
  b_6m_vt <- sort(unique(all_chil_dt_final[svy_year == year, ]$b_6m))
  
  for (i in 1:length(b_6m_vt)){
    b_6m_i <- b_6m_vt[i]
    
    svydata <- all_chil_dt_final[b_6m == b_6m_i & svy_year == year, ]
    
    # design object
    svydes <- svydesign(data = svydata, 
                        id = ~cluster+hh, 
                        weights = ~sampwgt, 
                        strata = ~StateName, 
                        nest = T)
    
    # state-level mcv estimates
    mcv_state_dt <- as.data.table(svyby(formula = ~mcv, by = ~StateName, design = svydes, FUN = svymean))
    mcv_state_dt$b_6m <- unique(svydata$b_6m)
    mcv_state_dt$t_6m <- unique(svydata$t_6m)
    mcv_state_dt$svy_year <- unique(svydata$svy_year)
    mcv_state_dt$svy_type <- unique(svydata$svy_type)
    
    state_dir_est_temp <- rbind(state_dir_est_temp, mcv_state_dt)
    
    print(b_6m_i)
  }
}

##################################
# match to sia status and StateID
##################################

state_svy_b_t_lookup_dt <- unique(all_chil_dt_final[, c("StateName", "StateID", "b_6m", "t_6m", "N_sia")])

state_dir_est_final <- merge(state_dir_est_temp, state_svy_b_t_lookup_dt, 
                             by = c("StateName", "b_6m", "t_6m"), all.x = T)

##################################
# save results
##################################

save(state_dir_est_final,
     file = "state_dir_est_6m.RData")
