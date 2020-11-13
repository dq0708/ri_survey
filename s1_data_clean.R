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
library(stringi)
library(DHScalendR)
library(gdata)
library(INLA)
library(spdep)
library(foreign)
library(rgdal)
library(raster)
library(Hmisc)

options(survey.lonely.psu = "adjust")

################################
# load and clean state map
################################

# Nigeria state map from GADM website
map_nga_state_shp <- readOGR(dsn = "path_to_data", layer = "layer_name", stringsAsFactors = F)

map_nga_state_shp$StateName <- as.character(map_nga_state_shp$NAME_1)
map_nga_state_shp$StateID <- as.integer(map_nga_state_shp$ID_1)
map_nga_state_shp <- map_nga_state_shp[c("StateName", "StateID")]

map_nga_state_shp$RegionName <- 
  ifelse(map_nga_state_shp$StateName %in% c("Benue", "Federal Capital Territory", "Kogi", "Kwara", "Nassarawa", "Niger", "Plateau"), "North Central", 
         ifelse(map_nga_state_shp$StateName %in% c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"), "North East", 
                ifelse(map_nga_state_shp$StateName %in% c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara"), "North West", 
                       ifelse(map_nga_state_shp$StateName %in% c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"), "South East", 
                              ifelse(map_nga_state_shp$StateName %in% c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers"), "South South", "South West")))))


nb <- poly2nb(map_nga_state_shp, queen = F, row.names = map_nga_state_shp$StateID)
mat <- nb2mat(nb, style = "B", zero.policy = TRUE)

################################
# create SIA schedule lookup
################################

# Information from the WHO SIA calendar
state_sia_lookup_dt <- data.table(map_nga_state_shp@data) 
state_sia_lookup_dt[, "sia_1_cmc" := ifelse(stri_sub(RegionName, 1, 1) == "N", 
                                            cmc_from_Date(as.Date("2005-12-10")),
                                            cmc_from_Date(as.Date("2006-10-09")))]
state_sia_lookup_dt[, "sia_2_cmc" := ifelse(stri_sub(RegionName, 1, 1) == "N", 
                                            cmc_from_Date(as.Date("2008-12-15")),
                                            cmc_from_Date(as.Date("2008-12-15")))]
state_sia_lookup_dt[, "sia_3_cmc" := ifelse(stri_sub(RegionName, 1, 1) == "N", 
                                            cmc_from_Date(as.Date("2011-01-30")),
                                            cmc_from_Date(as.Date("2011-02-27")))]
state_sia_lookup_dt[, "sia_4_cmc" := ifelse(stri_sub(RegionName, 1, 1) == "N", 
                                            cmc_from_Date(as.Date("2013-10-09")),
                                            cmc_from_Date(as.Date("2013-11-06")))]
state_sia_lookup_dt[, "sia_5_cmc" := ifelse(stri_sub(RegionName, 1, 1) == "N", 
                                            cmc_from_Date(as.Date("2015-11-25")),
                                            cmc_from_Date(as.Date("2016-02-01")))]
state_sia_lookup_dt[, "sia_6_cmc" := ifelse(stri_sub(RegionName, 1, 1) == "N", 
                                            cmc_from_Date(as.Date("2017-11-15")),
                                            cmc_from_Date(as.Date("2018-03-15")))]

################################################################
# compile child-level survey data - DHS 2003 as an example
################################################################

all_chil_dt <- NULL

# Raw survey data from the DHS website
dhs2003_chil_raw <- read.dta("path_to_data", convert.factors = F)

col_vt <- c("caseid", "v001", "v002", "v003", "v005", "v006", "v007", "v008", "v016", 
            "v021", "v022", "v023", "v024", "v025", 
            "h1", "h2", "h2d", "h2m", "h2y", "h3", "h3d", "h3m", "h3y", 
            "h4", "h4d", "h4m", "h4y", "h5", "h5d", "h5m", "h5y", 
            "h6", "h6d", "h6m", "h6y", "h7", "h7d", "h7m", "h7y", 
            "h8", "h8d", "h8m", "h8y", "h9", "h9d", "h9m", "h9y", 
            "h35", 
            "h0", "h0d", "h0m", "h0y", "h10",
            "hw1", "hw16", "sstate")

col_name_vt <- c("caseid", "cluster", "hh", "respond", "hhsampwgt", "interv_m", "interv_y", "interv_date", "interv_d", 
                 "psu", "strata", "domain", "region", "type", 
                 "hascard", "bcg", "bcg_d", "bcg_m", "bcg_y", "dpt1", "dpt1_d", "dpt1_m", "dpt1_y",
                 "polio1", "polio1_d", "polio1_m", "polio1_y", "dpt2", "dpt2_d", "dpt2_m", "dpt2_y",
                 "polio2", "polio2_d", "polio2_m", "polio2_y", "dpt3", "dpt3_d", "dpt3_m", "dpt3_y",
                 "polio3", "polio3_d", "polio3_m", "polio3_y", "measles", "measles_d", "measles_m", "measles_y",
                 "polio0", "polio0_d", "polio0_m", "polio0_y", "evervacc",
                 "vacc_camp2y",
                 "age_m", "birth_d", "state")

dhs2003_chil_dt <- as.data.table(dhs2003_chil_raw[, col_vt])
setnames(dhs2003_chil_dt, col_name_vt)

state_lookup <- data.table(state = seq(1, 37, 1),
                           statename = c("Akwa Ibom", "Anambra", "Bauchi", "Edo", "Benue", "Borno", "Cross River", 
                                         "Adamawa", "Imo", "Kaduna", "Kano", "Katsina", "Kwara", "Lagos", "Niger", 
                                         "Ogun", "Ondo", "Oyo", "Plateau", "Rivers", "Sokoto", "Abia", "Delta", "Enugu", 
                                         "Jigawa", "Kebbi", "Kogi", "Osun", "Taraba", "Yobe", "Bayelsa", "Ebonyi", "Ekit", 
                                         "Gombe", "Nassarawa", "Zamfora", "FCT Abuja"))

state_lookup <- state_lookup[order(statename)]
state_lookup$StateName <- sort(map_nga_state_shp$StateName)
dhs2003_chil_dt <- merge(dhs2003_chil_dt, state_lookup[, .(state, StateName)], by = "state", all.x = T)

dhs2003_chil_dt[, "sampwgt" := hhsampwgt/1e6] 
dhs2003_chil_dt[, "mcv" := ifelse(is.na(measles), 0, 
                                  ifelse(measles %in% c(1, 2, 3), 1, 0))]
dhs2003_chil_dt[, "interv_cmc" := interv_date ]
dhs2003_chil_dt[, "birth_cmc" := interv_cmc - age_m]

dhs2003_chil_dt <- merge(dhs2003_chil_dt, state_sia_lookup_dt, 
                         by = "StateName", all.x = T)

dhs2003_chil_dt_clean <- dhs2003_chil_dt[!is.na(birth_cmc), 
                                         c("StateName", "StateID", "type", "strata", "cluster", "hh", "sampwgt", "mcv",
                                           "age_m", "interv_cmc", "birth_cmc", 
                                           "sia_1_cmc", "sia_2_cmc", "sia_3_cmc", "sia_4_cmc", "sia_5_cmc", "sia_6_cmc")]

dhs2003_chil_dt_clean[, "svy_year" := 2003]
dhs2003_chil_dt_clean[, "svy_type" := "DHS"] 

all_chil_dt <- rbind(all_chil_dt, dhs2003_chil_dt_clean)

# The survey data from other surveys can be cleaned and appended to "all_chil_dt" in a similar way.

###############################
# standardize child-level data
###############################

# determine ri indicator
all_chil_dt[, "N_ri" := ifelse((interv_cmc - birth_cmc >= 9), 1, 0)]

# determine sia indicator
all_chil_dt[, "sia_1_yes" := ifelse((sia_1_cmc < interv_cmc) & (sia_1_cmc - birth_cmc >= 9), 1, 0)]
all_chil_dt[, "sia_2_yes" := ifelse((sia_2_cmc < interv_cmc) & (sia_2_cmc - birth_cmc >= 9), 1, 0)]
all_chil_dt[, "sia_3_yes" := ifelse((sia_3_cmc < interv_cmc) & (sia_3_cmc - birth_cmc >= 9), 1, 0)]
all_chil_dt[, "sia_4_yes" := ifelse((sia_4_cmc < interv_cmc) & (sia_4_cmc - birth_cmc >= 9), 1, 0)]
all_chil_dt[, "sia_5_yes" := ifelse((sia_5_cmc < interv_cmc) & (sia_5_cmc - birth_cmc >= 9), 1, 0)]
all_chil_dt[, "sia_6_yes" := ifelse((sia_6_cmc < interv_cmc) & (sia_6_cmc - birth_cmc >= 9), 1, 0)]

# create 6-monthly birth cohort
calib <- cmc_from_Date("1999-12-31") # 2000-01 to 2000-06 -> b_6m == 1

all_chil_dt[, "b_6m" := ceiling((birth_cmc - calib)/6)] 

# create 6_monthly observe cohort
all_chil_dt[, "t_6m" := ceiling((interv_cmc - calib)/6)]
svy_t_6m_dt <- all_chil_dt[, .(N_t_6m = .N), by = .(svy_year, svy_type, t_6m)]
svy_t_6m_lookup_dt <- svy_t_6m_dt[N_t_6m > 3000, c("svy_year", "svy_type", "t_6m")]

# count the no. of sia's experieced by each child
all_chil_dt[, "N_sia" := sia_1_yes + sia_2_yes + sia_3_yes + sia_4_yes + sia_5_yes + sia_6_yes]

# only use b_6m that (1) has ri oppotunities and (1) only 0 or 1 sia opportunities
state_svy_unique_N_sia_dt <- all_chil_dt[, .(max_N_sia = max(N_sia), 
                                             unique_N_sia = length(unique(N_sia)),
                                             min_ri = min(N_ri), 
                                             unique_N_ri = length(unique(N_ri))), 
                                         by = .(StateName, StateID, svy_year, svy_type, b_6m)]

state_svy_b_6m_dt <- state_svy_unique_N_sia_dt[max_N_sia < 2 & unique_N_sia == 1 & min_ri == 1 & unique_N_ri == 1, 
                                               c("StateName", "StateID", "svy_year", "svy_type", "b_6m")]

all_chil_dt_1 <- merge(all_chil_dt[, -c("t_6m")], state_svy_b_6m_dt, 
                       by = c("StateName", "StateID", "svy_year", "svy_type", "b_6m"), all.y = T)

# standardize t_6m for each survey
all_chil_dt_1 <- merge(all_chil_dt_1, svy_t_6m_lookup_dt, by = c("svy_year", "svy_type"), all.x = T)

# only use b_6m > 0
all_chil_dt_final <- all_chil_dt_1[b_6m > 0, 
                                   c("StateName", "StateID", "type", "strata", "cluster", "hh", "sampwgt", "mcv",
                                     "b_6m", "t_6m", "svy_year", "svy_type", "age_m", 
                                     "birth_cmc", "interv_cmc", "N_sia", "N_ri")]

save(map_nga_state_shp, nb, mat, 
     state_sia_lookup_dt, svy_t_6m_lookup_dt, 
     all_chil_dt, all_chil_dt_final, 
     file = "all_chil_6m.RData")

