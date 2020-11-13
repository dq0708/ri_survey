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
library(INLA)
library(spdep)

##################################
# load previous results
##################################

load("all_chil_6m.RData")
load("state_dir_est_6m.RData")

######################
# useful functions
######################

logit<-function(x){
  log(x/(1-x))
}

expit<-function(x){
  exp(x)/(1+exp(x))
}

######################
# prepare inla data
######################

# create logit_p and variance
state_dir_est_final[, c("logit_est", "var_logit_est") := list(logit(mcv), (se^2)/(mcv^2*(1-mcv)^2))]

state_dir_est_final[, c("i_id", "b_id", "t_id", "x_sia", "pred", "s_id", "outcome", "prec") := 
                      list(StateID, b_6m, t_6m, N_sia, 0, 
                           ifelse(svy_type == "DHS", 1, ifelse(svy_type == "MICS", 2, 3)),
                           ifelse(mcv < 0.0001 | se < 0.0001, NA, logit_est),
                           ifelse(mcv < 0.0001 | se < 0.0001, NA, 1/var_logit_est))]

inladata_dt <- state_dir_est_final[, c("i_id", "b_id", "t_id", "s_id",
                                       "x_sia", "outcome", "prec", "pred")]

inladata_dt[, c("i_id1", "i_id2", "b_id1", "b_id2") := list(i_id, i_id, b_id, b_id)]

# create prediction table
i_id_vt <- unique(inladata_dt[, i_id])
b_id_vt <- 1:40

inlapred_dt <- data.table(i_id = rep(i_id_vt, each = length(b_id_vt)),
                          b_id = rep(b_id_vt, length(i_id_vt)), 
                          t_id = NA, s_id = NA, 
                          x_sia = 0, outcome = NA, prec = NA, pred = 1)

inlapred_dt[, c("i_id1", "i_id2", "b_id1", "b_id2") := list(i_id, i_id, b_id, b_id)]

# combine data and prediction tables
inla_dt_all <- rbind(inladata_dt, inlapred_dt)

###############
# fit model 
###############

m1 <- outcome ~ 1 + x_sia + 
  f(i_id, model = "besag", graph = mat, scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(i_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(b_id, model = "rw2", scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(b_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(t_id, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(i_id2, model = "iid", group = b_id2, control.group = list(model = "iid"))

m2 <- outcome ~ 1 + x_sia + 
  f(i_id, model = "besag", graph = mat, scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(i_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(b_id, model = "rw2", scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(b_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) +  
  f(t_id, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(i_id2, model = "besag", graph = mat, scale.model = T, group = b_id2, control.group = list(model = 'iid'))

m3 <- outcome ~ 1 + x_sia + 
  f(i_id, model = "besag", graph = mat, scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(i_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(b_id, model = "rw2", scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(b_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(t_id, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(i_id2, model = "iid", group = b_id2, control.group = list(model = "rw2", scale.model = TRUE))

m4 <- outcome ~ 1 + x_sia + 
  f(i_id, model = "besag", graph = mat, scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(i_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(b_id, model = "rw2", scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(b_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(t_id, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(i_id2, model = "besag", graph = mat, scale.model = T, group = b_id2, control.group = list(model = "rw2", scale.model = TRUE))

m5 <- outcome ~ 1 + x_sia + 
  f(i_id, model = "besag", graph = mat, scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(i_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(b_id, model = "rw2", scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(b_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(t_id, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(i_id2, model = "iid", group = b_id2, control.group = list(model = "ar1", scale.model = TRUE))

m6 <- outcome ~ 1 + x_sia + 
  f(i_id, model = "besag", graph = mat, scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(i_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(b_id, model = "rw2", scale.model = T, 
    hyper = list(theta = list(prior = "pc.prec", param = c(2, 0.01)))) + 
  f(b_id1, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(t_id, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) + 
  f(i_id2, model = "besag", graph = mat, scale.model = T, group = b_id2, control.group = list(model = "ar1", scale.model = TRUE))


  
##################################

inlafit_m1 <- inla(formula = m1, 
                   family = "gaussian", 
                   data = inla_dt_all, 
                   control.predictor = list(compute = T),
                   control.compute = list(cpo = T, dic = T, config = T, waic = T),
                   control.family = list(hyper = list(prec = list(initial = log(1), fixed = T))),
                   scale = prec, verbose = F)

inlafit_m2 <- inla(formula = m2, 
                   family = "gaussian", 
                   data = inla_dt_all, 
                   control.predictor = list(compute = T),
                   control.compute = list(cpo = T, dic = T, config = T, waic = T),
                   control.family = list(hyper = list(prec = list(initial = log(1), fixed = T))),
                   scale = prec, verbose = F)

inlafit_m3 <- inla(formula = m3, 
                   family = "gaussian", 
                   data = inla_dt_all, 
                   control.predictor = list(compute = T),
                   control.compute = list(cpo = T, dic = T, config = T, waic = T),
                   control.family = list(hyper = list(prec = list(initial = log(1), fixed = T))),
                   scale = prec, verbose = F)

inlafit_m4 <- inla(formula = m4, 
                   family = "gaussian", 
                   data = inla_dt_all, 
                   control.predictor = list(compute = T),
                   control.compute = list(cpo = T, dic = T, config = T, waic = T),
                   control.family = list(hyper = list(prec = list(initial = log(1), fixed = T))),
                   scale = prec, verbose = F)

inlafit_m5 <- inla(formula = m5, 
                   family = "gaussian", 
                   data = inla_dt_all, 
                   control.predictor = list(compute = T),
                   control.compute = list(cpo = T, dic = T, config = T, waic = T),
                   control.family = list(hyper = list(prec = list(initial = log(1), fixed = T))),
                   scale = prec, verbose = F)

inlafit_m6 <- inla(formula = m6, 
                   family = "gaussian", 
                   data = inla_dt_all, 
                   control.predictor = list(compute = T),
                   control.compute = list(cpo = T, dic = T, config = T, waic = T),
                   control.family = list(hyper = list(prec = list(initial = log(1), fixed = T))),
                   scale = prec, verbose = F)

########################### 
# append model results
########################### 

inla_dt_all[, "est_med_m1"] <- expit(inlafit_m1$summary.fitted.values$`0.5quant`)
inla_dt_all[, "est_low_m1"] <- expit(inlafit_m1$summary.fitted.values$`0.025quant`)
inla_dt_all[, "est_up_m1"] <- expit(inlafit_m1$summary.fitted.values$`0.975quant`)

inla_dt_all[, "est_med_m2"] <- expit(inlafit_m2$summary.fitted.values$`0.5quant`)
inla_dt_all[, "est_low_m2"] <- expit(inlafit_m2$summary.fitted.values$`0.025quant`)
inla_dt_all[, "est_up_m2"] <- expit(inlafit_m2$summary.fitted.values$`0.975quant`)

inla_dt_all[, "est_med_m3"] <- expit(inlafit_m3$summary.fitted.values$`0.5quant`)
inla_dt_all[, "est_low_m3"] <- expit(inlafit_m3$summary.fitted.values$`0.025quant`)
inla_dt_all[, "est_up_m3"] <- expit(inlafit_m3$summary.fitted.values$`0.975quant`)

inla_dt_all[, "est_med_m4"] <- expit(inlafit_m4$summary.fitted.values$`0.5quant`)
inla_dt_all[, "est_low_m4"] <- expit(inlafit_m4$summary.fitted.values$`0.025quant`)
inla_dt_all[, "est_up_m4"] <- expit(inlafit_m4$summary.fitted.values$`0.975quant`)

inla_dt_all[, "est_med_m5"] <- expit(inlafit_m5$summary.fitted.values$`0.5quant`)
inla_dt_all[, "est_low_m5"] <- expit(inlafit_m5$summary.fitted.values$`0.025quant`)
inla_dt_all[, "est_up_m5"] <- expit(inlafit_m5$summary.fitted.values$`0.975quant`)

inla_dt_all[, "est_med_m6"] <- expit(inlafit_m6$summary.fitted.values$`0.5quant`)
inla_dt_all[, "est_low_m6"] <- expit(inlafit_m6$summary.fitted.values$`0.025quant`)
inla_dt_all[, "est_up_m6"] <- expit(inlafit_m6$summary.fitted.values$`0.975quant`)

################## 
# save results
################## 

save(logit, expit, 
     inlafit_m1, inlafit_m2, inlafit_m3, inlafit_m4, inlafit_m5, inlafit_m6, 
     inla_dt_all,
     file = "analysis_results.RData")
