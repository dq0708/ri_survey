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
library(ggplot2)
library(Cairo)
library(geofacet)

##################################
# load results
##################################

load("all_chil_6m.RData")
load("state_dir_est_6m.RData")
load("analysis_results.RData")

##################################
# proportion of variace
##################################

varprop_alpha_vt <- varprop_gamma_vt <- varprop_delta_vt <- varprop_tau_vt <- varprop_phi_vt <- varprop_epsilon_vt <- NULL

for (m in c("m1", "m2", "m3", "m4", "m5", "m6")){
  inlafit <- get(paste0("inlafit_", m))
  
  i_ids <- inlafit$marginals.random$i_id
  marg.i_id <- matrix(NA, nrow = length(i_ids), ncol = 10000)
  for(i in 1:length(i_ids)){
    marg.i_id[i, ] <- inla.rmarginal(10000, i_ids[[i]])
  }
  vars.i_id <- apply(marg.i_id, 2, var)
  var.i_id <- median(vars.i_id)
  
  i_id1s <- inlafit$marginals.random$i_id1
  marg.i_id1 <- matrix(NA, nrow = length(i_id1s), ncol = 10000)
  for(i in 1:length(i_id1s)){
    marg.i_id1[i, ] <- inla.rmarginal(10000, i_id1s[[i]])
  }
  vars.i_id1 <- apply(marg.i_id1, 2, var)
  var.i_id1 <- median(vars.i_id1)
  
  b_ids <- inlafit$marginals.random$b_id
  marg.b_id <- matrix(NA, nrow = length(b_ids), ncol = 10000)
  for(i in 1:length(b_ids)){
    marg.b_id[i, ] <- inla.rmarginal(10000, b_ids[[i]])
  }
  vars.b_id <- apply(marg.b_id, 2, var)
  var.b_id <- median(vars.b_id)
  
  b_id1s <- inlafit$marginals.random$b_id1
  marg.b_id1 <- matrix(NA, nrow = length(b_id1s), ncol = 10000)
  for(i in 1:length(b_id1s)){
    marg.b_id1[i, ] <- inla.rmarginal(10000, b_id1s[[i]])
  }
  vars.b_id1 <- apply(marg.b_id1, 2, var)
  var.b_id1 <- median(vars.b_id1)
  
  i_id2s <- inlafit$marginals.random$i_id2
  marg.i_id2 <- matrix(NA, nrow = length(i_id2s), ncol = 10000)
  for(i in 1:length(i_id2s)){
    marg.i_id2[i, ] <- inla.rmarginal(10000, i_id2s[[i]])
  }
  vars.i_id2 <- apply(marg.i_id2, 2, var)
  var.i_id2 <- median(vars.i_id2)
  
  t_ids <- inlafit$marginals.random$t_id
  marg.t_id <- matrix(NA, nrow = length(t_ids), ncol = 10000)
  for(i in 1:length(t_ids)){
    marg.t_id[i, ] <- inla.rmarginal(10000, t_ids[[i]])
  }
  vars.t_id <- apply(marg.t_id, 2, var)
  var.t_id <- median(vars.t_id)
  
  ####
  all.vars <- c(var.i_id, var.i_id1, 
                var.b_id, var.b_id1, 
                var.i_id2, var.t_id)
  var.tot <- sum(all.vars)
  var.prop <- all.vars/var.tot
  
  varprop_alpha_vt <- append(varprop_alpha_vt, var.prop[1]) 
  varprop_gamma_vt <- append(varprop_gamma_vt, var.prop[2]) 
  varprop_delta_vt <- append(varprop_delta_vt, var.prop[3]) 
  varprop_tau_vt <- append(varprop_tau_vt, var.prop[4]) 
  varprop_phi_vt <- append(varprop_phi_vt, var.prop[5]) 
  varprop_epsilon_vt <- append(varprop_epsilon_vt, var.prop[6]) 

}

summ1_dt <- data.table(Model = mod_vt,
                       "ICAR" = varprop_alpha_vt,  "Space IID" =  varprop_gamma_vt,
                       "RW2" = varprop_delta_vt, "Time IID" = varprop_tau_vt,
                       "Space Time" = varprop_phi_vt,
                       "Survey" = varprop_epsilon_vt) 

##################################
# plots for m6
##################################

m <- "m6"
inlafit <- get(paste0("inlafit_", m))

#################
# estimated alpha
#################

re_est <- inlafit$summary.random$i_id$`0.5quant`
min(re_est)
max(re_est)
shp_plot <- map_nga_state_shp
shp_plot$re_est <- re_est

col_regions_med <- colorRampPalette(brewer.pal(8, "RdYlGn"))(1000)
at_med <- seq(-1.55, 1.55, 0.005)
labelat_med <- seq(-1.5, 1.5, 0.5)
labeltext_med <- format(round(seq(-1.5, 1.5, 0.5), 1), nsmall = 1)

jpeg("path_of_plot", width = 4, height = 3.2, units = "in", res = 600, type = "cairo")
sp_plot <- spplot(shp_plot, "re_est",
                  main = "ICAR Random Effects", xlab = "", ylab = "",
                  # sp.layout = list(scale_bar, text_x, text_y),
                  col.regions = col_regions_med,
                  at = at_med,
                  colorkey = list(col = col_regions_med,
                                  at = at_med,
                                  labels = list(at = labelat_med, labels = labeltext_med)))
print(sp_plot)
dev.off()

#################
# estimated gamma
#################

re_est <- inlafit$summary.random$i_id1$`0.5quant`
min(re_est)
max(re_est)
shp_plot <- map_nga_state_shp
shp_plot$re_est <- re_est

col_regions_med <- colorRampPalette(brewer.pal(8, "RdYlGn"))(1000)
at_med <- seq(-0.05, 0.05, 0.0002)
labelat_med <- seq(-0.05, 0.05, 0.025)
labeltext_med <- format(round(seq(-0.05, 0.05, 0.025), 3), nsmall = 3)

jpeg("path_of_plot", width = 4, height = 3.2, units = "in", res = 600, type = "cairo")
sp_plot <- spplot(shp_plot, "re_est",
                  main = "IID Random Effects", xlab = "", ylab = "",
                  # sp.layout = list(scale_bar, text_x, text_y),
                  col.regions = col_regions_med,
                  at = at_med,
                  colorkey = list(col = col_regions_med,
                                  at = at_med,
                                  labels = list(at = labelat_med, labels = labeltext_med)))
print(sp_plot)
dev.off()

#################
# estimated delta
#################

re_est <- inlafit$summary.random$b_id$`0.5quant`
re_low <- inlafit$summary.random$b_id$`0.025quant`
re_up <- inlafit$summary.random$b_id$`0.975quant`
data_plot <- data.table(b_id = 1:40,
                        b_year = seq(2000, 2019.5, 0.5),
                        re_est = re_est, re_low = re_low, re_up = re_up)
min(data_plot$re_low)
max(data_plot$re_up)

jpeg("path_of_plot", width = 6, height = 4, units = "in", res = 600, type = "cairo")
par(mar=c(4,4,1,1))

plot(x = -100, y = -100, 
     xlim = c(1, 36), ylim = c(-1.2, 1.2), 
     ann = FALSE, xaxt = "n", yaxt = "n")
mtext(side = 1, text = "Birth Cohort (Year of Birth)", line = 3, font = 2, cex = 0.9)
mtext(side = 2, text = "RW2 Random Effects", line = 2.7, font = 2, cex = 0.9)
axis(1, at = seq(1, 38, 2), 
     labels = 2000:2018, 
     las = 2, cex.axis = 0.9, font = 2)
axis(2, at = seq(-1.5, 1, 0.5), labels = format(round(seq(-1.5, 1, 0.5), 1), nsmall = 1), 
     las = 1, cex.axis = 0.9, font = 2)

abline(h = 0, lty = 2, lwd = 1.5, col = alpha("red", 0.5))

lines(x = data_plot[, b_id], 
      y = data_plot[, re_est],
      col = alpha("black", 0.7), lwd = 2.5)
polygon(c(data_plot[, b_id], rev(data_plot[, b_id])),
        c(data_plot[, re_low],
          rev(data_plot[, re_up])),
        col = alpha("black", 0.2), border = NA)
dev.off()

#################
# estimated tau
#################

re_est <- inlafit$summary.random$b_id1$`0.5quant`
re_low <- inlafit$summary.random$b_id1$`0.025quant`
re_up <- inlafit$summary.random$b_id1$`0.975quant`
data_plot <- data.table(b_id = 1:40,
                        b_year = seq(2000, 2019.5, 0.5),
                        re_est = re_est, re_low = re_low, re_up = re_up)
min(data_plot$re_low)
max(data_plot$re_up)

jpeg("path_of_plot", width = 6, height = 4, units = "in", res = 600, type = "cairo")
par(mar=c(4,4,1,1))

plot(x = -100, y = -100, 
     xlim = c(1, 36),  ylim = c(-0.4, 0.4), 
     ann = FALSE, xaxt = "n", yaxt = "n")
mtext(side = 1, text = "Birth Cohort (Year of Birth)", line = 3, font = 2, cex = 0.9)
mtext(side = 2, text = "IID Random Effects", line = 2.7, font = 2, cex = 0.9)
axis(1, at = data_plot[b_id%%2 == 1, b_id], 
     labels = data_plot[b_id%%2 == 1, b_year], 
     las = 2, cex.axis = 0.9, font = 2)
axis(2, at = seq(-0.4, 0.4, 0.1), labels = format(round(seq(-0.4, 0.4, 0.1), 1), nsmall = 1), 
     las = 1, cex.axis = 0.9, font = 2)

abline(h = 0, lty = 2, lwd = 1.5, col = alpha("red", 0.5))

lines(x = data_plot[, b_id], 
      y = data_plot[, re_est],
      col = alpha("black", 0.7), lwd = 2.5)
polygon(c(data_plot[, b_id], rev(data_plot[, b_id])),
        c(data_plot[, re_low],
          rev(data_plot[, re_up])),
        col = alpha("black", 0.2), border = NA)
dev.off()

#################
# estimated epsilon
#################

re_est <- inlafit$summary.random$t_id$`0.5quant`
re_low <- inlafit$summary.random$t_id$`0.025quant`
re_up <- inlafit$summary.random$t_id$`0.975quant`

svy_t_6m_lookup_dt <- svy_t_6m_lookup_dt[order(t_6m)]
svy_t_6m_lookup_dt$svy_char <- paste(svy_t_6m_lookup_dt$svy_year, svy_t_6m_lookup_dt$svy_type)
svy_t_6m_lookup_dt$svy_char[8] <- "2016-17 MICS/NICS"
data_plot <- data.table(t_id = 1:nrow(svy_t_6m_lookup_dt),
                        svy_char = svy_t_6m_lookup_dt$svy_char,
                        re_est = re_est, re_low = re_low, re_up = re_up)
min(data_plot$re_low)
max(data_plot$re_up)

jpeg("path_of_plot", width = 6, height = 4, units = "in", res = 600, type = "cairo")
par(mar=c(8.5,4,1,1))

plot(x = -100, y = -100, 
     xlim = c(0.5, 9.5), ylim = c(-1.2, 1.2), 
     ann = FALSE, xaxt = "n", yaxt = "n")
mtext(side = 1, text = "Survey", line = 6, font = 2, cex = 0.9)
mtext(side = 2, text = "Survey-specific Effects", line = 2.7, font = 2, cex = 0.9)
axis(1, at = data_plot$t_id, 
     labels = data_plot$svy_char, 
     las = 2, cex.axis = 0.9, font = 2)
axis(2, at = seq(-1, 1, 0.5), labels = format(round(seq(-1, 1, 0.5), 1), nsmall = 1), 
     las = 1, cex.axis = 0.9, font = 2)

abline(h = 0, lty = 2, lwd = 1.5, col = alpha("red", 0.5))

points(x = data_plot[, t_id], 
      y = data_plot[, re_est],
      col = "grey20", pch = 19, cex = 1.2)
segments(data_plot[, t_id], data_plot[, re_low],
         data_plot[, t_id], data_plot[, re_up],
         col = "grey20", lwd = 2.5)
dev.off()


#################
# estimated phi
#################

re_est <- inlafit$summary.random$i_id2$`0.5quant`
min(re_est)
max(re_est)

col_regions_med <- colorRampPalette(brewer.pal(8, "RdYlGn"))(1000)
at_med <- seq(-2, 2, 0.005)
labelat_med <- seq(-2, 2, 0.5)
labeltext_med <- format(round(seq(-2, 2, 0.5), 1), nsmall = 1)

for (b in 1:40){
  re_est <- inlafit$summary.random$i_id2$`0.5quant`[(b-1)*37 + 1:37]
  shp_plot <- map_nga_state_shp
  shp_plot$re_est <- re_est
  
  jpeg("path_of_plot", width = 4, height = 3.2, units = "in", res = 600, type = "cairo")
  sp_plot <- spplot(shp_plot, "re_est",
                    main = as.character(2000+(b-1)/2), xlab = "", ylab = "",
                    # sp.layout = list(scale_bar, text_x, text_y),
                    col.regions = col_regions_med,
                    at = at_med,
                    colorkey = list(col = col_regions_med,
                                    at = at_med,
                                    labels = list(at = labelat_med, labels = labeltext_med)))
  print(sp_plot)
  dev.off()
}

re_est_dt <- inla_dt_all[pred == 1, ]
re_est_dt <- re_est_dt[order(b_id, i_id)]
re_est_dt$phi_med_m6 <- inlafit_m6$summary.random$i_id2$`0.5quant`
re_est_dt$phi_low_m6 <- inlafit_m6$summary.random$i_id2$`0.025quant`
re_est_dt$phi_up_m6 <- inlafit_m6$summary.random$i_id2$`0.975quant`

state_region_lookup <- data.table(i_id = map_nga_state_shp$StateID, 
                                  StateName = map_nga_state_shp$StateName,
                                  RegionName = map_nga_state_shp$RegionName,
                                  name = sort(ng_state_grid1$name)[c(1, 3:15, 2, 16:37)])

b_year_lookup <- data.table(b_id = 1:40,
                            b_year = seq(2000, 2019.5, 0.5))

t_year_lookup <- data.table(t_id = 1:40,
                            t_year = seq(2000, 2019.5, 0.5))

x_sia_lookup <- data.table(x_sia = c(0, 1),
                           col = c("navy", "dark red"))

t_id_lookup <- data.table(t_id = svy_t_6m_lookup_dt$t_6m, 
                          survey = svy_t_6m_lookup_dt$svy_char)

re_est_dt <- merge(re_est_dt, state_region_lookup, by = "i_id", all.x = T)
re_est_dt <- merge(re_est_dt, b_year_lookup, by = "b_id", all.x = T)
re_est_dt <- merge(re_est_dt, t_year_lookup, by = "t_id", all.x = T)
re_est_dt <- merge(re_est_dt, x_sia_lookup, by = "x_sia", all.x = T)
re_est_dt <- merge(re_est_dt, t_id_lookup, by = "t_id", all.x = T)

jpeg("path_of_plot", width = 12, height = 9, units = "in", res = 600, type = "cairo")
print(ggplot(data = re_est_dt[b_year < 2019, ]) + 
        geom_line(data = re_est_dt[b_year < 2019, ], 
                  aes(x = b_year, y = phi_med_m6)) + 
        facet_geo(~ name, grid = "ng_state_grid1", label = "name") +
        geom_ribbon(data = re_est_dt[b_year < 2019, ], 
                    aes(x = b_year, ymin = phi_low_m6, ymax = phi_up_m6), 
                    linetype = 1, alpha = 0.3) + 
        facet_geo(~ name, grid = "ng_state_grid1", label = "name") + 
        labs(x = "Birth Cohort (Year of Birth)", y = "Estimated Space-time Interaction") + 
        theme(legend.position = "top"))
dev.off()

#################
# estimated RI coverage using geo_facet
#################

head(ng_state_grid1)

state_region_lookup <- data.table(i_id = map_nga_state_shp$StateID, 
                                  StateName = map_nga_state_shp$StateName,
                                  RegionName = map_nga_state_shp$RegionName,
                                  name = sort(ng_state_grid1$name)[c(1, 3:15, 2, 16:37)])

b_year_lookup <- data.table(b_id = 1:40,
                            b_year = seq(2000, 2019.5, 0.5))

t_year_lookup <- data.table(t_id = 1:40,
                            t_year = seq(2000, 2019.5, 0.5))

x_sia_lookup <- data.table(x_sia = c(0, 1),
                           col = c("navy", "dark red"))

t_id_lookup <- data.table(t_id = svy_t_6m_lookup_dt$t_6m, 
                          survey = svy_t_6m_lookup_dt$svy_char)

inla_dt_all <- merge(inla_dt_all, state_region_lookup, by = "i_id", all.x = T)
inla_dt_all <- merge(inla_dt_all, b_year_lookup, by = "b_id", all.x = T)
inla_dt_all <- merge(inla_dt_all, t_year_lookup, by = "t_id", all.x = T)
inla_dt_all <- merge(inla_dt_all, x_sia_lookup, by = "x_sia", all.x = T)
inla_dt_all <- merge(inla_dt_all, t_id_lookup, by = "t_id", all.x = T)

inla_dt_all[, "Survey" := as.factor(survey)]
inla_dt_all[, "Vaccination Activity" := factor(ifelse(x_sia == 0, "RI Only", "RI + 1 SIA"), levels = c("RI Only", "RI + 1 SIA"))]

for (m in c("m1", "m2", "m3", "m4", "m5", "m6")){
  
  jpeg("path_of_plot", width = 12, height = 9, units = "in", res = 600, type = "cairo")
  
  print(ggplot(data = inla_dt_all[b_year < 2019, ]) + 
    geom_point(data = inla_dt_all[pred == 0 & b_year < 2019, ], 
               aes(x = b_year, y = expit(outcome), colour = Survey, shape = `Vaccination Activity`), 
               alpha = 0.6) + 
    facet_geo(~ name, grid = "ng_state_grid1", label = "name") + 
    geom_segment(data = inla_dt_all[pred == 0 & b_year < 2019, ], 
                 aes(x = b_year, xend = b_year, y = expit(outcome - 1.96/sqrt(prec)), yend = expit(outcome + 1.96/sqrt(prec)), colour = Survey), 
                 alpha = 0.6) + 
    facet_geo(~ name, grid = "ng_state_grid1", label = "name") +
    geom_line(data = inla_dt_all[pred == 1 & b_year < 2019, ], 
              aes(x = b_year, y = get(paste0("est_med_", m )))) + 
    facet_geo(~ name, grid = "ng_state_grid1", label = "name") +
    geom_ribbon(data = inla_dt_all[pred == 1 & b_year < 2019, ], 
                aes(x = b_year, ymin = get(paste0("est_low_", m )), ymax = get(paste0("est_up_", m ))), 
                linetype = 1, alpha = 0.3) + 
    facet_geo(~ name, grid = "ng_state_grid1", label = "name") + 
    labs(x = "Birth Cohort (Year of Birth)", y = "MCV1 Coverage") + 
    theme(legend.position = "top") + 
    guides(colour = guide_legend(nrow = 2,byrow = F), 
           shape = guide_legend(nrow = 2,byrow = T))
  )
  dev.off()
}

#################
# direct estimates 
#################

jpeg("path_of_plot", width = 12, height = 9, units = "in", res = 600, type = "cairo")

ggplot(data = inla_dt_all[b_year < 2019, ]) + 
  geom_point(data = inla_dt_all[pred == 0 & b_year < 2019, ], 
             aes(x = b_year, y = expit(outcome), colour = Survey, shape = `Vaccination Activity`), 
             alpha = 0.8) + 
  facet_geo(~ name, grid = "ng_state_grid1", label = "name") + 
  geom_segment(data = inla_dt_all[pred == 0 & b_year < 2019, ], 
               aes(x = b_year, xend = b_year, y = expit(outcome - 1.96/sqrt(prec)), yend = expit(outcome + 1.96/sqrt(prec)), colour = Survey), 
               alpha = 0.8) + 
  facet_geo(~ name, grid = "ng_state_grid1", label = "name") +
  labs(x = "Birth Cohort (Year of Birth)", y = "MCV1 Coverage") + 
  theme(legend.position = "top") + 
  guides(colour = guide_legend(nrow = 2,byrow = F), 
         shape = guide_legend(nrow = 2,byrow = T))

dev.off()
  



