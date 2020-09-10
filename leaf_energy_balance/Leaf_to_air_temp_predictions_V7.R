### Code for modelling leaf temps for B2 paper
###

#--Using tealeaves package by Chris Muir
# https://github.com/cdmuir/tealeaves/blob/master/README.md
# https://www.biorxiv.org/content/biorxiv/early/2019/01/24/529487.full.pdf 

#install.packages("devtools")
#library(devtools)
#devtools::install_github("cdmuir/tealeaves", force=TRUE)
#library(magrittr)
#library(tealeaves)
require(tealeaves)
require(ggplot2)


#####################--USE FULL DISTRIBUTION OF ENVIRONMENTAL PARAMETERS--#################################

# Import the datasets - just for Topt values for K34 and B2 (28 and 38 degrees, respectively)
dir <- "C:/Users/Marielle/Documents/PhD/RESEARCH PROJECTS/B2 NEE paper"
setwd(dir)
load(file="Tleaf_modelling_params_FULL") 
# b2.dat and K34.dat; note: par column for K34 has been converted to short-wave incoming radiation


############################### FUNCTIONS FOR TLEAF TEMP ESTIMATIONS

#--Configure B2 dataset
B2dat <- b2.dat
B2dat$tair <- 38 + 273.15
B2dat$pressure <- 86
B2dat$rh <- B2dat$rh/100
## NOTE: make sure order of columns is: par, rh, wind, tair, pressure

#--Configure K34 dataset
K34dat <- K34.dat
K34dat$tair <- 28 + 273.15
K34dat$pressure <- 99
K34dat$rh <- K34dat$rh/100
## NOTE: make sure order of columns is: par, rh, wind, tair, pressure

#--Configure a test dataset -- data from Doughty & Goulden 2008
DGdat <- data.frame(par=c(250,1500), rh=0.7, wind=c(1,1), tair=c(28.5+273.15,29.5+273.15), pressure=99, leaf.size=0.108, gs=c(1.2,1.05), tleaf.meas=c(30,37.5), scenario=c(2,1))
DGdat$par <- DGdat$par*0.5 # change PAR to short-wave incoming radiation


#--B2 dataset -- with same DISTRIBUTION of PAR as K34 (but same number of values as B2 data)
B2dat.par.test <- B2dat

dens.obs.par = density(K34dat$par, adjust=0.8)
set.seed(439)
resample.obs.par = sample(dens.obs.par$x, nrow(B2dat), replace=TRUE, prob=dens.obs.par$y)
dat = data.frame(value=c(K34dat$par,resample.obs.par), 
                 group=rep(c("Observed","Modeled"), c(length(K34dat$par),length(resample.obs.par))))
ggplot(dat, aes(value, fill=group, colour=group)) +
  stat_ecdf(geom="step") +
  theme_bw()

ggplot(dat, aes(x = value, colour=group)) + theme_bw(21) +
  geom_density(size=1) 

B2dat.par.test$par <- resample.obs.par

#--B2 dataset -- with same DISTRIBUTION of wind speed as K34
B2dat.wind.test <- B2dat

dens.obs.wind = density(K34dat$wind, adjust=0.8)
set.seed(439)
resample.obs.wind = sample(dens.obs.wind$x, nrow(B2dat), replace=TRUE, prob=dens.obs.wind$y)
dat = data.frame(value=c(K34dat$wind,resample.obs.wind), 
                 group=rep(c("Observed","Modeled"), c(length(K34dat$wind),length(resample.obs.wind))))
ggplot(dat, aes(value, fill=group, colour=group)) +
  stat_ecdf(geom="step") +
  theme_bw()

ggplot(dat, aes(x = value, colour=group)) + theme_bw(21) +
  geom_density(size=1) 

B2dat.wind.test$wind <- resample.obs.wind


#--B2 dataset -- with same DISTRIBUTION of humidity as K34
B2dat.rh.test <- B2dat  

dens.obs.rh = density(K34dat$rh, adjust=0.8)
set.seed(439)
resample.obs.rh = sample(dens.obs.rh$x, nrow(B2dat), replace=TRUE, prob=dens.obs.rh$y)
dat = data.frame(value=c(K34dat$rh,resample.obs.rh), 
                 group=rep(c("Observed","Modeled"), c(length(K34dat$rh),length(resample.obs.rh))))
ggplot(dat, aes(value, fill=group, colour=group)) +
  stat_ecdf(geom="step") +
  theme_bw()

ggplot(dat, aes(x = value, colour=group)) + theme_bw(21) +
  geom_density(size=1) 

B2dat.rh.test$rh <- resample.obs.rh


#--B2 dataset -- with same distributions of wind speed and PAR as K34
B2dat.parwind.test <- B2dat
B2dat.parwind.test$wind <- resample.obs.wind
B2dat.parwind.test$par <- resample.obs.par
B2dat.parwind.test$rh <- resample.obs.rh


get_tleaf <- function(sw, rh, wind, tair, pressure, leaf.size, gs) {
  
  # Set the environmental parameters (Tair, RH, PAR, and wind speed)
  enviro_par <- make_enviropar(
    replace = list(
      T_air = set_units(c(tair), "K"),
      RH = set_units(c(rh)),
      S_sw = set_units(c(sw), "W/m^2"),
      wind = set_units(c(wind), "m/s"),
      P = set_units(c(pressure), "kPa")
    )
  )
  
  # Set the leaf parameters (stomatal conductance)
  # b2.gs <- 3 # Medium amount, according to tealeaves creator
  
  leaf_par <- make_leafpar(
    replace = list(
      g_sw = set_units(gs, "umol/m^2/s/Pa"),
      leafsize = set_units(leaf.size, "m") # leaf size from 5 cm wide leaf * Tim's scaler; this is the width x 0.72 = the definition of the characteristic dimension of the leaf
    )
  )
  
  # Physical constants probably do not need to be replaced in most cases,
  # that's why we call them 'constants'!
  constants  <- make_constants()
  
  T_leaf <- tleaf(leaf_par, enviro_par, constants, quiet = TRUE)
  
  #--Extract Tleaf in degrees C
  T_leaf_degC <- as.numeric(T_leaf$T_leaf)-273.15
  
  return(T_leaf_degC)
  
}


#--Call get_tleaf (note: column headings must be: par, rh, wind, tair, pressure, leaf.size, gs)
# Note: B2dat is redefined as 'data' (by the apply function)
B2dat$tleaf <- apply (B2dat, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=3)) 

# B2 gs tests
B2dat$tleaf.gs.low <- apply (B2dat, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=1.5))
#
B2dat$tleaf.gs.high <- apply (B2dat, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=6))

K34dat$tleaf <- apply (K34dat, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=3))

# K34 gs tests
K34dat$tleaf.gs.low <- apply (K34dat, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=1.5))
#
K34dat$tleaf.gs.high <- apply (K34dat, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=6))


# B2dat.par.test$tleaf <- apply (B2dat.par.test, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=3)) 
# 
# B2dat.wind.test$tleaf <- apply (B2dat.wind.test, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=3)) 
# 
# B2dat.parwind.test$tleaf <- apply (B2dat.parwind.test, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=3)) 
# 
# B2dat.rh.test$tleaf <- apply (B2dat.rh.test, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=0.036, gs=3))
# 
# DGdat$tleaf <- apply (DGdat, 1, function (data) get_tleaf (sw=data[1], rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size=data[6], gs=data[7]))


# Convert Tair values back to degrees C
B2dat$tair <- B2dat$tair - 273.15
K34dat$tair <- K34dat$tair - 273.15
# B2dat.par.test$tair <- B2dat.par.test$tair - 273.15
# B2dat.wind.test$tair <- B2dat.wind.test$tair - 273.15
# B2dat.parwind.test$tair <- B2dat.parwind.test$tair - 273.15
# B2dat.rh.test$tair <- B2dat.rh.test$tair - 273.15
# DGdat$tair <- DGdat$tair - 273.15

#--Calculate the difference between measured and modelled tleaf
# DGdat$diff <- DGdat$tleaf - DGdat$tleaf.meas

#--FIGURE
# Density plots of B2 vs K34 leaf temps 

#--Combine data
B2dat$site <- "B2-TF"
K34dat$site <- "K34"
# B2dat.par.test$site <- "B2-TF rad test"
# B2dat.wind.test$site <- "B2-TF wind test"
# B2dat.parwind.test$site <- "B2-TF rad_wind_RH test"
# B2dat.rh.test$site <- "B2-TF RH test"

# For environmental tests
# temp.dat <- rbind(B2dat, K34dat, B2dat.par.test, B2dat.wind.test, B2dat.parwind.test, B2dat.rh.test) 
# temp.dat$site <- factor(temp.dat$site, levels= c("K34", "B2-TF", "B2-TF rad test", "B2-TF wind test", "B2-TF RH test", "B2-TF rad_wind_RH test"))

# For gs tests
temp.dat <- rbind(B2dat, K34dat) 
temp.dat$site <- factor(temp.dat$site, levels= c("K34", "B2-TF"))

# Calculate temp differential
temp.dat$diff <- temp.dat$tleaf-temp.dat$tair
# gs tests
temp.dat$diff.gs.low <- temp.dat$tleaf.gs.low-temp.dat$tair
temp.dat$diff.gs.high <- temp.dat$tleaf.gs.high-temp.dat$tair
cols <- c("dodgerblue", "red")

# temp.plot <- ggplot(temp.dat, aes(x = diff, colour=site)) + theme_bw(21) +
#   geom_density(size=1) +
#   ylab('Density') +
#   xlab(bquote('Leaf temperature ('*degree*'C)')) #+
# temp.plot <- temp.plot + theme(legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0.03,0.95))
# temp.plot


#--Boxplot of Tleaf distributions
# temp.plot <- ggplot(temp.dat, aes(x=site, y=tleaf, colour=site)) +
#   ylab(bquote('Leaf temperature ('*degree*'C)')) + theme_bw(21) +
#   xlab(" ") +
#   geom_boxplot(width=0.6) +
#   #scale_color_manual(values=cols) +
#   scale_y_continuous(breaks=seq(28, 42, 2)) 
# temp.plot <- temp.plot + theme(legend.position="none")
# temp.plot
# 
# ggplot_build(temp.plot)$data
# 
# ## Save the plot
# ggsave(filename="Tleaf_boxplot_V2.png", width=6, height=5, dpi=300)


#--Boxplot of Tleaf-Tair (temp differential) distributions
# Just select K34 and B2
# temp.dat.new <- temp.dat[temp.dat$site %in% c("K34", "B2-TF"),]

# For gs plots
temp.dat.new <- temp.dat[,c("site", "diff", "diff.gs.low", "diff.gs.high")]
temp.dat.new <- melt(temp.dat.new, id.vars=c("site"))
levels(temp.dat.new$variable) <- c("medium", "low", "high")

temp.plot <- ggplot(temp.dat.new, aes(x=variable, y=value, colour=site)) +
  ylab(bquote('Tleaf - Tair ('*degree*'C)')) + theme_bw(21) +
  xlab(" ") +
  geom_boxplot(width=0.6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust=0.5))
  #scale_color_manual(values=cols) #+
  #scale_y_continuous(breaks=seq(28, 42, 2))
temp.plot <- temp.plot + facet_wrap (~site) #+ theme(legend.position="none") 
temp.plot

# ggplot_build(temp.plot)$data

# ## Save the plot
# ggsave(filename="Tleaf_Tair_diff_boxplot_all_scenarios_V6.png", width=10, height=7, dpi=300)


#--Plot frequency distributions of environmetal parameters
par(mfrow=c(1,3))
# RH
plot(density(K34.dat$rh), col='blue', xlim=c(45,100), xlab='Relative humidity', main=' ', cex.lab=1.8, cex.axis=1.8, lwd=2)
lines(density(b2.dat$rh), col='red', lwd=2)

# PAR
plot(density(K34.dat$par), col='blue', xlim=c(100,1000), xlab='Incident short-wave radiation', main=' ', ylab=" ", cex.lab=1.8, cex.axis=1.8, lwd=2)
lines(density(b2.dat$par), col='red', lwd=2)

# Windspeed
plot(density(K34.dat$wind), col='blue', ylim=c(0,1.3), xlab='Windspeed', main=' ', ylab=" ", cex.lab=1.8, cex.axis=1.8, lwd=2)
lines(density(b2.dat$wind), col='red', lwd=2)

# 1200 x 500

