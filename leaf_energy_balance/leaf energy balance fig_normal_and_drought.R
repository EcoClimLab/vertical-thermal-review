####################################
# Purpose: Create leaf energy balance figure using HARV neon data parameters for a typical overstory
# and understory, normal and drought scenarios
# Tleaves R package created by Chris Muir, 2019
# Developed by: Nidhi Vinod, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 --First created in October 2020, updated on 5/28/2021
# Oct-August 2021
####################################
#install packages if needed
library(latex2exp)
library(magrittr)
library(tealeaves)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(RColorBrewer)
####################################
#dataframe for biophysical parameters from NEON HARV data
#normal scenario represents HARV NEON environmental parameters as observed. 
#drought scenario represents similar HARV NEON parameters, with max PAR for overstory, and 50% increase in normal PAR for understory, and 50% decrease in normal relative humidity for both positions.
#typical normal stomtal conductane(gs) values are referred from Tleaves supplementary manual for range of minimum and maximum values and obtained from Cavendar-Bares and Bazzaz, 2000.
#drought condition ~gs values obtained from Cavendar-Bares and Bazzaz, 2000.  
#respective leaf sizes (m) are measured from sun and shade red oak leaves 
#all variables are constant except for the y-axis variables that represent minimum- maximum range
####################################

# create a dataframe for overstory and understory, for scenarios: normal and drought
# 1. Short wave radiation, normal
#understory
sw_l<- data.frame(par = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200), 
                  rh = 0.97, wind = 0.24, tair = 296.1, pressure = 99, leaf.size = 0.10, 
                  gs = 2.0)
#overstory
sw_u<- data.frame(par = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200), 
                  rh = 0.91, wind = 2.88, tair = 298.1, pressure = 99, leaf.size = 0.04, 
                  gs = 4.0)

sw_l$par <- sw_l$par*0.5#sw
sw_u$par <- sw_u$par*0.5#sw

#apply Tleaves function

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
  # 3 # Medium amount, according to tealeaves creator, found in Tleaves supplementary material
  
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

# 1.Apply function for all 10 rows 2.convert tair K to celcius 3.Create a new column for Tleaf-Tair for overstory and understory(repeated throughout)
sw_l$tleaf <- apply (sw_l, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
sw_l$tair <- sw_l$tair-273.15
sw_l$l_tleaf_tair<- sw_l$tleaf - sw_l$tair

sw_u$tleaf <- apply (sw_u, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
sw_u$tair <- sw_u$tair-273.15
sw_u$u_tleaf_tair<- sw_u$tleaf - sw_u$tair


# 1. Shortwave radiation, drought
dr_sw_l<- data.frame(par = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200), 
                  rh = 0.49, wind = 0.24, tair = 296.1, pressure = 99, leaf.size = 0.10, 
                  gs = 0.1)
dr_sw_u<- data.frame(par = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200), 
                  rh = 0.46, wind = 2.88, tair = 298.1, pressure = 99, leaf.size = 0.04, 
                  gs = 1.0)
dr_sw_l$par <- dr_sw_l$par*0.5#sw
dr_sw_u$par <- dr_sw_u$par*0.5#sw

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

#1. Short wave radiation, drought scenario
# 1.Apply function for all 10 rows 2.convert tair K to celcius 3.Create a new column for Tleaf-Tair for overstory and understory (repeated throughout)
dr_sw_l$tleaf <- apply (dr_sw_l, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
# convert tair to celcius
dr_sw_l$tair <- dr_sw_l$tair-273.15
#Tleaf - Tair is assigned in a new column
dr_sw_l$dr_l_tleaf_tair<- dr_sw_l$tleaf - dr_sw_l$tair


dr_sw_u$tleaf <- apply (dr_sw_u, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_sw_u$tair <- dr_sw_u$tair-273.15
dr_sw_u$dr_u_tleaf_tair<- dr_sw_u$tleaf - dr_sw_u$tair

sw<- data.frame(l_tla = sw_l$l_tleaf_tair, u_tla = sw_u$u_tleaf_tair, dr_ltla = dr_sw_l$dr_l_tleaf_tair, dr_utla = dr_sw_u$dr_u_tleaf_tair, par = sw_l$par)

l_tla<-data.frame(par = sw$par, tleaf_tair = sw$l_tla, canopy_position = "understory normal")
u_tla<-data.frame(par = sw$par, tleaf_tair = sw$u_tla, canopy_position = "overstory normal")
dr_ltla<-data.frame(par = sw$par, tleaf_tair = sw$dr_ltla, canopy_position = "understory drought")
dr_utla<-data.frame(par = sw$par, tleaf_tair = sw$dr_utla, canopy_position = "overstory drought") 

sw<- data.frame(rbind(u_tla, l_tla, dr_utla, dr_ltla))

#plot sw data

a1<-ggplot(data = sw)+
  geom_smooth(aes(x = par, y = tleaf_tair, color = canopy_position, linetype = canopy_position), 
              method = lm, se = FALSE)+
  ylab(TeX("$T_{Leaf}$ - $T_{air}$ (°C)"))+xlab(TeX("short wave radiation (swr, W/m$^{2}$)"))+
  theme_few()+theme(text = element_text(size = 14))+ylim(-5, 14)+
  scale_colour_manual(values=c("#008837","#7b3294","#008837","#7b3294"), name = "canopy position") + 
  scale_linetype_manual(values = c(1,1,2,2), name = "canopy position")+
  theme( 
    legend.key=element_blank(),
    legend.position = c(0, 1.05),
    legend.justification = c("left", "top"), 
    legend.text = element_text(size=10),
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.key.size = unit(0.4, 'cm'),
    legend.title = element_blank(),
    legend.background = element_blank(),
    #legend.box.background = element_rect(color = "darkgrey"))
    legend.box.background = element_blank())
a1

############################################################################################################
# 2. WINDSPEED

#Windspeed, normal

#understory
ws_l<- data.frame(par = 203.69, 
                  rh = 0.97, wind = c(0, 0.24, 0.42, 0.88, 1.07, 1.42, 1.88, 2.07, 2.42, 2.88, 3.88), 
                  tair = 296.1, pressure = 99, 
                  leaf.size = 0.10, 
                  gs = 2.0)
#overstory
ws_u<- data.frame(par = 1741.65, 
                  rh = 0.91, wind = c(0, 0.24, 0.42, 0.88, 1.07, 1.42, 1.88, 2.07, 2.42, 2.88, 3.88), 
                  tair = 298.1, pressure = 99, 
                  leaf.size = 0.04, 
                  gs = 4.0)

ws_l$par <- ws_l$par*0.5#par to sw
ws_u$par <- ws_u$par*0.5#par to sw

ws_l$tleaf <- apply (ws_l, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
ws_l$tair <- ws_l$tair -273.15
ws_l$l_tla<- ws_l$tleaf - ws_l$tair

ws_u$tleaf <- apply (ws_u, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
ws_u$tair <- ws_u$tair -273.15
ws_u$u_tla<- ws_u$tleaf - ws_u$tair


#2. Windspeed, drought
#understory
dr_wsl<- data.frame(par = 305.54, rh = 0.49, 
                         wind = c(0, 0.24, 0.42, 0.88, 1.07, 1.42, 1.88, 2.07, 2.42, 2.88, 3.88), tair = 296.1, pressure = 99, leaf.size = 0.10, 
                         gs = 0.1)
#overstory
dr_wsu<- data.frame(par = 2302.15, 
                         rh = 0.46, wind = c(0, 0.24, 0.42, 0.88, 1.07, 1.42, 1.88, 2.07, 2.42, 2.88, 3.88), tair = 298.1, pressure = 99, leaf.size = 0.04, 
                         gs = 1.0)

dr_wsl$par <- dr_wsl$par*0.5#par to sw
dr_wsu$par <- dr_wsu$par*0.5#par to sw


dr_wsl$tleaf <- apply (dr_wsl, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_wsl$tair <- dr_wsl$tair-273.15
dr_wsl$l_tla<- dr_wsl$tleaf - dr_wsl$tair

dr_wsu$tleaf <- apply (dr_wsu, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_wsu$tair <- dr_wsu$tair-273.15
dr_wsu$u_tla<- dr_wsu$tleaf - dr_wsu$tair

# create Tleaf- tairs of normal and drought into one common dataframe
ws<- data.frame(l_tla = ws_l$l_tla,  u_tla = ws_u$u_tla, dr_ltla = dr_wsl$l_tla, dr_utla = dr_wsu$u_tla, wind = ws_l$wind)

#plot ws

b1<-ggplot(ws)+
  geom_smooth(aes(x = wind, y = l_tla),  method = lm, se = FALSE, color = "#7b3294")+
  geom_smooth(aes(x = wind, y = u_tla),  method = lm, color = "#008837", se = FALSE)+
  geom_smooth(aes(x = wind, y = dr_ltla),  method = lm, color = "#7b3294", se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = wind, y = dr_utla),  method = lm, color = "#008837", se = FALSE, linetype = "dashed")+
  ylab(TeX("$T_{Leaf}$ - $T_{air}$ ($°C$)"))+xlab(TeX("windspeed (ws, $m/s$)"))+
  theme_few()+theme(text = element_text(size = 14))+ ylim(-5, 14) 

b1



##########################################################################################################
#3. Stomtal conductance(gs)

# 3. stomatal conductance, normal
#understory 
gs_l<- data.frame(par = 203.69, rh = 0.97, 
                  wind = 0.24, tair = 296.1, pressure = 99, leaf.size = 0.10, 
                  gs = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5))
#overstory
gs_u<- data.frame(par = 1741.65, 
                  rh = 0.91, wind = 2.88, tair = 298.1, pressure = 99, leaf.size = 0.04, 
                  gs = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5))

gs_l$par <- gs_l$par*0.5#par to sw
gs_u$par <- gs_u$par*0.5#par to sw


gs_l$tleaf <- apply (gs_l, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
gs_l$tair <- gs_l$tair-273.15
gs_l$l_tleaf_tair<- gs_l$tleaf - gs_l$tair

gs_u$tleaf <- apply (gs_u, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
gs_u$tair <- gs_u$tair -273.15
gs_u$u_tleaf_tair<- gs_u$tleaf - gs_u$tair


#3. stomatal conductance, drought
#understory
dr_gsl<- data.frame(par = 305.54, rh = 0.49, 
                  wind = 0.24, tair = 296.1, pressure = 99, leaf.size = 0.10, 
                  gs = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5))
#overstory
dr_gsu<- data.frame(par = 2302.15, rh = 0.46, wind = 2.88, tair = 298.1, pressure = 99, 
                  leaf.size = 0.04, gs = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5))
dr_gsl$par <- dr_gsl$par*0.5#par to sw
dr_gsu$par <- dr_gsu$par*0.5#pa to sw

dr_gsl$tleaf <- apply (dr_gsl, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_gsl$tair <- dr_gsl$tair-273.15
dr_gsl$l_tla<- dr_gsl$tleaf - dr_gsl$tair

dr_gsu$tleaf <- apply (dr_gsu, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_gsu$tair <- dr_gsu$tair-273.15
dr_gsu$u_tla<- dr_gsu$tleaf - dr_gsu$tair

#create a data frame for Tleaf-Tair, for overstory and understory, in normal and drought scenario
gs<-data.frame(l_tla = gs_l$l_tleaf_tair, u_tla = gs_u$u_tleaf_tair, dr_ltla = dr_gsl$l_tla, dr_utla = dr_gsu$u_tla, gs = gs_l$gs)

#plot gs
c1<-ggplot(gs)+
  geom_smooth(aes(x = gs, y = l_tla),  method = lm, se = FALSE, color = "#7b3294")+
  geom_smooth(aes(x = gs, y = u_tla),  method = lm, color = "#008837", se = FALSE)+
  geom_smooth(aes(x = gs, y = dr_ltla),  method = lm, color = "#7b3294", se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = gs, y = dr_utla),  method = lm, color = "#008837", se = FALSE, linetype = "dashed")+
  ylab(TeX("$T_{Leaf}$ - $T_{air}$ (°C)"))+xlab(TeX("stomatal conductance ($g_{s}$, $\\mu mol/m^2/s/Pa$)"))+
  theme_few()+theme(text = element_text(size = 14))+ylim(-5, 14)
c1

#########################################################################################################
# 4.LEAF CHARACTERISTIC DIMMENSION (LEAF SIZE) 

# 4. leaf size, normal
#understory
ls_l<- data.frame(par = 203.69, rh = 0.97, 
                        wind = 0.24, tair = 296.1, pressure = 99, 
                        leaf.size = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12), 
                        gs = 2.0)
ls_u<- data.frame(par = 1741.65, 
                        rh = 0.91, wind = 2.88, tair = 298.1, pressure = 99, 
                        leaf.size = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12), 
                        gs = 4.0)
ls_l$par <- ls_l$par*0.5#sw
ls_u$par <- ls_u$par*0.5#sw


ls_l$tleaf <- apply (ls_l, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
ls_l$tair <- ls_l$tair-273.15
ls_l$ls_tla<- ls_l$tleaf - ls_l$tair

ls_u$tleaf <- apply (ls_u, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
ls_u$tair <- ls_u$tair-273.15
ls_u$ls_tla<- ls_u$tleaf - ls_u$tair


#4. leaf size, drought
#overstory
dr_lsl<- data.frame(par = 305.54, rh = 0.49, 
                        wind = 0.24, tair = 296.1, pressure = 99, 
                        leaf.size = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12), 
                        gs = 0.1)
dr_lsu<- data.frame(par = 2302.15, 
                        rh = 0.46, wind = 2.88, tair = 298.1, pressure = 99, 
                        leaf.size = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12), 
                        gs = 1.0)
dr_lsl$par <- dr_lsl$par*0.5#sw
dr_lsu$par <- dr_lsu$par*0.5#sw


dr_lsl$tleaf <- apply (dr_lsl, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_lsl$tair <- dr_lsl$tair-273.15
dr_lsl$l_tla<- dr_lsl$tleaf - dr_lsl$tair

dr_lsu$tleaf <- apply (dr_lsu, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_lsu$tair <- dr_lsu$tair -273.15
dr_lsu$u_tla<- dr_lsu$tleaf - dr_lsu$tair

ls<-data.frame(l_tla = ls_l$ls_tla, u_tla = ls_u$ls_tla, dr_ltla = dr_lsl$l_tla, dr_utla = dr_lsu$u_tla, ls = ls_l$leaf.size)

#plot ls
d1<-ggplot(ls)+
  geom_smooth(aes(x = ls, y = l_tla),  method = lm, se = FALSE, color = "#7b3294")+
  geom_smooth(aes(x = ls, y = u_tla),  method = lm, color = "#008837", se = FALSE)+
  geom_smooth(aes(x = ls, y = dr_ltla),  method = lm, color = "#7b3294", se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = ls, y = dr_utla),  method = lm, color = "#008837", se = FALSE, linetype = "dashed")+
  ylab(TeX("$T_{Leaf}$ - $T_{air}$ (°C)"))+xlab("leaf size (ls, m)")+
  theme_few()+theme(text = element_text(size = 14))+ylim(-5, 14) 
d1

###################################################################################################################
# 5. Relative Humidity, (rh)

#5. RH, normal
# understory
rh_l<- data.frame(par = 203.69, rh = c(0.00, 0.15, 0.25, 0.35, 0.45, 0.65, 0.75, 0.85, 0.95, 1.00), 
                  wind = 0.24, tair = 296.1, pressure = 99, 
                  leaf.size =  0.10, 
                  gs = 2.0)
rh_u<- data.frame(par = 1741.65, 
                  rh = c(0.00, 0.15, 0.25, 0.35, 0.45, 0.65, 0.75, 0.85, 0.95, 1.00), wind = 2.88, tair = 298.1, pressure = 99, 
                  leaf.size = 0.04, 
                  gs = 4.0)


rh_l$par <- rh_l$par*0.5#par to sw
rh_u$par <- rh_u$par*0.5#par to sw


rh_l$tleaf <- apply (rh_l, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
rh_l$tair <- rh_l$tair-273.15
rh_l$rh_tla<- rh_l$tleaf - rh_l$tair

rh_u$tleaf <- apply (rh_u, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
rh_u$tair <- rh_u$tair-273.15
rh_u$rh_tla<- rh_u$tleaf - rh_u$tair

# rh, drought
#understory
dr_rhl<- data.frame(par = 305.54, rh = c(0.00, 0.15, 0.25, 0.35, 0.45, 0.65, 0.75, 0.85, 0.95, 1.00), 
                    wind = 0.24, tair = 296.1, pressure = 99, 
                    leaf.size = 0.10, 
                    gs = 0.1)
#overstory
dr_rhu<- data.frame(par = 2302.15, 
                    rh = c(0.00, 0.15, 0.25, 0.35, 0.45, 0.65, 0.75, 0.85, 0.95, 1.00), wind = 2.88, tair = 298.1, pressure = 99, 
                    leaf.size = 0.04,  
                    gs = 1.0)
dr_rhl$par <- dr_rhl$par*0.5#sw
dr_rhu$par <- dr_rhu$par*0.5#sw


dr_rhl$tleaf <- apply (dr_rhl, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_rhl$tair <- dr_rhl$tair-273.15
dr_rhl$l_tla<- dr_rhl$tleaf - dr_rhl$tair

dr_rhu$tleaf <- apply (dr_rhu, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_rhu$tair <- dr_rhu$tair-273.15
dr_rhu$u_tla<- dr_rhu$tleaf - dr_rhu$tair

# create a dataframe for all Tleaf-Tair values for rh
rh<-data.frame(l_tla = rh_l$rh_tla, u_tla = rh_u$rh_tla, dr_ltla = dr_rhl$l_tla, dr_utla = dr_rhu$u_tla, rh = rh_l$rh)

#plot rh
e1<-ggplot(rh)+
  geom_smooth(aes(x = rh, y = l_tla),  method = lm, se = FALSE, color = "#7b3294")+
  geom_smooth(aes(x = rh, y = u_tla),  method = lm, color = "#008837", se = FALSE)+
  geom_smooth(aes(x = rh, y = dr_ltla),  method = lm, color = "#7b3294", se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = rh, y = dr_utla),  method = lm, color = "#008837", se = FALSE, linetype = "dashed")+
  ylab(TeX("$T_{Leaf}$ - $T_{air}$ (°C)"))+ xlab("relative humidity (rh, %)")+
  theme_few()+theme(text = element_text(size = 14))+ ylim (-5, 14) 
e1

#constructing a biophyscial constants table for the plot


table<- data.frame(biophysical = c( "swr", "ws", "rh", "ls", "gs", "tair"),
                   normalno = c("871", 2.88, 0.91, 0.04, "4.0", "298"),
                   normal_u = c("102", 0.24, 0.97, 0.10, "2.0", "296"),
                   drought_o = c("1151", 2.88, 0.46, 0.04, "1.0", "298"),
                   normalno = c("153", 0.24, 0.49, 0.10, "0.1", "296"))

colnames(table) <- c("variable",
                 "overstory\nnormal", "understory\nnormal", "overstory\ndrought", "understory\ndrought")

main.title <- "Biophysical Constants"

table1<-ggtexttable(table, rows = NULL, theme = ttheme("mBlue", ))


table1<-table1 %>%
  tab_add_title(text = main.title, face = "bold")

table1

#plotting multiple plots together 

figure1<-ggarrange(a1, b1, e1, d1, c1, table1, ncol=3, nrow =2, align = c('hv'),
                   labels = c("(a)", "(b)", "(c)", "(d)", "(e)"))
figure1

