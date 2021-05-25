####################################
# Purpose: Create leaf energy balance figure using HARV neon data parameters for a typical overstory
# and understory, normal and drought scenarios
# Tleaves R package created by Chris Muir, 2019
# Developed by: Nidhi Vinod, contact Nidhi Vinod (nidrup@gmail.com)
# R version 3.5.3 --First created in October 2020
# Sept-August 2021
####################################
#install packages if needed
library(latex2exp)
library(magrittr)
library(tealeaves)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(moderndive)
library(ggpubr)
library(cowplot)
library(data.table)
library(formattable)
library(gridExtra)

####################################
#create data frames using HARV NEON data for environmental parameters. Leaf characteristic dimmension in m is measured from Sun and shade Red oak leaves
#all variables are constant except for the independent variable that represents minimum- maximum range
#normal represents HARV neon environmental parameters as observed
#Stomtal conductane(gs) values for typical condition obtained from Tleaves supplementary manual for range of minimum and maximum values
#drought condition is representative of gs values from Cavendar-Bares and Bazzaz, 2000 and typically low relative humidity, with other variables similar to normal condition
####################################

# Create a dataframe for overstory and understory, for scenarios: normal and drought
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

#apply Tleaves function (this is repeated througout)

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

# 1.Apply function for all 10 rows 2.convert celcius to K 3.Create a new column for Tleaf-Tair for overstory and understory 4. convert to C
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
# 1.Apply function for all 10 rows 2.convert celcius to K 3.Create a new column for Tleaf-Tair for overstory and understory
dr_sw_l$tleaf <- apply (dr_sw_l, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
# convert tair to celcius
dr_sw_l$tair <- dr_sw_l$tair-273.15
#Tleaf - Tair is assigned in a new column
dr_sw_l$dr_l_tleaf_tair<- dr_sw_l$tleaf - dr_sw_l$tair


dr_sw_u$tleaf <- apply (dr_sw_u, 1, function (data) get_tleaf (sw=data[1] , rh=data[2], wind=data[3], tair=data[4], pressure=data[5], leaf.size= data[6], gs=data[7])) 
dr_sw_u$tair <- dr_sw_u$tair-273.15
dr_sw_u$dr_u_tleaf_tair<- dr_sw_u$tleaf - dr_sw_u$tair

sw<- data.frame(l_tla = sw_l$l_tleaf_tair, u_tla = sw_u$u_tleaf_tair, dr_ltla = dr_sw_l$dr_l_tleaf_tair, dr_utla = dr_sw_u$dr_u_tleaf_tair, par = sw_l$par)

#plot, without transforming the figure
a1 <-ggplot(data = sw)+
  geom_smooth(aes(x = par, y = l_tla, colour = "blue"), method = lm, se = FALSE, linetype = 1)+
  geom_smooth(aes(x = par, y = u_tla, colour = "red"), method = lm, se = FALSE, linetype = 1)+
  geom_smooth(aes(x = par, y = dr_ltla, colour = " blue"), method = lm, linetype = 2, se = FALSE)+
  geom_smooth(aes(x = par, y = dr_utla, colour = "red"), method = lm, linetype = 2, se = FALSE)+
  
  scale_color_identity(name = "canopy position",
                       breaks = c("red", "blue"),
                       labels = c("overstory, dashed-lines represent drought", "understory, dashed-lines represent drought"),
                       guide = "legend")+
  ylab(TeX("$T_{Leaf}$ - $T_{air}$ (°C)"))+xlab(TeX("short wave radiation (swr, W/m$^{2}$)"))+
  theme_few()+theme(text = element_text(size = 14))+
  ylim(-5, 14)

a1 

#transforming data for standardization of values, for slope at the midpoint of variable x
#lower<-lm(sw$l_tla ~ sw$par)
#coef(lower)
#y<- 0.01280917*450 + 0.04960374
#y
#y= 7.542647
#sw$l_tla<-sw$l_tla - 5.81373

#upper<-lm(sw$u_tla ~ sw$par)
#coef(upper)
#y<- 0.005356825*450 + -0.451573590
#y
#y = 2.961966
#sw$u_tla<-sw$u_tla - 1.958998

#drought_lower<-lm(sw$dr_ltla ~ sw$par)
#coef(drought_lower)
#y<- 0.01504194*450 + -2.06317804
#y
#y = 7.214889
#sw$dr_ltla<- sw$dr_ltla - 4.705695

#drought_upper<-lm(sw$dr_utla ~ sw$par)
#coef(drought_upper)
#y<- 0.006067448*450 + -2.669403031
#y
#sw$dr_utla<- sw$dr_utla - 0.06094857

# plot with tranformed data
#a<-ggplot(data = sw)+
#  geom_smooth(aes(x = par, y = l_tla, colour = "blue"), method = lm, se = FALSE, linetype = 1)+
#  geom_smooth(aes(x = par, y = u_tla, colour = "red"), method = lm, se = FALSE, linetype = 1)+
#  geom_smooth(aes(x = par, y = dr_ltla, colour = " blue"), method = lm, linetype = 2, se = FALSE)+
#  geom_smooth(aes(x = par, y = dr_utla, colour = "red"), method = lm, linetype = 2, se = FALSE)+
  
#  scale_color_identity(name = "canopy position",
#                       breaks = c("red", "blue"),
#                       labels = c("overstory", "understory"),
#                       guide = "legend")+
#  scale_linetype_manual(name = "canopy position",
#                        labels = c("overstory", "understory", "drought overstory", "drought understory"),
#                        values = c( 1, 1, 2, 2), guide = "legend")+
#  ylab("TLeaf - Tair*")+xlab("short wave radiation")+ ylim(-3.5, 3.5)
  
#a
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

#plot ws, without transformation

b1<-ggplot(ws)+
  geom_smooth(aes(x = wind, y = l_tla),  method = lm, se = FALSE, color = "blue")+
  geom_smooth(aes(x = wind, y = u_tla),  method = lm, color = "red", se = FALSE)+
  geom_smooth(aes(x = wind, y = dr_ltla),  method = lm, color = "blue", se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = wind, y = dr_utla),  method = lm, color = "red", se = FALSE, linetype = "dashed")+
  ylab(TeX("$T_{Leaf}$ - $T_{air}$ ($°C$)"))+xlab(TeX("windspeed (ws, $m/s$)"))+
  theme_few()+theme(text = element_text(size = 14))+ ylim(-5, 14) 

b1

#transforming

#lower_ws<-lm(ws$l_tla ~ ws$wind)
#coef(lower_ws)
#y<- -0.3609749*1.44 + 1.5134131
#y
#y = 1.314345
#ws$l_tla<-ws$l_tla - 0.9936092

#upper_ws<-lm(ws$u_tla ~ ws$wind)
#coef(upper_ws)
#y<- -1.455528*1.44 + 7.624159
#y
#ws$u_tla<-ws$u_tla - 5.528199

#dr_lower<-lm(ws$dr_ltla ~ ws$wind)
#coef(dr_lower)
#y<- -0.05042344*1.44 + -0.43273604
#y
#ws$dr_ltla<-ws$dr_ltla - -0.5053458

#dr_upper<-lm(ws$dr_utla ~ ws$wind)
#coef(dr_upper)
#y<- -1.359072*1.44 + 5.734046
#y
#ws$dr_utla<-ws$dr_utla - 3.776982

#plot, transformed

#b<-ggplot(ws)+
  #geom_smooth(aes(x = wind, y = l_tla),  method = lm, se = FALSE, color = "blue")+
  #geom_smooth(aes(x = wind, y = u_tla),  method = lm, color = "red", se = FALSE)+
  #geom_smooth(aes(x = wind, y = dr_ltla),  method = lm, color = "blue", se = FALSE, linetype = "dashed")+
  #geom_smooth(aes(x = wind, y = dr_utla),  method = lm, color = "red", se = FALSE, linetype = "dashed")+
  #ylab("TLeaf - Tair*")+xlab("windspeed (ms-1)")+scale_y_continuous(breaks = c(-2, 0, 2))
#b


##########################################################################################################
#3. Stomtal conductance(GS)

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

#plot gs, without tranformation
c1<-ggplot(gs)+
  geom_smooth(aes(x = gs, y = l_tla),  method = lm, se = FALSE, color = "blue")+
  geom_smooth(aes(x = gs, y = u_tla),  method = lm, color = "red", se = FALSE)+
  geom_smooth(aes(x = gs, y = dr_ltla),  method = lm, color = "blue", se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = gs, y = dr_utla),  method = lm, color = "red", se = FALSE, linetype = "dashed")+
  ylab(TeX("$T_{Leaf}$ - $T_{air}$ ($°C$)"))+xlab(TeX("stomatal conductance ($g_{s}$, $\\mu mol/m^2/s/Pa$)"))+
  theme_few()+theme(text = element_text(size = 14))+ylim(-5, 14)
c1


#with transformation
#lower_gs<-lm(gs$l_tla ~ gs$gs)
#coef(lower_gs)
#y<- -0.195501*2.25 + 1.786546
#y
#y= 1.748807
#gs$l_tla<-gs$l_tla - 1.346669

#upper_gs<-lm(gs$u_tla ~ gs$gs)
#coef(upper_gs)
#y<- -0.5589508*2.25 + 6.3538099
#y
#gs$u_tla<-gs$u_tla - 5.096171


#dr_gslower<- lm(gs$dr_ltla ~ gs$gs)
#coef(dr_gslower)
#y<- -1.0167057*2.25 + 0.7295698
#y
#y= 0.5201044
#gs$dr_ltla<-gs$dr_ltla - -1.558018


#dr_gsupper<- lm(gs$dr_utla ~ gs$gs)
#coef(dr_gsupper)
#y<- -1.216157*2.25 + 5.938667
#y
#y= 0.5201044
#gs$dr_utla<-gs$dr_utla - 3.202314


#c<-ggplot(gs)+
#  geom_smooth(aes(x = gs, y = l_tla),  method = lm, se = FALSE, color = "blue")+
#  geom_smooth(aes(x = gs, y = u_tla),  method = lm, color = "red", se = FALSE)+
#  geom_smooth(aes(x = gs, y = dr_ltla),  method = lm, color = "blue", se = FALSE, linetype = "dashed")+
#  geom_smooth(aes(x = gs, y = dr_utla),  method = lm, color = "red", se = FALSE, linetype = "dashed")+
#  ylab("TLeaf - Tair*")+xlab("stomal conductance")+ylim(-3, 3)
#c

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

#without tansforming
d1<-ggplot(ls)+
  geom_smooth(aes(x = ls, y = l_tla),  method = lm, se = FALSE, color = "blue")+
  geom_smooth(aes(x = ls, y = u_tla),  method = lm, color = "red", se = FALSE)+
  geom_smooth(aes(x = ls, y = dr_ltla),  method = lm, color = "blue", se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = ls, y = dr_utla),  method = lm, color = "red", se = FALSE, linetype = "dashed")+
  ylab(TeX("T_{Leaf}$ - $T_{air}$ ($°C$)"))+xlab("leaf size (ls, m)")+
  theme_few()+theme(text = element_text(size = 14))+ylim(-5, 14) 
d1


#with transforming                 
#lower_ls<- lm(ls$l_tla ~ ls$ls)
#coef(lower_ls)
#y<- 6.319849*0.055 + 0.720737
#y
#y = 8.941457
#ls$l_tla<- ls$l_tla - 1.068329

#upper_ls<- lm(ls$u_tla ~ ls$ls)
#coef(upper_ls)
#y<- 23.77844*0.055 + 2.72652
#y
#y = 5.627328
#ls$u_tla = ls$u_tla - 4.034334

#dr_lower<-lm(ls$dr_ltla ~ ls$ls)
#coef(dr_lower)
#y<- -0.8393397*0.055 + -0.5468557
#y
#ls$dr_ltla<- ls$dr_ltla - -0.5930194

#dr_upper<-lm(ls$dr_utla ~ ls$ls)
#coef(dr_upper)
#y<- 17.243851*0.055 + 1.555269
#y
#ls$dr_utla<- ls$dr_utla - 2.503681

#d<-ggplot(ls)+
#  geom_smooth(aes(x = ls, y = l_tla),  method = lm, se = FALSE, color = "blue")+
#  geom_smooth(aes(x = ls, y = u_tla),  method = lm, color = "red", se = FALSE)+
#  geom_smooth(aes(x = ls, y = dr_ltla),  method = lm, color = "blue", se = FALSE, linetype = "dashed")+
#  geom_smooth(aes(x = ls, y = dr_utla),  method = lm, color = "red", se = FALSE, linetype = "dashed")+
#  ylab("TLeaf - Tair*")+xlab("leaf size")+ylim(-3,3)
#d

###################################################################################################################
# 5. Relative Humidity, (RH)

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

# RH, drought

dr_rhl<- data.frame(par = 305.54, rh = c(0.00, 0.15, 0.25, 0.35, 0.45, 0.65, 0.75, 0.85, 0.95, 1.00), 
                    wind = 0.24, tair = 296.1, pressure = 99, 
                    leaf.size = 0.10, 
                    gs = 0.1)
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

# create a dataframe for all Tleaf-Tair values for RH
rh<-data.frame(l_tla = rh_l$rh_tla, u_tla = rh_u$rh_tla, dr_ltla = dr_rhl$l_tla, dr_utla = dr_rhu$u_tla, rh = rh_l$rh)

#plot rh, without transformation
e1<-ggplot(rh)+
  geom_smooth(aes(x = rh, y = l_tla),  method = lm, se = FALSE, color = "blue")+
  geom_smooth(aes(x = rh, y = u_tla),  method = lm, color = "red", se = FALSE)+
  geom_smooth(aes(x = rh, y = dr_ltla),  method = lm, color = "blue", se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = rh, y = dr_utla),  method = lm, color = "red", se = FALSE, linetype = "dashed")+
  ylab(TeX("$T_{Leaf}$-$T_{air}$ (°C)"))+ xlab("relative humidity (rh, %)")+
  theme_few()+theme(text = element_text(size = 14))+ ylim (-5, 14) 
e1

#constructing a table for the plot


table<- data.frame(biophysical = c( "swr", "ws", "rh", "ls", "gs", "tair"),
                   normalno = c("871", 2.88, 0.91, 0.04, "4.0", "298"),
                   normal_u = c("102", 0.24, 0.97, 0.10, "2.0", "296"),
                   drought_o = c("1151", 2.88, 0.46, 0.04, "1.0", "298"),
                   normalno = c("153", 0.24, 0.49, 0.010, "0.1", "296"))

colnames(table) <- c("variable",
                 "ON", "UN", "OD", "UD")

main.title <- "Biophysical Constants"

table1<-ggtexttable(table, rows = NULL, theme = ttheme("mBlue"))

table1<-table1 %>%
  tab_add_title(text = main.title, face = "bold")

table1

#without transformation

figure1<-ggarrange(a1, b1, e1, d1, c1, table1, ncol=3, nrow =2, common.legend = TRUE, align = c("v"), legend = "top", 
                   labels = c("(a)", "(b)", "(c)", "(d)", "(e)"))
figure1


#transformed data
#figure<-ggarrange(a, b, d, c, ncol=2, nrow =2, common.legend = TRUE, align = c("v"), legend = "top")
#figure

#annotate_figure(
#  figure,
#  bottom = text_grob("constants from harvard NEON data 
#                    \n overstory- sw: 871, tair: 298.1 [k], ws: 2.88, gs: 4.0, leaf size: 0.04m, rh: 0.91, 
#                     \n understory- sw: 102, tair: 296.1 [k], ws: 0.24, gs: 2.0, leaf size: 0.10m, rh: 0.97
#                     \n drought overstory- sw: 871, tair: 298.1 [k], ws: 2.88, gs: 2.5, leaf size: 0.04m, rh: 0.31, 
#                     \n drought understory- sw: 102, tair: 296.1 [k], ws: 0.24, gs: 1.0, leaf size: 0.10m, rh: 0.37 
#                     \n *TLeaf - Tair standardized at the midpoint of x variable", 
#                     lineheight = 0.7, color = "black", size = 8))
  
#write csv for all data of this figure





