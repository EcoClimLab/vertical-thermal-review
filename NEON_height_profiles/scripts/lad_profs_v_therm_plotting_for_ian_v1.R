######################################################
# Purpose: Make LAD light profiles for Tansley Review
# Developed by: Marielle Smith, edited by Ian McGregor
# R version 3.6.2 - Edited January 2021
######################################################

#--Load required packages
library(ggplot2)
library(ggsci) # Needed for the scale_color_jco() colour scale

load("data/lad_n_light_profs_for_ian_v1.RData") 
# Loads 3 dataframes: LAD.all (LAD profiles), lgt.all (light transmission profs), prop.sun.all (proportion of sun leaves profiles)

#--LAD scaling factor 
lad.scale <- 5.5 # I have checked our LAI estimates against published values for SERC, but would be good to check more values

dfList <- list(LAD = LAD.all, lgt = lgt.all, sun = prop.sun.all)

plotsLAD <- list()
for(i in 1:3){
  df <- dfList[[i]]
  
  if(i==1){
    # df$val <- df$lad*lad.scale #not standardizing as of Feb. 2021
    df$val <- df$lad
  } else if(i==2){
    df <- df[df$ht>5, ]
    df$val <- df$lgt
  } else if(i==3){
    df <- df[df$ht>5, ]
    all.ones <- which(df$lad==1)
    not.these <- c(2,4,6,8,10,12)
    exclude <- all.ones[not.these]
    
    df <- df[-exclude,]
    df$val <- df$lad
  }
  
  #normalize the height. If not normalizing, comment this out and change the 
  ## y-values of the plot to be ht instead of ht.norm
  df$ht.norm <- df$ht/(df$max.ht)
  
  #create the plots and send to list
  plotsLAD[[i]] <- local({
    graph <- ggplot(df, aes(val, ht.norm, color=site)) +
      geom_path(size=1) + 
      ylim(0,1.75) + #if not normalizing height, ylim=c(0,80). Otherwise, c(0,2)
      labs(x="", y = "") +
      theme_bw() +
      # theme_classic(16)  + 
      scale_color_jco()
    
    if(i==1){ #LAD
      graph <- graph +
        xlab(expression(Modelled~leaf~area~density~"[m"^{2}~"m"^{-3}~"]")) +
        ylab("Height [m]")
    } else if(i==2){ # light profile
      graph <- graph +
        xlab('Proportion of incident light') +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    } else if(i==3){ #sun leaves
      graph <- graph +
        xlab("Calculated proportion of sun leaves") +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    } 
    graph <- graph + 
      theme(plot.title = element_text(size=16),
            legend.position="none")
  })
}

names(plotsLAD) <- names(dfList)



#################################################################################
## original script ####
# #>>---------------------------------------------------------------------------------------------------------------------------
# # PLOTTING
# #>>---------------------------------------------------------------------------------------------------------------------------
# 
# #--Load the data
# # setwd("~/Dropbox (MSU Forest-Atmo)/MSU Forest-Atmo Team Folder/Marielle/Collaborations/Vertical_thermal_review/Codes")
# 
# ########## PLOT THE PROFILES ###########
# 
# #--LAD scaling factor 
# lad.scale <- 5.5 # I have checked our LAI estimates against published values for SERC, but would be good to check more values 
# 
# #--LAD
# plot.dat <- LAD.all
# 
# lad.plot <- ggplot(plot.dat, aes(ht, lad*lad.scale, colour=site)) + 
#   geom_path(size=0.7) + coord_flip() +
#   xlim(0,60) + 
#   labs(x="Canopy height (m)", y = expression(Leaf~area~density~(m^{2}~m^{-3}))) +
#   theme_classic(16)  + scale_color_jco() 
# # lad.plot <- lad.plot + theme(legend.position = "none")
# lad.plot
# 
# #--Check the LAI values for the sites
# LAD.all <- LAD.all[LAD.all$ht <= 60,]
# sum((LAD.all[LAD.all$site=="SERC","lad"]*lad.scale), na.rm=TRUE)
# sum((LAD.all[LAD.all$site=="HARV","lad"]*lad.scale), na.rm=TRUE)
# sum((LAD.all[LAD.all$site=="PUUM","lad"]*lad.scale), na.rm=TRUE)
# sum((LAD.all[LAD.all$site=="OSBS","lad"]*lad.scale), na.rm=TRUE)
# sum((LAD.all[LAD.all$site=="SCBI","lad"]*lad.scale), na.rm=TRUE)
# sum((LAD.all[LAD.all$site=="WREF","lad"]*lad.scale), na.rm=TRUE)
# 
# 
# ggsave(filename="LAD_profs_panel_w_legend_v1.png", width=5, height=7, dpi=200)
# # ggsave(filename="LAD_profs_panel_no_legend_v1.png", width=4, height=7, dpi=200)
# 
# 
# #--LAD normalised by max height
# plot.dat <- LAD.all
# plot.dat$ht.norm <- plot.dat$ht/(plot.dat$max.ht)
# 
# lad.plot <- ggplot(plot.dat, aes(ht.norm, lad*lad.scale, colour=site)) + 
#   geom_path(size=0.7) + coord_flip() +
#   xlim(0,1) +
#   labs(x="Scaled canopy height", y = expression(Leaf~area~density~(m^{2}~m^{-3}))) +
#   theme_classic(16)  + scale_color_jco() 
# # lad.plot <- lad.plot + theme(legend.position = "none")
# lad.plot
# 
# 
# #--Light profiles
# plot.dat <- lgt.all[lgt.all$ht>5,]  # Exclude bottom 5 m
# 
# lgt.plot <- ggplot(plot.dat, aes(ht, lgt, colour=site)) + 
#   geom_path(size=0.7) + coord_flip() +
#   xlim(0,60) + 
#   labs(x="Canopy height (m)", y = 'Proportion incident light') +
#   theme_classic(16)  + scale_color_jco() 
# # lgt.plot <- lgt.plot + theme(legend.position = "none")
# lgt.plot
# 
# 
# 
# ##-Height normalised light profiles
# plot.dat <- lgt.all[lgt.all$ht>5,]  # Exclude bottom 5 m
# plot.dat$ht.norm <- plot.dat$ht/(plot.dat$max.ht)
# 
# lgt.plot <- ggplot(plot.dat, aes(ht.norm, lgt, colour=site)) + 
#   geom_path(size=0.7) + coord_flip() +
#   xlim(0,1) + 
#   labs(x="Scaled canopy height (m)", y = 'Proportion incident light') +
#   theme_classic(16)  + scale_color_jco() 
# lgt.plot <- lgt.plot + theme(legend.position = "none")
# lgt.plot
# 
# 
# #--Proportion of layer 1 (sun) LAD
# plot.dat <- prop.sun.all[prop.sun.all$ht>5,]  # Exclude bottom 5 m 
# 
# # Exclude second 1.0 prop from df
# all.ones <- which(plot.dat$lad==1)
# not.these <- c(2,4,6,8,10,12)
# exclude <- all.ones[not.these]
# 
# plot.dat <- plot.dat[-exclude,]
# 
# sun.prop.plot <- ggplot(plot.dat, aes(ht, lad, colour=site)) + 
#   geom_path(size=0.7) + 
#   coord_flip() +
#   xlim(0,60) + 
#   labs(x="Canopy height (m)", y = "Proportion sun leaves") +
#   theme_classic(16)  + scale_color_jco() 
# #sun.prop.plot <- sun.prop.plot + theme(legend.position = "none")
# sun.prop.plot
# 
# 
# #--Proportion of layer 1 (sun) LAD - height normalised
# plot.dat <- prop.sun.all[prop.sun.all$ht>=5,] 
# plot.dat$ht.norm <- plot.dat$ht/(plot.dat$max.ht)
# 
# # Exclude second 1.0 prop from df
# all.ones <- which(plot.dat$lad==1)
# not.these <- c(2,4,6,8,10,12)
# exclude <- all.ones[not.these]
# 
# plot.dat <- plot.dat[-exclude,]
# 
# sun.prop.plot <- ggplot(plot.dat, aes(ht.norm, lad, colour=site)) + 
#   geom_path(size=0.7) + 
#   coord_flip() +
#   xlim(0,1) + 
#   labs(x="Scaled canopy height (m)", y = "Proportion sun leaves") +
#   theme_classic(16)  + scale_color_jco() 
# # sun.prop.plot <- sun.prop.plot + theme(legend.position = "none")
# sun.prop.plot
# 
# 
