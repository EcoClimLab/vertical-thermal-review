######################################################
# Purpose: Create vertical profiles of climate variables for ForestGEO 
##          plot using NEON tower data. 
##          **Main figure** = One site per forest type + LAD profiles
##          **SI figure** = Multiple sites per forest type
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 - First created August 2020
## Updated May 2021
######################################################
#install needed packages
library(data.table)
library(readxl)
library(ggplot2)
library(ggpubr)
library(patchwork)

#
#1. Download and save neon data ####
# this is filtered to only the columns needed for plots
# (mean measurements per each 30 min observation) in order to not have massive files.
# source("scripts/get_neon_data.R")
# sites <- c("HARV", "OSBS", "PUUM", "SCBI", "SERC", "WREF")
# sites <- c("DELA", "GUAN", "JERC", "LENO", "CLBJ", "SJER", "YELL",
#               "GRSM", "ORNL", "TREE", "UKFS", "SOAP", "TALL", "TEAK",
#               "BART", "BONA", "DEJU")
# 
# library(parallel)
# cl <- makeCluster(detectCores()-1)
# clusterEvalQ(cl, library(data.table))
# clusterEvalQ(cl, library(neonUtilities))
# clusterExport(cl, list("cl", "sites", "get_NEON", "dp", "date"))
# 
# parLapply(cl, X=1:length(sites), function(X){
#   full_data <- get_NEON(x=sites[X])
#   save(full_data, file=paste0("data/", sites[X], "test.Rdata"))
# })
# stopCluster(cl)

#1a. Get NEON coordinates ####
# library(data.table)
# meta <- fread("data/site_data/NEON_Field_Site_Metadata_20201204.csv")
# meta <- meta[field_site_id %in% c("BART","BONA","CLBJ","DEJU","DELA","GRSM","GUAN","HARV","JERC","LENO","MLBS","ORNL","OSBS", "PUUM", "SCBI","SERC","SJER","SOAP","STEI","TALL","TEAK","TREE","UKFS","UNDE","WREF","YELL"), .(field_site_id, field_latitude, field_longitude, field_utm_easting, field_utm_northing, field_utm_zone)]
# 
# fwrite(meta, "data/site_data/neon_coordinates.csv")

#2. Organize data for plotting ####

data <- fread("forested_NEON_sites.csv")
data <- data[include==1, ]
sites <- data[,site]

dp <- data.table(data = c("2DWSD", "RH", "SAAT", "IRBT", "PARPAR"),
                 id = c("DP1.00001.001", "DP1.00098.001",
                        "DP1.00002.001", "DP1.00005.001",
                        "DP1.00024.001"),
                 name = c("windSpeedMean", "RHMean",
                          "tempSingleMean", "bioTempMean",
                          "PARMean"),
                 xlabs = c("Windspeed [m/s]", "RH [%]",
                           expression(paste("Mean T"["air"], " [",degree,"C]")), 
                           expression(paste("Mean T"["bio"], " [",degree,"C]")),
                           "PAR"),
                 stat = c("max", "min", "max", "max", "max"))

#############################################################################
## Make plots for manuscript ####
#only focus on 6 core sites
sites <- c("HARV", "OSBS", "PUUM", "SCBI", "SERC", "WREF")
clrs <- c("#3399FF", "gold", "grey", "red", "#66CCFF", "#003399")

#bring in normalized height (from Marielle)
meta <- read_excel("data/site_data/TIS site metadata_20190403_forNOAA.xlsx", sheet=1)
meta <- data.table(meta)
meta <- meta[SITE %in% sites,
             ][,`:=` (DistZaxsCnpy = as.numeric(DistZaxsCnpy),
                    maxCanHeightMarielle = c(32, 43, 39, 21, 53))]
normHeightData <- 
  data.table(site=c("HARV", "OSBS", "PUUM", "SCBI", "SERC", "WREF"),
             normH=c(32, 21, 23, 43, 39, 53))
# HARV=32, OSBS: 21, PUUM: 23, SCBI: 43, SERC: 39, WREF: 53

plots <- list()
for(i in 1:length(dp[,name])){
  alldata <- NULL
  allTopHeight <- c()
  allNormHeight <- c()
  for(j in 1:length(sites)){
    load(paste0("data/neon_rdata/", sites[j], "test.Rdata"))
    
    #get max vertical height (for use in plots)
    hei <- sapply(full_data, function(x){unique(x[["verticalPosition"]])})
    topHeight <- max(unlist(hei, use.names = FALSE))
    normHeight <- normHeightData[site == sites[j], ]
    
    var <- as.data.table(full_data[names(full_data) == dp[,name][i]])
    colnames(var) <- 
      gsub(paste0(dp[,name][i], "."), "", 
           colnames(var))
    
    #get daily min/max by vertical position
    test <- var[, day := as.Date(startDateTime)
                ][, .(day_max = max(get(dp[,name][i]), na.rm=TRUE),
                      day_min = min(get(dp[,name][i]), na.rm=TRUE)),
                  by = .(day, zOffset)
                  ][, `:=` (day_max = ifelse(day_max %in% c(-Inf, Inf), NA, 
                                             day_max),
                            day_min = ifelse(day_min %in% c(-Inf, Inf), NA, 
                                             day_min))
                    ][, `:=` (month_num = month(day),
                              yr = year(day))
                      ][yr <= 2020, ]
    
    ## originally we were getting delta[var] as being var at height h - var 
    ## at lowest height. We have decided (as of Mar. 2021) to only do raw values.
    ## The delta var code is at the bottom of this script in the Archive section
    
    ## only calculate raw mean values
    whei <- test[order(day, zOffset),
                 ][, .(max_mean = round(mean(day_max, na.rm=TRUE),2),
                       min_mean = round(mean(day_min, na.rm=TRUE),2),
                       max_sd = round(sd(day_max, na.rm=TRUE),2),
                       min_sd = round(sd(day_min, na.rm=TRUE),2)),
                   by = .(zOffset, month_num)
                   ][order(month_num, zOffset), 
                     ][, `:=` (max_mean = 
                                 ifelse(is.nan(max_mean), NA, 
                                        max_mean),
                               min_mean = 
                                 ifelse(is.nan(min_mean), NA, 
                                        min_mean))]
    setnames(whei, old="zOffset", new="plotHeight")
    
    whei <- whei[, month_char := ifelse(month_num==7, "July", "January")]
    
    #only keep July values (from edit in Jan 2021)
    whei <- whei[month_char=="July", 
                 ][, `:=` (var = dp[,name][i], site=sites[j], col=clrs[j])]
    
    alldata <- rbind(alldata, whei)
    allTopHeight <- c(allTopHeight, topHeight)
    allNormHeight <- c(allNormHeight, normHeight)
  }
  
  #only keep relevant stat - min for RH, max for everything else
  if(dp[,name][i]=="RHMean"){
    keep <- colnames(alldata)
    keep <- keep[!grepl("max", keep)]
    alldata <- alldata[,(.SD), .SDcols=keep]
    setnames(alldata, old=c("min_mean", "min_sd"), 
             new=c("all_mean", "all_sd"))
  } else {
    keep <- colnames(alldata)
    keep <- keep[!grepl("min", keep)]
    alldata <- alldata[,(.SD), .SDcols=keep]
    setnames(alldata, old=c("max_mean", "max_sd"), 
             new=c("all_mean", "all_sd"))
  }
  
  plots[[i]] <- local({
    graph <- ggplot(alldata) +
      scale_color_manual(values = clrs, name = "Sites")
    
    if(i %in% c(1,2)){ #windspeed, RH
      graph <- graph + 
        ylim(0, 80)
    } else {
      graph <- graph + 
        ylim(0,80)
        # geom_hline(yintercept=1, linetype="dotted") +
        # ylim(0, ceiling(max(alldata$plotHeight)))
    }
    
    graph <- graph +
      geom_point(aes(x = all_mean, y = plotHeight, color = site), 
                 shape=19) +
      geom_path(aes(x = all_mean, y = plotHeight, color = site)) +
      ggplot2::geom_errorbarh(aes(xmin = all_mean - all_sd, 
                                  xmax = all_mean + all_sd, 
                                  y=plotHeight, color = site, height=0.1)) +
      labs(x = "",
           y = "") +
      theme_bw()
    
    if(i %in% c(1,2)){ #windspeed, RH
      graph <- graph + 
        xlab(paste0(dp[,stat][i], " ", dp[,xlabs][i]))
      if(i==1){
        graph <- graph + ylab("Height [m]")
      }
    } else if(i==3){ #tempSingle
      graph <- graph +
        xlab(expression(
          paste("max T"["air"], " [",degree,"C]")))
    } else if(i==4){ #bioTempMean
      graph <- graph +
        xlab(expression(
          paste("max T"["bio"], " [",degree,"C]")))
    } else if(i==5){ #PAR
      graph <- graph + 
        xlab("max PAR")
    }
    
    ## remove y-axis from all but windspeed
    if(i %in% c(2:5)){
      graph <- graph + 
          theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
    }
    
    if(i==4){ #biotemp - legend purposes
      graph <- graph + theme(legend.position=c(0.8,0.7))
    } else {
      graph <- graph + theme(legend.position="none")
    }
  })
}

names(plots) <- dp[,name]

# void <- ggplot() + theme_void()

## bring in LAD profiles
source("scripts/lad_profs_v_therm_plotting_for_ian_v1.R")

p <- ggarrange(plotsLAD[["LAD"]], plotsLAD[["sun"]], plotsLAD[["lgt"]], 
               plots[["PARMean"]], 
               plots[["windSpeedMean"]], plots[["RHMean"]], 
               plots[["tempSingleMean"]], plots[["bioTempMean"]],
               nrow=2, ncol=4, labels=c("A", "B", "C", "D", "E", "F", "G", "H"))

png(paste0("figures/profile_all1.png"), height=600, width=960)
print(p)
dev.off()

##########################################################################
## Make plots for SI ####
# define groupings of forest sites
allSites <- c("DELA", "GUAN", "JERC", "LENO", "PUUM", "CLBJ", "OSBS", 
              "SJER", "YELL", "GRSM", "ORNL", "SCBI", "TREE", "UKFS", 
              "SOAP", "TALL", "TEAK", "WREF",  "BART", "BONA", "DEJU", 
              "HARV")

sites <- list(subBroad = c("DELA", "GUAN", "JERC", "LENO", "PUUM"),
              savOpenTemp = c("CLBJ", "OSBS", "SJER", "YELL"),
              tempBroad = c("GRSM", "ORNL", "SCBI", "TREE", "UKFS"),
              tempCon = c("SOAP", "TALL", "TEAK", "WREF"),
              northBor = c("BART", "BONA", "DEJU", "HARV"))

clrs <- list(subBroad = c("#80E271", "#7EEACA", "#73CDDF", "#D59A8C", "grey"),
             savOpenTemp = c("#72789A", "gold", "#D082C8", "#D4DE9A"),
             tempBroad = c("#DE9D4E", "#DBBADA", "red", "#DF5574", "#D6E2DA"),
             tempCon = c("#836AD7", "#78A383", "#8AA4E8", "#003399"),
             northBor = c("#8B3CE1", "#D3E251", "#E354DE", "#3399FF"))

# sites <- c("HARV", "OSBS", "PUUM", "SCBI", "SERC", "WREF")
# clrs <- c("#3399FF", "gold", "grey", "red", "#66CCFF", "#003399")

groupPlots <- lapply(1:length(sites), function(X){
  sitesFocus <- sites[[X]]
  clrsFocus <- clrs[[X]]
  
  plots <- list()
  allNEON <- NULL
  for(i in 1:length(dp[,name])){
    alldata <- NULL
    allTopHeight <- c()
    for(j in 1:length(sitesFocus)){
      load(paste0("data/neon_rdata/", sitesFocus[j], "test.Rdata"))
      # meta_site <- meta[SITE == sitesFocus[j], ]
      
      #get max vertical height (for use in plots)
      hei <- sapply(full_data, function(x){unique(x[["verticalPosition"]])})
      topHeight <- max(unlist(hei, use.names = FALSE))
      
      var <- as.data.table(full_data[names(full_data) == dp[,name][i]])
      colnames(var) <- 
        gsub(paste0(dp[,name][i], "."), "", 
             colnames(var))
      
      #get daily min/max by vertical position
      test <- var[, day := as.Date(startDateTime)
                  ][, .(day_max = max(get(dp[,name][i]), na.rm=TRUE),
                        day_min = min(get(dp[,name][i]), na.rm=TRUE)),
                    by = .(day, zOffset)
                    ][, `:=` (day_max = ifelse(day_max %in% c(-Inf, Inf), NA, 
                                               day_max),
                              day_min = ifelse(day_min %in% c(-Inf, Inf), NA, 
                                               day_min))
                      ][, `:=` (month_num = month(day),
                                yr = year(day))
                        ][yr <= 2020, ]
      
      ## originally we were getting delta[var] as being var at height h - var 
      ## at lowest height. We have decided (as of Mar. 2021) to only do raw values.
      ## The delta var code is at the bottom of this script in the Archive section
      
      ## only calculate raw mean values
      whei <- test[order(day, zOffset),
                   ][, .(max_mean = round(mean(day_max, na.rm=TRUE),2),
                         min_mean = round(mean(day_min, na.rm=TRUE),2),
                         max_sd = round(sd(day_max, na.rm=TRUE),2),
                         min_sd = round(sd(day_min, na.rm=TRUE),2)),
                     by = .(zOffset, month_num)
                     ][order(month_num, zOffset), 
                       ][, `:=` (max_mean = 
                                   ifelse(is.nan(max_mean), NA, 
                                          max_mean),
                                 min_mean = 
                                   ifelse(is.nan(min_mean), NA, 
                                          min_mean))]
      setnames(whei, old="zOffset", new="plotHeight")
      
      whei <- whei[, month_char := ifelse(month_num==7, "July", "January")]
      
      #only keep July values (from edit in Jan 2021)
      whei <- whei[month_char=="July", 
                   ][, `:=` (var = dp[,name][i], site=sitesFocus[j], 
                             col=clrsFocus[j], biome=names(sites[X]))]
      
      alldata <- rbind(alldata, whei)
      allTopHeight <- c(allTopHeight, topHeight)
    }
    
    #only keep relevant stat - min for RH, max for everything else
    if(dp[,name][i]=="RHMean"){
      keep <- colnames(alldata)
      keep <- keep[!grepl("max", keep)]
      alldata <- alldata[,(.SD), .SDcols=keep]
      setnames(alldata, old=c("min_mean", "min_sd"), 
               new=c("all_mean", "all_sd"))
    } else {
      keep <- colnames(alldata)
      keep <- keep[!grepl("min", keep)]
      alldata <- alldata[,(.SD), .SDcols=keep]
      setnames(alldata, old=c("max_mean", "max_sd"), 
               new=c("all_mean", "all_sd"))
    }
    allNEON <- rbind(allNEON, alldata)
    
    plots[[i]] <- local({
      graph <- ggplot(alldata) +
        scale_color_manual(values = clrsFocus, name = "Sites")
      
      if(i %in% c(1,2)){ #windspeed, RH
        graph <- graph + 
          ylim(0, 50)
      } else {
        graph <- graph + 
          ylim(0,50)
        # geom_hline(yintercept=1, linetype="dotted") +
        # ylim(0, ceiling(max(alldata$plotHeight)))
      }
      
      graph <- graph +
        geom_point(aes(x = all_mean, y = plotHeight, color = site, size=1.1), 
                   shape=19) +
        geom_path(aes(x = all_mean, y = plotHeight, color = site), size=1.1) +
        ggplot2::geom_errorbarh(aes(xmin = all_mean - all_sd, 
                                    xmax = all_mean + all_sd, 
                                    y=plotHeight, color = site, height=0.1,
                                    size=1)) +
        labs(x = "",
             y = "") +
        theme_bw() +
        theme(text = element_text(size=14))
      
      if(X==5){#only labels on bottom row for putting all 25 plots together
        if(i %in% c(1,2)){ #windspeed, RH
          graph <- graph + 
            xlab(paste0(dp[,stat][i], " ", dp[,xlabs][i]))
          } else if(i==3){ #tempSingle
          graph <- graph +
            xlab(expression(
              paste("max T"["air"], " [",degree,"C]")))
        } else if(i==4){ #bioTempMean
          graph <- graph +
            xlab(expression(
              paste("max T"["bio"], " [",degree,"C]")))
        } else if(i==5){ #PAR
          graph <- graph + 
            xlab("max PAR")
        }
      }
      
    if(i==5){ # add label for PAR only (for each row)
      graph <- graph + ylab("Height [m]")
    }
      
      ## remove y-axis from all but PAR
      if(!i==5){
        graph <- graph + 
          theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
      }
      
      if(i==4){ #biotemp - legend purposes
        graph <- graph + 
          theme(legend.position=c(0.8,0.7), 
                legend.text=element_text(size=14)) +
          guides(size=FALSE)
      } else {
        graph <- graph + theme(legend.position="none")
      }
    })
  }
  
  names(plots) <- dp[,name]
  
  # void <- ggplot() + theme_void()
  
  if(X==1) titleTx <- "(A) (Sub) Subtropical and warm temperate broadleaf forests"
  if(X==2) titleTx <- "(B) Temperate open forests / savanna"
  if(X==3) titleTx <- "(C) Temperate mesic broadleaf forests"
  if(X==4) titleTx <- "(D) Temperate conifer forests"
  if(X==5) titleTx <- "(E) Northern and boreal forests"
  
  p <- plots[["PARMean"]] + plots[["windSpeedMean"]] + plots[["RHMean"]] + 
    plots[["tempSingleMean"]] + plots[["bioTempMean"]]
  p <- p + 
    plot_annotation(title=titleTx, 
                    theme = theme(plot.title = element_text(size = 18))) +
    plot_layout(ncol = 5)
  
  # p <- ggarrange(NULL, plot_0, NULL, NULL, NULL,
  #                plots[["PARMean"]], plots[["windSpeedMean"]],
  #                plots[["RHMean"]], plots[["tempSingleMean"]], 
  #                plots[["bioTempMean"]],
  #                nrow=2, ncol=5, heights=c(0.1,1))
  
  return(list(p, allNEON))
})


#get canopy minus bottom values across aggregate groupings
aggNeon <- rbindlist(lapply(groupPlots, `[[`, 2))

ttt <- aggNeon[, .SD[c(1,.N)], by=.(site,var)][order(site,var,plotHeight)]
tr <- ttt[, diff := all_mean - shift(all_mean, fill = first(all_mean)), 
    by = .(site,var)
    ][diff != 0,]

tr[,mean(all_mean), by=var] #mean canopy minus bottom for all sites
tr[,mean(all_mean), by=.(biome, var)][order(var)] #same as above but per biome

#continuing with the plotting here
groupPlots <- lapply(groupPlots, `[[`, 1)
names(groupPlots) <- names(sites)

## for individual plots
# sapply(1:length(groupPlots), function(X){
#   png(paste0("figures/profile_all_", names(groupPlots)[X], ".png"), 
#       height=600, width=960)
#   print(groupPlots[[X]])
#   dev.off()
# })

## for all 25 plots together

q <- ggarrange(groupPlots[["subBroad"]], groupPlots[["savOpenTemp"]],
               groupPlots[["tempBroad"]], groupPlots[["tempCon"]],
               groupPlots[["northBor"]],
               nrow=5, ncol=1)
png(paste0("figures/profile_all_groups.png"), 
    height=1800, width=1500)
print(q)
dev.off()

##########################################################################
# Archive
## code for Delta variable ####
## originally we were getting delta[var] as being var at height h - var 
## at lowest height. We have decided (as of Mar. 2021) to only do raw values.

newt <- test[order(day, zOffset),
             ][, `:=` (delta_max = day_max - day_max[1],
                       delta_min = day_min - day_min[1]),
               by=.(day)
               ][, .(max_mean = round(mean(delta_max, na.rm=TRUE),2),
                     min_mean = round(mean(delta_min, na.rm=TRUE),2),
                     max_sd = round(sd(delta_max, na.rm=TRUE),2),
                     min_sd = round(sd(delta_min, na.rm=TRUE),2)),
                 by = .(zOffset, month_num)
                 ][order(month_num, zOffset), 
                   ][, `:=` (max_mean = 
                               ifelse(is.nan(max_mean), NA, 
                                      max_mean),
                             min_mean = 
                               ifelse(is.nan(min_mean), NA, 
                                      min_mean))]

## bring in normalized height
whei <- newt[, plotHeight := round(zOffset / normHeight[,normH], 2)]

## We also had ggplot do horizontal line at y=1 for the normalized height.
### This is commnted out in the main code above.

## original script for 5-plot, individual site figures ####
### if using this script, CHANGE VERTICAL POSITION TO BE ZOFFSET

## 5 sites for the SI
sites <- c("HARV", "OSBS", "PUUM", "SCBI", "WREF")
clrs <- c("#3399FF", "gold", "grey", "red", "#003399")

meta <- read_excel("data/site_data/TIS site metadata_20190403_forNOAA.xlsx", sheet=1)
meta <- data.table(meta)
meta <- meta[SITE %in% sites, 
             ][,DistZaxsCnpy := as.numeric(DistZaxsCnpy)]

for(j in 1:length(sites)){
  load(paste0("data/neon_rdata/", sites[j], "test.Rdata"))
  meta_site <- meta[SITE == sites[j], ]

  #get max vertical height (for use in plots)
  hei <- sapply(full_data, function(x){unique(x[["verticalPosition"]])})
  topHeight <- max(unlist(hei, use.names = FALSE))
  normHeight <- meta[SITE == sites[j], DistZaxsCnpy]

  plots <- list()
  allstats <- NULL
  for(i in 1:length(dp[,name])){
    var <- as.data.table(full_data[names(full_data) == dp[,name][i]])
    colnames(var) <-
      gsub(paste0(dp[,name][i], "."), "",
           colnames(var))

    # if(dp[,data][i] == "IRBT"){
    #   colnames(var) <-
    #     gsub(paste0(dp[,data][i], "_30_minute."), "",
    #          colnames(var))
    # } else if(dp[,data][i] %in% c("2DWSD", "RH", "SAAT")){
    #   colnames(var) <-
    #     gsub(paste0(dp[,name][i], "."), "",
    #          colnames(var))
    # } else {
    #   colnames(var) <-
    #     gsub(paste0(dp[,data][i], "_30min."), "",
    #          colnames(var))
    # }

    #get daily min/max by vertical position
    test <- var[, day := as.Date(startDateTime)
                ][, .(day_max = max(get(dp[,name][i]), na.rm=TRUE),
                      day_min = min(get(dp[,name][i]), na.rm=TRUE)),
                  by = .(day, verticalPosition)
                  ][, `:=` (day_max = ifelse(day_max %in% c(-Inf, Inf), NA, day_max),
                            day_min = ifelse(day_min %in% c(-Inf, Inf), NA, day_min))
                    ][, `:=` (month_num = month(day),
                              yr = year(day))
                      ]

    stat <- test[, `:=` (mmax = mean(day_max, na.rm=TRUE),
                         mmin = mean(day_min, na.rm=TRUE),
                         sd_max = sd(day_max, na.rm=TRUE),
                         sd_min = sd(day_min, na.rm=TRUE))]

    out <- test[, .(max_mean = round(mean(day_max, na.rm=TRUE),2),
                    min_mean = round(mean(day_min, na.rm=TRUE),2),
                    max_sd = round(sd(day_max, na.rm=TRUE),2),
                    min_sd = round(sd(day_min, na.rm=TRUE),2)),
                by = .(verticalPosition, month_num)
                ][order(month_num, verticalPosition),
                  ][, `:=` (max_mean =
                              ifelse(is.nan(max_mean), NA, max_mean
                              ),
                            min_mean =
                              ifelse(is.nan(min_mean), NA, min_mean
                              ))]
    allstats <- rbind(allstats, out)

    #get mean change of max/min variable by height by month by year
    ## monthly mean [(daily max at h_x)-(daily max at h_0)]

    # #TEAK the bottom height meas is NA
    # if(j==20 & i %in% c(1,3,5)){ #windSpeedMean, tempSingleMean, PARMean
    #   test <- test[verticalPosition==10 & month_num==1,
    #                `:=` (day_max =0, day_min=0)]
    # }

    ## we define delta[var] as being var at height h - var at lowest height
    newt <- test[order(day, verticalPosition),
                 ][, `:=` (delta_max = day_max - day_max[1],
                         delta_min = day_min - day_min[1]),
                 by=.(day)
                 ][, .(delta_max_mean = round(mean(delta_max, na.rm=TRUE),2),
                       delta_min_mean = round(mean(delta_min, na.rm=TRUE),2),
                       delta_max_sd = round(sd(delta_max, na.rm=TRUE),2),
                       delta_min_sd = round(sd(delta_min, na.rm=TRUE),2)),
                   by = .(verticalPosition, month_num)
                   ][order(month_num, verticalPosition),
                     ][, `:=` (delta_max_mean =
                                 ifelse(is.nan(delta_max_mean), NA, delta_max_mean
                                        ),
                               delta_min_mean =
                                 ifelse(is.nan(delta_min_mean), NA, delta_min_mean
                                        ))]

    ## bring in normalized height
    whei <- newt[, norm_height :=
                   round(verticalPosition /
                           meta[SITE == sites[j], DistZaxsCnpy], 2)
                 ][, month_char := ifelse(month_num==7, "January", "July")]

    #only keep July values (from edit in Jan 2021)
    whei <- whei[month_char=="July", 
                 ][, `:=` (var = dp[,name][i], site=sites[j], col=clrs[j])]

    # allstats <- rbind(allstats, whei)

    #plot
    clr <- unique(whei$col)
    plots[[i]] <- local({
      graph <- ggplot(whei) +
        ylim(0, ceiling(topHeight/normHeight)) +
        geom_hline(yintercept=1, linetype="dotted") +
        geom_point(aes(x = delta_max_mean, y = norm_height), color=clr,
                   shape=19) +
        geom_point(aes(x = delta_min_mean, y = norm_height), color = clr,
                   shape=17) +
        geom_path(aes(x = delta_max_mean, y = norm_height, 
                      linetype = "Max"), color = clr, size = 1) +
        geom_path(aes(x = delta_min_mean, y = norm_height, 
                      linetype = "Min"), color = clr,  size = 1) +
        ggplot2::geom_errorbarh(aes(xmin = delta_max_mean - delta_min_sd,
                                    xmax = delta_max_mean + delta_max_sd,
                                    y=norm_height, color = clr,
                                    linetype = "Max", height=0.1)) +
        ggplot2::geom_errorbarh(aes(xmin=delta_min_mean - delta_min_sd,
                                    xmax=delta_min_mean + delta_max_sd,
                                    y=norm_height, color = clr,
                                    linetype = "Min", height=0.1)) +
        scale_color_manual(values = clr) +
        # labs(x = expression(paste(Delta, "Climate Variable")),
        #      y = "Normalized Height") +
        labs(x = "",
             y = "") +
        ggtitle(dp[,xlabs][i]) +
      theme_bw() +
        guides(linetype = guide_legend("Line type"))
    })


    # if(i != 1){
    #   graph <- graph + theme(axis.title.y = element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank())
    # }
    #
    # if(i == 2){
    #   graph <- graph + scale_x_continuous(breaks=c(40,60,80,100))
    # }
    #
    # if(i == 3){
    #   graph <-
    #     graph +
    #     labs(x= expression(paste("T"["air"], " [",degree,"C]"))) +
    #     scale_x_continuous(breaks=c(10,20,30), limits=c(5,35))
    # }
    #
    # # if(i == 4){
    # #   graph <-
    # #     graph +
    # #     labs(x = expression(paste("T"["biological"], " [",degree,"C]"))) +
    # #     scale_x_continuous(breaks=c(10,20,30), limits=c(5,35))
    # # }
    #
    # if(i==1){
    #   assign(paste0("wind", "_plot"), graph)
    # } else {
    #   assign(paste0(dp$data[[i]], "_plot"), graph)
    # }
    # print(graph)
  }
  names(plots) <- dp[,name]

  void <- ggplot() + theme_void()

  p <- ggarrange(void, void, void, plots[["PARMean"]],
                 plots[["windSpeedMean"]], plots[["RHMean"]],
                 plots[["tempSingleMean"]], plots[["bioTempMean"]],
                 nrow=2, ncol=4, common.legend=TRUE,
                 legend="top")

  png(paste0("figures/profile_", sites[j], ".png"), height=600, width=960)
  print(annotate_figure(p,
    top=text_grob(paste0("Site: ", sites[j]), color="black", face="bold", size=12),
    left = text_grob("Normalized Height", rot = 90),
    bottom = text_grob(expression(paste(Delta, "Climate Variable"))))
  )
  dev.off()
}

allstats <- allstats[month_num==7, ]
write.csv(allstats, "HARV_neon_stats.csv", row.names=FALSE)
