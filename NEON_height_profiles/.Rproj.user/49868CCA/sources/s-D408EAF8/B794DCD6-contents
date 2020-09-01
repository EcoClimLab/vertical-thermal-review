######################################################
# Purpose: Create vertical profiles of climate variables for ForestGEO plot using NEON tower data
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 - First created August 2020
## Aug-Sept. 2020
######################################################
#install needed packages
library(data.table)
library(readxl)

#
#1. Download and save neon data ####
## this is filtered to only the columns needed for plots 
## (mean measurements per each 30 min observation) in order to not have massive files.
source("scripts/get_neon_data.R")

pbapply::pblapply(sites, function(st){
  full_data <- get_NEON(x=st)
  save(full_data, file=paste0(st, "test.Rdata"))
})

#2. Organize data for plotting ####

data <- fread("forested_NEON_sites.csv")
data <- data[include==1, ]
sites <- data[,site]

#bring in normalized height
meta <- read_excel("data/site_data/TIS site metadata_20190403_forNOAA.xlsx", sheet=1)
meta <- data.table(meta)
meta <- meta[SITE %in% data[,site], ]

load("data/neon_rdata/SCBItest.Rdata")

dp <- data.table(data = c("2DWSD", "RH", "SAAT", "IRBT", "PARPAR", "SLRNR"),
                 id = c("DP1.00001.001", "DP1.00098.001",
                        "DP1.00002.001", "DP1.00005.001",
                        "DP1.00024.001", "DP1.00023.001"),
                 name = c("windSpeedMean", "RHMean",
                          "tempSingleMean", "bioTempMean",
                          "PARMean", "SWIR"),
                 xlabs = c("Wind speed [m/s]", "RH [%]",
                           "Mean Air Temperature [°C]", 
                           "Mean Infrared Biological Temperature [°C]",
                           "Photosynthetic Active Radiation",
                           "Shortwave and longwave radiation"))

for(i in 1:length(dp[,name])){
  var <- as.data.table(full_data[names(full_data) == dp[,name][i]])
  
  if(dp[,data][i] == "IRBT"){
    colnames(var) <- 
      gsub(paste0(dp[,data][i], "_30_minute."), "", 
           colnames(var))
  } else if(dp[,data][i] == "2DWSD"){
    colnames(var) <- 
      gsub(paste0(dp[,name][i], "."), "", 
           colnames(var))
  } else {
    colnames(var) <- 
      gsub(paste0(dp[,data][i], "_30min."), "", 
           colnames(var))
  }
  
  #get daily min/max by vertical position
  test <- var[, day := as.Date(startDateTime)
             ][, .(day_max = max(get(dp[,name][i]), na.rm=TRUE),
                  day_min = min(get(dp[,name][i]), na.rm=TRUE)),
               by = .(day, verticalPosition)
               ][, `:=` (day_max = ifelse(day_max %in% c(-Inf, Inf), NA, day_max),
                         day_min = ifelse(day_min %in% c(-Inf, Inf), NA, day_min))]

  #get mean change of max/min variable by height by month by year
  newt <- test[, `:=` (delta_max = day_max - day_max[1],
                       delta_min = day_min - day_min[1]),
               by = .(day)
               ][, `:=` (month_num = month(day),
                         yr = year(day))
                 ][, .(delta_max_mean = round(mean(delta_max, na.rm=TRUE),2),
                       delta_min_mean = round(mean(delta_min, na.rm=TRUE),2)),
                   by = .(verticalPosition, month_num, yr)
                   ][order(yr, month_num, verticalPosition), 
                     ][, `:=` (delta_max_mean = 
                                 ifelse(is.nan(delta_max_mean), NA, delta_max_mean),
                               delta_min_mean = 
                                 ifelse(is.nan(delta_min_mean), NA, delta_min_mean))]
  heights <- meta[SITE == j, DistZaxsLvlMeasTow]
  heights <- strsplit(heights, ",")
  heights <- lapply(heights, as.numeric)
  
  bleh <- as.vector(heights[[1]])
  newt <- newt[,verticalPosition := rep(bleh, nrow(newt)/)]
  
  whei <- newt[,]
}





#Next steps
##adapt rest of vertical_height_NEON script
##make plots

