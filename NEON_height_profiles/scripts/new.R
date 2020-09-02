######################################################
# Purpose: Create vertical profiles of climate variables for ForestGEO plot using NEON tower data
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 - First created August 2020
## Aug-Sept. 2020
######################################################

#install needed packages
library(data.table)
library(readxl)

#1. Download and save neon data ####
## this is filtered to only the columns needed for plots 
## (mean measurements per each 30 min observation) in order to not have massive files.
source("scripts/get_neon_data.R")

data <- fread("NEON_height_profiles/forested_NEON_sites.csv")
data <- data[include==1, ]
sites <- data[,site]

pbapply::pblapply(sites, function(st){
  full_data <- get_NEON(x=st)
  save(full_data, file=paste0(st, "test.Rdata"))
})

#2. Organize data for plotting ####
load("neon_rdata/SCBItest.Rdata")

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
  
  var <- var[, day := substr(startDateTime, 1, 10)]
  
  #bring in normalized height
  meta <- read_excel("NEON_height_profiles/data/site_data/TIS site metadata_20190403_forNOAA.xlsx", sheet=1)
  meta <- data.table(meta)
  
  meta <- meta[SITE %in% data[,site], ]
  
  
}





#Next steps
##adapt rest of vertical_height_NEON script
##make plots

