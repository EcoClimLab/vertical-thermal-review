######################################################
# Purpose: Batch download NEON data
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 - First created May 2019, updated for vertical-thermal-rv
## Aug-Sept. 2020
######################################################
library(neonUtilities)
library(data.table)

#traits from NEON
##1. single aspirated air temp: DP1.00002.001, avg = 1min or 30min
##2. 2-D wind speed and direction: DP1.00001.001, avg = 2min or 30min
##3. IR Biological Temp (infrared): DP1.00005.001, avg = 1min or 30min
##4. Relative humidity: DP1.00098.001, avg = 1min or 30min

#the function "loadByProduct" will load the data into R and collapse into one df (within a list).
#it will not download/store anything on the computer, but working with large dfs will run slowly. Hence, it is a good idea to look at the 30min avg first.

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

date <- data.table("year" = c(rep(2015, 2), rep(2016, 2), rep(2017, 2), 
                              rep(2018, 2), rep(2019, 2),
                              rep(2020, 2)),
                   "month" = c(rep(c(1,7), 6)))

# date <- data.table("year" = c(rep(2019, 2)),
#                    "month" = c(rep(c(1,7), 1)))

# years <- c("2015", "2016", "2017", "2018", "2019", "2020")

# years <- c("2019")

#2. Loop through and download data ####
# alldt <- list()
# plotlist <- list()

# sites <- c("BART","BONA","CLBJ","DEJU","DELA","GRSM","GUAN","HARV","JERC",
#            "LENO","MLBS","ORNL","OSBS","SCBI","SERC","SJER","SOAP","STEI",
#            "TALL","TEAK","TREE","UKFS","UNDE","WREF","YELL")

get_NEON <- function(x="SCBI"){
  # full <- list()
  # for(x in seq(along=sites)){
  vars <- list()
  for (i in seq(along=dp[,name])){
    
    neon_data_all <- NULL
    # for (j in seq(1:nrow(date))){
    neon_tower <- 
      loadByProduct(dpID=dp[,id][i], 
                    site=x,
                    package="basic", 
                    check.size = FALSE, avg=30,
                    nCores = 2)
    # startdate=paste0(date[,year][j], "-0",
    #                  date[,month][j]),
    # enddate = paste0(date[,year][j], "-0",
    #                  date[,month][j])
    #(use TRUE outside loop to see 
    #how big the dowloads are)
    # )
    
    neon_data <- neon_tower[grepl(dp[,data][i], names(neon_tower))]
    neon_data <- as.data.table(neon_data)
    
    if(dp[,data][i] == "IRBT"){
      colnames(neon_data) <- 
        gsub(paste0(dp[,data][i], "_30_minute."), "", 
             colnames(neon_data))
    } else if(dp[,data][i] == "2DWSD"){
      colnames(neon_data) <- 
        gsub(paste0("X", dp[,data][i], "_30min."), "", 
             colnames(neon_data))
    } else {
      colnames(neon_data) <- 
        gsub(paste0(dp[,data][i], "_30min."), "", 
             colnames(neon_data))
    }
    
    #filter neon_data and add day column
    if(dp[,data][i] == "SLRNR"){
      neon_data <- neon_data[month(startDateTime) %in% c(1,7), 
                             ][, verticalPosition := as.numeric(verticalPosition)
                               ][,.(siteID, verticalPosition, startDateTime,
                                    inSWMean, outSWMean, inLWMean, outLWMean)]
    } else {
      neon_data <- neon_data[month(startDateTime) %in% c(1,7), 
                             ][, verticalPosition := as.numeric(verticalPosition)
                               ][,.(siteID, verticalPosition, startDateTime,
                                    get(dp[,name][i]))]
      colnames(neon_data)[4] <- dp[,name][i]
    }
    
    #bring in real sensor positions
    nm <- names(neon_tower)[grepl("sensor_positions", names(neon_tower))]
    heights <- as.data.table(neon_tower[nm])
    colnames(heights) <- gsub(paste0(nm, "\\."), "", colnames(heights))
    heights <- heights[,.(HOR.VER, zOffset)][order(HOR.VER,zOffset),]
    
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    
    # lab <- unique(neon_data$verticalPosition)
    
    heights[, HOR.VER := as.numeric(substrRight(heights$HOR.VER, 2))]
    heights <- unique(heights, by="HOR.VER")
    setnames(heights, old="HOR.VER", new="verticalPosition")
    
    
    # if(nrow(heights)==2){
    #   heightRange <- c(0,60)
    # } else if(nrow(heights)==5){
    #   heightRange <- seq(10,50,by=10)
    # } else if(nrow(heights)==6){
    #   heightRange <- seq(0,50,by=10)
    # } else if(nrow(heights)==7){
    #   heightRange <- seq(0,60,by=10)
    # } else if(nrow(heights)==8){
    #   heightRange <- seq(0,70,by=10)
    # } else if(nrow(heights)==9){
    #   heightRange <- seq(0,60,by=10)
    # }
    # heights <- heights[, pos := heightRange]
    
    setkeyv(neon_data, "verticalPosition")
    setkeyv(heights, "verticalPosition")
    neon_data <- neon_data[heights]
    
    
    # neon_data_all <- rbind(neon_data_all, neon_data)
    # } 
    vars[[i]] <- neon_data
  }
  names(vars) <- dp[,name]
  # full[[x]] <- vars
  return(vars)
}