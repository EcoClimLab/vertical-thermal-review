######################################################
# Purpose: Create vertical profiles of climate variables for ForestGEO plot using NEON tower data
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 - First created May 2019, updated for vertical-thermal-rv
## Aug-Sept. 2020
######################################################
library(neonUtilities)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(grid)
library(gridExtra)
library(ggplot2)

#traits from NEON
##1. single aspirated air temp: DP1.00002.001, avg = 1min or 30min
##2. 2-D wind speed and direction: DP1.00001.001, avg = 2min or 30min
##3. IR Biological Temp (infrared): DP1.00005.001, avg = 1min or 30min
##4. Relative humidity: DP1.00098.001, avg = 1min or 30min

#the function "loadByProduct" will load the data into R and collapse into one df (within a list).
#it will not download/store anything on the computer, but working with large dfs will run slowly. Hence, it is a good idea to look at the 30min avg first.

dp <- data.table(data = c("2DWSD", "RH", "SAAT", 
                          "IRBT"),
                 id = c("DP1.00001.001", "DP1.00098.001",
                        "DP1.00002.001", "DP1.00005.001"),
                 name = c("windSpeedMean", "RHMean",
                           "tempSingleMean", "bioTempMean"),
                 xlabs = c("Wind speed [m/s]", "RH [%]",
                           "Mean Air Temperature [°C]", 
                           "Mean Infrared Biological Temperature [°C]"))

date <- data.table("year" = c(rep(2016, 2), rep(2017, 2), 
                              rep(2018, 2), rep(2019, 2),
                              rep(2020, 2)),
                   "month" = c(rep(c(1,7), 5)))

date <- data.table("year" = c(rep(2019, 2)),
                   "month" = c(rep(c(1,7), 1)))

years <- c("2016", "2017", "2018", "2019", "2020")

years <- c("2019")

#2. Loop through and download data ####
alldt <- list()
plotlist <- list()

sites <- c("BART","BONA","CLBJ","DEJU","DELA","GRSM","GUAN","HARV","JERC",
           "LENO","MLBS","ORNL","OSBS","SCBI","SERC","SJER","SOAP","STEI",
           "TALL","TEAK","TREE","UKFS","UNDE","WREF","YELL")

site_data <- lapply(sites, function(x){
  vars <- list()
  for (i in seq(along=dp[,name])){
    
    neon_data_all <- NULL
    for (j in seq(1:nrow(date))){
      neon_tower <- 
        loadByProduct(dpID=dp[,id][i], 
                      site=x,
                      package="basic", 
                      check.size = FALSE, avg=30,
                      nCores = 2
                      #(use TRUE outside loop to see 
                      #how big the dowloads are)
                      )
      
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
      
      neon_data$verticalPosition <- as.numeric(neon_data$verticalPosition)
      
      #filter neon_data and add day column
      neon_data <- neon_data[month(startDateTime) %in% c(1,7), 
                             .(verticalPosition, 
                                 startDateTime, 
                                 get(dp[,name][i]))]
      names(neon_data)[3] <- dp[,name][i]
      
      neon_data_all <- rbind(neon_data_all, neon_data)
    } 
    vars[[i]] <- neon_data_all
  }
  names(vars) <- dp[,name]
  return(vars)
})
names(site_data)


  [, day := substr(startDateTime, 1, 10)]
  
  #the 10m air temperature values are completely off, and stop at 19 May 2018. The sensor is broken and hasn't been fixed
  if (value == "tempSingleMean"){
    neon_data_all$tempSingleMean <- 
      ifelse(neon_data_all$verticalPosition == 10 & 
               grepl("2018", neon_data_all$day), NA, 
               neon_data_all$tempSingleMean)
  }
  
  #put full df in list to run anova later
  alldt[[i]] <- neon_data_all
  
  #get mean of values per month per verticalPosition
  data_analy <- neon_data_all %>% 
    group_by(day, verticalPosition) %>% 
    summarize(test_max = max(get(value), na.rm=TRUE),
              test_min = min(get(value), na.rm=TRUE))
  
  data_analy$test_max <- ifelse(grepl("Inf", data_analy$test_max), NA, data_analy$test_max)
  data_analy$test_min <- ifelse(grepl("Inf", data_analy$test_min), NA, data_analy$test_min)
  
  data_analy$month <- NA
  data_analy$month <- 
    ifelse(grepl(paste0(years[[1]], "-05"), data_analy$day) |
             grepl(paste0(years[[2]], "-05"), data_analy$day), "May",
    ifelse(grepl(paste0(years[[1]], "-06"), data_analy$day) |
             grepl(paste0(years[[2]], "-06"), data_analy$day), "June",
    ifelse(grepl(paste0(years[[1]], "-07"), data_analy$day) |
             grepl(paste0(years[[2]], "-07"), data_analy$day), "July", 
                         "August")))
  
  #get rid of random days that aren't full dates
  data_analy <- data_analy[grepl(".{10}", data_analy$day), ]
  
  data_analy <- data_analy %>%
    group_by(month, verticalPosition) %>%
    summarize(mmax = mean(test_max, na.rm=TRUE),
              mmin = mean(test_min, na.rm=TRUE),
              sd_max = sd(test_max, na.rm=TRUE),
              sd_min = sd(test_min, na.rm=TRUE))
              # quant_95 = quantile(test_max, c(0.95), na.rm=TRUE),
              # quant_05 = quantile(test_min, c(0.05), na.rm=TRUE))
  
  #base ggplot, all months on same graph
  data_analy$month_f <- factor(data_analy$month, levels=c("May", "June", "July", "August"))
  
  #CORRECT HEIGHTS (according to tower dimensions, not downloaded data)
  data_analy$verticalPosition <-
    ifelse(data_analy$verticalPosition == 10, 5.8,
    ifelse(data_analy$verticalPosition == 20, 19.2,
    ifelse(data_analy$verticalPosition == 30, 26,
    ifelse(data_analy$verticalPosition == 40, 32.9,
    ifelse(data_analy$verticalPosition == 50, 38,
    ifelse(data_analy$verticalPosition == 60, 51.8,
           data_analy$verticalPosition))))))
  
  data_analy$vertPos_jitter <- NA
  
  if(!i == 2){
    data_analy$vertPos_jitter <- 
      ifelse(data_analy$month == "May", data_analy$verticalPosition + 0.05,
       ifelse(data_analy$month == "June", data_analy$verticalPosition + 0.25,
       ifelse(data_analy$month == "July", data_analy$verticalPosition + 0.5,
                                          data_analy$verticalPosition + 0.75)))
  } else {
    data_analy$vertPos_jitter <- 
      ifelse(data_analy$month == "May" & data_analy$verticalPosition == 0, 
             data_analy$verticalPosition + 0.05,
      ifelse(data_analy$month == "June" & data_analy$verticalPosition == 0,
              data_analy$verticalPosition + 0.25,
      ifelse(data_analy$month == "July" & data_analy$verticalPosition == 0,
             data_analy$verticalPosition + 0.5,
      ifelse(data_analy$month == "August" & data_analy$verticalPosition == 0,
             data_analy$verticalPosition + 0.75,
      
      ifelse(data_analy$month == "May" & data_analy$verticalPosition == 60, 
             data_analy$verticalPosition - 0.75,
      ifelse(data_analy$month == "June" & data_analy$verticalPosition == 60,
                    data_analy$verticalPosition - 0.5,
      ifelse(data_analy$month == "July" & data_analy$verticalPosition == 60,
                           data_analy$verticalPosition - 0.25,
                                  data_analy$verticalPosition - 0.05)))))))
  }
  
  ##make plots and save to list
  plotlist[[i]] <- local({
    i <- i
  graph <-
    data_analy %>%
      arrange(verticalPosition) %>%
      ggplot() +
      scale_color_manual(values = c("darkorange", "red", "darkgreen", "blue"), 
                         name = "Month") +
      geom_point(aes(x = mmax, y = vertPos_jitter, color = month_f), 
                 shape=19, position = "jitter") +
      geom_point(aes(x = mmin, y = vertPos_jitter, color = month_f), 
                 shape=17, position = "jitter") +
      geom_path(aes(x = mmax, y = vertPos_jitter, color = month_f, 
                    linetype = "Max"), size = 1) +
      geom_path(aes(x = mmin, y = vertPos_jitter, color = month_f, 
                    linetype = "Min"), size = 1) +
      ggplot2::geom_errorbarh(aes(xmin=mmax-sd_min, xmax=mmax+sd_max, y=vertPos_jitter, color = month_f, linetype = "Max", height=0.8)) +
      ggplot2::geom_errorbarh(aes(xmin=mmin-sd_min, xmax=mmin+sd_max, y=vertPos_jitter, color = month_f, linetype = "Min", height=0.8)) +
      labs(x = dp$xlabs[[i]], y = "Height [m]") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits=c(0,60)) +
      theme_bw() +
      guides(linetype = guide_legend("Line type"))
  
  if(i != 1){
    graph <- graph + theme(axis.title.y = element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank())
  }
  
  if(i == 2){
    graph <- graph + scale_x_continuous(breaks=c(40,60,80,100))
  }
  
  if(i == 3){
    graph <- 
      graph + 
      labs(x= expression(paste("T"["air"], " [",degree,"C]"))) +
      scale_x_continuous(breaks=c(10,20,30), limits=c(5,35))
  }
  
  # if(i == 4){
  #   graph <- 
  #     graph + 
  #     labs(x = expression(paste("T"["biological"], " [",degree,"C]"))) +
  #     scale_x_continuous(breaks=c(10,20,30), limits=c(5,35))
  # }
  
  if(i==1){
    assign(paste0("wind", "_plot"), graph)
  } else {
    assign(paste0(dp$data[[i]], "_plot"), graph)
  }
  print(graph)
  })
}

names(alldt) <- dp$value[1:3]
names(plotlist) <- c("wind_plot", "RH_plot", "SAAT_plot")

save(alldt, file="data/physical/neondata.Rdata")
save(plotlist, file="data/physical/neonplots.Rdata")


# #arrange all graphs together and save image ####
# #extract legend
# #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# mylegend <- g_legend(SAAT_plot)
# 
# #formatting
# png("manuscript/tables_figures/NEON_vertical_profiles.png", width = 1000, height = 1000, pointsize = 18)
# graph <- grid.arrange(arrangeGrob(SAAT_plot + theme(legend.position = "none"),
#                                   wind_plot + theme(legend.position = "none"),
#                                   RH_plot + theme(legend.position = "none"),
#                                   biotemp_plot + theme(legend.position = "none"), 
#                                   nrow=1), 
#                       mylegend, nrow=1, 
#                       top = textGrob(expression(bold("NEON Vertical Profile 2016-2018"))))
# 
# dev.off()
# 
# library(ggpubr)
# NEON <- ggarrange(SAAT_plot, wind_plot, biotemp_plot, RH_plot, nrow=1, ncol=4, common.legend = TRUE, legend = "right")
# 
# annotate_figure(NEON,
#                 left = text_grob("Height (m)", rot = 90),
#                 fig.lab = "Figure 1", fig.lab.face = "bold"
# )
# 
# 
# SAAT_plot
# wind_plot
# biotemp_plot
# RH_plot
# SR_plot
# 

# #determine threshold for sunny/cloudy day ####
# neon_data_sub$day <- substr(neon_data_sub$startDateTime, 1, nchar(neon_data_sub$startDateTime)-0)
# 
# q <- neon_data_sub %>%
#       group_by(day) %>%
#       summarize(mean_difRad = mean(difRadMean, na.rm=TRUE), mean_dirRad = mean(dirRadMean, na.rm=TRUE))
# 
# q <- neon_data_sub %>%
#   group_by(day) %>%
#   summarize(mean_sun = sum(sunPres, na.rm=TRUE))
# 
# q1 <- q[c(1,2)]
# q1$type <- "difRad"
# setnames(q1, old="mean_difRad", new="measure")
# q2 <- q[c(1,3)]
# q2$type <- "dirRad"
# setnames(q2, old="mean_dirRad", new="measure")
# q3 <- rbind(q1,q2)
