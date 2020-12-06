######################################################
# Purpose: Create vertical profiles of climate variables for ForestGEO plot using NEON tower data
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 - First created August 2020
## Aug-Sept. 2020
######################################################
#install needed packages
library(data.table)
library(readxl)
library(ggplot2)
library(ggpubr)

#
#1. Download and save neon data ####
## this is filtered to only the columns needed for plots 
## (mean measurements per each 30 min observation) in order to not have massive files.
# source("scripts/get_neon_data.R")
# 
# pbapply::pblapply(sites, function(st){
#   full_data <- get_NEON(x=st)
#   save(full_data, file=paste0(st, "test.Rdata"))
# })

#2. Organize data for plotting ####

data <- fread("forested_NEON_sites.csv")
data <- data[include==1, ]
sites <- data[,site]

#bring in normalized height
meta <- read_excel("data/site_data/TIS site metadata_20190403_forNOAA.xlsx", sheet=1)
meta <- data.table(meta)
meta <- meta[SITE %in% data[,site], 
             ][,DistZaxsCnpy := as.numeric(DistZaxsCnpy)]

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
    
    #change colnames for SWIR so that the variable name is in the colnames
    if(i==6){ #SWIR
      setnames(var, old="inSWMean", new="SWIR")
    }
    
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

    ## bring in normalized height
    whei <- out[, norm_height :=
                   round(verticalPosition /
                           meta[SITE == sites[j], DistZaxsCnpy], 2)
                 ][, month_char := ifelse(month_num==1, "January", "July")]
    whei <- whei[, var := dp[,name][i]]
    allstats <- rbind(allstats, out)
    
    #get mean change of max/min variable by height by month by year
    ## monthly mean [(daily max at h_x)-(daily max at h_0)]
    
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
                 ][, month_char := ifelse(month_num==1, "January", "July")]
    
    # for (j in seq_len(ncol(whei))){
    #   set(whei,which(is.na(whei[[j]])),j,0)
    # }
    
    # whei <- whei[complete.cases(whei),]
  
    # allstats <- rbind(allstats, whei)
    
    #plot
    plots[[i]] <- local({
      graph <- ggplot(whei) +
        scale_color_manual(values = c("darkorange", "red"), 
                           name = "Month") +
        ylim(0, ceiling(topHeight/normHeight)) +
        geom_hline(yintercept=1, linetype="dotted") +
        geom_point(aes(x = delta_max_mean, y = norm_height, color = month_char), 
                   shape=19) +
        geom_point(aes(x = delta_min_mean, y = norm_height, color = month_char), 
                   shape=17) +
        geom_path(aes(x = delta_max_mean, y = norm_height, color = month_char, 
                      linetype = "Max"), size = 1) +
        geom_path(aes(x = delta_min_mean, y = norm_height, color = month_char, 
                      linetype = "Min"), size = 1) +
        ggplot2::geom_errorbarh(aes(xmin = delta_max_mean - delta_min_sd, 
                                    xmax = delta_max_mean + delta_max_sd, 
                                    y=norm_height, color = month_char, 
                                    linetype = "Max", height=0.1)) +
        
        ggplot2::geom_errorbarh(aes(xmin=delta_min_mean - delta_min_sd, 
                                    xmax=delta_min_mean + delta_max_sd, 
                                    y=norm_height, color = month_char, 
                                    linetype = "Min", height=0.1)) +
        # labs(x = expression(paste(Delta, "Climate Variable")), 
        #      y = "Normalized Height") +
        labs(x = "",
             y = "") +
        ggtitle(dp[,name][i]) +
      # scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits=c(0,60)) +
      theme_bw() +
        guides(linetype = guide_legend("Line type")) 
      
      # print(graph)
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
  
  p <- ggarrange(plots[[1]], plots[[2]], plots[[3]], 
                 plots[[4]], plots[[5]], plots[[6]],
                 nrow=2, ncol=3, common.legend=TRUE,
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

# only write out HARV monthly mean max (add to loop)
