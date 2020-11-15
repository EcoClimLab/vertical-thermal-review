######################################################
# Purpose: Create conglomerate graphs for McGregor et al 2019
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 - First created July 2019
######################################################
library(ggplot2)

#1 Figure 1: Distribution of resistance values and time series
## necessary packages ####
library(ggplot2)
library(png)
library(grid)
library(gridExtra)
library(cowplot)

##1a make density plot ####
trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv")
trees_all_sub$year <- as.character(trees_all_sub$year)

#In each drought, roughly 30% of the cored trees
#Find n trees that have resistance <= 0.7 per year: #% in 1966, #2% in 1977, and #% in 1999. #Do same thing for those that have resistance >1 for each year
library(data.table)
test <- as.data.table(trees_all_sub)

test_reduct <- test[resist.value<=0.7,.(reduct = length(resist.value)), by=.(year)]
by_year <- test[, .(total=length(resist.value)), by=.(year)]
test_reduct$total_re <- by_year$total
test_reduct$perc_re <- test_reduct$reduct/test_reduct$total_re

test_growth <- test[resist.value>1,.(growth = length(resist.value)), by=.(year)]
test_growth$total_gr <- by_year$total
test_growth$perc_gr <- test_growth$growth/test_growth$total_gr

test_full <- merge(test_reduct, test_growth)

test[,.(ran = range(resist.value, na.rm=TRUE)), by=.(year)]
test[,.(mean = mean(resist.value, na.rm=TRUE)), by=.(year)]

Mode <- function(x) {
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
}

# apply(d, 2, Mode)
# test[,.(mode = Mode(resist.value)), by=.(year)]

#save as png so that way you're arranging only png
png("manuscript/tables_figures/publication/Figure1b_density_plot.png", res = 300, width = 150, height = 50, units = "mm", pointsize = 10)
ggplot(trees_all_sub) +
   aes(x = resist.value, fill = year) +
   geom_density(adjust = 1L, alpha=.5) +
   geom_vline(xintercept=1) +
   scale_fill_hue(labels=c("1966", "1977", "1999")) +
   labs(x="resistance value") +
   theme_minimal() +
   annotate(geom="text", x=0.1, y=1.5, 
            label = "(b)", fontface="bold", size=7)
dev.off()

plot1 <- readPNG("manuscript/tables_figures/publication/Figure1a_Time_series_for_each_species.png")
plot2 <- readPNG("manuscript/tables_figures/publication/Figure1b_density_plot.png")

png("manuscript/tables_figures/publication/Figure1.png", res=300, height=200, width=150, units="mm", pointsize=10)
g <- plot_grid(rasterGrob(plot1), rasterGrob(plot2), align = "v", nrow = 2, rel_heights = c(3/4, 1/4), axis = "b")
g + annotate(geom="text", x=0.13, y=0.99, 
             label = "(a)", fontface="bold", size=3) +
   annotate(geom="text", x=0.13, y=0.23, 
            label = "(b)", fontface="bold", size=3)
dev.off()

##1b Figure 3: Distribution by species and anova of Rt by sp ####
library(data.table)
library(agricolae)
rt <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE)
rt$year <- as.character(rt$year)
x1966 <- rt[rt$year == 1966, ]
x1977 <- rt[rt$year == 1977, ]
x1999 <- rt[rt$year == 1999, ]
model_df <- list(x1966, x1977, x1999)
names(model_df) <- c("x1966", "x1977", "x1999")

varlist <- list()
for(i in 1:3){
   anovout <- aov(resist.value ~ sp, data=model_df[[i]])
   
   hsdout <- HSD.test(anovout, "sp", group=TRUE)
   grouptab <- hsdout$groups
   grouptab$var <- names(model_df[i])
   grouptab <- grouptab[order(rownames(grouptab)), ]
   grouptab$groups <- as.character(grouptab$groups)
   
   varlist[[i]] <- grouptab
}
names(varlist) <- names(model_df)
varlist[["x1966"]]

#all droughts per species
q <- ggplot(rt) +
   aes(x = sp, y = resist.value) +
   geom_boxplot(aes(fill = year), alpha=0.5) +
   scale_color_discrete() +
   ylab("Rt") +
   xlab("Species") +
   theme_minimal() +
   theme(axis.text = element_text(size=14),
         axis.title=element_text(size=16,face="bold"),
         legend.text=element_text(size=14),
         legend.title=element_text(size=14))

q <- q +
   annotate("text", 
            x=1:12, y= 2.3, size=5,
            label=varlist[["x1966"]]$groups,
            color="#FF9999") +
   annotate("text", 
            x=1:12, y= 2.2, size=5,
            label=varlist[["x1977"]]$groups,
            color="#009900") +
   annotate("text", 
            x=1:12, y= 2.1, size=5,
            label=varlist[["x1999"]]$groups,
            color="#6699CC")

png("manuscript/tables_figures/publication/Figure3_rt_across_sp.png", width=960)
q
dev.off()

rt <- as.data.table(rt)
avg_rt <- rt[, .(avg_rt = round(mean(resist.value),2)), by=.(sp,year)]
setnames(avg_rt, old=c("sp", "avg_rt"), new=c("Species", "Mean Rt"))
write.csv(avg_rt ,"manuscript/tables_figures/publication/mean_rt_by_sp.csv", 
          row.names = FALSE)

##########################################################################
## SI figure anova results full model ####
trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE)

x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")

bleh <- lapply(model_df, function(x) aov(resist.value ~ sp, data=x))


anova_out <- lapply(c(1:4), function(x){
   output <- TukeyHSD(bleh[[x]])
   ne <- as.data.frame(output$sp)
   out <- data.frame(occ = rownames(ne),
                     txt = ne$`p adj`)
   colnames(out) <- c("occ", names(model_df)[x])
   out$occ <- as.character(out$occ)
   
   
   return(out)
})

full <- Reduce(function(...) merge(...,by="occ"), anova_out)

ggplot(full) +
   aes(x=occ, y=trees_all_sub) +
   geom_point(aes(color="all")) +
   geom_point(aes(y=x1966, color="1966")) +
   geom_point(aes(y=x1977, color="1977")) +
   geom_point(aes(y=x1999, color="1999")) +
   geom_jitter() +
   scale_colour_manual(breaks = c("all", "1966", "1977", "1999"),
                       values = c("black", "red", "green", "blue")) +
   ylab("P value") +
   xlab("") +
   geom_abline(slope=0, intercept=0.05) +
   theme(axis.text.x = element_text(angle = 90))


# Figure S4. anova of traits compared to sp ####
library(data.table)
library(ggplot2)
library(agricolae)
rt <- fread("manuscript/tables_figures/trees_all_sub.csv")
traits_hydr <- fread("https://raw.githubusercontent.com/EcoClimLab/HydraulicTraits/master/data/SCBI/processed_trait_data/SCBI_all_traits_table_indvidual_level.csv?token=AJNRBEPQVCRRCIDQSBGNC2S7EC5A2")
traits_hydr <- traits_hydr[,.(sp,PLA_dry_percent, mean_TLP_Mpa, 
                              WD_g_per_cm3, LMA_g_per_m2)]

varlist <- list()
var <- c("height.ln.m", "TWI.ln", "PLA_dry_percent", "mean_TLP_Mpa",
         "WD_g_per_cm3", "LMA_g_per_m2")
for(i in 1:6){
   
   if(i %in% c(1,2)){
      anovout <- aov(get(var[i]) ~ sp, data=rt)
   } else {
      anovout <- aov(get(var[i]) ~ sp, data=traits_hydr)
   }
   hsdout <- HSD.test(anovout, "sp", group=TRUE)
   grouptab <- hsdout$groups
   grouptab$var <- var[i]
   grouptab <- grouptab[order(rownames(grouptab)), ]
   grouptab$groups <- as.character(grouptab$groups)
   
   varlist[[i]] <- grouptab
}
names(varlist) <- var
# View(varlist[["height.ln.m"]])

rt <- rt[,year := as.character(year)]
traits <- c("height.ln.m", "TWI.ln")
ylabs <- c("ln[H]", "ln[TWI]")

gglist <- list()
for(i in 1:2){
   varnm <- traits[[i]]
   gglist[[i]] <- local({
      i <- i
      q <- 
         ggplot(rt) +
         aes(x = sp, y = get(traits[i])) +
         geom_boxplot(alpha=0.5) +
         scale_color_discrete() +
         ylab(ylabs[i]) +
         xlab("Species") +
         theme_minimal() +
         theme(axis.text = element_text(size=12),
               axis.text.x=element_text(angle=90),
               axis.title=element_text(size=12, face="bold"),
               legend.text=element_text(size=12),
               legend.title=element_text(size=12))
      
      if(i==1){ypos <- 4.1} else {ypos <- 2.75}
      
      q <- q +
         annotate("text", 
                  x=1:12, y= ypos, size=3, angle=45,
                  label=varlist[[varnm]]$groups)
      
      print(q)
   })
}
pos <- rt[,.(sp,position_all)
          ][,.N,by=.(sp,position_all)
            ][order(sp),]
others <- data.table(sp=c("caco", "cato", "cato", "fagr", 
                          "fram", "juni", "juni"),
                     position_all=c("dominant", "dominant", 
                                    "co-dominant","dominant", 
                                    "dominant", "dominant", 
                                    "suppressed"),
                     N=c(0,0,1,0,0,0,1))
pos <- rbind(pos, others)

library(viridis)
cols <- viridis(4)
posplot <-  
   ggplot(pos) +
   aes(x = sp, y=N) +
   geom_point(aes(fill=position_all), color="black", pch=21, size=3) +
   # geom_jitter() +
   scale_fill_manual(values=cols) +
   ylab("Count") +
   xlab("Species") +
   scale_y_continuous(limits = c(-4,150), expand = c(0, 0)) +
   theme_minimal() +
   theme(axis.text = element_text(size=12),
         axis.text.x=element_text(angle=90),
         axis.title=element_text(size=12, face="bold"),
         legend.text=element_text(size=12),
         legend.title=element_blank(),
         legend.position = "bottom")
   

hydr <- c("PLA_dry_percent", "mean_TLP_Mpa")
ylabs <- c(expression(PLA[dry]), expression(pi[TLP]))
ggtraits <- list()
for(i in 1:2){
   varnm <- hydr[[i]]
   ggtraits[[i]] <- local({
      i <- i
      
      q <- 
         ggplot(traits_hydr) +
         aes(x = sp, y = get(hydr[i])) +
         geom_boxplot(alpha=0.5) +
         scale_color_discrete() +
         ylab(ylabs[i]) +
         xlab("Species") +
         theme_minimal() +
         theme(axis.text = element_text(size=12),
               axis.text.x=element_text(angle=90),
               axis.title=element_text(size=12, face="bold"),
               legend.text=element_text(size=12),
               legend.title=element_text(size=12))
      
      if(i==1){ypos <- 32} else {ypos <- -1.68}
      q <- q +
         annotate("text", 
                  x=1:12, y= ypos, size=3, angle=45,
                  label=varlist[[varnm]]$groups)
      print(q)
   })
}

library(ggpubr)
p <- ggarrange(gglist[[1]], gglist[[2]],
          ggtraits[[1]], ggtraits[[2]],
          ncol = 2, nrow = 2,
          labels=c("(a)", "(b)", "(c)", "(d)"),
          label.y=c(0.91,0.91,0.91,0.91))
png("manuscript/tables_figures/publication/Figure5_traits_signif.png", width=960, height=480)
ggarrange(p, posplot, ncol=2, nrow=1, labels=c("", "(e)"), label.y=c(0.95))
dev.off()
#########################################################################
#2. Figure 2: NEON vertical height profiles with 
## necessary packages and define 95% quantile height ####
library(ggpubr)
library(extrafont)
library(rasterVis)
library(RCurl)
library(stringi)

loadfonts(device="win") #to get TNR

# quantile(current_ht$height.m, c(.95), na.rm=TRUE) #95% quantile = 38.34479m
quant <- data.frame(yintercept = 38.34479, Lines = "95th percentile")

##2a. Load NEON plots ####
# source('scripts/vertical_height_neon.R', echo=TRUE)
load("data/physical/neondata.Rdata")
load("data/physical/neonplots.Rdata")
NEON_list <- plotlist; rm(plotlist)
#
##2ai. run anova (only necessary if need to recalculate what's sig) ####
# library(data.table)
# library(lubridate)
# 
# value <- c("windSpeedMean", "RHMean", "tempSingleMean")
# month_var <- c("May", "Jun", "Jul", "Aug")
# 
# dtfull <- NULL
# for(i in 1:3){
#    dt <- as.data.table(alldt[[i]])
#    dt <- dt[, month := as.character(lubridate::month(
#       startDateTime, label=TRUE))]
#    
#    dt_full_sub <- NULL
#    for(k in 1:4){
#       dt_sub <- dt[month==month_var[k], ]
#       month_sub <- month_var[k]
#       
#       dtout_sub <- NULL
#       for(j in 1:2){
#          if(j==1){
#             
#             #get min value per day per verticalPosition
#             neon <- dt_sub[,.(val = min(get(value[i]), na.rm=TRUE)),
#                        by=.(day, verticalPosition)
#                        ][,val := ifelse(grepl("Inf", val), NA, val)]
#             type <- "min"
#             
#             small <- neon[verticalPosition == min(verticalPosition, 
#                                                   na.rm=TRUE), 
#                           .(verticalPosition, val)]
#             large <- neon[verticalPosition == max(verticalPosition, 
#                                                   na.rm=TRUE), 
#                           .(verticalPosition, val)]
#             testdt <- rbind(small, large)
#             
#          } else {
#             #get max value per day per verticalPosition
#             neon <- dt_sub[,.(val = max(get(value[i]), na.rm=TRUE)),
#                        by=.(day, verticalPosition)
#                        ][,val := ifelse(grepl("Inf", val), NA, val)]
#             type <- "max"
#             
#             small <- neon[verticalPosition == min(verticalPosition, 
#                                                   na.rm=TRUE), 
#                           .(verticalPosition, val)]
#             large <- neon[verticalPosition == max(verticalPosition, 
#                                                   na.rm=TRUE), 
#                           .(verticalPosition, val)]
#             testdt <- rbind(small, large)
#          }
#          
#          #now check if the values at 10m are signif diff from values at 60m
#          #are the data normal? if pvalue is sig, then no.
#          #if normal, then can do t-test. Good pval means yes means are sig diff
#          #if not normal, then need to run wilcox. Good pval means the two
#          # groups are sign diff
#          
#          if(nrow(neon)>0){
#             ss <- shapiro.test(small$val)
#             sl <- shapiro.test(large$val)
#          } else {
#             ss <- NA
#             sl <- NA
#          }
#          
#          if(all(is.na(ss) & is.na(sl))){
#             final <- NA
#             outcome <- "NA"
#             typetest <- "no data"
#             ssp <- NA
#             slp <- NA
#             finalp <- NA
#          } else if(all(ss$p.value < 0.05 & sl$p.value < 0.05)){
#             final <- wilcox.test(val ~ verticalPosition, data=testdt)
#             outcome <- ifelse(final$p.value < 0.05, "yes", "no")
#             typetest <- "wilcox"
#             ssp <- ss$p.value
#             slp <- sl$p.value
#             finalp <- final$p.value
#          } else {
#             final <- t.test(val ~ verticalPosition, data=testdt)
#             outcome <- ifelse(final$p.value < 0.05, "yes", "no")
#             typetest <- "t-test"
#             ssp <- ss$p.value
#             slp <- sl$p.value
#             final <- final$p.value
#          }
#          
#          dtout <- data.table(variable=value[i],
#                              valtype = type,
#                              month = month_sub, 
#                              shapiro_smallht_p = ssp,
#                              shapiro_largeht_p = slp,
#                              test = typetest,
#                              full_p = finalp,
#                              signif_diff = outcome) 
#          dtout_sub <- rbind(dtout_sub, dtout)
#       }
#       dt_full_sub <- rbind(dt_full_sub, dtout_sub)
#    }
#    dtfull <- rbind(dtfull, dt_full_sub)
# }
# 

##2b. Format the NEON plots ####
NEON_order <- c("(a)", "(b)", "(c)")
NEON_order_x <- c(0.35, 30, 6)
NEON_order_y <- c(57.5, 57.5, 57.5)

for (i in seq(along=1:3)){
   NEON_list[[i]] <-
      NEON_list[[i]] +
      theme_bw(base_size = 16) + 
      # theme_bw(base_family = "serif") + #for TNR font
      geom_hline(aes(yintercept = yintercept), linetype = "longdash", quant) +
      annotate(geom="text", x=NEON_order_x[[i]], y=NEON_order_y[[i]], 
               label = NEON_order[[i]], fontface="bold", size=7)
   if(i==1){
      NEON_list[[i]] <-
         NEON_list[[i]] +
         geom_point(aes(x=c(0.4), y=c(44), shape=8), color="blue", size=3) +
         geom_point(aes(x=c(0.3), y=c(42), shape=8), color="darkgreen", size=3) +
         geom_point(aes(x=c(0.5), y=c(42), shape=8), color="darkorange", size=3) +
         geom_point(aes(x=c(0.6), y=c(44), shape=8), color="red", size=3) +
         geom_point(aes(x=c(4), y=c(42), shape=8), color="blue", size=3) +
         geom_point(aes(x=c(4.6), y=c(42), shape=8), color="darkgreen", size=3) +
         geom_point(aes(x=c(5.1), y=c(42), shape=8), color="red", size=3) +
         geom_point(aes(x=c(5.5), y=c(42), shape=8), color="darkorange", size=3) +
         scale_shape_identity() +
         theme(legend.title = element_blank(),
               legend.box = "vertical",
               legend.position = c(0.8, 0.25),
               legend.background = element_rect(fill=alpha("white", 0.01)),
               legend.text=element_text(size=12),
               legend.key.size=unit(4,"mm"),
               legend.spacing=unit(-0.5,"cm")
         )
   }
   
   if(!i==1){
      NEON_list[[i]] <-
         NEON_list[[i]] +
         theme(legend.position = "none")
   }
   
   if(i==2){
      NEON_list[[i]] <- 
         NEON_list[[i]] +
         geom_point(aes(x=c(40.5), y=c(55), shape=8), color="red", size=3) +
         geom_point(aes(x=c(76), y=c(55), shape=8), color="red", size=3) +
         geom_point(aes(x=c(44), y=c(55), shape=8), color="darkgreen", size=3) +
         geom_point(aes(x=c(83), y=c(55), shape=8), color="darkgreen", size=3) +
         geom_point(aes(x=c(53.5), y=c(55), shape=8), color="blue", size=3) +
         scale_shape_identity() +
         theme(axis.title.y = element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank())
   }
   #may = darkorange, june=red, july=darkgreen, aug=blue
   if(i==3){
      NEON_list[[i]] <- 
         NEON_list[[i]] +
         geom_point(aes(x=c(12.25), y=c(42), shape=8), color="darkorange", size=3) +
         geom_point(aes(x=c(25), y=c(42), shape=8), color="red", size=3) +
         geom_point(aes(x=c(26), y=c(42), shape=8), color="blue", size=3) +
         scale_shape_identity()
   }
}

NEON <- ggarrange(NEON_list$wind_plot, NEON_list$RH_plot, NEON_list$SAAT_plot,  nrow=2, ncol=2, align="h")

##2c. Get the boxplots for 2018 heights (this is also prep for #3) ####
trees_all <- read.csv("manuscript/tables_figures/trees_all.csv", stringsAsFactors = FALSE) #these graphs are meant to be for all cored trees, not just the ones being used in analysis

scbi.stem3 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"), stringsAsFactors = FALSE)

scbi.stem3$dbh <- as.numeric(scbi.stem3$dbh)

current_ht <- trees_all[!duplicated(trees_all$tree), ]
current_ht$year <- 2018
current_ht <- current_ht[,c(1:4,16:18)]

current_ht$dbh_old.mm <- scbi.stem3$dbh[match(current_ht$tree, scbi.stem3$tag)]
current_ht$dbh_old.cm <- current_ht$dbh_old.mm/10
current_ht$year_dbh <- ifelse(!is.na(current_ht$dbh_old.cm), 2018, NA)

#get original dbh if they died before 2018 (only need 2013)
scbi.stem2 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem2.csv"), stringsAsFactors = FALSE)

current_ht$dbh_old.mm <- ifelse(is.na(current_ht$dbh_old.mm),
                                scbi.stem2$dbh[match(current_ht$tree, scbi.stem2$tag)],
                                current_ht$dbh_old.mm)
current_ht$dbh_old.cm <- ifelse(is.na(current_ht$dbh_old.cm),
                                current_ht$dbh_old.mm/10,
                                current_ht$dbh_old.cm)
current_ht$year_dbh <- ifelse(is.na(current_ht$year_dbh), 2013, current_ht$year_dbh)

#get the log dbh and get heights
current_ht$dbh.ln.cm <- log(current_ht$dbh_old.cm)

#linear log-log regression
#the full equation is using all points for which we have data to create the equation, despite that for several species we don't have enough data to get a sp-specific equation

#this csv is created from #4 of canopy_heights.R
height_regr <- read.csv("manuscript/tables_figures/publication/tableS2_height_regression.csv", stringsAsFactors = FALSE)
height_regr$sp <- as.character(c("caco", "cagl", "caovl", "cato",
                                 "fagr", "litu", "qual", "qupr",
                                 "quru", "all"))

current_ht$height.ln.m <- NA
for(w in seq(along=height_regr$sp)){
   sp_foc <- height_regr$sp[[w]]
   ht_eq <- height_regr[height_regr$sp == sp_foc, ]
   num <- gsub("\\*x", "", ht_eq$Equations)
   num1 <- as.numeric(stri_extract_first_regex(num, "[[:digit:]].[[:digit:]]+"))
   num2 <- as.numeric(stri_extract_last_regex(num, "[[:digit:]].[[:digit:]]+"))
   
   if(sp_foc %in% current_ht[,"sp"]){
      current_ht$height.ln.m <- 
         ifelse(current_ht$sp == sp_foc,
                num1 + num2*current_ht$dbh.ln.cm,
                current_ht$height.ln.m)
   } else {
      current_ht$height.ln.m <- 
         ifelse(current_ht$sp != sp_foc,
                num1 + num2*current_ht$dbh.ln.cm,
                current_ht$height.ln.m)
   }
}

current_ht$height.m <- exp(current_ht$height.ln.m)
current_ht <- current_ht[order(current_ht$tree, current_ht$year), ]

#graphing height by crown position (for paper)
current_ht <- current_ht[!is.na(current_ht$position_all), ]
current_ht$position_all_abb <- substring(current_ht$position_all, 0, 1)
current_ht$position_all_abb <- toupper(current_ht$position_all_abb)
current_ht$position_all_abb <- factor(current_ht$position_all_abb, levels = c("D", "C", "I", "S"))

heights_box <-
   ggplot(data = current_ht, aes(x = position_all_abb, y = height.m, group = position_all)) +
   # aes(x=position_all, y=height.m, fill=year) +
   geom_boxplot() +
   xlab("Crown position") +
   ylab("Height [m]") +
   scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits=c(0,60)) +
   scale_x_discrete(labels=c("dominant", "co-dominant",
                             "intermediate", "suppressed"),) +
   theme_minimal() +
   theme(axis.text = element_text(size=12)) +
   theme(axis.title = element_text(size=14))

##2ci. anova for height vs crown position ####
anovout <- aov(height.ln.m ~ position_all_abb, data=current_ht)

output <- TukeyHSD(anovout)
ne <- as.data.frame(output$position_all_abb)
##2d. Format the height boxplot and add to NEON, export ####
heights_box <-
   heights_box + 
   theme_bw(base_size = 16) + 
   # theme_bw(base_family = "serif") + #for TNR font
   geom_hline(aes(yintercept = yintercept), linetype = "longdash", quant) +
   annotate(geom="text", x=0.65, y=57.5, 
            label = "(d)", fontface="bold", size=7) +
   annotate(geom="text", x=1:4, y=60, label = c("A","B","C","D"), size=4) +
   # annotate(geom="text", x=2, y=60, label = "B", size=4) +
   # annotate(geom="text", x=3, y=60, label = "C", size=4) +
   # annotate(geom="text", x=4, y=60, label = "D", size=4) +
   # geom_point(aes(x=c(1), y=c(60), shape=8), size=3) +
   # geom_point(aes(x=c(2), y=c(60), shape=8), size=3) +
   # geom_point(aes(x=c(3), y=c(60), shape=8), size=3) +
   # geom_point(aes(x=c(4), y=c(60), shape=8), size=3) +
   # scale_shape_identity() +
   theme(axis.title.y = element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())

###put plots together
png("manuscript/tables_figures/publication/Figure2_NEON_vars_height_profile.png", width=11, height=11, units="in", res=300)
ggarrange(NEON_list$wind_plot, NEON_list$RH_plot, NEON_list$SAAT_plot, heights_box,
          nrow=2, ncol=2, align="h")
dev.off()

#########################################################################
#3. Figure S2: Export map of plot using TWI and cored tree locations
## necessary packages ####
library(raster)
library(rasterVis)

##3a. create map ####
topo <- raster("data/physical/plot_TWI.tif")

species <- read.csv("data/core_files/core_list.csv", stringsAsFactors = FALSE)
species <- species[!species$sp %in% c("frni", "pist"), ]
species$sp_fact <- as.factor(species$sp)

color_pallete_function <- colorRampPalette(
   colors = c("pink","lightskyblue", "turquoise", "springgreen", "white", "khaki", "gold", "brown"),
   space = "Lab" # Option used when colors do not represent a quantitative scale
)
species_colors <- color_pallete_function(nlevels(species$sp_fact))
# coltab <- data.frame(sp = unique(cored$sp), cols = species_colors)
# 
# cored$cols <- as.character(coltab$cols[match(cored$sp, coltab$sp)])

cored_points <- SpatialPointsDataFrame(data.frame(species$NAD83_X, species$NAD83_Y), data=species)

plot.new()
t <- levelplot(topo, margin=FALSE, scales=list(draw=FALSE),
               key=list(space="right",
                        text=list(lab=levels(cored_points@data$sp_fact)),
                        points=list(pch=20, fill=species_colors, 
                                    col=species_colors)),
               colorkey=list(space="left", width=0.75, height=0.75)) +
   layer(sp.points(cored_points, pch=20, 
                   col = species_colors[species$sp_fact]))
#add scale bar at bottom
t <- t +
   layer({
      xs <- seq(747553, 747653, by=20)
      grid.rect(x=xs, y=4308515,
                width=20, height=5,
                gp=gpar(fill=rep(c("white", "black"), 3)),
                default.units='native')
      grid.text(x= xs - 5, y=4308530, seq(0, 100, by=20),
                gp=gpar(col="white", cex=0.5),
                default.units='native')
   })

png("manuscript/tables_figures/publication/figureS2_location_cored_trees.png", width=5, height=7, units="in", res=300)

t + #add north arrow
   layer({
      SpatialPolygonsRescale(layout.north.arrow(),
                             offset = c(747380,4309123),
                             scale = 40,
                             col="white")
   })
dev.off()

########################################################################
#4. Other Figure (not in pub): height by crown position in 2018
## necessary packages ####
library(RCurl) #2
library(tidyr) #2
library(grid) #2
library(gridExtra) #2
library(data.table)
library(stringi)
library(ggplot2)

##4a. heights for all cored trees ####
##4b. Figure S3: combine heights to make plot with all years ####

trees_all$position_all_abb <- ifelse(trees_all$position_all == "dominant", "D",
                                     ifelse(trees_all$position_all == "co-dominant", "C",
                                            ifelse(trees_all$position_all == "suppressed", "S", "I")))
trees_all_plot <- trees_all[,c("tree", "year", "position_all_abb", "height.m", "dbh_old.cm")]

current_ht$position_all_abb <- ifelse(current_ht$position_all == "dominant", "D",
                                      ifelse(current_ht$position_all == "co-dominant", "C",
                                             ifelse(current_ht$position_all == "suppressed", "S", "I")))
current_ht_sub <- current_ht[,c("tree", "year", "position_all_abb", "height.m", "dbh_old.cm")]


#make plot
heights_allplot <- rbind(trees_all_plot, current_ht_sub)
heights_allplot$position_all_abb <- factor(heights_allplot$position_all_abb, levels=c("D","C","I","S"))
heights_allplot$year <- as.character(heights_allplot$year)
heights_allplot <- heights_allplot[order(heights_allplot$tree, heights_allplot$year), ]

png("manuscript/tables_figures/figureS3_height_plot_analysis.png")
ggplot(na.omit(heights_allplot), aes(position_all_abb, height.m)) +
   geom_boxplot(aes(fill=year)) +
   xlab("Crown position") +
   ylab("Height [m]") +
   ggtitle("Height comparison with only trees in analysis")
dev.off()


heights_allplot$tree <- as.character(heights_allplot$tree)
heights_allplot$position_all_abb <- as.character(heights_allplot$position_all_abb)

########################################################################
#5. Figure 4. Visualizing regression output ####
library(lme4)
library(ggpubr)
# remotes::install_github("pbreheny/visreg")
library(visreg)

## https://pbreheny.github.io/visreg/cross.html

trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE); arima_vals=FALSE
# trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub_arimaratio.csv", stringsAsFactors = FALSE); arima_vals=TRUE
top_models <- read.csv("manuscript/tables_figures/top_models_dAIC_lmer_CPout.csv", stringsAsFactors = FALSE)
top_models <- top_models[top_models$Delta_AICc==0, ]

x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")

fitall <- lmer(top_models[,"Modnames"][1], 
              data = model_df[[1]], REML=TRUE, 
              control = lmerControl(optimizer ="Nelder_Mead"))
fit66 <- lmer(top_models[,"Modnames"][2], 
               data = model_df[[2]], REML=TRUE, 
               control = lmerControl(optimizer ="Nelder_Mead"))
fit77 <- lmer(top_models[,"Modnames"][3], 
               data = model_df[[3]], REML=TRUE, 
               control = lmerControl(optimizer ="Nelder_Mead"))
fit99 <- lmer(top_models[,"Modnames"][4], 
               data = model_df[[4]], REML=TRUE, 
               control = lmerControl(optimizer ="Nelder_Mead"))

library(bootpredictlme4)
predict(fitall, re.form=NA, se.fit=TRUE, nsim=1000)

#plot fits for each drought scenario models separately (and overlay with visreg)
v <- visregList(visreg(fit66, 'height.ln.m', plot=FALSE),
                visreg(fitall, 'height.ln.m', plot=FALSE),
                   labels=rev(c("All", "1966")),
                   collapse=TRUE)
q_ht <- plot(v, overlay=TRUE, gg=TRUE, 
             partial=FALSE, rug=FALSE,
             fill.par=list(alpha=0.2)) +
   # guides(fill=FALSE) +
   scale_color_manual(
      values = rev(c("black", "#FF9999")),
      labels=rev(c("All", "1966")),
      name="Droughts") +
   scale_fill_manual(values=rev(c("black", "#FF9999"))) +
   # guides(color=guide_legend(override.aes=list(fill=NA))) +
   ylab("Rt") + xlab("ln[H]") +
   ylim(0.5, 1.2) + geom_hline(yintercept=1, lty=2) +
   theme_minimal() +
   theme(legend.position="none",
         axis.text = element_text(size=12),
         axis.title = element_text(size=14))

## TWI
v <- visregList(visreg(fit99, 'TWI.ln', plot=FALSE),
                visreg(fit77, 'TWI.ln', plot=FALSE),
               visreg(fitall, 'TWI.ln', plot=FALSE),
                labels=rev(c("ALL", "1977", "1999")),
                collapse=TRUE)
q_twi <- plot(v, overlay=TRUE, gg=TRUE, 
              partial=FALSE, rug=FALSE,
              fill.par=list(alpha=0.2)) +
   guides(fill=FALSE) +
   scale_color_manual(
      values = rev(c("black", "#009900", "#6699CC")),
      labels=rev(c("All", "1977", "1999")),
      name="Droughts") +
   scale_fill_manual(values=rev(c("black", "#009900", "#6699CC"))) +
   guides(color=guide_legend(override.aes=list(fill=NA))) +
   ylab("Rt") + xlab("ln[TWI]") +
   ylim(0.5, 1.2) + geom_hline(yintercept=1, lty=2) +
   theme_minimal() +
   theme(legend.position="none",
         axis.text = element_text(size=12),
         axis.title = element_text(size=14))

##PLA
v <- visregList(
   visreg(fit66, 'PLA_dry_percent', plot=FALSE),
   visreg(fitall, 'PLA_dry_percent', plot=FALSE),
   labels=rev(c("All", "1966")), 
                collapse=TRUE)
q_pla <- plot(v, overlay=TRUE, gg=TRUE, 
              partial=FALSE, rug=FALSE, 
              fill.par=list(alpha=0.2),) +
   guides(fill=FALSE) +
   scale_color_manual(
      values = rev(c("black", "#FF9999")),
      labels=rev(c("All", "1966")),
      name="Droughts") +
   scale_fill_manual(values=rev(c("black", "#FF9999"))) +
   guides(color=guide_legend(override.aes=list(fill=NA))) +
   ylab("Rt") + xlab(expression(PLA[dry])) +
   ylim(0.5, 1.2) + geom_hline(yintercept=1, lty=2) +
   theme_minimal() +
   theme(legend.position="none",
         axis.text = element_text(size=12),
         axis.title = element_text(size=14))

##TLP
v <- visregList(visreg(fit77, 'mean_TLP_Mpa', plot=FALSE),
               visreg(fitall, 'mean_TLP_Mpa', plot=FALSE),
                # visreg(fit99, 'mean_TLP_Mpa', plot=FALSE),
                labels=c("1977", "All"),
                collapse=TRUE)
q_tlp <- plot(v, overlay=TRUE, gg=TRUE, 
              partial=FALSE, rug=FALSE,
              fill.par=list(alpha=0.2)) +
   guides(fill=FALSE) +
   scale_color_manual(
      values = rev(c("black", "#009900")),
      labels=rev(c("All", "1977")),
      name="Droughts") +
   scale_fill_manual(values=rev(c("black", "#009900"))) +
   guides(color=guide_legend(override.aes=list(fill=NA))) +
   ylab("Rt") + xlab(expression(pi[TLP])) +
   ylim(0.5, 1.2) + geom_hline(yintercept=1, lty=2) +
   theme_minimal() +
   theme(legend.position="none",
         axis.text = element_text(size=12),
         axis.title = element_text(size=14))

## make legend
library(grid)
library(gridExtra)
trees_all_sub$year <- "all"
full <- rbind(trees_all_sub, x1966, x1977, x1999)
full$color <- 
   ifelse(full$year=="all", "black",
          ifelse(full$year=="1966", "#FF9999",
                 ifelse(full$year=="1977", "#009900","#6699CC")))
cols <- unique(full$color)

w <- ggplot(full) +
   aes(height.ln.m, resist.value) +
   geom_line(aes(color=year), size=1.2, alpha=0.8) +
   facet_wrap(~year) +
   scale_color_manual(name="Droughts",
                      labels=c("All", "1966", "1977", "1999"),
                      values = cols) +
   theme(legend.text = element_text(size=12),
         legend.title=element_text(size=14))
legend <- cowplot::get_legend(w)

## put all together
png("manuscript/tables_figures/publication/Figure4_model_vis.png", height=480, width=600)
arr <- ggarrange(q_ht, q_twi, q_pla, q_tlp, 
          labels = c("(a)", "(b)", "(c)", "(d)"),
          label.x=0.15,
          label.y=1,
          ncol = 2, nrow = 2,
          common.legend=FALSE)

grid.arrange(arr, legend, ncol=2, widths=c(4,1))

dev.off()


## Figure S6. only plot the all-years model with visreg ####
xl <- c("ln[H]", "ln[TWI]", "PLA", "TLP")
vars <- c("height.ln.m", "TWI.ln", "PLA_dry_percent", "mean_TLP_Mpa")

par(mar=c(4,4,2,2))
layout(matrix(c(1,2,3,4,5,5), nrow=3, ncol=2, byrow=TRUE),
       heights=c(0.4, 0.4, 0.2))
for(i in 1:4){
   visreg(fitall, vars[i],
          points=
             list(col=scales::alpha(c("#FF9999", "#009900", "#6699CC"), 0.5)),
          line=list(col="black", lwd=0.5), 
          fill=list(col="grey"),
          xlab="", ylab="")
   title(x=xl[i], y="Rt")
   abline(h=1, lty=2)
}

par(mar=c(1,1,1,1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- scales::alpha(c("#FF9999", "#009900", "#6699CC"), 0.5)
legend(x = "top",inset = 0,
       legend = c("1966", "1977", "1999"), 
       col=plot_colors, lwd=3, cex=1, horiz = TRUE)

ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], 
          labels = c("(a)", "(b)", "(c)", "(d)"),
          ncol = 2, nrow = 2,
          common.legend=FALSE)

## using ggplot (not ideal bc can't handle lmer) ####
library(ggplot2)
library(ggpubr)
trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE); arima_vals=FALSE
# trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub_arimaratio.csv", stringsAsFactors = FALSE); arima_vals=TRUE
top_models <- read.csv("manuscript/tables_figures/top_models_dAIC_reform.csv", stringsAsFactors = FALSE)
top_models <- top_models[top_models$Delta_AICc==0, ]

trees_all_sub$position_num <- 
   ifelse(trees_all_sub$position_all=="dominant", 1,
          ifelse(trees_all_sub$position_all=="co-dominant", 2,
                 ifelse(trees_all_sub$position_all=="intermediate", 3,4)))

x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")

vars <- c("height.ln.m", "position_all", "TWI.ln", "PLA_dry_percent", "mean_TLP_Mpa")
lab <- c("ln[H]", "CP", "ln[TWI]", "PLA", "TLP")
gglist <- list()
colors <- c("All" = "black", 
            "1966" = "red", 
            "1977" = "green", "1999" = "blue")
for(i in 1:5){
   gglist[[i]] <- local({
      i <- i
      q <- 
         ggplot(glmm_all[["trees_all_sub"]]) +
         aes(x=get(vars[i]), y=fit) +
         ylab("Rt") +
         xlab(lab[i]) +
         coord_cartesian(ylim=c(0.5,1.15))
      
      if(i %in% c(1:3)){ #1999
         if(i %in% c(1,3)){
            q <- 
               q + 
               geom_smooth(data=glmm_all[["x1999"]], 
                           aes(get(vars[i]), fit, color="blue"),
                           method=lm , color=NA, fill="blue", 
                           alpha=0.2, se=TRUE) +
               geom_line(data=glmm_all[["x1999"]], 
                         aes(get(vars[i]), fit, color="blue"),
                         stat = "smooth", method = lm,
                         color = "blue", size = 1.5, alpha = 0.2)
         } else if(i==2){
            short <- as.data.table(glmm_all[["x1999"]])
            short <- short[,.(avg=mean(fit)), by=.(position_all)]
            
            q <- q +
               geom_segment(aes(x=0.6,xend=1.4,
                                y=short$avg[1],yend=short$avg[1]),
                                color="blue", size=1.3) +
               geom_segment(aes(x=1.6,xend=2.4,
                                y=short$avg[2],yend=short$avg[2]),
                            color="blue", size=1.3) +
               geom_segment(aes(x=2.6,xend=3.4,
                                y=short$avg[3],yend=short$avg[3]),
                            color="blue", size=1.3) +
               geom_segment(aes(x=3.6,xend=4.4,
                                y=short$avg[4],yend=short$avg[4]),
                            color="blue", size=1.3)
         }
      } 
      
      if(i %in% c(2,3,5)){#1977
         if(i %in% c(3,5)){
            q <- 
               q +
               geom_smooth(data=glmm_all[["x1977"]], 
                           aes(get(vars[i]), fit, color="green"),
                           method=lm , color=NA, fill="green", alpha=0.2,
                           se=TRUE) +
               geom_line(data=glmm_all[["x1977"]], 
                         aes(get(vars[i]), fit, color="green"),
                         stat = "smooth", method = lm,
                         color = "green", size = 1.5, alpha = 0.2)
         } else if(i==2){
            short1 <- as.data.table(glmm_all[["x1977"]])
            short1<- short1[,.(avg=mean(fit)), by=.(position_all)]
            
            q <- q +
               geom_segment(aes(x=0.6,xend=1.4,
                                y=short1$avg[1],yend=short1$avg[1]),
                            color="green", size=1.3) +
               geom_segment(aes(x=1.6,xend=2.4,
                                y=short1$avg[2],yend=short1$avg[2]),
                            color="green", size=1.3) +
               geom_segment(aes(x=2.6,xend=3.4,
                                y=short1$avg[3],yend=short1$avg[3]),
                            color="green", size=1.3) +
               geom_segment(aes(x=3.6,xend=4.4,
                                y=short1$avg[4],yend=short1$avg[4]),
                            color="green", size=1.3)
         }
      }
      
      if(i %in% c(1,4)){#1966
         q <- 
            q +
            geom_smooth(data=glmm_all[["x1966"]], 
                        aes(get(vars[i]), fit, color="red"),
                        method=lm , color=NA, fill="red", alpha=0.2,
                        se=TRUE) +
            geom_line(data=glmm_all[["x1966"]], 
                      aes(get(vars[i]), fit, color="red"),
                      stat = "smooth", method = lm,
                      color = "red", size = 1.5, alpha = 0.2)
      }
      
      if(i %in% c(1,3,4,5)){#all
         q <- q +
            geom_smooth(method=lm, color="black", fill="black", alpha=0.2,
                        se=TRUE, aes(color="black")) +
            scale_color_identity(
               name = "Droughts",
               breaks = c("black", "red", "green", "blue"),
               labels = c("All", "1966", "1977", "1999"),
               guide = "legend")
         # geom_line(stat = "smooth", method = lm,
         #           color = "black", size = 1.5, alpha = 0.2)
      }
      
      if(i==2){
         q <- q + scale_x_discrete(breaks=c("1","2","3", "4"),
                                   labels=c("C", "D", "I", "S"))
      }
      
      q <- q  
      
      print(q)
   })
}

png("manuscript/tables_figures/publication/test_fig.png", width=960)
ggarrange(gglist[[1]], gglist[[3]], gglist[[2]], 
          gglist[[4]], gglist[[5]], 
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)"),
          ncol = 3, nrow = 2,
          common.legend=TRUE)
dev.off()

#make legend
trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE); arima_vals=FALSE

x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

trees_all_sub$year <- "all"
full <- rbind(trees_all_sub, x1966, x1977, x1999)
full$color <- 
   ifelse(full$year=="all", "black",
          ifelse(full$year=="1966", "red",
                 ifelse(full$year=="1977", "green","blue")))

ggplot(full) +
   aes(height.ln.m, resist.value) +
   geom_line(aes(color=year), size=1.2, alpha=0.4) +
   facet_wrap(~year) +
   scale_color_manual(name="Droughts",
                      labels=c("All", "1966", "1977", "1999"),
                      values = c("black", "red", "green", "blue"))

#
library(png)
library(grid)
library(gridExtra)
fullplot <- readPNG("manuscript/tables_figures/publication/test_fig.png")
legend <- readPNG("manuscript/tables_figures/legend.png")

fullplot <- rasterGrob(fullplot)
legend <- rasterGrob(legend)

library(cowplot)
png("manuscript/tables_figures/publication/regr_visuals.png",
    width=960, height=500)
ggdraw() +
   draw_plot(fullplot, x = 0, y = 0, width = 1, height = 1) +
   draw_plot(legend, x = .6, y = .18, width = .5, height = .25)
dev.off()


#






########################################################################
#6. Fig S3. Make histogram of height per drought year ####
trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE); arima_vals=FALSE

x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("All years", "1966", "1977", "1999")

layout(matrix(1:4, nrow=2, byrow=TRUE))
for(i in 1:4){
   hist(exp(model_df[[i]][,"height.ln.m"]),
        xlab="Height [m]", ylab="N trees",
        ylim=c(0,350),
        main=names(model_df)[i])
}
hist(exp(x1966$height.ln.m))




########################################################################
#7. compare Rt values with arima_ratio ####
library(data.table)
arima_ratio <- read.csv("manuscript/tables_figures/trees_all_sub_arimaratio.csv", stringsAsFactors = FALSE)

rt <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE)

years <- c(1966, 1977, 1999)

layout(matrix(1:8, nrow=2, byrow=TRUE))
res_full <- NULL
for(i in 1:4){
   cols <- c("tree", "resist.value")
   
   if(i==1){
      cols <- c("year", cols)
      compare <- rt[,cols]
      arimadf <- arima_ratio
      compare$arimart <- arimadf$resist.value[
         match(paste0(compare$year, compare$tree),
               paste0(arimadf$year, arimadf$tree))]
   } else {
      compare <- rt[rt$year == years[i-1], cols]
      arimadf <- arima_ratio[arima_ratio$year == years[i-1], ]
      compare$arimart <- arimadf$resist.value[
         match(compare$tree, arimadf$tree)]
   }
   
   compare <- compare[complete.cases(compare),]
   
   plot(compare$resist.value, compare$arimart, 
        main=if(i==1){"All years"} else {as.character(years[i-1])},
        xlab="Rt", ylab="ARIMA")
   abline(coef=c(0,1), col="red")
   
   #put together all direct comparisons
   if(i==1){
      tabs4 <- compare
      setnames(tabs6, old=c("year", "tree", "resist.value", "arimart"),
               new=c("Year", "Tree", "$Rt$", "$Rt_{ARIMA}$"))
   }
   
   #calculate top 3 +- deviations from 1:1 line
   y <- compare$arimart
   x <- compare$resist.value
   compare$resid_val <- resid(lm(y-x ~ 0))
   compare <- compare[order(compare$resid_val), ]
   hist(compare$resid_val, xlab="Residual value from 1:1 line",
        main=if(i==1){"All years"} else {as.character(years[i-1])})
   
   resids <- data.frame(rbind(head(compare, n=3), tail(compare, n=3)))
   resids$year <- if(i==1){"all"} else {years[i-1]}
   
   res_full <- rbind(res_full, resids)
}
write.csv(tabs4, "manuscript/tables_figures/Rt_arimaratio_comparison.csv", row.names=FALSE)
write.csv(res_full, "manuscript/tables_figures/top_residual_deviations.csv", row.names=FALSE)
##################################################################
# appendix ####
allhei <- as.data.table(heights_allplot)
#height growth showing massive negative growth from 1999 to 2018
test <- allhei[year %in% c("1999", "2018"), .(shrunk = height.m[1] - height.m[2]), 
               by=.(tree, position_all_abb)
               ][, .(perc= sum(shrunk >0, na.rm=TRUE)/sum(shrunk>0 | shrunk <=0, na.rm=TRUE)), 
                 by=.(position_all_abb)]

#avg growth between each of the scenario years
allhei$group <- cut(allhei$year, breaks=3)
test66 <- allhei[year %in% c("1966", "1977"), .(gro = height.m[2] - height.m[1]), by=tree
                 ][, .(avg_gro = mean(gro, na.rm=TRUE))]
test77 <- allhei[year %in% c("1977", "1999"), .(gro = height.m[2] - height.m[1]), by=tree
                 ][, .(avg_gro = mean(gro, na.rm=TRUE))]
test99 <- allhei[year %in% c("1999", "2018"), .(gro = height.m[2] - height.m[1]), by=tree
                 ][, .(avg_gro = mean(gro, na.rm=TRUE))]
avg_growth <- data.frame(g66_77 = test66$avg_gro,
                         g77_99 = test77$avg_gro,
                         g99_18 = test99$avg_gro)
