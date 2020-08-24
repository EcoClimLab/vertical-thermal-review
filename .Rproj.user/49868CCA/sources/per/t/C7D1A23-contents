library(data.table)
library(readxl)
data <- fread("NEON_height_profiles/forested_NEON_sites.csv")
data <- data[include==1, ]

meta <- read_excel("NEON_height_profiles/data/site_data/TIS site metadata_20190403_forNOAA.xlsx", sheet=1)
meta <- data.table(meta)

meta <- meta[SITE %in% data[,site], ]

#Next steps
##adapt rest of vertical_height_NEON script
##make plots

