####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# 00b-extract-aci-landcover.R
# Created March 2024
# Last Updated March 2024

####### Import Libraries and External Files #######

library(terra)

####### Read Data #################################

bbs_sites <- readRDS(file = "data/generated/bbs_sites.RDS"); bbs_sites$rt_st <- NULL
lc_rasters <- vector(mode = "list", length = 13)
names(lc_rasters) <- as.character(seq(2011,2023))
for (y in 2011:2023)
{
  lc_rasters[[as.character(y)]] <- rast(paste0("data/raw/spatial/aci/aci_",
                                 y,
                                 "_on.tif"))
}

####### Set Constants #############################

closed_habitat <- c(200, 210, 220, 230)

####### Main Code #################################

bbs_sites$forest_coverage <- NA

#' This is the loop that will extract most of the forest coverage using the
#' Annual Crop Indices dataset.
for (i in 1:nrow(bbs_sites))
{
  year <- bbs_sites$year[i]
  if (year < 2011)
  {
    next
  }
  
  if (bbs_sites$st_abrev[i] != "ON")
  {
    next
  }
  
  # There are some weird entries that i need to talk to BBS office about
  if (bbs_sites$latitude[i] > 1000)
  {
    next
  }
  
  bbs_point <- project(vect(matrix(c(bbs_sites$longitude[i], bbs_sites$latitude[i]), ncol = 2),
                            crs = "+proj=longlat"),
                       crs(lc_rasters[[as.character(year)]]))
  bbs_radius <- buffer(bbs_point, width = 400)
  landcover_types <- extract(lc_rasters[[as.character(year)]], bbs_radius)
  bbs_sites$forest_coverage[i] <- sum(landcover_types[,2] >= 200) / nrow(landcover_types)
}

####### Output ####################################

saveRDS(object = bbs_sites, file = "data/generated/bbs_sites_aci.RDS")
