####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# 11-route-level.R
# Created June 2024
# Last Updated June 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(terra)
library(geodata)
library(ggpubr)
library(tidyterra)
library(cowplot)
theme_set(theme_pubclean())

####### Set Constants #############################

sp <- "OVEN"

####### Read Data #################################

route_model <- readRDS(paste0("output/model_runs/", sp, "-route.rds"))
point_model <- readRDS(paste0("output/model_runs/", sp, "-point.rds"))
detectability_model <- readRDS(paste0("output/model_runs/", sp, "-varprop.rds"))

route_indices <- readRDS(paste0("output/indices/", sp, "_route.RDS"))
point_indices <- readRDS(paste0("output/indices/", sp, "_point.RDS"))
detect_indices <- readRDS(paste0("output/indices/", sp, "_varprop.RDS"))

nalcms_2010 <- rast("data/raw/spatial/nalcms/CAN_Land_cover_2010v2_30m_TIF/CAN_NALCMS_landcover_2010v2_30m/data/CAN_NALCMS_landcover_2010v2_30m.tif")
nalcms_2015 <- rast("data/raw/spatial/nalcms/can_land_cover_2015v3_30m_tif/CAN_NALCMS_landcover_2015v3_30m/data/CAN_NALCMS_landcover_2015v3_30m.tif")
nalcms_2020 <- rast("data/raw/spatial/nalcms/can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/CAN_NALCMS_landcover_2020_30m.tif")

latlong <- load_map("latlong")

####### Main Code #################################

data <- detectability_model$raw_data

points <- unique(data$route)

fc_change <- data.frame(Point = points,
                        Stratum = NA,
                        Change = NA,
                        N_Years = NA,
                        Longitude = NA,
                        Latitude = NA)

for (i in 1:nrow(fc_change))
{
  pt <- fc_change$Point[i]
  temp <- data[which(data$route == pt), ]
  
  stratum_list <- data[which(data$route == pt), ]
  fc_change$Stratum[i] <- unique(stratum_list$strata_name)
  
  n_year <- max(temp$year) - min(temp$year) + 1
  fc_min_year <- as.numeric(temp[which(temp$year == min(temp$year)), "forest_coverage"])
  fc_max_year <- as.numeric(temp[which(temp$year == max(temp$year)), "forest_coverage"])
  
  fc_change$Change[i] <- fc_max_year - fc_min_year
  fc_change$N_Years[i] <- n_year
  fc_change$Longitude[i] <- temp$longitude[1]
  fc_change$Latitude[i] <- temp$latitude[1]
}

# Landcover change at a specific route
mean_fc_change_stratum <- aggregate(fc_change$Change, list(fc_change$Stratum), mean)
names(mean_fc_change_stratum) <- c("Stratum", "Mean_Change")

pt_trends <- generate_trends(point_indices)
detect_trends <- generate_trends(detect_indices)

mean_fc_change_stratum$Point_Trend <- NA
mean_fc_change_stratum$Detectability_Trend <- NA
for (s in mean_fc_change_stratum$Stratum)
{
  det_trend <- detect_trends$trends[which(detect_trends$trends$region == s), ]$trend
  point_trend <- pt_trends$trends[which(pt_trends$trends$region == s),]$trend
  
  mean_fc_change_stratum[which(mean_fc_change_stratum$Stratum == s), "Point_Trend"] <- point_trend
  mean_fc_change_stratum[which(mean_fc_change_stratum$Stratum == s), "Detectability_Trend"] <- det_trend
}

mean_fc_change_stratum$Difference <- mean_fc_change_stratum$Detectability_Trend - mean_fc_change_stratum$Point_Trend

point_rast <- vect(matrix(c(fc_change$Longitude, fc_change$Latitude), ncol = 2),
                   atts = data.frame(Change = fc_change$Change),
                   crs = "+proj=longlat")

route_indices_plot <- plot_indices(indices = route_indices, title = FALSE, 
                              axis_text_size = 10, axis_title_size = 12)
ind_plot <- plot_indices(indices = point_indices, title = FALSE, 
                         axis_text_size = 10, axis_title_size = 12)
det_plot <- plot_indices(indices = detect_indices, title = FALSE, 
                         axis_text_size = 10, axis_title_size = 12)

st_min <- mean_fc_change_stratum[which(mean_fc_change_stratum$Mean_Change == min(mean_fc_change_stratum$Mean_Change)), "Stratum"]
st_max <- mean_fc_change_stratum[which(mean_fc_change_stratum$Mean_Change == max(mean_fc_change_stratum$Mean_Change)), "Stratum"]

fc_change_stratum <- fc_change[which(fc_change$Stratum == st_min), ]
strat_rast <- vect(matrix(c(fc_change_stratum$Longitude, fc_change_stratum$Latitude), ncol = 2),
                   atts = data.frame(Change = fc_change_stratum$Change),
                   crs = "+proj=longlat")

canada_plot <- ggplot() +
  geom_spatvector(data = point_rast, aes(color = Change), size = 0.5) +
  geom_rect(aes(xmin = -99.5,
            xmax = -98,
            ymin = 52,
            ymax = 53.2), alpha = 0, color = "black", size = 0.5) +
    scale_color_stepsn(colors=c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                                "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"),
                      n.breaks=10, limits=c(-1,1), show.limits=T) +
  theme(legend.position = "left") +
  NULL

route_plot <- ggplot() +
  geom_spatvector(data = strat_rast, aes(color = Change)) +
  xlim(-99.1, -98.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_stepsn(colors=c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                              "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"),
                     n.breaks=10, limits=c(-1,1), show.limits=T) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=2),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  NULL

map <- ggdraw()+
  draw_plot(canada_plot)+
  draw_plot(route_plot,height=0.5,x=0.37,y=0.5)

p1 <- ind_plot[which(names(ind_plot) == gsub("-", "", st_min))][[1]]
p2 <- det_plot[which(names(ind_plot) == gsub("-", "", st_min))][[1]]
p2 <- p2 +   geom_dog(aes(x = 2020, y = 18), dog = "thisisfine", size = 5) +
  geom_label(aes(x = 2020, y = 22, label = "This is fine"))
p3 <- route_indices_plot[which(names(route_indices_plot) == gsub("-", "", st_min))][[1]]

####### Output ####################################

png(filename = "output/plots/route-level-change.png",
    width = 6, height = 6, units = "in", res = 300)
ggarrange(map, ggarrange(p1, p2, nrow = 1, labels = c("B", "C")), labels = c("A"), nrow = 2)
dev.off()
