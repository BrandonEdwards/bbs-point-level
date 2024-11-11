####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <13-point-vs-detectability-trend.R>
# Created August 2024
# Last Updated August 2024

####### Import Libraries and External Files #######

library(bbsBayes2)
library(ggpubr)

####### Set Constants #############################

sp <- "OVEN"

####### Main Code #################################

point_indices <- readRDS(file = paste0("output/indices/", sp, "_point.RDS"))
detect_indices <- readRDS(file = paste0("output/indices/", sp, "_detectability.RDS"))
varprop_indices <- readRDS(file = paste0("output/indices/", sp, "_varprop.RDS"))

point_trends <- generate_trends(indices = point_indices)
detectability_trends <- generate_trends(indices = detect_indices)
varprop_trends <- generate_trends(indices = varprop_indices)

point_vs_detect <- detectability_trends
point_vs_detect$trends$trend <- point_vs_detect$trends$trend - point_trends$trends$trend

point_vs_varprop <- varprop_trends
point_vs_varprop$trends$trend <- point_vs_varprop$trends$trend - point_trends$trends$trend

detect_vs_varprop <- detectability_trends
detect_vs_varprop$trends$trend <- detect_vs_varprop$trends$trend - varprop_trends$trends$trend

point_vs_detect_map <- plot_map(point_vs_detect, title = FALSE)
point_vs_varprop_map <- plot_map(point_vs_varprop, title = FALSE)
detect_vs_varprop_map <- plot_map(detect_vs_varprop, title = FALSE)


####### Output ####################################

png(filename = paste0("output/plots/", sp, "point-vs-detectability-map.png"),
    width = 5, height = 5, units = "in", res = 600)
ggarrange(point_vs_varprop_map, point_vs_detect_map, ncol = 1,
          labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
dev.off()
