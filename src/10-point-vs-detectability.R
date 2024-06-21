####### Script Information ########################
# Brandon P.M. Edwards
# BBS Point Level
# <10-point-vs-detectability.R>
# Created March 2024
# Last Updated June 2024

####### Import Libraries and External Files #######

library(cmdstanr)
library(bbsBayes2)
library(ggpubr)
library(ggdogs)
theme_set(theme_pubclean())
bayesplot::color_scheme_set("red")

####### Set Constants #############################

sp <- "OVEN"

####### Read Data #################################

point_model <- readRDS(paste0("output/model_runs/", sp, "-point.rds"))
detectability_model <- readRDS(paste0("output/model_runs/", sp, "-varprop.rds"))

point_indices <- readRDS(paste0("output/indices/", sp, "_point.RDS"))
detect_indices <- readRDS(paste0("output/indices/", sp, "_varprop.RDS"))

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

model_data <- list(N = nrow(mean_fc_change_stratum),
                   fc_change = mean_fc_change_stratum$Mean_Change,
                   trend_change = mean_fc_change_stratum$Difference)

model <- cmdstan_model(stan_file = "models/fc-vs-trend.stan")

model_run <- model$sample(
  data = model_data,
  iter_warmup = 1000,
  iter_sampling = 2000,
  chains = 4,
  parallel_chains = 4,
  refresh = 10
)

mod_summary <- model_run$summary()

model_draws <- model_run$draws(variables = c("intercept", "beta"), format = "df")

to_plot <- data.frame(fc = model_data$fc_change,
                      trend = model_data$trend_change)
comp_plot <- ggplot(data = to_plot, aes(x = fc, y = trend)) + 
  geom_abline(intercept = model_draws$intercept, slope = model_draws$beta, color = "grey", alpha = 0.1) +
  geom_abline(intercept = mean(model_draws$intercept),
              slope = mean(model_draws$beta),
              color = "black", size = 1) +
  geom_point(alpha = 0.5) +
  xlab("Change in Forest Coverage") +
  ylab("Trend Difference (VARPROP - POINT)") +
  NULL

####### Output ####################################

png(filename = paste0("output/plots/trend-vs-forest.png"),
    width = 6, height = 4, res = 600, units = "in")
print(comp_plot)
dev.off()






