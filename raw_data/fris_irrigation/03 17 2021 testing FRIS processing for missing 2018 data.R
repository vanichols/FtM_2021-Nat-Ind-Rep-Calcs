# Packages
library(tidyverse)

#======================================
# Purpose: Figure out how to do imputation of data for 2018 for missing non-irrigated yield and acreage
#======================================

# Import raw data

fris <- read_csv("fris_irrigation.csv")

# Plot data points

vars_to_plot <- c("acres_irrigated", "yield_irrigated", "acres_non_irrigated", "yield_non_irrigated")

for (i in 1:length(vars_to_plot)) {
  graph <- ggplot(fris, aes(x = as.factor(year), y = get(vars_to_plot[i]))) +
    geom_line(aes(group = crop)) +
    geom_point() +
    labs(y = paste(vars_to_plot[i]), title = paste(vars_to_plot[i])) +
    facet_wrap(~ crop, scales = "free") +
    theme(axis.text.x = element_text(angle = 90))
  
  print(graph)
}

# Calculate ratios for yield and acreage (non-irrigated / irrigated)

ratio_test <- fris %>% 
  mutate(ratio_yield = yield_non_irrigated/yield_irrigated,
         ratio_acreage = acres_non_irrigated/acres_irrigated)

# Plot ratios

ggplot(ratio_test, aes(x = as.factor(year), y = ratio_yield)) +
  geom_line(aes(group = crop)) +
  geom_point() +
  facet_wrap(~ crop, scales = "free") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(ratio_test, aes(x = as.factor(year), y = ratio_acreage)) +
  geom_line(aes(group = crop)) +
  geom_point() +
  facet_wrap(~ crop, scales = "free") +
  theme(axis.text.x = element_text(angle = 90))

# Keeping last four data points for each crop (years vary)

ratio_last_four_samplings <- ratio_test %>% 
  filter(year != 2018) %>% # 2018 is the sampling without non-irrigated data
  arrange(crop, year) %>% 
  group_by(crop) %>% 
  slice(tail(row_number(), 4))

# Plotting the last four data points for ratios

ggplot(ratio_last_four_samplings, aes(x = as.factor(year), y = ratio_acreage)) +
  geom_line(aes(group = crop)) +
  geom_point() +
  ylim(0,3) +
  facet_wrap(~ crop, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(ratio_last_four_samplings, aes(x = as.factor(year), y = ratio_yield)) +
  geom_line(aes(group = crop)) +
  geom_point() +
  ylim(0,3) +
  facet_wrap(~ crop, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

# Calculating average yield and acreage (non-irrigated / irrigated) for the last four sampling events by crop

ratio_avgs <- ratio_last_four_samplings %>% 
  group_by(crop) %>% 
  summarize(avg_yield_ratio = mean(ratio_yield, na.rm = T),
            avg_acreage_ratio = mean(ratio_acreage, na.rm = T))

# These ratios are then appended to 2018 FRIS data by crop and multiplied by the irrigated yield and acreage to fill in the data for non-irrigated yield and acreage