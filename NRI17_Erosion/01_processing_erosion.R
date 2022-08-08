#--because OneDrive doesn't work great with Rprojects
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)

# Function to read all files
crop_files <- list.files(pattern = ".xlsx")

tmp3 <- data.frame()

for (i in 1:length(crop_files)) {
  
  tmp1 <- readxl::read_excel(crop_files[i], sheet = 2, skip = 6)
  
  crop_sheet <- readxl::read_excel(crop_files[i], sheet = 2, range = "A1:A2")
  
  crop_name <- word(crop_sheet$`2017 National Resources Inventory`, -1)
  
  tmp2 <- tmp1 %>% 
    filter(is.na(State) == F) %>% 
    select(1:3, 15:16) %>% 
    mutate(crop = crop_name) %>% 
    rename("state" = 1,
           "year" = 2,
           "acreage_thousands" = 3,
           "erosion_rate" = 4,
           "erosion_rate_error" = 5)
  
  tmp3 <- rbind(tmp3, tmp2)
}

# Remove Caribbean, rename category that includes Alfalfa
tmp4 <- tmp3 %>%
  mutate(year = as.factor(as.numeric(year))) %>% 
  select(crop, everything()) %>% 
  mutate(crop = ifelse(test = crop == "Crops",
                       yes = "Other Crops (Alfalfa)",
                       no = crop)) %>% 
  filter(!state %in% c("Caribbean", "National")) # 19 (Caribbean) + 88 (National) obs removed or 4%

# Calculate lower bound and remove if estimate is negative
tmp5 <- tmp4 %>% 
  mutate(lower_bound = erosion_rate - erosion_rate_error) %>% 
  filter(lower_bound > 0) #453 obs removed or 17.2% of data

# Calculate quantiles, IQR to classify outliers and remove them
tmp6 <- tmp5 %>% 
  group_by(crop, year) %>% 
  mutate(IQR = IQR(erosion_rate, na.rm = T),
         Q25 = quantile(erosion_rate, 0.25, na.rm = T),
         Q75 = quantile(erosion_rate, 0.75, na.rm = T),
         is_outlier = (erosion_rate < Q25 - (3 * IQR) | erosion_rate > Q75 + (3 * IQR))) %>% 
  ungroup() %>% 
  filter(is_outlier == F) # 51 extreme outliers removed or 2.3% of data

NRI_soil_erosion_final <- tmp6 %>% 
  group_by(year, crop) %>% 
  summarize(soil_erosion_tons_acre = mean(erosion_rate),
            n = n()) %>% 
  ungroup()

#6/14/22 gn--was this NRI_soil_erosion_final saved somewhere? I can't find it. 

NRI_soil_erosion_final %>% 
  ggplot(aes(year, soil_erosion_tons_acre, group = crop)) +
  geom_line() +
  facet_wrap(~ crop, scales = "free")

# Number of states represented by crop and year
# Most crops are pretty constant in terms of crop representation, except barley, potatoes, sorghum
tmp6 %>% 
  group_by(crop, year) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_point() +
  facet_wrap( ~ crop, scales = "free_x")

tmp4 %>% 
  count(crop) %>% 
  arrange(desc(n))

# Graph of nice raw data
tmp6 %>% 
  ggplot(aes(x = year, y = erosion_rate)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_point(alpha = 0.6, position = position_jitter(width = 0.1), aes(color = erosion_rate_error)) +
  facet_wrap( ~ crop, scales = "free") +
  stat_summary(fun = mean, geom = "line", size = 1, aes(group = crop)) +
  labs(color = "Margin of Error", y = "Erosion Rate (tons soil/acre)", x = "Year",
       title = "Raw Erosion Rates by Crop and Years") +
  scale_color_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = -35),
        legend.position = "bottom",
        axis.text = element_text(color = "black", face = "bold"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "black", face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        legend.text = element_text(color = "black", face = "bold"),
        legend.title = element_text(color = "black", face = "bold"),
        strip.text = element_text(color = "black", face = "bold"))

ggsave(filename = "Raw Erosion Rate.png", scale = 1)

tmp6 %>% 
  ggplot(aes(x = erosion_rate_error)) +
  geom_histogram()

tmp6 %>% 
  ggplot(aes(y = erosion_rate_error, x = year)) +
  geom_boxplot() +
  facet_wrap( ~ crop, scales = "free")

# Some insights from the data, looked at for the first time on 2/10/2021
# There are extreme outliers for several crops, and some very high margins of error
# So the cleaning would be like this:
# Remove Caribbean data
# Remove values where lower bound = (erosion - margin of error) is negative. Margin of  error is half the CI.
# Remove values where IQR > 3 after grouping by crop and year

# Graphs

# Count of states before and after data cleaning
after_count <- tmp6 %>% 
  group_by(crop, year) %>% 
  summarize(n = n()) %>% 
  mutate(type = "After Cleaning")

before_count <- tmp4 %>% 
  group_by(crop, year) %>% 
  summarize(n = n()) %>% 
  mutate(type = "Before Cleaning")

# Joining
both_count <- after_count %>% 
  bind_rows(before_count) %>% 
  mutate(type = factor(type, levels = c("Before Cleaning", "After Cleaning")))

# Plotting Counts
ggplot(data = both_count, aes(x = year, y = n, color = type, shape = type)) +
  geom_point() +
  labs(x = "Year", y = "Number of States", color = "Data Source",
       title = "Number of States Represented Before and After Data Cleaning") +
  facet_wrap( ~ crop, scales = "free_x") +
  scale_color_manual(values = c("blue", "red")) +
  scale_shape_manual(values = c(16, 15)) +
  guides(shape = F) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -35),
        legend.position = "bottom",
        axis.text = element_text(color = "black", face = "bold"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "black", face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        legend.text = element_text(color = "black", face = "bold"),
        legend.title = element_text(color = "black", face = "bold"),
        strip.text = element_text(color = "black", face = "bold"))

ggsave(filename = "State Counts.png", scale = 1)

# Soil Erosion Means before and after cleaning
after_ero <- tmp6 %>% 
  group_by(crop, year) %>% 
  summarize(ero_mean = mean(erosion_rate)) %>% 
  mutate(type = "After Cleaning")

before_ero <- tmp4 %>% 
  group_by(crop, year) %>% 
  summarize(ero_mean = mean(erosion_rate)) %>% 
  mutate(type = "Before Cleaning")

# Joining
both_ero <- after_ero %>% 
  bind_rows(before_ero) %>% 
  mutate(type = factor(type, levels = c("Before Cleaning", "After Cleaning")))

# Plotting Soil Erosion
ggplot(data = both_ero, aes(x = year, y = ero_mean, color = type, shape = type)) +
  geom_point() +
  labs(x = "Year", y = "Mean Soil Erosion (tons/acre)", color = "Data Source",
       title = "Average Soil Erosion Before and After Data Cleaning") +
  facet_wrap( ~ crop, scales = "free_x") +
  scale_color_manual(values = c("blue", "red")) +
  scale_shape_manual(values = c(16, 15)) +
  guides(shape = F) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -35),
        legend.position = "bottom",
        axis.text = element_text(color = "black", face = "bold"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "black", face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
        legend.text = element_text(color = "black", face = "bold"),
        legend.title = element_text(color = "black", face = "bold"),
        strip.text = element_text(color = "black", face = "bold"))

ggsave(filename = "Mean Soil Erosion.png", scale = 1)
