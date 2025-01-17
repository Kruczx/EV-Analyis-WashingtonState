#Project#1
#Dheeraj & Marco ITEC610 Fall 2024 - Professor Janzen
####################################################

library(readr)
evd <- read.csv("Electric_Vehicle_Population_Data.csv")
install.packages("dplyr")

### Keeping only Model Year, Electric Vehicle Type, Electric Range, City, Make ###

trimmed_data <- evd[,c(3,6,7,9,11)]
print(trimmed_data)

### Subsetting City for Seattle, Tacoma, Yakima and Olympia ###

library(dplyr)
sub_evd <- trimmed_data |>
  filter(City=="Seattle" | City=="Tacoma" | City =="Yakima" | City == "Olympia")
print(sub_evd)

### Trimmed down Manufactures to 8 Makes ###

library(dplyr)
make_evd <- sub_evd |>
  filter(Make=="TESLA" | Make=="TOYOTA" |
           Make=="FORD" | Make=="NISSAN" |
           Make=="BMW" | Make=="KIA" |
           Make=="VOLKSWAGEN" | Make=="CHEVROLET")

print(make_evd)

install.packages("questionr")

library(questionr)

### Univariate plot for Make

freq(make_evd$Make)

freq_data_make <- table(make_evd$Make)

custom_colors <- c("red", "blue", "green", "purple", "orange", "yellow", "pink", "cyan")
par(mar = c(8, 4, 4, 2))

barplot(freq_data_make, 
        main = "Distribution of Vehicle Makes", 
        xlab = "Vehicle Make", 
        ylab = "Frequency", 
        col = custom_colors,  
        las = 2, 
        cex.names = 0.6)

### Univariate plot for Model year

library(ggplot2)

MYear_hist <- ggplot(make_evd, aes(x = Model.Year)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Model Year", x = "Model Year", y = "Count") +
  theme_minimal()

print(MYear_hist)

box_plot <- ggplot(make_evd, aes(x = "", y = Model.Year)) +
  geom_boxplot(fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Model Year", y = "Model Year") +
  theme_minimal()

print(box_plot)

### Univariate plot for electric range

summary(evd$Electric.Range)

library(ggplot2)

electric_range_plot <- ggplot(evd, aes(y = Electric.Range)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Box Plot of Electric Range", y = "Range (miles)")

print(electric_range_plot)

### Univariate Plot for electric vehicle type

ev_type <- table(evd$Electric.Vehicle.Type)

print(ev_type)

ev_type_plot <- ggplot(evd, aes(x = Electric.Vehicle.Type)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Electric Vehicle Types",
       x = "Electric Vehicle Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(ev_type_plot)

### Univariate Plot for City

City_freq <- table(sub_evd$City)
print(City_freq)

City_percent <- prop.table(City_freq)*100
print(round(City_percent,2))
summary(evd$City)

city_plot <- ggplot(sub_evd, aes(x = City, fill = City)) +
  geom_bar(color = "black", "linewidth" = 0.5) +
  labs(title = "Distribution of Cities",
       x = "City",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
  scale_fill_manual(values = c("red", "blue","green","orange"))

print(city_plot)

### Bivariate Plot for Model Year vs. Electric Range

Bivariate_plot1 <- ggplot(evd, aes(x = Model.Year, y = Electric.Range)) +
  geom_point(alpha = 0.5, color = "blue") +  
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "Model Year vs Electric Range",
       x = "Model Year",
       y = "Electric Range (miles)",
       caption = "Source: Electric Vehicle Population Data") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(Bivariate_plot1)

## Without the linear trend line

ggplot(evd, aes(x = Model.Year, y = Electric.Range)) +
  geom_point(alpha = 0.5, color = "blue") +  # Add points
  labs(title = "Model Year vs Electric Range",
       x = "Model Year",
       y = "Electric Range (miles)",
       caption = "Source: https://catalog.data.gov/dataset/electric-vehicle-population-data") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Summary statistics for Electric Range vs. City

city_summary <- sub_evd |> 
  group_by(City) |> 
  summarise(Mean_Range = mean(Electric.Range, na.rm = TRUE),
    Median_Range = median(Electric.Range, na.rm = TRUE),
    SD_Range = sd(Electric.Range, na.rm = TRUE),
    Min_Range = min(Electric.Range, na.rm = TRUE),
    Max_Range = max(Electric.Range, na.rm = TRUE))

print(city_summary)

### Bivariate Plot for Electric Range vs. City

library(tidyr)

Bivariate_plot2 <- ggplot(sub_evd, aes(x = City, y = Electric.Range, fill = City)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("Olympia" = "#FF9970", 
                               "Seattle" = "#66B2FF", 
                               "Tacoma" = "#99FF99", 
                               "Yakima" = "#FFCC60")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Distribution of Electric Range by City",
       x = "City",
       y = "Electric Range (miles)")

print(Bivariate_plot2)

### Creating a table for Make vs Electric Vehicle Type

table1 <- table(make_evd$Make, make_evd$Electric.Vehicle.Type)

### Calculating percentages 

percent_table <- prop.table(table1, margin = 1) * 100

print(round(percent_table, 2))
print(table1)

### Bivariate Plot for Make vs. Electric Vehicle Type

Bivariate_Plot3 <- ggplot(make_evd, aes(x = Make, fill = Electric.Vehicle.Type)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Electric Vehicle Types by Make",
       x = "Make",
       y = "Proportion",
       fill = "Electric Vehicle Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = c("Battery Electric Vehicle (BEV)" = "#56AE57",
                      "Plug-in Hybrid Electric Vehicle (PHEV)" = "#5E5CB2"))

print(Bivariate_Plot3)

## Bivariate Plot for Make vs. Electric Vehicle Type by percentage

ggplot(make_evd, aes(x = Make, fill = Electric.Vehicle.Type)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of Electric Vehicle Types by Make", y = "Percentage", x = "Make") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("Battery Electric Vehicle (BEV)" = "#77DD77",
                        "Plug-in Hybrid Electric Vehicle (PHEV)" = "#5E5CB2"))

### Trivariate analysis for Model Year, City, Electric Vehicle Type

summary_table <- make_evd |> 
  group_by(Model.Year, City, Electric.Vehicle.Type) |> 
  summarise(Count = n(), .groups = 'drop') |> 
  arrange(desc(Count))

print(summary_table)

Trivariate_plot <- ggplot(make_evd, aes(x = Model.Year, fill = Electric.Vehicle.Type)) +
  geom_bar(position = "stack") +
  facet_wrap(~ City, scales = "free_y") +
  labs(title = "Distribution of Electric Vehicle Types by Model Year and City",
       x = "Model Year",
       y = "Count",
       fill = "Electric Vehicle Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = c("Battery Electric Vehicle (BEV)" = "#77DD77",
                      "Plug-in Hybrid Electric Vehicle (PHEV)" = "#5E5CB2"))

print(Trivariate_plot)

### Heat Map 

heatmap_data <- make_evd |> 
  group_by(Model.Year, City, Electric.Vehicle.Type) |> 
  summarise(Count = n(), .groups = 'drop')

print(heatmap_data)

Trivariate_heatmap <- ggplot(heatmap_data, aes(x = Model.Year, y = City, fill = Count)) +
  geom_tile() +
  facet_wrap(~ Electric.Vehicle.Type) +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Electric Vehicle Counts by Model Year, City, and Type",
       x = "Model Year",
       y = "City",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(Trivariate_heatmap)