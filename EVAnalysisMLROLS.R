### Project #5
### Dheeraj Shetty ITEC 610 Fall 2024 - Professor Janzen
### Last updated October 14, 2024

library(readr)
library(dplyr)
library(summarytools)

evd <- read_csv("Electric_Vehicle_Population_Data.csv")

trimmed_data <- evd |> 
  select(`Model Year`, `Electric Range`, `Clean Alternative Fuel Vehicle (CAFV) Eligibility`, 
         `Electric Vehicle Type`, `County`) |> 
  filter(`Electric Range` > 0)

# Subset data for top counties

top_counties <- c("King", "Snohomish", "Pierce", "Kitsap", "Island")
sub_evd <- trimmed_data |>
  filter(County %in% top_counties)

# Print the first few rows of the subsetted data

print(head(sub_evd))

# Univariate Analysis

# 1. Electric Range (Dependent Variable)

range_summary <- descr(sub_evd$`Electric Range`)
print(range_summary)

# 2. Model Year (Independent Variable)

year_summary <- descr(sub_evd$`Model Year`)
print(year_summary)

# 3. Clean Alternative Fuel Vehicle (CAFV) Eligibility (Independent Variable)

cafv_summary <- sub_evd |>
  group_by(`Clean Alternative Fuel Vehicle (CAFV) Eligibility`) |>
  summarise(n = n()) |>
  mutate(percentage = n / sum(n) * 100)

print(cafv_summary)

# 4. Electric Vehicle Type (Independent Variable)

ev_type_summary <- sub_evd |>
  group_by(`Electric Vehicle Type`) |>
  summarise(n = n()) |>
  mutate(percentage = n / sum(n) * 100)

print(ev_type_summary)

# 5. County (Independent Variable)

county_summary <- sub_evd |>
  group_by(County) |>
  summarise(n = n()) |>
  mutate(percentage = n / sum(n) * 100) |>
  arrange(desc(n))

print(county_summary)

library(car)

# Prepare data for regression

sub_evd <- sub_evd |> 
  mutate(CAFV_Eligible = ifelse(`Clean Alternative Fuel Vehicle (CAFV) Eligibility` == "Clean Alternative Fuel Vehicle Eligible", 1, 0),
    EV_Type_BEV = ifelse(`Electric Vehicle Type` == "Battery Electric Vehicle (BEV)", 1, 0),
    County = factor(County, levels = c("King", "Snohomish", "Pierce", "Kitsap", "Island")))

# Check for correlations
cor_matrix <- cor(sub_evd[c("Model Year", "Electric Range", "CAFV_Eligible", "EV_Type_BEV")])
print(cor_matrix)

# Perform Multiple OLS Regression

model <- lm(`Electric Range` ~ `Model Year` + CAFV_Eligible + EV_Type_BEV + County, data = sub_evd)
print(model)

# Check OLS regression assumptions

# Check for multicollinearity
vif(model)

# 1. Linearity
plot(model, which = 1)

# 2. Independence (not shown in plots, assumed from data collection)

# 3. Homoscedasticity
plot(model, which = 3)

# 4. Normality of residuals
plot(model, which = 2)

# 5. Influential points

plot(model, which = 5)

# Print regression summary

summary(model)

# Set alpha level

alpha <- 0.05

library(stargazer)

# Create regression results table

stargazer(model, type = "text", title = "Regression Results", 
          dep.var.labels = "Electric Range", 
          covariate.labels = c("Model Year", "CAFV Eligible", "EV Type (BEV)", 
                               "County (Snohomish)", "County (Pierce)", "County (Kitsap)", "County (Island)"),
          omit.stat = c("f", "ser"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines = list(c("County Reference", "King")))

# Creating scatter plot

library(ggplot2)

plot3 <- plot <- ggplot(sub_evd, aes(x = `Model Year`, y = `Electric Range`, color = factor(CAFV_Eligible))) +
  geom_point(alpha = 0.6, size = 3) +  # Larger points with slight transparency
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1.2) +  # Dashed trend line for visibility
  facet_grid(EV_Type_BEV ~ County, 
             labeller = labeller(EV_Type_BEV = c("0" = "PHEV", "1" = "BEV"))) +  # Custom facet labels
  labs(
    title = "Electric Range vs Model Year by County, CAFV Eligibility, and EV Type",
    x = "Model Year", y = "Electric Range", color = "CAFV Eligible"
  ) +
  scale_color_manual(values = c("darkred", "darkgreen"), labels = c("Not Eligible", "Eligible")) +
  theme_minimal() +  # Apply minimal theme
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),  # Large, bold title
    axis.title.x = element_text(size = 18, face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(size = 18, face = "bold"),  # Bold y-axis label
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Larger x-axis ticks with rotation
    axis.text.y = element_text(size = 14),  # Larger y-axis ticks
    strip.text = element_text(size = 16, face = "bold"),  # Larger, bold facet labels
    legend.title = element_text(size = 16, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 14),  # Larger legend text
    legend.position = "top",  # Legend at the top for space efficiency
    panel.spacing = unit(1.5, "lines"),  # Add space between panels for clarity
    panel.grid.major = element_line(size = 0.5, linetype = 'dotted')  # Subtle dotted grid lines
  )

print(plot3)

ggsave("ElectricRange_AllCountiesNew1.jpeg", plot3, width = 14, height = 12, dpi = 300, device = "jpeg")

# Power analysis

library(pwr)

f2 <- summary(model)$r.squared / (1 - summary(model)$r.squared)

power_test <- pwr.f2.test(u = length(coef(model)) - 1,  # number of predictors
                          v = nrow(sub_evd) - length(coef(model)),  # df for error 
                          f2 = f2, 
                          sig.level = alpha)
print(power_test)
