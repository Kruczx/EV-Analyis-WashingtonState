#Project #4
#Dheeraj & Marco ITEC610 Fall 2024 - Professor Janzen
#October 8th, 2024
####################################################

library(readr)
evd <- read_csv("Electric_Vehicle_Population_Data.csv")
install.packages("dplyr")
library(dplyr)
### Keeping only Electric Vehicle Model Year & Electric Range ###
### Removing Vehicles with zero listed Ranges ###

trimmed_data <- evd |> 
  select(`Model Year`, `Electric Range`) |> 
  filter(`Electric Range`>0)

### Univariate Analysis for Model Year ###

modelyear <- trimmed_data %>%
  group_by(`Model Year`) %>%
  summarise(
    Frequency = n()
  ) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 2))

library(summarytools)
descr(trimmed_data$`Model Year`)

### Univariate Analysis for Electric Range ###

electricrange <- trimmed_data %>%
  group_by(`Electric Range`) %>%
  summarise(
    Frequency = n()
  ) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 2))

library(summarytools)
descr(trimmed_data$`Electric Range`)

### Regression Analysis

### 1. Specifying and Perform Simple OLS Regression

evd_model <- lm(`Electric Range` ~ `Model Year`, data = trimmed_data)
summary(evd_model)

### 2. Checking OLS Regression Assumptions

par(mfrow=c(2,2))

# Linearity and Homoscedasticity

plot(evd_model, which = 1)

# Normality of residuals

plot(evd_model, which = 2)

## Scale-location

plot(evd_model, which = 3)

# Influential points

plot(evd_model, which = 4)
plot(evd_model, which = 5)

# Reset the plot layout to default

par(mfrow=c(1,1))

# Check for multicollinearity (not applicable for simple regression, but included for completeness)

library(car)
vif(evd_model)

### 3. Regression Results Table

library(stargazer)
stargazer(evd_model, type = "text", title = "Regression Results", 
          dep.var.labels = "Electric Range", 
          covariate.labels = "Model Year")

### 4. Scatterplot with Regression Line

library(ggplot2)

ggplot(trimmed_data, aes(x = `Model Year`, y = `Electric Range`)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Electric Range vs Model Year",
       x = "Model Year",
       y = "Electric Range") +
  theme_minimal()

# 5. Power Analysis

library(pwr)

f2 <- summary(evd_model)$r.squared / (1 - summary(evd_model)$r.squared)

Power_test <- pwr.f2.test(u = 1, 
                          v = nrow(trimmed_data) - 2, 
                          f2 = f2, 
                          sig.level = 0.05)
print(Power_test)