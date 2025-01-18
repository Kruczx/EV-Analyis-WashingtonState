##Project#3
##Dheeraj, Marco, and Viraj ITEC610 Fall 2024 - Professor Janzen
## Date of Submission: October 1st, 2024
####################################################

library(readr)
evd <- read_csv("Electric_Vehicle_Population_Data.csv")
install.packages("dplyr")

### Keeping only Electric Vehicle Type & Electric Range ###

trimmed_data <- evd[,c(9,11)]
print(trimmed_data)

### Interval and Ratio Variables Statistics - Electric Range (Dependent Variable) ###

library(summarytools)
descr(trimmed_data$`Electric Range`)
summary(trimmed_data$`Electric Range`)
sd(trimmed_data$`Electric Range`, na.rm = TRUE)

### Categorical Variable Statistics - Electric Vehicle Type (Independent Variable) ###

freq(trimmed_data$`Electric Vehicle Type`)

### Bivarate Violin Plot ###

library(ggplot2)

ggplot(trimmed_data, aes(x = `Electric Vehicle Type`, y = `Electric Range`, fill = `Electric Vehicle Type`)) +
  geom_violin(trim = FALSE, adjust = 1.5, scale = "width") +  
  scale_fill_manual(values = c("blue", "green")) +  
  labs(title = "Electric Range Distribution by Vehicle Type", 
       x = "Electric Vehicle Type", 
       y = "Electric Range (miles)") +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.1, size = 16, face = "bold"),  
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),  
    legend.position = "none"  
  )

### T - test analysis

library(dplyr)

trimmed_data <- na.omit(trimmed_data)

trimmed_data$`Electric Vehicle Type` <- as.factor(trimmed_data$`Electric Vehicle Type`)

trimmed_data$`Electric Range` <- as.numeric(as.character(trimmed_data$`Electric Range`))

### Setting alpha level as 0.05
alpha <- 0.05

t_test_result <- t.test(`Electric Range` ~ `Electric Vehicle Type`, data = trimmed_data, var.equal = TRUE)

print(t_test_result)

if (t_test_result$p.value < alpha) {
  cat("Reject the null hypothesis. There is a significant difference in Electric Range between the Electric vehicle types.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference in Electric Range between the Electric vehicle types.\n")
}

### Conducting Effect Size test 

library(effsize)

Cohens_d <- cohen.d(trimmed_data$`Electric Range`, trimmed_data$`Electric Vehicle Type`)

print(Cohens_d)

### Conducting Power Test

library(pwr)

power_test <- pwr.t2n.test(n1 = 130293, 
                           n2 = 36507, 
                           d = 0.42959, 
                           sig.level = 0.05, 
                           power = NULL)

print(power_test)


