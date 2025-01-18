##Project#2
##Dheeraj, Marco, & Viraj ITEC610 Fall 2024 - Professor Janzen
####################################################
library(readr)
evd <- read.csv("Electric_Vehicle_Population_Data.csv")
install.packages("dplyr")

### Keeping only Model Year, Electric Vehicle Type, Electric Range, City, Make ###

trimmed_data <- evd[,c(3,7)]
print(trimmed_data)

### Subsetting City for Seattle & Spokane ###

library(dplyr)
sub_evd <- trimmed_data |>
  filter(City=="Seattle" | City=="Spokane")
print(sub_evd)

### Trimmed down Manufactures to 3 Makes ###

library(dplyr)
new_evd <- sub_evd |>
  filter(Make=="TESLA" | Make=="TOYOTA" |
           Make=="BMW")

### Summary Stats for both variables (Categorical)###

#Make
make_summary <- new_evd %>%
  group_by(Make) %>%
  summarise(
    Frequency = n()
  ) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 2))

#City
city_summary <- new_evd %>%
  group_by(City) %>%
  summarise(
    Frequency = n()
  ) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 2))

print(make_summary)
print(city_summary)

############################################################

### Creating a bivariate table for chi square test

Table1 <- table(new_evd$Make, new_evd$City)
print(Table1)

### Percentage Table

PTable <- prop.table(Table1)
print(PTable)

Percent_table <- PTable*100
round(Percent_table, 2)

### Running chi square test

Chi_Sq <- chisq.test(Table1)

print(Chi_Sq)    #### X-squared = 2.0097, df = 2, p-value = 0.3661

###Effect size using Cramer's V test

library(DescTools)
CramerV(Table1) ### effect size = 0.011 is small

###Power Test

library(pwr)

pwr.chisq.test(w=0.011, 
               N=14911,
               df=2,
               sig.level = 0.05,
               power=NULL)

### Creating data frame for Bivariate Plot

make_city_data <- data.frame( Make = c("BMW", "Tesla", "Toyota"),
                              Seattle_N = c(1537, 11163, 1000),
                              Seattle_PCT = c(10.3, 74.9, 6.7),
                              Spokane_N = c(140, 970, 101),
                              Spokane_PCT = c(0.94, 6.5, 0.7),
                              Total_N = c(1677, 12133, 1101),
                              Total_PCT = c(11.25, 81.4, 7.4))
print(make_city_data)

library(tidyr)
library(ggplot2)

Final_Data <- make_city_data |> 
  select(Make, Seattle_PCT, Spokane_PCT) |> 
  pivot_longer(cols = -Make, names_to = "City", values_to = "Percentage")

print(Final_Data)

### Creating Bivariate Plot

Bivariate_plot <- ggplot(Final_Data, aes(x = Make, y = Percentage, fill = City)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#000080", "#FF9939"))+
  labs(title = "Distribution of Makes as per the City",
       x = "Make",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank())

print(Bivariate_plot)
