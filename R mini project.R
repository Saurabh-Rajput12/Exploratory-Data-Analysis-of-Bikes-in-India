#download required library
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

#import dataset
setwd("C:/Users/Lenovo/Desktop")
bikes <- read.csv("bikes_india_10000.csv") 

#Data Inspection
str(bikes)
summary(bikes)

#Descriptive Statistics:
summary(bikes$price_inr)
summary(bikes$displacement_cc)
summary(bikes$mileage_kmpl)

#Data Visualization:
#Histogram of Mileage
ggplot(bikes, aes(x = mileage_kmpl)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Mileage") +
  xlab("Mileage (km/l)") +
  ylab("Frequency")

#Boxplot of Price
ggplot(bikes, aes(y = price_inr)) +
  geom_boxplot(fill = "lightcoral") +
  ggtitle("Boxplot of Price (INR)") +
  ylab("Price (INR)")

#Scatter plot: Displacemeny vs Mileage
ggplot(bikes, aes(x = displacement_cc, y = mileage_kmpl)) +
  geom_point(alpha = 0.6, color = "purple") +
  ggtitle("Displacement vs. Mileage") +
  xlab("Displacement (cc)") +
  ylab("Mileage (km/l)")

#Correlation Heatmap (for selected numeric variables):
install.packages("corrplot")
library(corrplot)
num_vars <- bikes[, c("displacement_cc", "cylinders", "power_ps", "mileage_kmpl", "price_inr")]
corr_matrix <- cor(num_vars, use = "complete.obs")
corrplot(corr_matrix, method = "circle")

#generation of bikes by all companies
company_counts <- table(bikes$company)
company_counts

#Bar plot
barplot(company_counts, main = "Number of Bike Models by Company", las = 2, col = "skyblue")

#Bike Generation by Company Per Year
library(dplyr)
library(ggplot2)

# Group by company and year, count models
gen_by_company_year <- bikes %>%
  group_by(company, year_launched) %>%
  summarise(num_models = n()) %>%
  arrange(company, year_launched)

# View summary table
head(gen_by_company_year, 15)

# Visualize trends for top companies
top_companies <- names(sort(table(bikes$company), decreasing=TRUE)[1:5])
gen_top <- gen_by_company_year %>% filter(company %in% top_companies)

ggplot(gen_top, aes(x=year_launched, y=num_models, color=company)) +
  geom_line(size=1) +
  geom_point() +
  labs(title = "Bike Generation by Year for Top Companies",
       x = "Year Launched",
       y = "Number of Models") +
  theme_minimal()
