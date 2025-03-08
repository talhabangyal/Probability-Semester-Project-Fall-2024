# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Replace with the correct path to your CSV file
dataset <- read.csv("D:/Fast National University CFD/Probability Course/Project/remitence.csv")
View(dataset)

# Plot histograms (ensure the data is numeric)
hist(dataset$Remittance....billion., main="Histogram of Remittance (Billion)", xlab="Remittance (Billion)")
hist(dataset$Overseas.Pakistani.population..By.Country., main="Histogram of Overseas Pakistani Population", xlab="Population")
hist(dataset$Population..by.Continent., main="Histogram of Population by Continent", xlab="Population")
hist(dataset$Year, main="Histogram of Year", xlab="Year")

# Example of pie charts - ensure data is aggregated or is categorical
# Here assuming we use some aggregated data for pie chart visualization
# Aggregating data by country or continent might be more meaningful for pie chart.
agg_remittance <- aggregate(Remittance....billion. ~ Year, data = dataset, FUN = sum)
pie(agg_remittance$Remittance....billion., labels = agg_remittance$Year)

# Use barplot with aggregation if necessary
barplot(agg_remittance$Remittance....billion., names.arg = agg_remittance$Year)

# Summary statistics
mean(dataset$Remittance....billion.)
mean(dataset$Overseas.Pakistani.population..By.Country.)
mean(dataset$Population..by.Continent.)
mean(dataset$Year)

median(dataset$Remittance....billion.)
median(dataset$Overseas.Pakistani.population..By.Country.)
median(dataset$Population..by.Continent.)
median(dataset$Year)

var(dataset$Remittance....billion.)
var(dataset$Overseas.Pakistani.population..By.Country.)
var(dataset$Population..by.Continent.)
var(dataset$Year)

quantile(dataset$Remittance....billion.)
quantile(dataset$Overseas.Pakistani.population..By.Country.)
quantile(dataset$Population..by.Continent.)
quantile(dataset$Year)

# Poisson distribution fitting (only if appropriate for your data)
lambda <- mean(dataset$Overseas.Pakistani.population..By.Country.)
fit <- ppois(dataset$Overseas.Pakistani.population..By.Country., lambda, lower.tail=TRUE)
# Print Poisson fit results
cat("Poisson fit results:\n", fit)

# Linear regression model
model <- lm(Remittance....billion. ~ Overseas.Pakistani.population..By.Country., data = dataset)
summary(model)

# Predicted values using regression model
new_data <- data.frame(Overseas.Pakistani.population..By.Country. = c(100000, 200000, 300000))
predictions <- predict(model, newdata = new_data)
cat("Predicted remittance amounts:\n", predictions)

# Confidence interval for mean (use normal distribution assumption or t-test)
mean_ci <- t.test(dataset$Remittance....billion.)$conf.int
cat("Confidence interval for mean of Remittance:\n", mean_ci)

# Confidence interval for median (this requires manual calculation)
median_ci <- quantile(dataset$Remittance....billion., probs = c(0.025, 0.975))
cat("Confidence interval for median of Remittance:\n", median_ci)

# Confidence interval for regression coefficients
coef_ci <- confint(model)
cat("Confidence intervals for regression coefficients:\n", coef_ci)
