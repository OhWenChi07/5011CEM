# packages
install.packages("tidyverse")
install.packages("data.table")
install.packages("microbenchmark")
install.packages("parallel")
install.packages("ggpubr")
install.packages("ggplot2")

# libraries
library(tidyverse)
library(data.table)
library(readr)
library(modeest)
library(ggpubr)
library(ggplot2)
library(scales) 
library(microbenchmark)
library(parallel)

# Chapter 2
# Use readr package
list_csv_files <- list.files(path = "D:/WenChi/BigData/Area-level grocery purchases/")
df <- readr::read_csv(list_csv_files, id = "file_name")
df

# Use tidyverse package
df <-
  list.files(path = "D:/WenChi/BigData/Area-level grocery purchases/", pattern = "*.csv") %>% 
  map_df(~read_csv(.))
df

# Use data.table package
df <- function(i){
  list.files(path = "D:/WenChi/BigData/Area-level grocery purchases/Apr_osward_grocery/", pattern = "*.csv") %>%
    map_df(~fread(.))
}
df

# Microbenchmark - sequential processing
mbm <- microbenchmark(lapply(1:100, df))
mbm
autoplot(mbm)

# With system.time - sequential processing
system.time(lapply(1:100, df))

# Parallel Processing
numCores <- detectCores()
numCores

cl <- makeCluster(numCores)
clusterEvalQ(cl, {
  library(tidyverse)
  library(parallel)}
)

df2 <-function(i){
  parLapply(cl, 1:100, df)
  stopCluster(cl)
}

# Microbenchmark - Parallel processing
mbm <- microbenchmark(parLapply(cl, 1:100, df))
mbm
autoplot(mbm)

# With system.time - parallel processing
system.time({
  parLapply(cl, 1:100, df)
  stopCluster(cl)
})

# Chapter 3
# Import the data using read.csv()
msoa_grocery <- read.csv("D:/WenChi/BigData/Area-level grocery purchases/year_msoa_grocery.csv")
View(msoa_grocery)

# show top few rows
head(msoa_grocery)

# show bottom few rows
tail(msoa_grocery)

# Familiar all variables name
names(msoa_grocery)

# Comprehension of the dataset's structure
str(msoa_grocery)

# Calculating summary
summary(msoa_grocery)

# Descriptive Analysis
# Compute the mean value 
mean(msoa_grocery$weight)

# Compute the median value 
median(msoa_grocery$weight)

# Compute the mode value
mode = mfv(msoa_grocery$weight)
print(mode)

# Compute the maximum value 
max(msoa_grocery$weight)

# Compute the minimum value 
min(msoa_grocery$weight)

# Calculating Range 
range(msoa_grocery$weight)

# Calculating Variance 
var(msoa_grocery$weight)

# Calculating Standard deviation 
sd(msoa_grocery$weight)

# Calculating Quartiles 
quantile(msoa_grocery$weight)

# Calculating Interquartile Range
IQR(msoa_grocery$weight)

# Exploratory Data Analysis
# Box Plot
# Draw box plots based on variables
ggboxplot(msoa_grocery, y = "weight", width = 0.5) + 
  labs(x = "x", y = "Weight", title = "Box Plot of Weight in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggboxplot(msoa_grocery, y = "fat", width = 0.5) + 
  labs(x = "x", y = "Fat", title = "Box Plot of Fat in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggboxplot(msoa_grocery, y = "carb", width = 0.5) + 
  labs(x = "x", y = "Carb", title = "Box Plot of Carb in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggboxplot(msoa_grocery, y = "female", width = 0.5) + 
  labs(x = "x", y = "Female", title = "Box Plot of Female in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggboxplot(msoa_grocery, y = "male", width = 0.5) + 
  labs(x = "x", y = "Male", title = "Box Plot of Male in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggboxplot(msoa_grocery, y = "age_0_17", width = 0.5) +
  labs(x = "x", y = "Age_0_17", title = "Box Plot of Age_0_17 in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggboxplot(msoa_grocery, y = "age_18_64", width = 0.5) + 
  labs(x = "x", y = "Age_18_64", title = "Box Plot v Age_18_64 in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggboxplot(msoa_grocery, y = "age_65_above", width = 0.5) + 
  labs(x = "x", y = "Age_65_above", title = "Box Plot of Age_65_above in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

# Empirical Cumulative Distribution Function (ECDF)
# Calculate ECDF based on variables
ggecdf(msoa_grocery, x = "weight") + 
  labs(x = "Weight", y = "Frequency", title = "ECDF Plot of Weight in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggecdf(msoa_grocery, x = "fat") + 
  labs(x = "Fat", y = "Frequency", title = "ECDF Plot of Fat in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggecdf(msoa_grocery, x = "carb") + 
  labs(x = "Carb", y = "Frequency", title = "ECDF Plot of Carb in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggecdf(msoa_grocery, x = "female") + 
  labs(x = "Female", y = "Frequency", title = "ECDF Plot of Female in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggecdf(msoa_grocery, x = "male") + 
  labs(x = "Male", y = "Frequency", title = "ECDF Plot of Male in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggecdf(msoa_grocery, x = "age_0_17") + 
  labs(x = "Age_0_17", y = "Frequency", title = "ECDF Plot of Age_0_17 in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggecdf(msoa_grocery, x = "age_18_64") + 
  labs(x = "Age_18_64", y = "Frequency", title = "ECDF Plot of Age_18_64 in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

ggecdf(msoa_grocery, x = "age_65_above") + 
  labs(x = "Age_65_above", y = "Frequency", title = "ECDF Plot of Age_65_above in MSOA region Tesco Grocery") +
  theme(plot.title=element_text(hjust=0.5))

# Histogram
# Draw Histogram based on variables
gghistogram(msoa_grocery, x = "weight", bins = 15, add = "mean") +
  labs(x = "Weight", y = "Frequency", title = "Histogram of Weight in MSOA region Tesco Grocery") + 
  theme(plot.title=element_text(hjust=0.5))

gghistogram(msoa_grocery, x = "fat", bins = 15, add = "mean") +
  labs(x = "Fat", y = "Frequency", title = "Histogram of Fat in MSOA region Tesco Grocery") + 
  theme(plot.title=element_text(hjust=0.5))

gghistogram(msoa_grocery, x = "carb", bins = 15, add = "mean") +
  labs(x = "Carb", y = "Frequency", title = "Histogram of Carb in MSOA region Tesco Grocery") + 
  theme(plot.title=element_text(hjust=0.5))

gghistogram(msoa_grocery, x = "female", bins = 15, add = "mean") +
  labs(x = "Female", y = "Frequency", title = "Histogram of Female in MSOA region Tesco Grocery") + 
  theme(plot.title=element_text(hjust=0.5))

gghistogram(msoa_grocery, x = "male", bins = 15, add = "mean") +
  labs(x = "Male", y = "Frequency", title = "Histogram of Male in MSOA region Tesco Grocery") + 
  theme(plot.title=element_text(hjust=0.5))

gghistogram(msoa_grocery, x = "age_0_17", bins = 15, add = "mean") +
  labs(x = "Age_0_17", y = "Frequency", title = "Histogram of Age_0_17 in MSOA region Tesco Grocery") + 
  theme(plot.title=element_text(hjust=0.5))

gghistogram(msoa_grocery, x = "age_18_64", bins = 15, add = "mean") +
  labs(x = "Age_18_64", y = "Frequency", title = "Histogram of Age_18_64 in MSOA region Tesco Grocery") + 
  theme(plot.title=element_text(hjust=0.5))

gghistogram(msoa_grocery, x = "age_65_above", bins = 15, add = "mean") +
  labs(x = "Age_65_above", y = "Frequency", title = "Histogram of age_65_above in MSOA region Tesco Grocery") + 
  theme(plot.title=element_text(hjust=0.5))

# create correlation matrix of (rounded to 2 decimal places)
# First Correlation Matrix
round(cor(msoa_grocery[c('weight', 'fat', 'carb', 'female', 'male')]), 2)

# Second Correlation Matrix
round(cor(msoa_grocery[c('weight', 'fat', 'carb', 'age_0_17', 'age_18_64', 'age_65_above')]), 2)

# Scatter Plot
ggscatter(msoa_grocery, x = "fat", y = "weight", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Fat", ylab = "Weight") + 
  labs(title="Scatter Plots between Weight and Fat") +
  theme(plot.title=element_text(hjust=0.5))

ggscatter(msoa_grocery, x = "fat", y = "carb", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Carb", ylab = "Weight") + 
  labs(title="Scatter Plots between Weight and Carb") +
  theme(plot.title=element_text(hjust=0.5))

# Shapiro-Wilk normality test for weight, fat, and carb
shapiro.test(msoa_grocery$weight) 
shapiro.test(msoa_grocery$fat)
shapiro.test(msoa_grocery$carb)

# Q-Q Plot for weight and fat
ggqqplot(msoa_grocery$weight, ylab = "Weight") +
  labs(title = "Q-Q Plot of Weight") + 
  theme(plot.title=element_text(hjust=0.5))

ggqqplot(msoa_grocery$fat, ylab = "Fat") + 
  labs(title = "Q-Q Plot of Fat") + 
  theme(plot.title=element_text(hjust=0.5))

ggqqplot(msoa_grocery$fat, ylab = "Carb") + 
  labs(title = "Q-Q Plot of Carb") + 
  theme(plot.title=element_text(hjust=0.5))

# Pearson correlation test
res <- cor.test(msoa_grocery$weight, msoa_grocery$fat, method = "pearson")
res

res <- cor.test(msoa_grocery$weight, msoa_grocery$carb, method = "pearson")
res

# Linear Regression

# Model 1: weight, fat
# Create linear regression model
model <- lm(weight~fat, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# not sure want or not
msoa_grocery %>%
  select(weight, fat) %>%
  pairs

# Model 2: weight, carb
# Create linear regression model
model <- lm(weight~carb, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 3: fat, female
# Create linear regression model
model <- lm(fat~female, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 4: fat, male
# Create linear regression model
model <- lm(fat~male, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 5: carb, female
# Create linear regression model
model <- lm(carb~female, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 6: carb, male
# Create linear regression model
model <- lm(carb~male, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 7: fat, age_0_17
# Create linear regression model
model <- lm(fat~age_0_17, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 8: carb, age_0_17
# Create linear regression model
model <- lm(carb~age_0_17, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 9: fat, age_18_64
# Create linear regression model
model <- lm(fat~age_18_64, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 10: carb, age_18_64
# Create linear regression model
model <- lm(carb~age_18_64, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 11: fat, age_65_above
# Create linear regression model
model <- lm(fat~age_65_above, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 12: carb, age_65_above
# Create linear regression model
model <- lm(carb~age_65_above, data=msoa_grocery)

# Print regression model
print(model)
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Multiple Linear Regression
attach(msoa_grocery)

# Model 1: weight, fat, carb
m1 <- msoa_grocery[1:50, c("weight", "fat", "carb")]

# Create regression model
model <- lm(weight~fat + carb, data = m1)
model

# Summary the regression model
summary(model)$coefficients
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 2: fat, female, male
m1 <- msoa_grocery[1:50, c("fat", "female", "male")]

# Create regression model
model <- lm(fat~female + male, data = m1)
model

# Summary the regression model
summary(model)$coefficients
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 3: carb, female, male
m1 <- msoa_grocery[1:50, c("carb", "female", "male")]

# Create regression model
model <- lm(carb~female + male, data = m1)
model

# Summary the regression model
summary(model)$coefficients
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 4: fat, age_0_17, age_18_64, age_65_above
m1 <- msoa_grocery[1:50, c("fat", "age_0_17", "age_18_64", "age_65_above")]

# Create regression model
model <- lm(fat~age_0_17 + age_18_64 + age_65_above, data = m1)
model

# Summary the regression model
summary(model)$coefficients
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)

# Model 5: carb, age_0_17, age_18_64, age_65_above
m1 <- msoa_grocery[1:50, c("carb", "age_0_17", "age_18_64", "age_65_above")]

# Create regression model
model <- lm(carb~age_0_17 + age_18_64 + age_65_above, data = m1)
model

# Summary the regression model
summary(model)$coefficients
summary(model)

# Plot the graph
par(mfrow=c(2,2))
plot(model)
