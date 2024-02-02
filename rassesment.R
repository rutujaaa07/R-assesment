# Load the relevant libraries

library("dplyr")
library("plyr")
library("ggplot2")
library("corrgram")
library("randomForest")
# Task 1: Exploratory Data Analysis

data <- read.csv("C:/Users/rutuj/Downloads/1657875746_day.csv", header = TRUE)
head(data)
dim(data)
# Display the structure of the dataset
str(data)

# Perform data type conversion of attributes (if needed)
# Example: Convert date column to Date format
data$dteday <- as.Date(data$dteday)

# Check data types after conversion
str(data)

#changing datatype of categorical variables
data$season = as.factor(data$season)
data$yr = as.factor(data$yr)
data$mnth =  as.factor(data$mnth)
data$holiday =  as.factor(data$holiday)
data$weekday =  as.factor(data$weekday)
data$workingday =  as.factor(data$workingday)
data$weathersit = as.factor(data$weathersit)
str(data)
summary(data)

# Carry out missing value analysis
sum(is.na(data))
#there are no missing values
sapply(data, function(x) sum(is.na(x)))

# Task 2: Attributes Distribution and Trends

#plotting boxplot for "cnt" variable 
boxplot(data$cnt, data=data, main = "Boxplot for cnt")

#plotting boxplot for "temp" variable
boxplot(data$temp, data=data, main = "Boxplot for temp")
# there are no outliers in temp variable

#plotting boxplot for "atemp" variable
boxplot(data$atemp, data=data, main = "Boxplot for atemp")
# there are no outliers in atemp variable

#plotting boxplot for "hum" variable
boxplot(data$hum, data=data, main = "Boxplot for hum")
#outliers present in hum variable

#plotting boxplot for "windspeed" variable
boxplot(data$windspeed, data=data, main = "Boxplot for windspeed")
#outliers present in windspeed variable

#plotting boxplot for "causal" variable
boxplot(data$casual, data=data, main = "Boxplot for casual")
#outliers present in casual variable

#plotting boxplot for "registered" variable
boxplot(data$registered, data=data, main = "Boxplot for registered")
#no outliers in registered variable

#treating outliers
#findout the numeric variables in dataset
numeric_index = sapply(data, is.numeric)

#prepare the numeric dataset 
numeric_data = data[, numeric_index]

cnames = colnames(numeric_data)

#loop to remove outliers from all variables
for(i in cnames){
  qnt = quantile(data[i], probs = c(.75, .25), na.rm = T)
  iqr = qnt[1] - qnt[2]
  min = qnt[2] - (iqr*1.5)
  max = qnt[1] + (iqr*1.5)
  print(min)
  print(max)
  data = subset(data, data[i]>min)
  data = subset(data, data[i]<max)
}

#there are 55 observations are dropped in outliers

#Correlation Analysis: Here we generating correlation matrix to understand 
#how the each variable related with each other. In that we plotting correlation matrix 
#and generate plot using corrgram library for better understanding

#this metrix wil be plot only using numeric data for that we provide numeric data
corr = cor(numeric_data)
print(corr)

#plotting correlation plot using corrgram library
corrgram(numeric_data, order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "Correlation Plot")

#The above correlation analysis shows that,
#temp and atemp are highly correlated
#temp and atemp have positive and strong correlation with cnt
#hum and windspeed have negative and weak correlation with cnt

#dropping atemp variable from a dataset
data1 = data[, !(names(data) %in% c("atemp"))]

# Plot monthly distribution of the total number of bikes rented
monthly_distribution <- aggregate(cnt ~ format(dteday, "%b %Y"), data = data1, sum)

# Plot using base R plot function
barplot(monthly_distribution$cnt, names.arg = monthly_distribution$`format(dteday, "%b %Y")`,
        col = "black" , main = "Monthly Distribution of Total Bike Rentals",
        xlab = "Month", ylab = "Total Rentals", las = 2, cex.names = 0.7)

#There is a seasonal pattern to the bike rentals, with peaks typically occurring in the warmer months (likely spring and summer) and troughs in the colder months (likely fall and winter).
#The peaks in bike rentals appear to be consistently higher during the middle of the year, which suggests that this period is the most popular time for bike rentals.
#The lowest points in the chart occur during the winter months, indicating a significant drop in bike rentals during this time.

# Yearly distribution of total number of bikes rented
yearly_distribution <- aggregate(cnt ~ format(dteday, "%Y"), data = data1, sum)

# Plot using base R barplot
barplot(yearly_distribution$cnt, names.arg = yearly_distribution$`format(dteday, "%Y")`,
        col = "skyblue", main = "Yearly Distribution of Total Bike Rentals",
        xlab = "Year", ylab = "Total Rentals", las = 2, cex.names = 0.7)

#There is some variation in the total number of bike rentals from year to year.
#The highest bars (indicating the most rentals) appear around the middle of the x-axis, suggesting that bike rentals were at their peak during these years.
#Towards the right end of the x-axis, there is a noticeable decline in the total number of bike rentals.

# Boxplot for outliers' analysis
boxplot(cnt ~ season, data = data1, col = "skyblue", border = "black",
        main = "Season vs. Rental Counts", xlab = "Season", ylab = "Total Rental Bikes")

#Season 2 has the highest median rental count, suggesting it is the most popular season for bike rentals.
#Season 1 has the lowest median rental count, indicating it is the least popular.
#The rental counts in seasons 2 and 3 have a higher spread (larger boxes), suggesting more variability in the rental counts during these seasons.
#Seasons 1 and 4 have outliers on the higher end, indicating there are some days with exceptionally high rental counts during these seasons, but they are not typical.

# Task 3: Split the dataset into train and test dataset

# Assuming you want to use 80% for training and 20% for testing
set.seed(42)  # for reproducibility
split = caTools::sample.split(data$cnt, SplitRatio = 0.8)
train_data = subset(data1, split == TRUE)
test_data = subset(data1, split == FALSE)

# Task 4: Create a model using the random forest algorithm

# Select features and target variable
X_train = train_data[, c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit", "temp", "hum", "windspeed")]
y_train = train_data$cnt

# Create the random forest model
rf_model = randomForest(y_train ~ ., data = data.frame(y_train, X_train), ntree = 100)

# Task 5: Predict the performance of the model on the test dataset

# Select features in the test dataset
X_test = test_data[, c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit", "temp", "hum", "windspeed")]

# Make predictions on the test dataset
y_pred = predict(rf_model, newdata = data.frame(X_test))

# Evaluate the model on the test dataset
mae = mean(abs(y_pred - test_data$cnt))
print(paste("Mean Absolute Error on Test Dataset:", mae))

#MAE of 483.22 means that, on average, model's predictions on the test dataset 
#are off by approximately 483.22 units with respect to the actual values.

#MAPE
y = test_data$cnt  
y_pred = predict(rf_model, newdata = data.frame(X_test))  

# Calculate MAPE
mape = mean(abs((y - y_pred) / y)) * 100

# Print the result
print(paste("Mean Absolute Percentage Error (MAPE):", mape))
#A MAPE of 20.05% indicates that, on average, model's predictions for daily bike rentals are off by 
#approximately 20.05% from the actual values on the test dataset.
