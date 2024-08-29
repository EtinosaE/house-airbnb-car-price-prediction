# Install and load required libraries
install.packages(c("corrplot","ggplot2","caTools","glmnet","Metrics"))#,"tidyverse", "caret"
library(corrplot)
library(ggplot2)
library(caTools)#for spliting 
library(glmnet)#lasso model
library(Metrics)

setwd("C:\\Users\\Admin\\Desktop\\DMM_FINAL")

# load the dataset
nyc_data <- read.csv("AB_NYC_2019.csv", header = TRUE, na.string = c(""), stringsAsFactors = TRUE)


###########################CLEANING AND PREPROCESSING ##########################

summary(nyc_data)#Display the first few rows

cat("Shape of the dataset:", dim(nyc_data), "\n") #Display the shape of the Dataset

str(nyc_data) #Display data types

dim(nyc_data)#let look at the dimension of dataset.

nyc_data$last_review <- as.Date(nyc_data$last_review) #Convert 'last_review' to datetime

# Display the count of missing values
colSums(is.na(nyc_data)) 

# Drop columns name, host_name and last_review
nyc_data <- nyc_data[, !(names(nyc_data) %in% c("name","host_name","last_review"))]

# Fill missing values in 'reviews_per_month' with mean
nyc_data$reviews_per_month[is.na(nyc_data$reviews_per_month)] <- mean(nyc_data$reviews_per_month, na.rm = TRUE)

# Display the count of missing values after hanling
colSums(is.na(nyc_data))
summary(nyc_data)

cat("Shape of the dataset:", dim(nyc_data), "\n") #Display the shape of the Dataset

# Label encoding for 'neighbourhood_group', 'neighbourhood', and 'room_type'
nyc_data$neighbourhood_group <- as.numeric(factor(nyc_data$neighbourhood_group))
nyc_data$neighbourhood <- as.numeric(factor(nyc_data$neighbourhood))
nyc_data$room_type <- as.numeric(factor(nyc_data$room_type))

# check for outliers using boxplot for visualizing the variable
variables <- c("id","host_id","neighbourhood_group","neighbourhood","latitude","longitude","room_type","price",
               "minimum_nights","number_of_reviews","reviews_per_month","calculated_host_listings_count","availability_365")

# Set up the layout for the plots
par(mfrow = c(3, 5))

# Create boxplots for each variable
for (variable in variables) {
  boxplot(nyc_data[[variable]], main = paste("Box Plot of", variable))
}
# Reset the layout to the default
par(mfrow = c(1, 1))

cat("Number of rows before removing outliers:", nrow(nyc_data), "\n")

# outlier detection and correct
columns <- c("id","host_id","neighbourhood_group","neighbourhood","latitude","longitude","room_type","price",
             "minimum_nights","number_of_reviews","reviews_per_month","calculated_host_listings_count","availability_365")

threshold <- 1.5  # set threshold 

for (col in columns) {
  Q1 <- quantile(nyc_data[[col]], 0.25)
  Q3 <- quantile(nyc_data[[col]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - threshold * IQR
  upper_bound <- Q3 + threshold * IQR
  
  nyc_data <- nyc_data[nyc_data[[col]] >= lower_bound & nyc_data[[col]] <= upper_bound, ]
}

# check for outliers using boxplot for visualizing the variable
variables <- c("id","host_id","neighbourhood_group","neighbourhood","latitude","longitude","room_type","price",
               "minimum_nights","number_of_reviews","reviews_per_month","calculated_host_listings_count","availability_365")

# Set up the layout for the plots
par(mfrow = c(3, 5))

# Create boxplots for each variable
for (variable in variables) {
  boxplot(nyc_data[[variable]], main = paste("Box Plot of", variable))
}
# Reset the layout to the default
par(mfrow = c(1, 1))

cat("Number of rows after removing outliers:", nrow(nyc_data), "\n")
summary(nyc_data)

# correlation visualization of relationship
selected_variables <- c("price", "id", "host_id", "neighbourhood_group", "neighbourhood", 
                        "latitude", "longitude", "room_type", "minimum_nights", 
                        "number_of_reviews", "reviews_per_month", "calculated_host_listings_count", "availability_365")

# Subset with selected variables
selected_data <- nyc_data[selected_variables]

# Calculate the correlation matrix
correlation_matrix <- cor(selected_data)

# Plotting the correlation heatmap (if u add this type = "upper",
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("white", "blue"))(20), 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)

# bar plot of Price by Neighbourhood Group 
ggplot(nyc_data, aes(x = neighbourhood_group, y = price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Bar Plot of Price by Neighbourhood Group", x = "Neighbourhood Group",y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# varriables to normalize 
variables_to_normalize <- c("price","host_id","neighbourhood_group","neighbourhood","latitude","longitude","room_type",
                            "minimum_nights","number_of_reviews","reviews_per_month","calculated_host_listings_count","availability_365")

# this is the Function to perform Min-Max scaling
min_max_scaling <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Min-Max scaling to selected variables
nyc_data_normalized <- nyc_data
nyc_data_normalized[variables_to_normalize] <- lapply(nyc_data[variables_to_normalize], min_max_scaling)
summary(nyc_data_normalized)

# Split the dataset into training and testing sets
set.seed(123)
split <- sample.split(nyc_data_normalized$price, SplitRatio = 0.8)
train_data <- subset(nyc_data_normalized, split == TRUE)
test_data <- subset(nyc_data_normalized, split == FALSE)

dim(train_data)
dim(test_data)

################ Fit Lasso regression model ##########################
lasso_model <- glmnet(
  x = model.matrix(price ~ id + host_id + neighbourhood_group + neighbourhood + latitude + longitude + room_type + minimum_nights + number_of_reviews + reviews_per_month + calculated_host_listings_count + availability_365, 
                   data = train_data)[,-1],
  y = train_data$price,
  alpha = 1,
  lambda = 0.001
)
print(lasso_model)

# predictions on the testing data
lasso_predictions <- predict(lasso_model, s = 0.001, newx = model.matrix(price ~ id + host_id + neighbourhood_group + neighbourhood + latitude + longitude + room_type + minimum_nights + number_of_reviews + reviews_per_month + calculated_host_listings_count + availability_365, data = test_data)[,-1])

# Calculate residuals using testing response variable
lasso_residuals <- test_data$price - lasso_predictions

# Plot residuals
plot(lasso_residuals, ylab = "Residuals", xlab = "Observation Index", main = "LASSO Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a reference line at y = 0

# Display regression metrics for Lasso Regression
cat("Lasso Regression Metrics:\n")
cat("Mean Squared Error:", sqrt(mean((test_data$price - lasso_predictions)^2)), "\n")#Mean Squared Error: 48.24595
cat("R2 Score:", cor(lasso_predictions, test_data$price)^2 * 100, "\n")#R2 Score: 46.24248 
cat("Mean Absolute Error:", mean(abs(test_data$price - lasso_predictions)), "\n")#Mean Absolute Error: 36.21038 

#############cross-validation and fitting the best lambda#######################

# Create model matrix for both training and testing data
X_train <- model.matrix(price ~ neighbourhood_group + neighbourhood + latitude + longitude + room_type + minimum_nights + number_of_reviews + reviews_per_month + calculated_host_listings_count + availability_365, data = train_data)[,-1]
X_test <- model.matrix(price ~ neighbourhood_group + neighbourhood + latitude + longitude + room_type + minimum_nights + number_of_reviews + reviews_per_month + calculated_host_listings_count + availability_365, data = test_data)[,-1]

# Convert response variable to a numeric vector
#y_train <- as.numeric(train_data$price)
y_train <- (train_data$price)
# Set up cross-validation control
cv <- cv.glmnet( x = X_train, y = y_train,
  alpha = 1,        
  nfolds = 10        # Number of folds for cross-validation
)

# Print cross-validation results
print(cv)

# Obtain the best lambda value from cross-validation
best_lambda <- cv$lambda.min
print(best_lambda)

# Fit the Lasso model using the best lambda
lasso_model <- glmnet(
  x = X_train,
  y = y_train,
  alpha = 1,
  lambda = best_lambda
)

# Obtain predictions on the testing data
lasso_predictions <- predict(lasso_model, s = best_lambda, newx = X_test)

# Calculate residuals using testing response variable
lasso_residuals <- test_data$price - lasso_predictions

# Plot residuals
plot(lasso_residuals, ylab = "Residuals", xlab = "Observation Index", main = "LASSO Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a reference line at y = 0

# Display regression metrics for Lasso Regression
cat("Lasso Regression Metrics:\n")
cat("Mean Squared Error:", sqrt(mean((test_data$price - lasso_predictions)^2)), "\n")
cat("R2 Score:", cor(lasso_predictions, test_data$price)^2 * 100, "\n")
cat("Mean Absolute Error:", mean(abs(test_data$price - lasso_predictions)), "\n")

