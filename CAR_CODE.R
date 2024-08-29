# install necessary packages  
install.packages("corrplot","ggplot2","gridExtra","tidyverse","class","caret","dplyr")
install.packages("pROC")
library(tidyverse)
library(ggplot2)
library(corrplot)
library(class)# knn
library(gridExtra)
library(caret)
library(dplyr)#select function
library(pROC)

# Set working directory
setwd("C:\\Users\\Admin\\Desktop\\DMM_FINAL")

# Load dataset
car_data <- read.csv("car_data.csv", header = TRUE, na.string = c(""), stringsAsFactors = TRUE)

str(car_data)#view the data frame

summary(car_data)# look at data summary

dim(car_data)# check the shape of the data: 11914    16

# renaming the columns 
colnames(car_data)[colnames(car_data) == "MSRP"] <- "price"
colnames(car_data)[colnames(car_data) == "Engine.Fuel.Type"] <- "Engine_FuelType"
colnames(car_data)[colnames(car_data) == "Engine.HP"] <- "Engine_HP"
colnames(car_data)[colnames(car_data) == "Engine.Cylinders"] <- "Engine_Cylinders"
colnames(car_data)[colnames(car_data) == "Transmission.Type"] <- "Transmission_Type"
colnames(car_data)[colnames(car_data) == "Number.of.Doors"] <- "Number_of_Doors"
colnames(car_data)[colnames(car_data) == "Market.Category"] <- "Market_Category"
colnames(car_data)[colnames(car_data) == "Vehicle.Size"] <- "Vehicle_Size"
colnames(car_data)[colnames(car_data) == "Vehicle.Style"] <- "Vehicle_Style"
colnames(car_data)[colnames(car_data) == "highway.MPG"] <- "highway_MPG"
colnames(car_data)[colnames(car_data) == "city.mpg"] <- "city_mpg"

# Check for missing values and display the result
# Count missing values in each column
missing_counts <- colSums(is.na(car_data))
print(missing_counts)

# Calculate the percentage of missing values for each column
null_vals <- colMeans(is.na(car_data)) * 100
null_df <- data.frame(Feature = names(null_vals), Percent_missing = null_vals)

# Create a bar plot
ggplot(null_df, aes(x = Feature, y = Percent_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Graph for Null Variables",
       x = "Feature",
       y = "Percent missing") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

# Remove rows with missing values
car_data <- na.omit(car_data)

# reCheck for missing values and display the result after removing
missing_counts <- colSums(is.na(car_data))
print(missing_counts)

# check the shape of the datac again
dim(car_data) # 11812    16

#check for infinite number
variables <- c("Make", "Model", "Year", "Engine_FuelType", "Engine_HP",
               "Engine_Cylinders", "Transmission_Type", "Driven_Wheels", "Number_of_Doors", "Market_Category",
               "Vehicle_Size", "Vehicle_Style", "highway_MPG", "city_mpg", "Popularity", "price")

for (variable in variables) {
  print(paste(variable, "has infinite values:", any(is.infinite(car_data[[variable]]))))
}


# Create a list of numeric variables you want to plot
numeric_var_plot <- c( "Year","Engine_HP","Engine_Cylinders","Number_of_Doors","highway_MPG","city_mpg","Popularity","price"
)
# Create a list to store the plots
plots <- list()
# Loop through the variables and create plots
for (variable in numeric_var_plot) {
  plot <- ggplot(car_data, aes(x = .data[[variable]])) +
    geom_histogram(fill = "blue", bins = 30, color = "black") +
    labs(title = paste("Distribution of", variable), x = variable, y = "Frequency")
  
  plots[[variable]] <- plot
}
# Arrange and plot the plots in a grid with a 2x4 layout
grid.arrange(grobs = plots, ncol = 2, nrow = 4)

# Distribution plot for factor variables
plot1 <- ggplot(car_data, aes(x = as.factor(Make))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Make", x = "Make", y = "Frequency")
plot2 <- ggplot(car_data, aes(x = as.factor(Model))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Model", x = "Model", y = "Frequency")
plot3 <- ggplot(car_data, aes(x = as.factor(Engine_FuelType))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Engine_FuelType", x = "Engine_FuelType", y = "Frequency")
plot4 <- ggplot(car_data, aes(x = as.factor(Transmission_Type))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Transmission_Type", x = "Transmission_Type", y = "Frequency")

# Arrange plots in a 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Distribution plot for factor variables
plot1 <- ggplot(car_data, aes(x = as.factor(Driven_Wheels))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Driven_Wheels", x = "Driven_Wheels", y = "Frequency")
plot2 <- ggplot(car_data, aes(x = as.factor(Market_Category))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Market_Category", x = "Market_Category", y = "Frequency")
plot3 <- ggplot(car_data, aes(x = as.factor(Vehicle_Size))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Vehicle_Size", x = "Vehicle_Size", y = "Frequency")
plot4 <- ggplot(car_data, aes(x = as.factor(Vehicle_Style))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Vehicle_Style", x = "Vehicle_Style", y = "Frequency")

# Arrange plots in a 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Data encoding
# Convert factor columns to numeric
car_data$Make_numeric <- as.integer(car_data$Make)
car_data$Model_numeric <- as.integer(car_data$Model)
car_data$Engine_FuelType_numeric <- as.integer(car_data$Engine_FuelType)
car_data$Transmission_Type_numeric <- as.integer(car_data$Transmission_Type)
car_data$Driven_Wheels_numeric <- as.integer(car_data$Driven_Wheels)
car_data$Market_Category_numeric <- as.integer(car_data$Market_Category)
car_data$Vehicle_Size_numeric <- as.integer(car_data$Vehicle_Size)
car_data$Vehicle_Style_numeric <- as.integer(car_data$Vehicle_Style)

# Remove original factor columns not needed since it has been encoded
cols_to_remove <- c("Make", "Model", "Engine_FuelType", "Transmission_Type","Driven_Wheels","Market_Category","Vehicle_Size","Vehicle_Style")
car_data <- car_data[, !colnames(car_data) %in% cols_to_remove]

# Check summary after encoding and  factor columns
summary(car_data)
#view the data frame
str(car_data)

# check for outliers using boxplot for visualizing the variables
variables <- c("Year","Engine_HP","Engine_Cylinders","Number_of_Doors","highway_MPG","city_mpg","Popularity","price")
# Set up the layout for the plots
par(mfrow = c(3, 3))
# Create boxplots for each variable
for (variable in variables) {
  boxplot(car_data[[variable]], main = paste("Box Plot of", variable))
}
# Reset the layout to the default
par(mfrow = c(1, 1))

# check for outliers using boxplot for visualizing the variables
variables <- c("Make_numeric","Model_numeric","Engine_FuelType_numeric","Transmission_Type_numeric","Driven_Wheels_numeric","Market_Category_numeric","Vehicle_Size_numeric","Vehicle_Style_numeric")
# Set up the layout for the plots
par(mfrow = c(3, 3))
# Create boxplots for each variable
for (variable in variables) {
  boxplot(car_data[[variable]], main = paste("Box Plot of", variable))
}
# Reset the layout to the default
par(mfrow = c(1, 1))
dim(car_data)

# correct outliers 
collumns <- c("price","Year","Engine_HP","Engine_Cylinders","Number_of_Doors","highway_MPG","city_mpg","Popularity",
              "Make_numeric","Model_numeric","Engine_FuelType_numeric","Transmission_Type_numeric","Driven_Wheels_numeric","Market_Category_numeric","Vehicle_Size_numeric","Vehicle_Style_numeric"
)
for (col in collumns) {
  Q1 <- quantile(car_data[[col]], 0.25)
  Q3 <- quantile(car_data[[col]], 0.75)
  IQR <- Q3 - Q1
  k <- 1.5 # IQR multiplier 
  lower_bound <- Q1 - k * IQR
  upper_bound <- Q3 + k * IQR
  car_data <- car_data[car_data[[col]] >= lower_bound & car_data[[col]] <= upper_bound, ]
}
summary(car_data)
dim(car_data)

# check after outliers using boxplot for visualizing the variables
variables <- c("Year","Engine_HP","Engine_Cylinders","Number_of_Doors","highway_MPG","city_mpg","Popularity","price")
# Set up the layout for the plots
par(mfrow = c(3, 3))
# Create boxplots for each variable
for (variable in variables) {
  boxplot(car_data[[variable]], main = paste("Box Plot of", variable))
}
# Reset the layout to the default
par(mfrow = c(1, 1))

# check for outliers using boxplot for visualizing the variables
variables <- c("Make_numeric","Model_numeric","Engine_FuelType_numeric","Transmission_Type_numeric","Driven_Wheels_numeric","Market_Category_numeric","Vehicle_Size_numeric","Vehicle_Style_numeric")
# Set up the layout for the plots
par(mfrow = c(3, 3))
# Create boxplots for each variable
for (variable in variables) {
  boxplot(car_data[[variable]], main = paste("Box Plot of", variable))
}
# Reset the layout to the default
par(mfrow = c(1, 1))


# check and remove near-zero variance variables(library(caret))
car_data_check <- nearZeroVar(car_data, saveMetrics = TRUE)
car_data <- car_data[, !car_data_check$nzv]

# correlation visualization of relationship
selected_variables <- c("price","Year","Engine_HP","Number_of_Doors","city_mpg","Popularity","Make_numeric","Model_numeric","Engine_FuelType_numeric","Driven_Wheels_numeric","Market_Category_numeric","Vehicle_Size_numeric","Vehicle_Style_numeric")

# Subset with selected variables
NewCar_data <- car_data[selected_variables]

# Plotting the correlation heatmap (if u add this type = "upper")
correlation_matrix <- cor(NewCar_data)
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("white", "blue"))(20), 
         tl.col = "black", tl.srt = 45,type = "upper", addCoef.col = "black", number.cex = 0.7)

# Standardize the selected features
features <- c("price","Year","Engine_HP","Number_of_Doors","city_mpg","Popularity","Make_numeric","Model_numeric",
              "Engine_FuelType_numeric","Driven_Wheels_numeric","Market_Category_numeric","Vehicle_Size_numeric","Vehicle_Style_numeric")

# Min-max normalization function
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Apply min-max scaling function to relevant columns
NewCar_data[features] <- lapply(NewCar_data[features], min_max_transform)
summary(NewCar_data)

# Split data into training and testing sets
set.seed(231)  # Set seed for reproducibility
split_data <- sample(nrow(NewCar_data), nrow(NewCar_data) * 0.8)
train_data <- NewCar_data[split_data, ] # training data
test_data <- NewCar_data[-split_data, ] # testing data
dim(train_data)
dim(test_data)
############################ Knn model ####################################################

#Model Training and Hyperparameter Tuning
features <- c( "Engine_HP", "Popularity", "Make_numeric", "Model_numeric", "Vehicle_Size_numeric", "Vehicle_Style_numeric")

target <- "price"
k_values <- c(1, 3, 5, 7, 9)# Define a range of k values
rmse_values <- numeric(length(k_values))# Initialize a vector to store RMSE values
best_k_predictions <- numeric(nrow(test_data))# Initialize a vector to store predictions for the best k

# Iterate through each k value
for (i in seq_along(k_values)) {
  knn_model <- kknn::kknn(formula = price ~ ., train = train_data[, c(features, target)], test = test_data[, features], k = k_values[i])
  # Predict on the test set
  predictions <- predict(knn_model, newdata = test_data[, features])
  # Calculate RMSE (Root Mean Squared Error)
  rmse_values[i] <- sqrt(mean((test_data[, target] - predictions)^2))
  # Save predictions for the best k
  if (i == which.min(rmse_values)) {
    best_k_predictions <- predictions
  }
}

# Plot the RMSE for each k value
plot(k_values, rmse_values, type = "b", pch = 19, xlab = "k", ylab = "RMSE",
     main = "KNN Regression Model RMSE for Different k Values")

######Making Predictions with the Best k#####
#from the plot it show that k = 5 is the best for the model
# Define predictors and target variable
X_train <- train_data[, features]
y_train <- train_data$price
X_test <- test_data[, features]
y_test <- test_data$price

# Build the KNN model
k_value <- 5  # You can choose your own value for k
knn_model <- knn(train = X_train, test = X_test, cl = y_train, k = k_value)

# Make predictions on the test set
y_predicts <- knn_model

# Ensure both knn_model and y_test are numeric
knn_model_numeric <- as.numeric(as.character(knn_model))  # Convert factors to numeric
y_test_numeric <- as.numeric(as.character(y_test))  # Convert factors to numeric

# Evaluate the model
mse <- mean((y_test_numeric - knn_model_numeric)^2)
r2 <- cor(y_test_numeric, knn_model_numeric)^2 * 100
mae <- mean(abs(y_test_numeric - knn_model_numeric))

cat("knn Metrics:\n")
cat("Mean Squared Error:", mse, "\n")# Mean Squared Error: 0.01016505 
cat("R2 Score:", r2, "\n")#R2 Score: 79.86067
cat("Mean Absolute Error:", mae, "\n")#Mean Absolute Error: 0.05586742

#Plotting Predictions vs. Actual Prices
x = 1:length(y_test_numeric)

plot(x, y_test_numeric, col = "red", type = "l", lwd=2,
     main = "car  test data prediction")
lines(x, knn_model_numeric, col = "blue", lwd=2)
legend("topright",  legend = c("original-price", "predicted-price"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

# plot accuracy vs choice of k(Plotting Accuracy vs. Neighbors)
plot(knn_model_numeric, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification accuracy",
     main = "Accuracy vs Neighbors")
# add lines indicating k with best accuracy
abline(v = which(knn_model_numeric == max(knn_model_numeric)), col = "darkorange", lwd = 1.5)
# add line for max accuracy seen
abline(h = max(knn_model_numeric), col = "grey", lty = 2)

# Evaluate the model(Printing Overall Accuracy)
conf_matrix <- table(knn_model, test_data[, target])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")


