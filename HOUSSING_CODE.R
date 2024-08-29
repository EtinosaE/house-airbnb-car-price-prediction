# install necessary packages  
install.packages(c("tidyverse", "ggplot2", "corrplot", "Hmisc", "Metrics", "MLmetrics", "lmtest","caTools", "car","caret","randomForest"))
library(tidyverse)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(Metrics)
library(lmtest)
library(car)# Function to calculate VIF
library(caret)
library(gridExtra)
library("randomForest")
library(tree)
library(caTools)#for spliting 

#Set the working directory if needed
setwd("C:\\Users\\Admin\\Desktop\\DMM_FINAL")
# load the dataset
housing_dat <- read.csv("kc_housing.csv", header = TRUE, na.string = c(""), stringsAsFactors = TRUE)

##### data exploration #####

summary(housing_dat)# look at data summary

dim(housing_dat)#let look at the dimension of dataset.

sapply(housing_dat, class)# list the types for every columns

str(housing_dat)#view the data frame

###### data cleaning and preprocess #####

# Check for missing values and display the result
col_missing_counts <- colSums(is.na(housing_dat))
col_missing_counts

head(housing_dat)# Display the first few rows

# Check for infinite values in each variable
variables <- c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
               "floors","waterfront","view","condition","grade","sqft_above",
               "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15")

for (variable in variables) {
  print(paste(variable, "has infinite values:", any(is.infinite(housing_dat[[variable]]))))
}

# Remove columns not needed 
cols_to_remove <- c("id","date","zipcode")
housing_dat <- housing_dat[, !colnames(housing_dat) %in% cols_to_remove]
summary(housing_dat)# look at data summary

# check for outliers using boxplot for visualizing the variables
variables <- c("price","bedrooms","bathrooms",
               "floors","waterfront","view","sqft_above",
               "sqft_basement","yr_built","yr_renovated","lat","long")
par(mfrow = c(3, 4))# Set up the layout for the plots
# Create boxplots for each variable
for (variable in variables) {
  boxplot(housing_dat[[variable]], main = paste("Box Plot of", variable))
}
# Reset the layout to the default
par(mfrow = c(1, 1))

# check for outliers using boxplot for visualizing the variables
variables <- c("sqft_living","sqft_lot","condition" ,"grade","sqft_living15","sqft_lot15")
par(mfrow = c(2, 3))# Set up the layout for the plots
# Create boxplots for each variable
for (variable in variables) {
  boxplot(housing_dat[[variable]], main = paste("Box Plot of", variable))}
# Reset the layout to the default
par(mfrow = c(1, 1))

cat("Number of rows before removing outliers:", nrow(housing_dat), "\n")

# remove outliers
cols <- c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
          "floors","waterfront","view","condition","grade","sqft_above",
          "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15")

for (col in cols) {
  Q1 <- quantile(housing_dat[[col]], 0.25)
  Q3 <- quantile(housing_dat[[col]], 0.75)
  IQR <- Q3 - Q1
  k <- 1.5 # IQR multiplier 
  lower_bound <- Q1 - k * IQR
  upper_bound <- Q3 + k * IQR
  housing_dat <- housing_dat[housing_dat[[col]] >= lower_bound & housing_dat[[col]] <= upper_bound, ]
}

cat("Number of rows after removing outliers:", nrow(housing_dat), "\n")

summary(housing_dat)

# check for outliers using boxplot for visualizing the variables
variables <- c("price","bedrooms","bathrooms",
               "floors","waterfront","view","sqft_above",
               "sqft_basement","yr_built","yr_renovated","lat","long")
par(mfrow = c(3, 4))# Set up the layout for the plots
# Create boxplots for each variable
for (variable in variables) {
  boxplot(housing_dat[[variable]], main = paste("Box Plot of", variable))
}
# Reset the layout to the default
par(mfrow = c(1, 1))

# check for outliers using boxplot for visualizing the variables
variables <- c("sqft_living","sqft_lot","condition" ,"grade","sqft_living15","sqft_lot15")
par(mfrow = c(2, 3))# Set up the layout for the plots
# Create boxplots for each variable
for (variable in variables) {
  boxplot(housing_dat[[variable]], main = paste("Box Plot of", variable))}
# Reset the layout to the default
par(mfrow = c(1, 1))

summary(housing_dat)

correlation_matrix <- cor( housing_dat)

# Plotting the correlation heatmap (if u add this type = "upper",
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("white", "blue"))(20), 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)

# Remove unnecessary columns (columns without range and high corrinated)  
cols_to_remove <- c("waterfront","view","yr_renovated","sqft_above")
housing_dat <- housing_dat[, !colnames(housing_dat) %in% cols_to_remove]

correlation_matrix <- cor( housing_dat)

# Plotting the correlation heatmap (if u add this type = "upper",
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("white", "blue"))(20), 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)

# Standardize the selected features( Sale_Price is excluded because it the predicted variable)
features <- c("bedrooms","bathrooms","sqft_living","sqft_lot",
              "floors","condition","grade",
              "sqft_basement","yr_built","lat","long","sqft_living15","sqft_lot15")

# Min-max normalization function
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Apply min-max scaling function to relevant columns
housing_dat[features] <- lapply(housing_dat[features], min_max_transform)
summary(housing_dat)

# Split the dataset into training and testing sets
set.seed(123)
split <- sample.split(housing_dat$price, SplitRatio = 0.8)
train_data <- subset(housing_dat, split == TRUE)
test_data <- subset(housing_dat, split == FALSE)
dim(train_data)
dim(test_data)
summary(train_data)
#################### fit the multiple linear regression model ########################

model1 <- lm(log(price) ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + grade + 
               sqft_basement + yr_built + lat + long + sqft_living15 + sqft_lot15, data = train_data)

# Print the summary of the model
summary(model1 )#Multiple R-squared:  0.6881,	Adjusted R-squared:  0.6877

vif(model1)

##Non-constant Variance Score Test
ncvTest(model1)#Chisquare = 5.980261, Df = 1, p = 0.014467

durbinWatsonTest(model1)
#lag Autocorrelation D-W Statistic p-value
# 1    0.002281887      1.995262   0.842

par(mfrow=c(2,2))
plot(model1, col = "lightblue")
par(mfrow = c(1, 1))# Reset the layout to the default

# Create a histogram of residuals
par(mfrow=c(1,1))
hist(residuals(model1))

# Making predictions with the linear regression model
lm_predictions <- predict(model1, newdata = test_data)

# To display regression evaluation metrics
cat("Multiple Linear Regresion Metrics:\n")
cat("Mean Squared Error:", sqrt(mean((test_data$price - lm_predictions)^2)), "\n")#Mean Squared Error: 452892.6
cat("R2 Score:", cor(lm_predictions, test_data$price)^2 * 100, "\n")#R2 Score: 61.05415
cat("Mean Absolute Error:", mean(abs(test_data$price - lm_predictions)), "\n")#Mean Absolute Error: 421816.3

# Cross-validation for lm
ctrl <- trainControl(method = "cv", number = 5)
cvmodel_results <- train(log(price) ~ ., data = train_data, method = "lm", trControl = ctrl)
print(cvmodel_results)
#RMSE       Rsquared   MAE      
# 0.2290382  0.6875987  0.1770411

####################### Decision Tree model ##############################
 
tree_model <- tree(log(price) ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + grade + 
                      sqft_basement + yr_built + lat + long + sqft_living15 + sqft_lot15, data = train_data)
# Make predictions
tree_predictions <- predict(tree_model, newdata = test_data)

# regression evaluation metrics for Decision Tree
cat("DecisionTree Metrics:\n")
cat("Mean Squared Error:", sqrt(mean((test_data$price - tree_predictions)^2)), "\n")#Mean Squared Error: 452892.6  
cat("R2 Score:", cor(tree_predictions, test_data$price)^2 * 100, "\n")#R2 Score: 62.63834
cat("Mean Absolute Error:", mean(abs(test_data$price - tree_predictions)), "\n")#Mean Absolute Error: 421816.3

# Plot the decision tree
#plot(tree_model, col = "red")
plot(tree_model, main = "ROC Curve", col = "blue", lwd = 2)
text(tree_model)
par(mfrow = c(1, 1))# Reset the layout to the default

# Set up cross-validation control
ctrl <- trainControl(method = "cv", number = 5)

# Train a decision tree model with cross-validation using rpart
cv_tree_model <- train(log(price) ~ ., data = train_data, method = "rpart", trControl = ctrl)

# Print cross-validation results
print(cv_tree_model)

################################ RandomForest ###############################

rf_model <- randomForest(log(price) ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + grade + 
                                      sqft_basement + yr_built + lat + long + sqft_living15 + sqft_lot15, data = train_data, ntree = 500)

# Print the random forest model
print(rf_model)

#Mean of squared residuals: 0.02504078
#% Var explained: 85.08
rf_predictions <- predict(rf_model, newdata = test_data)

# Regression evaluation metrics for random forest
cat("Random Forest Metrics:\n")
cat("Mean Squared Error:", sqrt(mean((test_data$price - rf_predictions)^2)), "\n")#Mean Squared Error: 452892.5
cat("R2 Score:", cor(rf_predictions, test_data$price)^2 * 100, "\n")#R2 Score: 79.78202  
cat("Mean Absolute Error:", mean(abs(test_data$price - rf_predictions)), "\n")#Mean Absolute Error: 421816.3

# Plot feature importance
varImpPlot(rf_model, main = "Random Forest - Feature Importance")

# the R-squared values (r_2)  stored in vectors
Model <- c("MLRegression", "DecisionTree", "RandomForest")
r_squared <- c(61.05415,62.63834,79.78202)  # Replace with your R-squared values

results <- data.frame(Model = Model, `R Squared` = r_squared)

# Print the results
print(results)

# Create a point plot with lines
ggplot(results, aes(x = Model, y = `r_squared`, group = 1)) +
  geom_point() +
  geom_line() +
  labs(title = 'R Squared Results',
       x = 'Model',
       y = 'Score') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Cross-validation for rf
ctrl <- trainControl(method = "cv", number = 5)
cvmodel_results <- train(log(price) ~ ., data = train_data, method = "rf", trControl = ctrl)
print(cvmodel_results)



