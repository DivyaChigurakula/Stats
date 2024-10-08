getwd()
setwd("C:/Users/Admin/OneDrive - The University of Memphis/Documents")
housing <- read.csv("C:/Users/Admin/OneDrive - The University of Memphis/Documents/train.csv")
ncol(housing)
housing <- housing[, -which(names(housing) == "Id")]
names(housing)
dim(housing)

#seperate categorical and numerical variables into two data frames.

# identify categorical columns
categorical_cols <- sapply(housing, function(x) is.factor(x) | is.character(x))
cat_cols <- names(housing[,categorical_cols])

cat_cols <- housing[, cat_cols]

length(cat_cols)
names(cat_cols)
str(cat_cols)

# identify numerical columns
numeric_cols <- sapply(housing, is.numeric)
num_cols <- names(housing[,numeric_cols])
length(num_cols)
names(num_cols)
str(num_cols)
num_cols <- housing[, num_cols]

#check for any missing values:
sum(is.na(num_cols))

#handling missing values

missing_values <- colSums(is.na(num_cols))

#we found that these columns has missing values: MasVnrArea, LotFrontage, GarageYrBlt
#Treating MasVnrArea, LotFrontage with Mice method is a reasonable approach.
#--> But, is it appropriate to use mice for treating missing values for GarageYrBlt?

#Using Multiple Imputation by Chained Equations (MICE) for imputing missing values in the "GarageYrBlt" variable may not be the most appropriate method, primarily due to the nature of the variable.

#"GarageYrBlt" represents the year a garage was built. This variable is typically a discrete numeric variable, as it represents specific years. MICE is commonly used for imputing missing values in continuous or categorical variables. Imputing missing years with MICE may lead to imputed values that don't make sense in the context of year-based data. 

#Instead, for "GarageYrBlt," it's more appropriate to use regression imputations.

#Regression Imputation:
# In this approach, we would build a regression model where "GarageYrBlt" is the dependent variable, and other relevant variables (e.g., "YearBuilt," "OverallQual," etc.) are used as independent predictors.
#The model is trained using rows where "GarageYrBlt" is not missing.
#Once the model is trained, we can use it to predict the missing values of "GarageYrBlt" based on the values of the predictor variables in rows where "GarageYrBlt" is missing.

#Takes into account the relationships between the variables and can provide more accurate imputations if there's a strong relationship between "GarageYrBlt" and the predictors.

#-->how do we know  if there's a strong relationship between "GarageYrBlt" and the predictors.
#p-values: Examine the p-values associated with each predictor in the model summary (summary(lm_model)). Lower p-values suggest that the predictor variable is statistically significant in explaining the variation in "GarageYrBlt."

#R2 value
###############################################################
#Treating MasVnrArea, LotFrontage with Mice method.
install.packages("mice")
# Load the mice package
library(mice)

# Create an empty mice data set with only the columns to be imputed
imputed_cols <- mice(data.frame(MasVnrArea = num_cols$MasVnrArea, LotFrontage = num_cols$LotFrontage))

# Impute missing values
imputed_data <- complete(imputed_cols)

# Combine the imputed values with the original dataset
num_cols$MasVnrArea <- imputed_data$MasVnrArea
num_cols$LotFrontage <- imputed_data$LotFrontage

colSums(is.na(num_cols))


#Treating GarageYrBlt with Regrssion imputation
lm_model <- lm(GarageYrBlt ~ ., data = num_cols[-37]) #exclude salesprice
summary(lm_model)
#From the summary we came to know that variables OverallCond, YearBuilt, YearRemodAdd,LowQualFinSF, Fireplaces, GarageArea, WoodDeckSF have strong relationship with GarageYrBlt.

library(dplyr)

# Step 1: Select predictor variables for the regression model
predictor_vars <- c("YearBuilt", "OverallCond", "YearRemodAdd", "LowQualFinSF", "Fireplaces", "GarageArea","WoodDeckSF")

# Step 2: Filter dataset to include only non-missing "GarageYrBlt" values
filtered_data <- num_cols %>%
  filter(!is.na(GarageYrBlt))

# Step 3: Train a regression model
lm_model <- lm(GarageYrBlt ~ ., data = filtered_data[, c("GarageYrBlt", predictor_vars)])

# Step 4: Use the trained model to predict missing "GarageYrBlt" values
missing_data <- num_cols %>%
  filter(is.na(GarageYrBlt)) %>%
  select(predictor_vars)

# Predict missing values
missing_data$Imputed_GarageYrBlt <- predict(lm_model, newdata = missing_data)

# Replace missing values with imputed values in the original dataset
num_cols[is.na(num_cols$GarageYrBlt), "GarageYrBlt"] <- missing_data$Imputed_GarageYrBlt

colSums(is.na(num_cols))

# Now we have treated Missing values in num cols.

#lets goahead and apply linear regression on num_cols.
library(caret)  # for creating train and test sets

set.seed(123)  # for reproducibility
train_index <- createDataPartition(num_cols$SalePrice, p = 0.7, list = FALSE)
train_data <- num_cols[train_index, ]
test_data <- num_cols[-train_index, ]
nrow(train_data)
nrow(test_data)
names(test_data)

# Fit a linear regression model
lm_model_n <- lm(SalePrice ~ ., data = train_data)
summary(lm_model_n)

# Make predictions on the test set
lm_predictions_n <- predict(lm_model_n, newdata = test_data)
housing$SalePrice

# calculate R-squared
rsq <- cor(test_data$SalePrice, lm_predictions_n)^2
print(paste0("R-squared: ", round(rsq, 2)))

# Calculate RMSE
rmse <- RMSE(lm_predictions_n, test_data$SalePrice)

print(paste0("RMSE: ", rmse))

# Calculate MAE
mae <- MAE(lm_predictions_n, test_data$SalePrice)
print(paste0("MAE: ", mae))



#PLOT:
Actual = test_data$SalePrice
Predicted = lm_predictions_n
length(Actual)
length(lm_predictions_n)


results_lm_n <- data.frame(Actual, Predicted)

ggplot(data = results_lm_n, aes(x = Actual, y = Predicted)) +geom_point() +geom_abline(color = "red") +
  labs(x = "Actual SalePrice", y = "Predicted SalePrice", 
       title = "linear regression(numcols- 77%) : Actual vs Predicted SalePrice")

################################################
#categorical data:

#handling missing values:

# check for missing values:
colSums(is.na(cat_cols))

# Remove variables with a large number of missing values
cat_cols <- subset(cat_cols, select = -c(Alley, PoolQC, Fence, MiscFeature,FireplaceQu))

#Now we need to treat the missing values of variables BsmtCond(37),Electrical(1),GarageCond(81),BsmtExposure(38),MasVnrType(8),BsmtFinType1(37),BsmtFinType2(38).
#To treat the missing values in categorical variables like BsmtCond, Electrical, GarageCond, BsmtExposure, MasVnrType, BsmtFinType1, and BsmtFinType2, we can consider the Mode Imputation:

#Mode Imputation: Replace missing values with the mode (most frequent category) of the respective variable. This is a simple and common method for handling missing categorical data.
#Mode imputation avoids this complexity.

#Why we used mode imputation here?

#No Assumption of Relationships: Here in our dataset we does not capture any relationships between the missing categorical variables and other predictors. These variables are missing completely at random and their missingness is not related to the values of other variables, mode imputation can be a reasonable choice.

# Replace missing values with the mode for each variable
cat_cols$BsmtCond[is.na(cat_cols$BsmtCond)] <- which.max(table(cat_cols$BsmtCond))
cat_cols$Electrical[is.na(cat_cols$Electrical)] <- which.max(table(cat_cols$Electrical))
cat_cols$GarageCond[is.na(cat_cols$GarageCond)] <- which.max(table(cat_cols$GarageCond))
cat_cols$BsmtExposure[is.na(cat_cols$BsmtExposure)] <- which.max(table(cat_cols$BsmtExposure))
cat_cols$MasVnrType[is.na(cat_cols$MasVnrType)] <- which.max(table(cat_cols$MasVnrType))
cat_cols$BsmtFinType1[is.na(cat_cols$BsmtFinType1)] <- which.max(table(cat_cols$BsmtFinType1))
cat_cols$BsmtFinType2[is.na(cat_cols$BsmtFinType2)] <- which.max(table(cat_cols$BsmtFinType2))
cat_cols$GarageType[is.na(cat_cols$GarageType)] <- which.max(table(cat_cols$GarageType))
cat_cols$GarageFinish[is.na(cat_cols$GarageFinish)] <- which.max(table(cat_cols$GarageFinish))
cat_cols$GarageQual[is.na(cat_cols$GarageQual)] <- which.max(table(cat_cols$GarageQual))
cat_cols$BsmtQual[is.na(cat_cols$BsmtQual)] <- which.max(table(cat_cols$BsmtQual))

colSums(is.na(cat_cols))

#Now we have 0 missing values in cat cols
# convert them into numbericals using label encoding.
# Import the required library
library(dplyr)

# Label encoding for all categorical variables
for (col in names(cat_cols)) {
  cat_cols[[col]] <- as.factor(cat_cols[[col]])
  cat_cols[[col]] <- as.numeric(cat_cols[[col]])
}

str(cat_cols)
str(num_cols)

combined_data <- data.frame(num_cols, cat_cols)
length(combined_data)
names(combined_data)

set.seed(123)
train_index <- createDataPartition(combined_data$SalePrice, p = 0.7, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

#Apply linear regression on combined data
# Fit a linear regression model
lm_model <- lm(SalePrice ~ ., data = train_data)
summary(lm_model)

# Make predictions on the test set
lm_predictions <- predict(lm_model, newdata = test_data)

# Calculate RMSE
rmse_lm <- RMSE(lm_predictions, test_data$SalePrice)
print(paste0("RMSE: ", rmse_lm))

# Calculate MAE
mae_lm <- MAE(lm_predictions, test_data$SalePrice)
print(paste0("MAE: ", mae_lm))

# Calculate R-squared (R2) manually
ss_residual <- sum((test_data$SalePrice - lm_predictions)^2)
ss_total <- sum((test_data$SalePrice - mean(test_data$SalePrice))^2)
rsquared <- 1 - (ss_residual / ss_total)
cat("R-squared (R2):", rsquared, "\n")


#PLOT:
Actual = test_data$SalePrice
Predicted = lm_predictions
length(Actual)
length(lm_predictions)

results_lm <- data.frame(Actual, Predicted)

ggplot(data = results_lm, aes(x = Actual, y = Predicted)) +geom_point() +geom_abline(color = "brown") +
  labs(x = "Actual SalePrice", y = "Predicted SalePrice", 
       title = "Linear regression(80%) : Actual vs Predicted SalePrice")
##########################################################################################################
#SUBSET SELECTION :
#Use backward method to find best subset

initial_model <- lm(SalePrice ~ ., data = train_data)

backward_selection <- step(initial_model, direction = "backward")

summary(backward_selection)

# Make predictions on the test set
bwd_predictions <- predict(backward_selection, newdata = test_data)

# Calculate R-squared (R2) manually
ss_residual <- sum((test_data$SalePrice - backward_selection)^2)
ss_total <- sum((test_data$SalePrice - mean(test_data$SalePrice))^2)
rsquared <- 1 - (ss_residual / ss_total)
cat("R-squared (R2):", rsquared, "\n")

coefficients_used<-coefficients(backward_selection)

length(coefficients_used)
length(combined_data)

#PLOT:
Actual = test_data$SalePrice
Predicted = bwd_predictions
length(Actual)
length(lm_predictions)

results_lm <- data.frame(Actual, Predicted)

ggplot(data = results_lm, aes(x = Actual, y = Predicted)) +geom_point() +geom_abline(color = "green") +
  labs(x = "Actual SalePrice", y = "Predicted SalePrice", 
       title = "subset selection(80%) : Actual vs Predicted SalePrice")

selected_vars <- combined_data[c(names(coefficients_used[-1]),"SalePrice")]
length(selected_vars)
##################################################################################################
#RIDGE:

library(caret)
library(glmnet)

set.seed(123)
train_index <- createDataPartition(selected_vars$SalePrice, p = 0.7, list = FALSE)
train_data <- selected_vars[train_index, ]
test_data <- selected_vars[-train_index, ]

# Prepare the feature matrix (X) and target variable (y) for both training and testing sets
X_train <- subset(train_data, select = -c(SalePrice))
y_train <- train_data$SalePrice
X_test <- subset(test_data, select = -c(SalePrice))
y_test <- test_data$SalePrice

# Create a grid of alpha values to be tested
alphas <- 10^seq(10, -2, length = 100)

# Set up cross-validation
set.seed(123)
cv <- cv.glmnet(x = as.matrix(X_train), y = y_train, alpha = 0, lambda = alphas)

# Find the optimal alpha with minimum cross-validated error
optimal_alpha <- cv$lambda.min

# Print the optimal alpha
cat("Optimal Alpha:", optimal_alpha, "\n")

# Fit the Ridge regression model with the optimal alpha on the full training data
ridge_model <- glmnet(x = as.matrix(X_train), y = y_train, alpha = 0, lambda = optimal_alpha)

# Predict on the test set
y_pred <- predict(ridge_model, s = optimal_alpha, newx = as.matrix(X_test))

# Evaluate the model
mae <- mean(abs(y_pred - y_test))
mse <- mean((y_pred - y_test)^2)
rmse <- sqrt(mse)
r_squared <- 1 - mse / var(y_test)

cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared (R^2):", r_squared, "\n")

#########################################################################################
#LASSO

# Set up cross-validation
set.seed(123)
cv <- cv.glmnet(x = as.matrix(X_train), y = y_train, alpha = 1, lambda = alphas)

# Find the optimal alpha with minimum cross-validated error
optimal_alpha <- cv$lambda.min

# Print the optimal alpha
cat("Optimal Alpha:", optimal_alpha, "\n")

# Fit the Lasso regression model with the optimal alpha on the full training data
lasso_model <- glmnet(x = as.matrix(X_train), y = y_train, alpha = 1, lambda = optimal_alpha)

# Predict on the test set
y_pred_lasso <- predict(lasso_model, s = optimal_alpha, newx = as.matrix(X_test))

# Evaluate the Lasso model
mae_lasso <- mean(abs(y_pred_lasso - y_test))
mse_lasso <- mean((y_pred_lasso - y_test)^2)
rmse_lasso <- sqrt(mse_lasso)
r_squared_lasso <- 1 - mse_lasso / var(y_test)

cat("Mean Squared Error (MSE) for Lasso:", mse_lasso, "\n")
cat("Root Mean Squared Error (RMSE) for Lasso:", rmse_lasso, "\n")
cat("R-squared (R^2) for Lasso:", r_squared_lasso, "\n")


###################################################################
#RANDOM FOREST
install.packages("randomForest")
library(randomForest)

set.seed(123)
trainIndex <- createDataPartition(selected_vars$SalePrice, p = 0.8, list = FALSE)
train_data <- selected_vars[trainIndex, ]
test_data <- train_data[-trainIndex, ]

# build random forest model
rf_model<- randomForest(SalePrice ~ ., data = train_data)

# predict on combined data
predictions_rf<- predict(rf_model, test_data)
combined_data$SalePrice

# calculate R-squared
rsq <- cor(test_data$SalePrice, predictions_rf)^2
print(paste0("R-squared: ", round(rsq, 2)))

# Calculate RMSE
rmse_rf <- RMSE(predictions_rf, test_data$SalePrice)
print(paste0("RMSE: ", rmse_rf))

# Calculate MAE
mae_rf <- MAE(predictions_rf, test_data$SalePrice)
print(paste0("MAE: ", mae))

#PLOT:
Actual = test_data$SalePrice
Predicted = predictions_rf

results_rf <- data.frame(Actual, Predicted)

ggplot(data = results_rf, aes(x = Actual, y = Predicted)) +geom_point() +geom_abline(color = "blue") +
  labs(x = "Actual SalePrice", y = "Predicted SalePrice", 
       title = "Random forest(98%) : Actual vs Predicted SalePrice")
#########################################################################################

#XGBOOST:

# Load required libraries
library(xgboost)
library(caret)

# Assuming your dataset is named combined_data
# Split the data into training (80%) and testing (20%) sets
set.seed(123)
trainIndex <- createDataPartition(combined_data$SalePrice, p = 0.8, list = FALSE)
train_data <- combined_data[trainIndex, ]
test_data <- train_data[-trainIndex, ]

# Prepare the feature matrix and target variable
X_train <- train_data[, -which(names(train_data) == "SalePrice")]
y_train <- train_data$SalePrice
X_test <- test_data[, -which(names(test_data) == "SalePrice")]
y_test <- test_data$SalePrice

# Define the tuning grid with appropriate XGBoost parameters
tuneGrid <- expand.grid(
  nrounds = c(100, 200, 300),      # Number of boosting rounds
  max_depth = c(3, 4, 5),         # Maximum tree depth
  eta = c(0.01, 0.1, 0.2),        # Learning rate (eta)
  gamma = 0,                      # Minimum loss reduction required to make a further partition
  colsample_bytree = 1,           # Fraction of features used for each tree
  min_child_weight = 1,           # Minimum sum of instance weight (hessian) needed in a child
  subsample = 1                   # Fraction of data used for each boosting round
)

# Set up cross-validation
control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Perform hyperparameter tuning with 'caret'
xgb_grid <- train(
  x = as.matrix(X_train), 
  y = y_train,
  method = "xgbTree",
  trControl = control,
  tuneGrid = tuneGrid
)

# Get the best hyperparameters
best_params <- xgb_grid$bestTune

# Train the XGBoost model with the best hyperparameters
xgb_model <- xgboost(
  data = as.matrix(X_train), 
  label = y_train,
  nrounds = best_params$nrounds,
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  gamma = best_params$gamma,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = best_params$min_child_weight,
  subsample = best_params$subsample
)

# Make predictions on the test set
y_pred <- predict(xgb_model, newdata = as.matrix(X_test), ntreelimit = xgb_model$best_iteration)
test_data$SalePrice

head(y_pred, 20)
head(test_data$SalePrice, 20)

# Evaluate the model
mse <- mean((y_pred - y_test)^2)
rmse <- sqrt(mse)
mae <- mean(abs(y_pred - y_test))
r_squared <- 1 - mse / var(y_test)

cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared (R^2):", r_squared, "\n")

#PLOT:
Actual = test_data$SalePrice
Predicted = y_pred

results_rf <- data.frame(Actual, Predicted)

ggplot(data = results_rf, aes(x = Actual, y = Predicted)) +geom_point() +geom_abline(color = "purple") +
  labs(x = "Actual SalePrice", y = "Predicted SalePrice", 
       title = "XGBOOST(99%) : Actual vs Predicted SalePrice")
