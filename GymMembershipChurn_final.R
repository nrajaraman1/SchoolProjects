
# Load libraries
library(randomForest)
library(tidyverse)
library(caret)
library(corrplot)
library(nnet)
library(rpart)
library(rpart.plot)


# 1. Load dataset


# Choose your CSV (e.g., gym_churn_us.csv)
Gym_Memberships_df <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Initial structure and summary
str(Gym_Memberships_df)
summary(Gym_Memberships_df)

# 2. Handle missing data (flag + impute)


# Check total missing values per column
colSums(is.na(Gym_Memberships_df))

# Create missingness indicator variables for any column that has NAs
missing_cols <- names(Gym_Memberships_df)[sapply(Gym_Memberships_df, function(x) any(is.na(x)))]

for (col in missing_cols) {
  Gym_Memberships_df[[paste0(col, "_missing")]] <- ifelse(is.na(Gym_Memberships_df[[col]]), 1, 0)
}

# Convert categorical columns to factors (as in original script)
cat_cols <- c("gender", "Near_Location", "Partner", "Promo_friends",
              "Phone", "Group_visits", "Churn")

Gym_Memberships_df[cat_cols] <- lapply(Gym_Memberships_df[cat_cols], factor)

# Helper function for mode (for factor imputation)
mode_impute <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(x)
  tab <- table(ux)
  mode_val <- names(tab)[which.max(tab)]
  x[is.na(x)] <- mode_val
  return(x)
}

# Impute numeric columns with median
num_cols <- sapply(Gym_Memberships_df, is.numeric)
for (col in names(Gym_Memberships_df)[num_cols]) {
  med_val <- median(Gym_Memberships_df[[col]], na.rm = TRUE)
  Gym_Memberships_df[[col]][is.na(Gym_Memberships_df[[col]])] <- med_val
}

# Impute factor columns with mode
Gym_Memberships_df[cat_cols] <- lapply(Gym_Memberships_df[cat_cols], mode_impute)

# Check again for missing values
colSums(is.na(Gym_Memberships_df))

# 3. Remove highly correlated numeric features (redundant)

# Select numeric predictors only (Churn is factor so it won't be included)
numeric_data <- Gym_Memberships_df %>%
  select(where(is.numeric))

# Correlation matrix for numeric variables
corr_matrix <- cor(numeric_data, use = "complete.obs")

# Find highly correlated variables (e.g., |r| > 0.9)
highCorr_idx <- findCorrelation(corr_matrix, cutoff = 0.9)  # adjust cutoff if needed
highCorr_vars <- colnames(numeric_data)[highCorr_idx]
highCorr_vars

# Remove highly correlated variables from the main dataset
Gym_Memberships_df <- Gym_Memberships_df[, !(names(Gym_Memberships_df) %in% highCorr_vars)]

# Check structure after removal
str(Gym_Memberships_df)


# 4. Visualizations (EDA + Model Insights)

## Visualization 1: Class balance (Churn distribution)
Gym_Memberships_df %>%
  ggplot(aes(x = Churn)) +
  geom_bar() +
  labs(
    title = "Distribution of Churn vs. Non-Churn Members",
    x = "Churn (0 = Stay, 1 = Churn)",
    y = "Number of Members"
  ) +
  theme_minimal()

## Visualization 2: Correlation heatmap of numeric features (after redundancy removal)
numeric_data2 <- Gym_Memberships_df %>%
  select(where(is.numeric))

corr_matrix2 <- cor(numeric_data2, use = "complete.obs")

corrplot(corr_matrix2, method = "color", type = "upper",
         tl.cex = 0.8, tl.srt = 45,
         title = "Correlation Between Numeric Features",
         mar = c(0, 0, 2, 0))


# 5. Train / Test Split


set.seed(123)

train_index <- createDataPartition(Gym_Memberships_df$Churn, p = 0.7, list = FALSE)

train <- Gym_Memberships_df[train_index, ]
test  <- Gym_Memberships_df[-train_index, ]

dim(train)  # ~2800 rows
dim(test)   # ~1200 rows


# 6. Model Training

# Logistic Regression
model_logit <- glm(Churn ~ ., data = train, family = binomial)

# Random Forest
model_rf <- randomForest(Churn ~ ., data = train, ntree = 500, mtry = 3, importance = TRUE)

# Decision Tree
model_tree <- rpart(Churn ~ ., data = train, method = "class")


# 7. Model Predictions and Evaluation

# Logistic Regression Predictions
pred_logit <- predict(model_logit, test, type = "response")
pred_class_logit <- ifelse(pred_logit > 0.5, "1", "0") %>%
  factor(levels = c("0", "1"))

# Random Forest Predictions
pred_rf <- predict(model_rf, test)

#Decision Tree Predictions
pred_tree <- predict(model_tree, test, type = "class")

# Confusion Matrices
confusionMatrix(pred_class_logit, test$Churn)
confusionMatrix(pred_rf, test$Churn)
confusionMatrix(pred_tree, test$Churn)


# 8. Model-based Visualizations

## Visualization 3: Random Forest variable importance
varImpPlot(model_rf, main = "Random Forest Variable Importance")

# Optional ggplot version
rf_importance <- importance(model_rf)
rf_importance_df <- data.frame(
  Feature = rownames(rf_importance),
  MeanDecreaseGini = rf_importance[, "MeanDecreaseGini"]
)

rf_importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  ggplot(aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Feature Importance (Random Forest)",
    x = "Feature",
    y = "Mean Decrease in Gini"
  ) +
  theme_minimal()

## Visualization 4: Decision Tree plot
rpart.plot(
  model_tree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Decision Tree for Gym Membership Churn"
)


