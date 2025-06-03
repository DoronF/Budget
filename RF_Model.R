library(DBI)
library(RSQLite)
library(dplyr)
library(caret)
library(tm)
library(SnowballC) # for wordStem
library(e1071) # for Naive Bayes
library(stringr)
library(tidyr) # For functions like replace_na if needed for funds
library(ranger)

# --- Configuration ---
DB_FILE <- "data/budget.db" # Name of your SQLite database file
TABLE_NAME <- "transactions"
TEXT_COL_DB <- "description"  # Column in your DB table for transaction description
LABEL_COL_DB <- "category_id" # Column in your DB table for the category ID (your target)
# New features to include
FUNDS_OUT_COL_DB <- "funds_out"
FUNDS_IN_COL_DB <- "funds_in"
ACCOUNT_ID_COL_DB <- "account_id"


# --- Ensure the database file exists and connect ---
if (!file.exists(DB_FILE)) {
    stop(paste("Database file not found:", DB_FILE,
               "\nPlease create the database and load some data into the 'transactions' table."))
}

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = DB_FILE)

# Fetch data from the database, including new columns
cat(paste("Fetching data from the '", TABLE_NAME, "' table...\n", sep=""))
transactions_data <- dbGetQuery(con, paste0("SELECT ",
                                            TEXT_COL_DB, ", ",
                                            FUNDS_OUT_COL_DB, ", ",
                                            FUNDS_IN_COL_DB, ", ",
                                            ACCOUNT_ID_COL_DB, ", ",
                                            LABEL_COL_DB,
                                            " FROM ", TABLE_NAME,
                                            " WHERE ", LABEL_COL_DB, " IS NOT NULL AND ", LABEL_COL_DB, " != ''",
                                            " AND account_id != 2 "))

# IMPORTANT: Ensure category_id is treated as a factor for classification
transactions_data[[LABEL_COL_DB]] <- as.factor(transactions_data[[LABEL_COL_DB]])

# Ensure account_id is treated as a factor (categorical)
transactions_data[[ACCOUNT_ID_COL_DB]] <- as.factor(transactions_data[[ACCOUNT_ID_COL_DB]])

# Handle potential NA values in funds_out/funds_in by replacing with 0
# (Transactions often have either funds_out OR funds_in, not both, so NAs are common for the other)
transactions_data[[FUNDS_OUT_COL_DB]] <- replace_na(transactions_data[[FUNDS_OUT_COL_DB]], 0)
transactions_data[[FUNDS_IN_COL_DB]] <- replace_na(transactions_data[[FUNDS_IN_COL_DB]], 0)


if (nrow(transactions_data) == 0) {
    stop("No labeled transactions found in the database for training. Please ensure 'category_id' column is populated.")
}

cat("Data loaded from database. Sample head:\n")
print(head(transactions_data))
cat("\nNumber of transactions loaded:", nrow(transactions_data), "\n")
cat("Unique categories (category_id):", levels(transactions_data[[LABEL_COL_DB]]), "\n")
cat("Unique account IDs:", levels(transactions_data[[ACCOUNT_ID_COL_DB]]), "\n")


# 1. Text Preprocessing Function (Same as before)
preprocess_text <- function(text_vector) {
    corpus <- VCorpus(VectorSource(text_vector))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, stemDocument, language = "english")
    return(corpus)
}

cat("\n--- Step 1: Preprocessing Text Data ---\n")
preprocessed_corpus <- preprocess_text(transactions_data[[TEXT_COL_DB]])

# 2. Create Document-Term Matrix (DTM)
cat("--- Step 2: Creating Document-Term Matrix (DTM) ---\n")
dtm <- DocumentTermMatrix(preprocessed_corpus,
                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))


# Convert DTM to a data frame for use with caret
text_features_df <- as.data.frame(as.matrix(dtm))

# Ensure column names are valid R names
#names(text_features_df) <- make.names(names(text_features_df))


# Combine text features with numerical and categorical features
cat("\n--- Step 2: Combining all features ---\n")
transaction_features <- cbind(
    text_features_df,
    transactions_data %>%
        select(!!sym(FUNDS_OUT_COL_DB), !!sym(FUNDS_IN_COL_DB), !!sym(ACCOUNT_ID_COL_DB)),
    category_id = transactions_data[[LABEL_COL_DB]] # Add the target label
)

# Clean up column names again in case cbind messed anything up for DTM
# (caret generally handles this but good practice)
names(transaction_features) <- make.names(names(transaction_features))

# Check the dimensions and a sample of the combined features
cat("\nDimensions of combined feature matrix:", dim(transaction_features), "\n")



# 3. Split Data into Training and Testing Sets (Same as before)
cat("\n--- Step 3: Splitting Data into Training and Testing Sets ---\n")
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(
    y = transaction_features$category_id, # Use the category_id column
    p = 0.8, # 80% for training, 20% for testing
    list = FALSE
)

training_set <- transaction_features[trainIndex, ]
testing_set <- transaction_features[-trainIndex, ]

cat("Training set rows:", nrow(training_set), "\n")
cat("Testing set rows:", nrow(testing_set), "\n")
cat("\nDistribution of categories in Training Set:\n")
print(prop.table(table(training_set$category_id)))
cat("\nDistribution of categories in Testing Set:\n")
print(prop.table(table(testing_set$category_id)))



# 2. Train a Machine Learning Model (Now using Random Forest)
cat("\n--- Step 2: Training the Machine Learning Model (Random Forest) ---\n")

# Define training control for Random Forest
# IMPORTANT:
# - Use "cv" (cross-validation) for robust evaluation.
# - Consider "smote" for 'sampling' if you have class imbalance issues
#   (which is highly likely given your previous confusion matrix).
#   You might need to install 'DMwR' or 'UBL' package for SMOTE.
#   For demonstration, I'll put 'smote' here, but comment it out if you
#   haven't installed the necessary package or want to try without it first.
train_control <- trainControl(
    method = "cv",     # Use cross-validation (e.g., k-fold)
    number = 5,        # Number of folds for cross-validation (5-10 is common)
    # sampling = "smote", # Uncomment this if you want to address class imbalance with SMOTE
    # Requires a package like 'DMwR' or 'UBL' (install.packages("DMwR") or install.packages("UBL"))
    classProbs = FALSE, # Set to TRUE if you need class probabilities (e.g., for ROC curves)
    savePredictions = "final",
    verboseIter = TRUE # See training progress
)

# Train the Random Forest model using 'ranger' method
# 'ranger' is a fast implementation of Random Forest.
model_rf <- train(
    category_id ~ ., # Predict category_id using all other features
    data = training_set,
    method = "ranger", # Use 'ranger' for Random Forest
    trControl = train_control,
    importance = "impurity", # Calculate variable importance
    num.trees = 500 # Number of trees in the forest (default is 500)
    # tuneGrid = expand.grid(
    #   mtry = c(sqrt(ncol(training_set) - 1), 5, 10), # Number of variables randomly sampled at each split
    #   splitrule = "gini", # Splitting rule
    #   min.node.size = c(1, 5, 10) # Minimum number of samples in a node to be split
    # ) # Uncomment and adjust for hyperparameter tuning
)

cat("\nModel Training Complete.\n")
print(model_rf)


# 3. Make Predictions on the Test Set
cat("\n--- Step 3: Making Predictions on the Test Set ---\n")
predictions <- predict(model_rf, newdata = testing_set)


# 4. Evaluate the Model
cat("\n--- Step 4: Evaluating the Model Performance ---\n")
true_labels <- testing_set$category_id

confusion_matrix <- confusionMatrix(predictions, true_labels)

cat("\nConfusion Matrix:\n")
print(confusion_matrix)

cat("\nOverall Statistics:\n")
cat("Accuracy:", confusion_matrix$overall['Accuracy'], "\n")
cat("Kappa:", confusion_matrix$overall['Kappa'], "\n")

cat("\nStatistics by Class:\n")
print(confusion_matrix$byClass)

# Optional: View feature importance
cat("\nFeature Importance (Top 20):\n")
print(varImp(model_rf)) # This provides importance scores for features


# --- Disconnect from the database ---
dbDisconnect(con)
cat("\nDisconnected from the database.\n")

model_save_path <- "models/transaction_categorizer_rf_model.rds"

# Ensure the directory exists
dir.create(dirname(model_save_path), recursive = TRUE, showWarnings = FALSE)

# Save the trained model
saveRDS(model_rf, file = model_save_path)

training_dtm_terms <- colnames(dtm) # Save these during training
saveRDS(training_dtm_terms, "models/training_dtm_terms.rds")

cat(paste0("\nModel saved successfully to: ", model_save_path, "\n"))