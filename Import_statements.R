
# source
source("extract_visa_transactions.R")
source("extract_chequing_transactions.R")

# --- Global Variables for Model ---
model_rf_global <- NULL
dtm_terms_global <- NULL

# Ensure the database connection is closed when the script exits
on.exit({
    if (dbIsValid(con)) {
        # Check if the connection is still valid before trying to disconnect
        # dbDisconnect(con)
        # cat("\nDatabase connection closed.\n")
    }
})
# --- Helper Functions ---

#' Initializes the database schema and prepopulates accounts and categories.
#'
#' @param con A DBI connection object.
initialize_database <- function(con) {
    cat(blue("Ensuring database tables exist and prepopulating initial data...\n"))
    
    # Create tables
    dbExecute(
        con,
        "CREATE TABLE IF NOT EXISTS accounts (
        account_id INTEGER PRIMARY KEY, 
        account_name TEXT UNIQUE
        );"
    )
    dbExecute(
        con,
        "CREATE TABLE IF NOT EXISTS categories (
        category_id INTEGER PRIMARY KEY, 
        category_name TEXT UNIQUE
        );"
    )
    dbExecute(
        con,
        "CREATE TABLE IF NOT EXISTS transactions (
      id INTEGER PRIMARY KEY,
      file_name TEXT,
      date TEXT,
      effective_date TEXT,
      description TEXT,
      funds_out REAL,
      funds_in REAL,
      balance REAL,
      account_id INTEGER,
      category_id INTEGER,
      FOREIGN KEY(account_id) REFERENCES accounts(id),
      FOREIGN KEY(category_id) REFERENCES categories(id)
    );"
    )
    dbExecute(
        con,
        "CREATE TABLE IF NOT EXISTS processed_files (
      file_name TEXT PRIMARY KEY,
      original_pdf TEXT,
      account_id INTEGER,
      year INTEGER,
      month INTEGER,
      processed_at TEXT DEFAULT CURRENT_TIMESTAMP,
      hash TEXT UNIQUE
    );"
    )
    
    # Prepopulate accounts (only if table is empty)
    existing_accounts_count <- dbGetQuery(con, "SELECT COUNT(*) AS count FROM accounts")$count
    if (existing_accounts_count == 0) {
        account_names <- c("Chequing", "Saving", "Simplii Visa", "CIBC Visa")
        cat(blue("Adding default accounts...\n"))
        for (acct in account_names) {
            dbExecute(
                con,
                "INSERT OR IGNORE INTO accounts (account_name) VALUES (?)",
                params = list(acct)
            )
        }
    }
    
    # Prepopulate categories
    default_categories <- data.frame(
        category_id = 1:21,
        category_name = c(
            "Groshalies",
            "Restaurants & Cafes",
            "Transportation",
            "Housing & Util",
            "Health & Wellness",
            "Pets",
            "family",
            "Education",
            "Personal Care",
            "Entertainment",
            "Travel",
            "Other",
            "Transfers / Income",
            "Work",
            "Fees & Charges",
            "Drugs and Alcohol",
            "saving",
            "investing",
            "Credit",
            "Taxes",
            "Intrest"
        ),
        stringsAsFactors = FALSE
    )
    # Insert default categories, ignoring if they already exist based on ID (or name for uniqueness)
    # This simple loop assumes default_categories$id corresponds to desired category_id in DB
    for (i in 1:nrow(default_categories)) {
        dbExecute(
            con,
            "INSERT OR IGNORE INTO categories (category_id, category_name) VALUES (?, ?)",
            params = list(
                default_categories$category_id[i],
                default_categories$category_name[i]
            )
        )
    }
    
    invisible(default_categories) # Return for use elsewhere if needed
}

#' Gets the list of available PDF files in the import directory.
#'
#' @param import_dir The directory to search for PDFs.
#' @return A character vector of PDF file paths.
get_pdf_files <- function(import_dir) {
    dir_ls(import_dir, regexp = "\\.pdf$")
}

#' Checks if a PDF file has already been processed using its hash.
#'
#' @param con A DBI connection object.
#' @param file_path The path to the PDF file.
#' @return TRUE if the file has been processed, FALSE otherwise.
is_pdf_processed <- function(con, file_path) {
    pdf_raw <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
    pdf_hash <- digest(pdf_raw, algo = "sha256")
    hush_match <- dbGetQuery(con,
                             "SELECT hash FROM processed_files WHERE hash = ?",
                             params = list(pdf_hash))
    return(nrow(hush_match) > 0)
}

#' Previews the first page of a PDF file.
#'
#' @param file_path The path to the PDF file.
preview_pdf <- function(file_path) {
    cat(blue("\n--- Previewing PDF:", basename(file_path), "---\n"))
    preview <- tryCatch(
        pdf_text(file_path)[1],
        error = function(e)
            "Could not read PDF."
    )
    cat(red(preview))
    cat(blue("----------------------------------\n"))
}

#' Prompts the user to select an account.
#'
#' @param con A DBI connection object.
#' @return The selected account ID.
select_account <- function(con) {
    accounts <- dbGetQuery(con, "SELECT * FROM accounts")
    cat(blue("\nSelect account:\n"))
    for (j in seq_len(nrow(accounts))) {
        cat(sprintf(
            "%d: %s\n",
            accounts$account_id[j],
            accounts$account_name[j]
        ))
    }
    account_id <- as.integer(readline("Enter the number of the account: "))
    if (!(account_id %in% accounts$account_id)) {
        stop("Invalid account ID selected.") # Stop or prompt again, depending on desired behavior
    }
    return(account_id)
}


#' Guides the user through categorizing transactions.
#'
#' @param df The data frame of transactions to categorize.
#' @param default_categories Data frame of category IDs and names.
#' @param con A DBI connection object for looking up past categorizations.
#' @return A data frame with added category_id for each transaction.
interactively_categorize_transactions <- function(df, default_categories, con) {
    # Add a temporary row_id to keep track of original row for re-categorization
    df <- df %>% mutate(temp_row_id = row_number())
    
    # --- CRITICAL MODIFICATION: Initialize categorized_df with FINAL column names and types ---
    # We are explicitly creating an empty tibble with the exact column names and types
    # that we expect in the final 'categorized_df'.
    categorized_df <- tibble(
        temp_row_id = integer(),
        date = character(),
        effective_date = character(),
        description = character(),
        funds_out = numeric(),
        funds_in = numeric(),
        balance = numeric(),
        category_id = integer(),
        account_id = integer()
    )
    # --- END CRITICAL MODIFICATION ---
    
    for (i in seq_len(nrow(df))) {
        row <- df[i, ]
        category_id <- NA # Ensure this is always initialized for each row
        
        cat(blue("\n--- Categorizing Transaction ---"))
        cat(sprintf(
            "\nTransaction %d of %d: %s",
            i,
            nrow(df),
            row$Description
        ))
        cat(
            sprintf(
                "\nDate: %s | Amount: %s | Type: %s\n",
                row$Date,
                if (!is.na(row$FundsOut))
                    row$FundsOut
                else
                    row$FundsIn,
                if (!is.na(row$FundsOut))
                    red("OUT")
                else
                    green("IN")
            )
        )
        
        # Check for past categorization for this description
        query <- "SELECT category_id FROM transactions WHERE description = ? ORDER BY date DESC LIMIT 1"
        existing_cat_result <- dbGetQuery(con, query, params = list(row$Description))
        
        if (nrow(existing_cat_result) > 0) {
            prev_cat_id <- existing_cat_result[1, 1]
            prev_cat_id <- as.integer(prev_cat_id)
            prev_cat_name <- default_categories$category_name[default_categories$category_id == prev_cat_id]
            cat(
                sprintf(
                    "Previously categorized as: %s (ID: %d)\n",
                    prev_cat_name,
                    prev_cat_id
                )
            )
            cat(blue("----------------------------------\n"))
            input <- readline("Confirm this category? (y/n/skip): ")
            input <- tolower(trimws(input))
            
            if (input == "y") {
                category_id <- prev_cat_id
            } else if (input == "skip") {
                category_id <- as.integer(NA)
            }
        }
        
        if (is.na(category_id)) {
            repeat {
                # Display categories in 2 columns (ensure category_id is used here)
                for (j in seq(1, nrow(default_categories), by = 2)) {
                    left <- sprintf(
                        "%2d: %-25s",
                        default_categories$category_id[j],
                        default_categories$category_name[j]
                    )
                    right <- if (j + 1 <= nrow(default_categories)) {
                        sprintf(
                            "%2d: %-25s",
                            default_categories$category_id[j + 1],
                            default_categories$category_name[j + 1]
                        )
                    } else {
                        ""
                    }
                    cat(left, right, "\n")
                }
                cat(blue("----------------------------------\n"))
                
                input_cat <- suppressWarnings(
                                as.integer(
                                    readline(
                    "Enter a number for the category (or press Enter to skip): "
                                            )
                                        ))
                if (is.na(input_cat)) {
                    category_id <- as.integer(NA)
                    break
                } else if (input_cat %in% default_categories$category_id) {
                    category_id <- input_cat
                    break
                } else {
                    cat(red("Invalid category. Please try again.\n"))
                }
            }
        }
        
        # Bind the current row's data to the growing categorized_df.
        # The column names here MUST match the column names defined in the
        # 'categorized_df' initialization above (all lowercase as desired).
        categorized_df <- bind_rows(
            categorized_df,
            tibble(
                temp_row_id = row$temp_row_id,
                date = as.character(row$Date),
                # Map 'Date' from original 'row' to 'date'
                effective_date = as.character(row$EffectiveDate),
                # Map 'EffectiveDate' to 'effective_date'
                description = row$Description,
                # Map 'Description' to 'description'
                funds_out = row$FundsOut,
                # Map 'FundsOut' to 'funds_out'
                funds_in = row$FundsIn,
                # Map 'FundsIn' to 'funds_in'
                balance = row$Balance,
                # Map 'Balance' to 'balance'
                category_id = as.integer(category_id),
                account_id = row$account_id
            )
        )
    }
    return(categorized_df)
}

#' Saves processed transactions and marks the file as processed.
#'
#' @param con A DBI connection object.
#' @param transactions_df Data frame of categorized transactions.
#' @param original_file_path The original path of the PDF.
#' @param new_file_path The new path where the PDF is moved.
#' @param account_id The ID of the account.
#' @param year The year of the statement.
#' @param month The month of the statement.
#' @param pdf_hash The SHA-256 hash of the PDF.
save_processed_data <- function(con,
                                transactions_df,
                                original_file_path,
                                new_file_path,
                                account_id,
                                year,
                                month,
                                pdf_hash) {
    if (nrow(transactions_df) > 0) {
        
        transactions_df$date <- format(transactions_df$date, "%Y-%m-%d")
        transactions_df$effective_date <- format(transactions_df$effective_date, "%Y-%m-%d")
        
        transactions_df <- transactions_df %>%
            select(date, effective_date, description, funds_out, funds_in, balance, category_id, account_id ) %>%
            mutate(file_name = basename(new_file_path))
        
        print(transactions_df, n = nrow(transactions_df), na.print = red("N/A"))
        cat(blue("----------------------------------\n"))
        confirm <- readline("Save to database? (y/n): ")
        if (tolower(confirm) == "y") {
            dbWriteTable(con, "transactions", transactions_df, append = TRUE)
            message("Saved to database.")
            
            # Move file after successful save
            file_move(original_file_path, new_file_path)
            cat(green("Moved to:", new_file_path, "\n"))
            
            # Mark as processed
            dbExecute(
                con,
                "INSERT INTO processed_files (file_name, original_pdf, account_id, year, month, processed_at, hash)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
                params = list(
                    new_file_path,
                    basename(original_file_path),
                    account_id,
                    year,
                    month,
                    as.character(Sys.time()),
                    pdf_hash
                )
            )
        } else {
            message("File skipped, no data saved.")
        }
    }
}


# --- Model Loading ---
# Load the model and DTM terms when the script starts
if (file.exists(model_path) && file.exists(dtm_terms_path)) {
    model_rf_global <<- readRDS(model_path)
    dtm_terms_global <<- readRDS(dtm_terms_path)
    cat(blue(
        "\nMachine Learning Model and DTM vocabulary loaded successfully.\n"
    ))
} else {
    cat(
        red(
            "\nWARNING: Model or DTM vocabulary not found. Automatic categorization will be skipped.\n"
        )
    )
    cat(
        red(
            "Please train your model and save 'transaction_categorizer_rf_model.rds' and 'training_dtm_terms.rds' in the 'models/' directory.\n"
        )
    )
    # If the model is not found, the `predict_categories_with_model` function will handle it by returning NA predictions.
}
#' Preprocesses new transaction data and predicts categories using the trained model.
#'
#' @param df The data frame of transactions to categorize (raw, as extracted from PDF).
#' @param model The trained Random Forest model object.
#' @param dtm_terms The vocabulary (column names) from the training DTM.
#' @return A data frame with a 'predicted_category_id' column, or NA if model not available.

predict_categories_with_model <- function(df, model, dtm_terms, default_categories) {
    if (is.null(model) || is.null(dtm_terms)) {
        cat(yellow(
            "Model or DTM vocabulary not loaded. Skipping model prediction.\n"
        ))
        # Ensure consistent column names even if skipping prediction
        return(
            df %>% mutate(predicted_category_id = as.integer(NA)) %>%
                rename(
                    description = Description,
                    funds_out = FundsOut,
                    funds_in = FundsIn,
                    date = Date,
                    # Rename here!
                    effective_date = EffectiveDate,
                    # Rename here!
                    balance = Balance # Rename here!
                )
        )
    }
    
    cat(blue("\n--- Generating Model Predictions ---\n"))
    
    # Store original columns (with their initial names, which are likely capitalized)
    # We will rename them to lowercase as part of the final output df
    original_data_for_merge <- df %>%
        select(Date,
               EffectiveDate,
               Balance,
               Description,
               FundsOut,
               FundsIn,
               account_id) # Capture all before renaming
    print(original_data_for_merge)
    # Standardize names for model input
    df_for_model <- df %>%
        select(Description, FundsOut, FundsIn, account_id) %>%
        rename(description = Description,
               funds_out = FundsOut,
               funds_in = FundsIn)
    
    TEXT_COL_DB <- "description"
    FUNDS_OUT_COL_DB <- "funds_out"
    FUNDS_IN_COL_DB <- "funds_in"
    ACCOUNT_ID_COL_DB <- "account_id"
    
    df_for_model$account_id <- as.factor(df_for_model$account_id)
    df_for_model$funds_out <- replace_na(df_for_model$funds_out, 0)
    df_for_model$funds_in <- as.numeric(replace_na(df_for_model$funds_in, 0))
    
    # Redefine preprocess_text if it's nested (though it's defined globally above)
    # If this is copied directly from your script, the inner definition might override outer.
    # It's better to remove the inner one if preprocess_text is global.
    # For now, assuming it's meant to be there or it's a copy-paste artifact.
    # If preprocess_text is defined globally, remove this nested definition:
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
    preprocessed_corpus <- preprocess_text(df_for_model[[TEXT_COL_DB]])
    
    # 2. Create Document-Term Matrix (DTM)
    cat("--- Step 2: Creating Document-Term Matrix (DTM) ---\n")
    dtm <- DocumentTermMatrix(preprocessed_corpus,
                              control = list(
                                  weighting = function(x)
                                      weightTfIdf(x, normalize = FALSE)
                              ))
    
    text_features_df <- as.data.frame(as.matrix(dtm))
    missing_terms <- setdiff(dtm_terms, names(text_features_df))
    if (length(missing_terms) > 0) {
        text_features_df[missing_terms] <- 0
    }
    text_features_df <- text_features_df[, dtm_terms, drop = FALSE]
    
    cat("\n--- Step 2: Combining all features ---\n")
    transaction_features <- cbind(text_features_df,
                                  df_for_model %>%
                                      select(
                                          !!sym(FUNDS_OUT_COL_DB),
                                          !!sym(FUNDS_IN_COL_DB),
                                          !!sym(ACCOUNT_ID_COL_DB)
                                      ))
    names(transaction_features) <- make.names(names(transaction_features))
    
    # 5. Make predictions
    predictions <- predict(model, newdata = transaction_features)
    
    # Combine original data (now with standardized lowercase names) with predictions
    df_with_predictions <- original_data_for_merge %>%
        mutate(temp_row_id = row_number(),
               category_id = as.integer(as.character(predictions))) %>%
        # Rename all columns to lowercase to match expectations in review_and_correct_transactions
        rename(
            
            date = Date,
            effective_date = EffectiveDate,
            description = Description,
            funds_out = FundsOut,
            funds_in = FundsIn,
            balance = Balance
        )
    
    # Map predicted_category_id to category_name for display
    df_with_predictions <- df_with_predictions %>%
        left_join(
            default_categories %>% rename(predicted_category_name = category_name),
            by = c("category_id" = "category_id")
        )
    
    return(df_with_predictions)
}

# --- Main function to run the PDF import workflow ---
run_pdf_import_workflow <- function(predict = 0) {
    # Initialize database and get categories
    default_categories <- initialize_database(con)
    
    repeat {
        pdf_files <- get_pdf_files(import_dir)
        
        if (length(pdf_files) == 0) {
            cat(red("No PDF files found in import folder. Exiting.\n"))
            break
        }
        
        cat(blue("\n--- PDF Files Available for Import ---\n"))
        for (i in seq_along(pdf_files)) {
            cat(sprintf("%2d: %s\n", i, basename(pdf_files[i])))
        }
        cat(" 0: Exit\n")
        cat(blue("----------------------------------\n"))
        selection <- suppressWarnings(as.integer(readline(
            "Select a file to import (0 to exit): "
        )))
        if (is.na(selection) || selection == 0) {
            cat(blue("Exiting import tool.\n"))
            
            break
        }
        if (!(selection %in% seq_along(pdf_files))) {
            cat(red("Invalid selection. Try again.\n"))
            next
        }
        
        file_path <- pdf_files[selection]
        
        # Check if file has already been processed by hash
        if (is_pdf_processed(con, file_path)) {
            cat(red("File already imported. Skipping.\n"))
            next
        }
        
        # Preview and confirm
        preview_pdf(file_path)
        
        confirm_import <- readline("Do you want to import this file? (y/n): ")
        if (tolower(confirm_import) != "y") {
            next
        }
        
        # Get account and date info
        account_id <- select_account(con)
        account_name <- dbGetQuery(
            con,
            "SELECT account_name FROM accounts WHERE account_id = ?",
            params = list(account_id)
        )$account_name
        year <- as.integer(readline("Enter year (e.g. 2025): "))
        month <- as.integer(readline("Enter month (1-12): "))
        
        # Generate new filename and create target directory
        new_filename <- sprintf("%04d-%02d_%s.pdf",
                                year,
                                month,
                                gsub(" ", "_", account_name))
        new_path <- file.path(target_dir, account_name, new_filename)
        dir_create(path_dir(new_path))
        
        # Extract transactions based on account type
        transactions_df <- NULL
        if (account_id == 1 || account_id == 2) {
            # Chequing and savings
            transactions_df <- extract_chequing_transactions(file_path, year)
        } else if (account_id == 3 || account_id == 4) {
            #  Visa
            transactions_df <- extract_visa_transactions(file_path, year)
        } else {
            cat(yellow("Extraction logic not implemented for this account type yet. Skipping.\n"))
            next
        }
        
        if (is.null(transactions_df) ||
            nrow(transactions_df) == 0) {
            cat(yellow("No transactions extracted. Skipping file.\n"))
            next
        }
        
        # Add account_id to the transactions_df before categorization
        transactions_df$account_id <- account_id
        if (predict == 1){
            # Use model to predict categories
            categorized_transactions <- predict_categories_with_model(
                transactions_df,
                model_rf_global,
                dtm_terms_global,
                default_categories # Pass default_categories for join
            )
        }
        else {
            # Categorize transactions (initial pass)
            categorized_transactions <- interactively_categorize_transactions(transactions_df, default_categories, con)
        }
        # --- Review and Correction Loop ---
        review_loop_active <- TRUE
        while (review_loop_active) {
            cat(blue("\n--- Review Categorized Transactions ---\n"))
            # Display current categorization for review
            review_display <- categorized_transactions %>%
                left_join(default_categories, by = "category_id") %>%
                mutate(
                    "Txn ID" = temp_row_id,
                    # Use the temporary ID for user reference
                    "Date" = date,
                    "Description" = description,
                    "Amount" =  ifelse(
                        !is.na(funds_out),
                        sprintf("-$%.2f", funds_out),
                        sprintf("+$%.2f", funds_in)
                    ),
                    "Category" = ifelse(
                        is.na(category_name),
                        "UNCATEGORIZED",
                        category_name
                    ),
                    "Cat ID" = case_when(
                        is.na(category_id) ~ "-",
                        TRUE ~ as.character(category_id)
                    )
                ) %>%
                select("Txn ID",
                       "Date",
                       "Description",
                       "Amount",
                       "Category",
                       "Cat ID")
            
            # Use n = -1 for tibbles for more robust full printing
            print(dplyr::as_tibble(review_display), n = -1 )
            
            # --- Logic for conditional saving option ---
            has_uncategorized <- any(is.na(categorized_transactions$category_id))
            cat(blue("----------------------------------\n"))
            cat(blue("\nOptions:"))
            if (!has_uncategorized) {
                cat(blue("\n  's' to save and proceed"))
            } else {
                cat(blue("\n  (Saving is disabled until all transactions are categorized)"))
            }
            cat(blue("\n  'r' to re-categorize a specific transaction"))
            cat(blue("\n  'q' to quit without saving (skip this file)"))
            
            # Adjust prompt based on whether saving is an option
            prompt_choices <- if (has_uncategorized)
                "r/q"
            else
                "s/r/q"
            review_choice <- tolower(trimws(readline(
                sprintf("Enter your choice (%s): ", prompt_choices)
            )))
            
            if (review_choice == "s") {
                if (has_uncategorized) {
                    cat(
                        red(
                            "ERROR: Cannot save. Please categorize all transactions or choose 'q' to quit this file.\n"
                        )
                    )
                    next # Stay in the review loop
                } else {
                    review_loop_active <- FALSE # All categorized, exit loop and proceed to save
                }
            } else if (review_choice == "r") {
                txn_to_reclassify <- suppressWarnings(as.integer(
                    readline("Enter the 'Txn ID' to re-categorize: ")
                ))
                if (is.na(txn_to_reclassify) ||
                    !(txn_to_reclassify %in% categorized_transactions$temp_row_id)) {
                    cat(red(
                        "Invalid Transaction ID. Please try again.\n"
                    ))
                    next
                }
                
                # Find the row to modify
                idx_to_modify <- which(categorized_transactions$temp_row_id == txn_to_reclassify)
                current_row <- categorized_transactions[idx_to_modify, ]
                
                cat(
                    sprintf(
                        "\nRe-categorizing: %s | Amount: %s | Current Category: %s\n",
                        current_row$description,
                        ifelse(
                            !is.na(current_row$funds_out),
                            sprintf("-$%.2f", current_row$funds_out),
                            sprintf("+$%.2f", current_row$funds_in)
                        ),
                        default_categories$category_name[default_categories$category_id == current_row$category_id]
                    )
                )
                
                # Display categories again
                for (j in seq(1, nrow(default_categories), by = 2)) {
                    left <- sprintf(
                        "%2d: %-25s",
                        default_categories$category_id[j],
                        default_categories$category_name[j]
                    )
                    right <- if (j + 1 <= nrow(default_categories)) {
                        sprintf(
                            "%2d: %-25s",
                            default_categories$category_id[j + 1],
                            default_categories$category_name[j + 1]
                        )
                    } else {
                        ""
                    }
                    cat(left, right, "\n")
                }
                cat(blue("----------------------------------\n"))
                new_cat_id <- suppressWarnings(as.integer(readline("Enter new category ID: ")))
                if (is.na(new_cat_id) ||
                    !(new_cat_id %in% default_categories$category_id)) {
                    cat(red(
                        "Invalid category ID. Re-categorization cancelled.\n"
                    ))
                } else {
                    categorized_transactions$category_id[idx_to_modify] <- new_cat_id
                    cat(green(
                        sprintf(
                            "Transaction %d updated to category: %s\n",
                            txn_to_reclassify,
                            default_categories$category_name[default_categories$category_id == new_cat_id]
                        )
                    ))
                }
            } else if (review_choice == "q") {
                cat(yellow("Skipping this file and returning to main menu.\n"))
                review_loop_active <- FALSE # Exit loop
                # Ensure we skip the saving part outside this loop as well
            } else {
                cat(red("Invalid choice. Please try again.\n"))
            }
        } # End of review loop
        
        # If the user chose to quit, skip the saving and moving part
        if (review_choice == "q") {
            next # Go to the next file selection loop iteration
        }
        
        # Remove the temporary row_id before saving to DB
        final_transactions_to_save <- categorized_transactions %>%
            select(-temp_row_id)
        
        # Save data and move file
        pdf_raw <- readBin(file_path,
                           what = "raw",
                           n = file.info(file_path)$size)
        pdf_hash <- digest(pdf_raw, algo = "sha256")
        save_processed_data(
            con,
            final_transactions_to_save,
            file_path,
            new_path,
            account_id,
            year,
            month,
            pdf_hash
        )
    }
}

