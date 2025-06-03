#' Main function to handle transaction editing workflow.
#'
#' @param con A DBI connection object.
edit_transactions <- function(con) {
    cat(blue("\n--- Edit Transactions ---\n"))
    
    # Need default_categories to map IDs to names for display and selection
    default_categories <- dbGetQuery(con, "SELECT category_id, category_name FROM categories")
    
    # Step 1: Select Account
    account_id <- select_account(con) # Reuse the existing select_account function from Import_statements.R
    if (is.null(account_id)) { # If user cancels account selection
        cat(yellow("Account selection cancelled. Returning to main menu.\n"))
        return(NULL)
    }
    
    repeat {
        # Step 2: Select Transaction for Editing
        selected_transaction <- select_transaction_for_editing(con, account_id, default_categories)
        if (is.null(selected_transaction)) {
            break # User chose to go back from transaction selection
        }
        
        current_category_name <- default_categories$category_name[
            default_categories$category_id == selected_transaction$category_id
        ]
        
        cat(blue("\n--- Editing Transaction (ID:", selected_transaction$id, ") ---\n"))
        cat(sprintf("Date: %s\n", selected_transaction$date))
        cat(sprintf("Current Description: %s\n", selected_transaction$description))
        cat(sprintf("Current Funds Out: %.2f\n", selected_transaction$funds_out))
        cat(sprintf("Current Funds In: %.2f\n", selected_transaction$funds_in))
        cat(sprintf("Current Category: %s (ID: %s)\n",
                    ifelse(length(current_category_name) > 0, current_category_name, "N/A"),
                    selected_transaction$category_id))
        
        
        cat(blue("\nWhat would you like to edit?\n"))
        cat("1. Description\n")
        cat("2. Category\n")
        cat("3. Funds Out (Debit)\n")
        cat("4. Funds In (Credit)\n")
        cat("5. Date\n") # New option for date
        cat("0. Cancel (Go back to transaction selection)\n")
        
        edit_choice <- suppressWarnings(as.integer(readline(blue("Enter your choice: "))))
        if (is.na(edit_choice)) {
            cat(red("Invalid input. Please enter a number.\n"))
            next
        }
        
        updated_value <- NULL
        update_column <- NULL
        
        switch(
            edit_choice,
            "1" = {
                new_description <- readline(blue(sprintf("Enter new description (Current: %s): ", selected_transaction$description)))
                if (trimws(new_description) != "") { # Only update if not empty
                    updated_value <- new_description
                    update_column <- "description"
                } else {
                    cat(yellow("Description not changed.\n"))
                }
            },
            "2" = {
                # Reuse the category selection logic from interactively_categorize_transactions
                # We need to print categories similar to that function
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
                new_category_id <- suppressWarnings(as.integer(readline(blue("Enter new category ID: "))))
                if (new_category_id %in% default_categories$category_id) {
                    updated_value <- new_category_id
                    update_column <- "category_id"
                } else {
                    cat(red("Invalid category ID. Category not changed.\n"))
                }
            },
            "3" = {
                new_funds_out <- suppressWarnings(as.numeric(readline(blue(sprintf("Enter new Funds Out (Current: %.2f, enter 0 if none): ", selected_transaction$funds_out)))))
                if (!is.na(new_funds_out) && new_funds_out >= 0) {
                    updated_value <- new_funds_out
                    update_column <- "funds_out"
                    # When funds_out is updated, funds_in should be 0
                    dbExecute(con, "UPDATE transactions SET funds_in = ? WHERE id = ?", params = list(0, selected_transaction$id))
                    cat(green("Funds In set to 0.\n"))
                } else {
                    cat(red("Invalid amount. Funds Out not changed.\n"))
                }
            },
            "4" = {
                new_funds_in <- suppressWarnings(as.numeric(readline(blue(sprintf("Enter new Funds In (Current: %.2f, enter 0 if none): ", selected_transaction$funds_in)))))
                if (!is.na(new_funds_in) && new_funds_in >= 0) {
                    updated_value <- new_funds_in
                    update_column <- "funds_in"
                    # When funds_in is updated, funds_out should be 0
                    dbExecute(con, "UPDATE transactions SET funds_out = ? WHERE id = ?", params = list(0, selected_transaction$id))
                    cat(green("Funds Out set to 0.\n"))
                } else {
                    cat(red("Invalid amount. Funds In not changed.\n"))
                }
            },
            "5" = { # New case for updating date
                repeat {
                    new_date_str <- readline(blue(sprintf("Enter new date (YYYY-MM-DD, Current: %s): ", selected_transaction$date)))
                    # Check if the input is a valid date
                    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", new_date_str)) {
                        # Further validate if it's a real date
                        if (!is.na(as.Date(new_date_str, format = "%Y-%m-%d"))) {
                            updated_value <- new_date_str
                            update_column <- "date"
                            break # Exit inner repeat loop if valid
                        } else {
                            cat(red("Invalid date format or non-existent date. Please use YYYY-MM-DD.\n"))
                        }
                    } else {
                        cat(red("Invalid date format. Please use YYYY-MM-DD.\n"))
                    }
                }
            },
            "0" = {
                cat(yellow("Cancelling edit. Returning to transaction selection.\n"))
                next # Go back to the start of the `repeat` loop for transaction selection
            },
            {
                cat(red("Invalid choice. Please enter a number between 0 and 5.\n"))
            }
        )
        
        if (!is.null(update_column) && !is.null(updated_value)) {
            sql_update <- sprintf("UPDATE transactions SET %s = ? WHERE id = ?", update_column)
            dbExecute(con, sql_update, params = list(updated_value, selected_transaction$id))
            cat(green(sprintf("Transaction ID %s: %s updated successfully.\n", selected_transaction$id, update_column)))
            # Refresh selected_transaction to reflect changes if further edits are made in the same session
            selected_transaction <- dbGetQuery(
                con,
                "SELECT id, date, description, funds_out, funds_in, category_id
                 FROM transactions
                 WHERE id = ?",
                params = list(selected_transaction$id)
            )
        }
    } # End of repeat loop for transaction selection
    cat(green("\nExiting Edit Transactions system. Returning to main menu.\n"))
}

#' Lists transactions for a given account and allows the user to select one for editing.
#'
#' @param con A DBI connection object.
#' @param account_id The ID of the account to filter transactions by.
#' @param default_categories Data frame of category IDs and names.
#' @return The selected transaction row as a data frame, or NULL if no selection.
select_transaction_for_editing <- function(con, account_id, default_categories) {
    cat(blue("\n--- Select Transaction to Edit ---\n"))
    
    # --- Filtering Options ---
    filter_sql <- " WHERE account_id = ?"
    filter_params <- list(account_id)
    
    # Ask user for filtering preferences
    cat("\nDo you want to apply filters to the transaction list?\n")
    cat("1. Filter by Year and/or Month\n")
    cat("2. Filter by Description (e.g., filename or keyword)\n")
    cat("0. No filters (show recent transactions)\n")
    
    filter_choice <- suppressWarnings(as.integer(readline(blue("Enter your choice: "))))
    
    if (filter_choice == 1) {
        # Filter by Year and Month
        year_input <- suppressWarnings(as.integer(readline(blue("Enter year (YYYY, or 0 to skip year filter): "))))
        month_input <- suppressWarnings(as.integer(readline(blue("Enter month (1-12, or 0 to skip month filter): "))))
        
        if (!is.na(year_input) && year_input > 0) {
            filter_sql <- paste0(filter_sql, " AND STRFTIME('%Y', date) = ?")
            filter_params <- c(filter_params, as.character(year_input))
        }
        
        if (!is.na(month_input) && month_input > 0 && month_input <= 12) {
            filter_sql <- paste0(filter_sql, " AND STRFTIME('%m', date) = ?")
            filter_params <- c(filter_params, sprintf("%02d", month_input)) # Ensure month is two digits
        }
        
    } else if (filter_choice == 2) {
        # Filter by Description
        search_term <- trimws(readline(blue("Enter a keyword or filename part to search in description: ")))
        if (search_term != "") {
            filter_sql <- paste0(filter_sql, " AND description LIKE ?")
            filter_params <- c(filter_params, paste0("%", search_term, "%"))
        } else {
            cat(yellow("No search term entered. Skipping description filter.\n"))
        }
    } else if (filter_choice == 0) {
        cat(blue("No filters applied. Showing recent transactions.\n"))
    } else {
        cat(red("Invalid filter choice. Showing recent transactions.\n"))
    }
    
    # Fetch transactions for the selected account, ordered by date, with filters
    query_sql <- paste0("SELECT id, date, description, funds_out, funds_in, category_id
                         FROM transactions",
                        filter_sql,
                        " ORDER BY date DESC, id DESC") # Add LIMIT here if you want to cap the number of results, e.g., " LIMIT 100"
    
    transactions_df <- dbGetQuery(con, query_sql, params = filter_params)
    
    if (nrow(transactions_df) == 0) {
        cat(yellow("No transactions found for this account with the applied filters.\n"))
        return(NULL)
    }
    
    # Join with categories for display
    transactions_display <- transactions_df %>%
        left_join(default_categories, by = "category_id") %>%
        mutate(
            "Txn ID" = id,
            "Date" = date,
            "Description" = description,
            "Amount" = ifelse(
                !is.na(funds_out),
                sprintf("-$%.2f", funds_out),
                sprintf("+$%.2f", funds_in)
            ),
            "Category" = ifelse(
                is.na(category_name),
                "UNCATEGORIZED",
                category_name
            )
        ) %>%
        select("Txn ID", "Date", "Description", "Amount", "Category")
    
    cat("\nTransactions for Selected Account (filtered):\n")
    print(dplyr::as_tibble(transactions_display), n = -1)
    
    repeat {
        txn_id_input <- suppressWarnings(as.integer(readline(blue("Enter the 'Txn ID' to edit (0 to go back): "))))
        if (is.na(txn_id_input)) {
            cat(red("Invalid input. Please enter a number.\n"))
            next
        }
        if (txn_id_input == 0) {
            return(NULL) # User chose to go back
        }
        
        selected_txn <- transactions_df %>% filter(id == txn_id_input)
        if (nrow(selected_txn) == 1) {
            return(selected_txn)
        } else {
            cat(red("Transaction ID not found in the filtered list. Please try again.\n"))
        }
    }
}