# Required libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(pdftools)
library(tabulapdf)
library(fs)
library(digest)
library(lubridate)
library(crayon)

# --- Config ---
import_dir <- "~/Documents/Budget/import"
target_dir <- "~/Documents/Budget/statements"
db_path <- "~/Documents/Budget/data/budget.db"

# --- Database Connection  ---
con <- dbConnect(SQLite(), db_path)

source("Import_statements.R")

# --- Main Menu Function ---
show_main_menu <- function() {
    cat(blue("\n--- Budgeting System Main Menu ---\n"))
    cat("1. Import e-Statements\n")
    cat("2. Edit Transactions\n")
    cat("3. Manage Accounts and Categories\n")
    cat("4. Reports\n")
    cat("5. Exit\n")
    cat(blue("----------------------------------\n"))
}

# --- Main Loop ---
main_loop_active <- TRUE
while (main_loop_active) {
    show_main_menu()
    choice <- suppressWarnings(as.integer(readline(blue("Enter your choice: "))))
    
    if (is.na(choice)) {
        cat(red("Invalid input. Please enter a number.\n"))
        next
    }
    
    switch(
        choice,
        "1" = {
            cat(green("\nStarting e-Statement Import Workflow...\n"))
            run_pdf_import_workflow() # This function is defined in Import_statements.R
            cat(green("\ne-Statement Import Workflow Finished. Returning to main menu.\n"))
        },
        "2" = {
            cat(yellow("\nEdit Transactions: This feature is not yet implemented.\n"))
            # You would call a function like edit_transactions(con) here
        },
        "3" = {
            cat(yellow("\nManage Accounts and Categories: This feature is not yet implemented.\n"))
            # You would call a function like manage_accounts_categories(con) here
        },
        "4" = {
            cat(yellow("\nReports: This feature is not yet implemented.\n"))
            # You would call a function like generate_reports(con) here
        },
        "5" = {
            cat(blue("\nExiting Budgeting System. Goodbye!\n"))
            main_loop_active <- FALSE
        },
        {
            cat(red("Invalid choice. Please enter a number between 1 and 5.\n"))
        }
    )
}


# --- Disconnect from DB ---
dbDisconnect(con)