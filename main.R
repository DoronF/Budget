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
# Load ML libraries
library(caret)
library(ranger)
library(tm) # For text mining operations (DocumentTermMatrix, etc.)
library(SnowballC) # For stemming if you used it in training
library(tidyr)

options(max.print = 10000)

# --- Database Connection  ---
con <- dbConnect(SQLite(), db_path)

source("Import_statements.R")
source("edit_transactions.R")

# --- Reports Menu Function ---
show_reports_menu <- function(con) {
    report_files <- list.files("reports", pattern = "\\.Rmd$", full.names = TRUE)
    if (length(report_files) == 0) {
        cat(yellow(
            "No R Markdown reports found in the 'reports/' directory.\n"
        ))
        return()
    }
    
    cat(blue("\n--- Available Reports ---\n"))
    for (i in seq_along(report_files)) {
        cat(sprintf("%d. %s\n", i, basename(report_files[i])))
    }
    cat("0. Back to Main Menu\n")
    cat(blue("-------------------------\n"))
    
    report_choice <- suppressWarnings(as.integer(readline(
        blue("Enter the number of the report to generate: ")
    )))
    
    if (is.na(report_choice)) {
        cat(red("Invalid input. Please enter a number.\n"))
        return()
    }
    
    if (report_choice == 0) {
        cat(blue("Returning to Main Menu.\n"))
        return()
    } else if (report_choice >= 1 &&
               report_choice <= length(report_files)) {
        selected_report_path <- report_files[report_choice]
        cat(green(sprintf(
            "\nGenerating report: %s...\n",
            basename(selected_report_path)
        )))
        
        # Render the Rmd file
        # You might want to specify an output directory, e.g., output_dir = "rendered_reports"
        # and an output format, e.g., output_format = "html_document"
        tryCatch({
            rmarkdown::render(selected_report_path,
                              output_dir = "rendered_reports",
                              envir = parent.frame())
            cat(green(
                sprintf(
                    "Report '%s' generated successfully in 'rendered_reports/'.\n",
                    basename(selected_report_path)
                )
            ))
        }, error = function(e) {
            cat(red(
                sprintf("Error generating report: %s\n", e$message)
            ))
        })
        file_show(str_replace(selected_report_path, ".Rmd", ".html"))
        
    } else {
        cat(red("Invalid report choice.\n"))
    }
}

# --- Main Menu Function ---
show_main_menu <- function() {
    cat(blue("\n--- Budgeting System Main Menu ---\n"))
    cat("1. Import e-Statements\n")
    cat("2. Import e-Statements with Predictor\n")
    cat("3. Edit Transactions\n")
    cat("4. Manage Accounts and Categories\n")
    cat("5. Reports\n")
    cat("0. Exit\n")
    cat(blue("----------------------------------\n"))
}

# --- Main Loop ---
main_loop_active <- TRUE
while (main_loop_active) {
    show_main_menu()
    choice <- suppressWarnings(as.integer(readline(blue(
        "Enter your choice: "
    ))))
    
    if (is.na(choice)) {
        cat(red("Invalid input. Please enter a number.\n"))
        next
    }
    
    switch(
        choice,
        "1" = {
            cat(green("\nStarting e-Statement Import Workflow...\n"))
            run_pdf_import_workflow(predict = 0) # This function is defined in Import_statements.R
            cat(green(
                "\ne-Statement Import Workflow Finished. Returning to main menu.\n"
            ))
        },
        "2" = {
            # --- Run the Workflow ---
            cat(green(
                "\nStarting e-Statement Import with Prediction Model Workflow...\n"
            ))
            run_pdf_import_workflow(predict = 1)
            cat(green(
                "\ne-Statement Import Workflow Finished. Returning to main menu.\n"
            ))
        },
        "3" = {
            edit_transactions(con)
            #cat(yellow("\nEdit Transactions: This feature is not yet implemented.\n"))
            # You would call a function like edit_transactions(con) here
        },
        "4" = {
            cat(
                yellow(
                    "\nManage Accounts and Categories: This feature is not yet implemented.\n"
                )
            )
            # You would call a function like manage_accounts_categories(con) here
        },
        "5" = {
            show_reports_menu(con)
            # You would call a function like generate_reports(con) here
        },
        "0" = {
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