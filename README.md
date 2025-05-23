# README: Budget - PDF Statement Import and Categorization System

This repository contains an R script designed to automate the import, categorization, and storage of financial transactions from PDF bank and credit card statements into a SQLite database. The system provides an interactive command-line interface for user input, categorization review, and file management.

## Features

- **Automated PDF Processing**: Identifies and extracts transaction data from specified PDF formats (Chequing/Savings and Visa statements).
- **Database Management**: Initializes and connects to a SQLite database, creating necessary tables for accounts, categories, transactions, and processed files.
- **Duplicate Prevention**: Uses SHA-256 hashing to prevent re-processing of already imported PDF statements.
- **Interactive Categorization**: Guides the user through assigning categories to transactions, leveraging past categorizations for similar descriptions to suggest defaults.
- **Review and Correction**: Allows users to review categorized transactions and correct any miscategorizations before saving.
- **Organized Storage**: Moves processed PDF statements to a structured `statements` directory, organized by account, year, and month.

## Prerequisites

Before running the script, ensure you have the following installed:

- **R**: The statistical programming language.
- **RStudio (Recommended)**: An integrated development environment for R.

### R Libraries

The following R packages are required. They will be loaded at the beginning of the script:

- `DBI`: Database Interface for R.
- `RSQLite`: SQLite database driver.
- `dplyr`: A grammar of data manipulation, used for data wrangling.
- `stringr`: Provides a consistent API for string manipulation.
- `pdftools`: Tools for text extraction and rendering of PDF files.
- `tabulapdf`: R wrapper for Tabula, used for extracting tables from PDFs.
- `fs`: Cross-platform file system operations.
- `digest`: Creates cryptographic hash digests for R objects.

You can install these packages by running the following commands in your R console:

R

```
install.packages(c("DBI", "RSQLite", "dplyr", "stringr", "pdftools", "tabulapdf", "fs", "digest"))
```

## Setup and Configuration

1. **Project Structure**: Create the following directory structure within your project:
    
    ```
    .
    ├── Import_statements.R           # The main script
    ├── extract_visa_transactions.R # Script for Visa transaction extraction
    ├── extract_chequing_transactions.R # Script for Chequing transaction extraction
    └── data/
        └── budget.db      # SQLite database file (will be created if it doesn't exist)
    └── import/            # Place your new PDF statements here
    └── statements/        # Processed PDFs will be moved here
    ```
    
2. **Configuration in `budget.R`**: Open the `budget.R` script and adjust the following paths to match your system:
    
    R
    
    ```
    # --- Config ---
    import_dir <- "~/Documents/Budget/import"
    target_dir <- "~/Documents/Budget/statements"
    db_path <- "~/Documents/Budget/data/budget.db"
    ```
    - `import_dir`: The folder where you will place new PDF statements for import.
    - `target_dir`: The folder where processed PDF statements will be moved.
    - `db_path`: The path to your SQLite database file.
3. **Statement Extraction Scripts**: Ensure that `extract_visa_transactions.R` and `extract_chequing_transactions.R` are present in the same directory as `budget.R`. These scripts contain the specific logic for parsing your bank and credit card statement formats. You may need to customize these if your statement formats differ.
    

## How to Use

1. **Place PDFs**: Drop your new PDF bank or credit card statements into the `import` directory.
2. **Run the Script**: Open `budget.R` in RStudio and click "Source" or run `source("budget.R")` in the R console.
3. **Follow Prompts**:
    - The script will list available PDF files in the `import` directory. Select the number corresponding to the file you wish to process.
    - It will preview the first page of the selected PDF. Confirm if you want to import it.
    - Select the **account type** (e.g., Chequing, Simplii Visa) from the provided list.
    - Enter the **year** and **month** of the statement.
    - The script will then extract transactions and guide you through **categorizing each one**. If a transaction description has been seen before, it will suggest the previous category. You can accept (`y`), manually enter a new category, or skip (`skip`) for later.
    - After initial categorization, you will enter a **review loop**. This displays all transactions with their assigned categories. You have options to:
        - `s`: Save the data and move the file (only available if all transactions are categorized).
        - `r`: Re-categorize a specific transaction by its "Txn ID".
        - `q`: Quit without saving and skip this file.
    - Once saved, the PDF will be moved to the `statements` directory within its respective account and year/month subfolders.
4. **Repeat**: The script will return to the main menu, allowing you to process another file or exit.

## Database Schema

The SQLite database (`budget.db`) will contain the following tables:

- **`accounts`**: Stores a list of your financial accounts.
    - `account_id` (INTEGER PRIMARY KEY)
    - `account_name` (TEXT UNIQUE)
- **`categories`**: Stores a predefined list of transaction categories.
    - `category_id` (INTEGER PRIMARY KEY)
    - `category_name` (TEXT UNIQUE)
- **`transactions`**: Stores all imported and categorized financial transactions.
    - `id` (INTEGER PRIMARY KEY)
    - `file_name` (TEXT): The name of the processed PDF file.
    - `date` (TEXT)
    - `effective_date` (TEXT)
    - `description` (TEXT)
    - `funds_out` (REAL)
    - `funds_in` (REAL)
    - `balance` (REAL)
    - `account_id` (INTEGER): Foreign key referencing `accounts`.
    - `category_id` (INTEGER): Foreign key referencing `categories`.
- **`processed_files`**: Tracks PDF files that have already been imported to prevent duplicates.
    - `file_name` (TEXT PRIMARY KEY): The new path of the moved PDF.
    - `original_pdf` (TEXT): The original filename.
    - `account_id` (INTEGER)
    - `year` (INTEGER)
    - `month` (INTEGER)
    - `processed_at` (TEXT): Timestamp of processing.
    - `hash` (TEXT UNIQUE): SHA-256 hash of the original PDF content.
