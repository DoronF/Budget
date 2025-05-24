# extract_chequing_transactions.R
# This script extracts transactions from Simplii Chequing account PDFs using text parsing.

#' Extracts transactions from a Chequing account PDF using text-based parsing.
#'
#' @param file_path Path to the PDF.
#' @param year The year of the transactions (e.g., 2025).
#' @return A data frame of extracted transactions, or NULL if extraction fails.
extract_chequing_transactions <- function(file_path, year) {
    
    # Load necessary libraries
    if (!requireNamespace("pdftools", quietly = TRUE)) install.packages("pdftools")
    if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
    if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
    library(pdftools)
    library(stringr)
    library(dplyr)
    
    # Define regex patterns
    # Date pattern: "Mon DD"
    date_pattern <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+\\d{1,2}"
    # Currency pattern: numbers with optional commas and two decimal places, optional negative
    currency_pattern <- "-?\\s*[0-9,]+\\.\\d{2}"
    
    # Get all text from all pages
    all_pages_text <- tryCatch(
        pdftools::pdf_text(file_path),
        error = function(e) {
            message("Error reading PDF text:", e$message)
            return(NULL)
        }
    )
    
    if (is.null(all_pages_text) || length(all_pages_text) == 0) {
        message("No text extracted from Chequing account PDF:", file_path)
        return(NULL)
    }
    
    temp_transactions_list <- list()
    balance_forward_value <- NA_real_
    
    # Loop through each page's text to extract raw transaction data
    for (page_num in seq_along(all_pages_text)) {
        page_text <- all_pages_text[page_num]
        lines <- stringr::str_split(page_text, "\n")[[1]]
        
        for (line_idx in seq_along(lines)) {
            line <- trimws(lines[line_idx])
            
            # --- Filter out known non-transaction lines early ---
            if (nchar(line) < 15 || # Very short lines are unlikely to be transactions
                grepl("statement period:|page \\d+ of \\d+|PO Box|SIMPLII FINANCIAL|Your no fee chequing account|Please note|Interest Charges|Date\\s+EffectiveDate\\s+Description|trans\\.\\s*date\\s*eff\\.\\s*date\\s*transaction\\s*funds\\s*out\\s*funds\\s*in\\s*balance", line, ignore.case = TRUE) || # Common headers/footers/table headers
                grepl("total funds out|closing balance|end of transactions", line, ignore.case = TRUE) # Summary/info lines
            ) {
                next
            }
            
            # Handle BALANCE FORWARD separately to get its value and then skip it as a transaction
            if (grepl("BALANCE FORWARD", line, ignore.case = TRUE)) {
                amounts_bf <- stringr::str_extract_all(line, currency_pattern)[[1]]
                amounts_bf_clean <- suppressWarnings(as.numeric(gsub(",", "", amounts_bf)))
                if (length(amounts_bf_clean) > 0) {
                    balance_forward_value <- tail(amounts_bf_clean, 1)
                }
                next # Skip this line from being processed as a regular transaction
            }
            
            
            # --- Attempt to parse a transaction line ---
            
            # 1. Find all dates
            all_dates_in_line <- stringr::str_extract_all(line, date_pattern)[[1]]
            all_dates_in_line <- all_dates_in_line[all_dates_in_line != ""] # Remove empty matches
            
            # 2. Find all currency numbers
            all_amounts_in_line <- stringr::str_extract_all(line, currency_pattern)[[1]]
            all_amounts_in_line <- all_amounts_in_line[all_amounts_in_line != ""] # Remove empty matches
            all_amounts_in_line_clean <- suppressWarnings(as.numeric(gsub(",", "", all_amounts_in_line)))
            
            
            # Check if line looks like a transaction (at least 2 dates and at least 1 amount for balance)
            if (length(all_dates_in_line) >= 2 && length(all_amounts_in_line_clean) >= 1) {
                
                trans_date_str <- all_dates_in_line[1]
                effective_date_str <- all_dates_in_line[2]
                
                balance_num <- tail(all_amounts_in_line_clean, 1) # Last amount is always balance
                
                # Extract description
                date1_loc <- stringr::str_locate(line, stringr::fixed(trans_date_str))[1,]
                date2_loc <- stringr::str_locate(line, stringr::fixed(effective_date_str))[1,]
                
                # Find the location of the first currency amount. This marks the end of the description.
                # Use min() in case there are multiple amounts and we need the first one's start position.
                first_amount_loc_start <- min(stringr::str_locate(line, stringr::fixed(all_amounts_in_line[1]))[1,1], na.rm = TRUE)
                
                description_start_char <- max(date1_loc[2], date2_loc[2]) + 1
                description_end_char <- first_amount_loc_start - 1
                
                # Extract raw description substring
                description <- trimws(stringr::str_sub(line, description_start_char, description_end_char))
                
                # --- More aggressive cleaning for Description ---
                # Remove any leftover date patterns that might have slipped into description
                description <- stringr::str_replace_all(description, date_pattern, "")
                # Remove general header-like terms that might appear in description
                description <- stringr::str_replace_all(description, "trans\\. date|eff\\. date|transaction|funds out|funds in|balance|account number", "")
                
                # Remove multiple spaces and replace with a single space
                description <- stringr::str_replace_all(description, "\\s+", " ")
                description <- trimws(description) # Trim again after replacements
                
                
                # If description is very short/empty and only a balance, might be junk.
                if (nchar(description) < 3 && length(all_amounts_in_line_clean) == 1) {
                    next
                }
                
                temp_transactions_list[[length(temp_transactions_list) + 1]] <- data.frame(
                    Date = trans_date_str,
                    EffectiveDate = effective_date_str,
                    Description = description,
                    Balance = balance_num,
                    stringsAsFactors = FALSE
                )
            }
        }
    }
    
    if (length(temp_transactions_list) == 0) {
        message("No transactions extracted from any page in Chequing account PDF:", file_path)
        return(NULL)
    }
    
    df <- bind_rows(temp_transactions_list)
    
    # Final cleaning and date parsing
    df$Date <- as.Date(paste(year, df$Date), format = "%Y %b %d")
    df$EffectiveDate <- as.Date(paste(year, df$EffectiveDate), format = "%Y %b %d")
    
    # Remove rows where date parsing failed
    df <- df %>% filter(!is.na(Date) & !is.na(EffectiveDate))
    
    # Order by Date and then EffectiveDate to ensure correct balance calculation
    df <- df %>% arrange(Date, EffectiveDate)
    
    # Add PreviousBalance column
    df$PreviousBalance <- lag(df$Balance, n = 1L)
    
    # Handle the very first transaction's previous balance
    # If a BALANCE FORWARD value was found, use it for the first actual transaction
    if (!is.na(balance_forward_value) && nrow(df) > 0) {
        df$PreviousBalance[1] <- balance_forward_value
    }
    
    # Determine FundsIn and FundsOut based on balance change
    df <- df %>%
        mutate(
            BalanceDifference = round(Balance - PreviousBalance, 2),
            FundsIn = case_when(
                BalanceDifference > 0 ~ BalanceDifference, # If balance increased, it's funds in
                TRUE ~ NA_real_
            ),
            FundsOut = case_when(
                BalanceDifference < 0 ~ abs(BalanceDifference), # If balance decreased, it's funds out (positive value)
                TRUE ~ NA_real_
            )
        ) %>%
        select(-BalanceDifference, -PreviousBalance) # Remove temporary columns
    
   # df$account_id <- 1 # Assuming Chequing is always account_id 1.
    
    # Remove duplicates (important for robust parsing)
    df <- df %>% distinct(Date, Description, FundsOut, FundsIn, Balance, .keep_all = TRUE)
    
    return(df)
}