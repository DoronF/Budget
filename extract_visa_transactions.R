

extract_visa_transactions <- function(file_path, year) {
    all_transactions <- list()
    
  #  cat(paste0("--- Starting extraction for: ", file_path, " (Year: ", year, ") ---\n"))
    
    tryCatch({
        # Read PDF text. pdf_text returns a character vector, one element per page.
        pdf_pages <- pdf_text(file_path)
        
        # Assuming transactions start from page 2 (index 1 in 0-based list in Python, but 2 in R's 1-based indexing)
        # Adjust if transactions are on page 1, etc.
        pages_to_extract_indices <- 2:length(pdf_pages) 
        
      #  cat(paste0("Total pages in PDF: ", length(pdf_pages), ". Pages to attempt extraction from: ", length(pages_to_extract_indices), "\n"))
        
        # Define common spend categories to be removed
        # IMPORTANT: Expand this list if you find other categories in your statements
        spend_categories_to_remove <- c(
            "Retail and Grocery",
            "Professional and Financial Services",
            "Health and Education",
            "Home and Office Improvement",
            "Hotel, Entertainment and Recreation",
            "Personal and Household Expenses",
            "Other Transactions",
            "Foreign Currency Transactions",
            "Transportation",
            "Miscellaneous", # Example of another possible category
            "Restaurant and Dining", # Example of another possible category
            "Travel", # Example of another possible category
            "Utilities", # Example of another possible category
            "Entertainment", # Example of another possible category
            "Shopping", # Example of another possible category
            "Transportation and Automotive" # Example of another possible category
        )
        
        # Convert category names to regex patterns for end-of-string matching.
        # The `\\s*$` ensures we match them at the very end of the string,
        # and `\\s+` accounts for one or more spaces before the category name.
        regex_categories_to_remove <- paste0("\\s+", str_replace_all(spend_categories_to_remove, " ", "\\\\s+"), "\\s*$")
        
        for (page_idx in pages_to_extract_indices) {
           # cat(paste0("\n--- Processing Page ", page_idx, " (Pattern Matching Approach) ---\n"))
            
            page_text <- pdf_pages[page_idx]
            
            if (is.null(page_text) || nchar(page_text) == 0) {
                #cat(paste0("  No text extracted from page ", page_idx, ". Skipping.\n"))
                next
            }
            
            # Split the extracted text into individual lines
            lines <- unlist(str_split(page_text, "\n"))
            
            in_payments_section <- FALSE
            in_charges_section <- FALSE
            
            for (line_num in 1:length(lines)) {
                line <- str_trim(lines[line_num]) # Trim whitespace
                if (nchar(line) == 0) { # Skip empty lines
                    next
                }
                
                # Section identification logic
                if (str_detect(line, "Your payments")) {
                    in_payments_section <- TRUE
                    in_charges_section <- FALSE
                  #  cat("  Detected 'Your payments' section.\n")
                    next
                } else if (str_detect(line, "Your new charges and credits")) {
                    in_payments_section <- FALSE
                    in_charges_section <- TRUE
                    #cat("  Detected 'Your new charges and credits' section.\n")
                    next
                } else if (str_detect(line, "Total payments") && in_payments_section) {
                    in_payments_section <- FALSE
                   # cat("  Detected end of 'Your payments' section.\n")
                    next
                } else if (str_detect(line, "Total for \\d{4} XXXX XXXX \\d{4}") && in_charges_section) {
                  #  cat("  Detected end of a card block for charges.\n")
                    next
                } else if (str_detect(line, "Card number \\d{4} XXXX XXXX \\d{4}$") && in_charges_section) {
                   # cat("  Detected 'Card number' line, starting new charges block.\n")
                    next
                }
                
                # --- Extract Payments ---
                if (in_payments_section) {
                    payment_pattern <- "([A-Za-z]{3}\\s+\\d{1,2})\\s+([A-Za-z]{3}\\s+\\d{1,2})\\s+(PAYMENT THANK YOU.*)\\s+([\\d,]+\\.\\d{2})$"
                    match <- str_match(line, payment_pattern)
                    
                    if (!is.na(match[1,1])) { 
                       # cat(paste0("  Found payment: ", line, "\n"))
                        amount_value <- as.numeric(str_replace_all(match[1,5], ",", ""))
                        
                        # Clean description: collapse multiple spaces and trim
                        cleaned_description <- str_trim(str_replace_all(match[1,4], "\\s+", " "))
                        
                        # Remove spend categories from the end of the description
                        for (pattern in regex_categories_to_remove) {
                            cleaned_description <- str_replace(cleaned_description, pattern, "")
                        }
                        cleaned_description <- str_trim(cleaned_description) # Re-trim after removal
                        
                        # all_transactions[[length(all_transactions) + 1]] <- list(
                        #     Date = paste(year, match[1,2]),
                        #     EffectiveDate = paste(year, match[1,3]),
                        #     Description = cleaned_description,
                        #     FundsOut = NA,
                        #     FundsIn = amount_value,
                        #     Balance = NA
                        # )
                    }
                }
                
                # --- Extract Charges and Credits ---
                if (in_charges_section) {
                    transaction_pattern <- "([A-Za-z]{3}\\s+\\d{1,2})\\s+([A-Za-z]{3}\\s+\\d{1,2})\\s+(.*?)\\s+([\\d,]+\\.\\d{2})$"
                    match <- str_match(line, transaction_pattern)
                    
                    if (!is.na(match[1,1])) { 
                        trans_date_str <- match[1,2]
                        post_date_str <- match[1,3]
                        description_str <- match[1,4] 
                        amount_str <- match[1,5]
                        
                        # Clean description: collapse multiple spaces and trim
                        cleaned_description <- str_trim(str_replace_all(description_str, "\\s+", " "))
                        
                        # Remove spend categories from the end of the description
                        for (pattern in regex_categories_to_remove) {
                            cleaned_description <- str_replace(cleaned_description, pattern, "")
                        }
                        cleaned_description <- str_trim(cleaned_description) # Re-trim after removal
                        
                        amount_value <- as.numeric(str_replace_all(amount_str, ",", ""))
                        
                        if (!is.na(amount_value) && amount_value != 0) {
                            funds_out <- ifelse(amount_value > 0, amount_value, NA)
                            funds_in <- ifelse(amount_value < 0, abs(amount_value), NA)
                            
                          #  cat(paste0("  Found charge: ", line, "\n"))
                            all_transactions[[length(all_transactions) + 1]] <- list(
                                Date = paste(year, trans_date_str),
                                EffectiveDate = paste(year, post_date_str),
                                Description = cleaned_description,
                                FundsOut = funds_out,
                                FundsIn = funds_in,
                                Balance = NA
                            )
                        }
                    }
                }
            }
        }
        
    }, error = function(e) {
        cat(paste0("Error processing PDF: ", e$message, "\n"))
        return(data.frame()) 
    })
    
    if (length(all_transactions) > 0) {
        df <- bind_rows(all_transactions) %>%
            mutate(
                Date = ymd(Date, quiet = TRUE), 
                EffectiveDate = ymd(EffectiveDate, quiet = TRUE)
            ) %>%
            filter(!is.na(Date) & !is.na(EffectiveDate)) %>%
            distinct(Date, EffectiveDate, Description, FundsOut, FundsIn, .keep_all = TRUE)
        
       # cat(paste0("--- Extraction complete. Extracted ", nrow(df), " transactions. ---\n"))
    } else {
        df <- data.frame(Date=as.Date(character()), EffectiveDate=as.Date(character()), 
                         Description=character(), FundsOut=numeric(), FundsIn=numeric(), 
                         Balance=numeric(), stringsAsFactors=FALSE)
       # cat("--- No valid transactions extracted. DataFrame is empty. ---\n")
    }
    
    return(df)
}
