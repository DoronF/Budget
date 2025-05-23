#' Extracts transactions from a Chequing account PDF.
#'
#' @param file_path Path to the PDF.
#' @param year The year of the transactions.
#' @return A data frame of extracted transactions, or NULL if extraction fails.
extract_chequing_transactions <- function(file_path, year) {
    column_names <- c("Date",
                      "EffectiveDate",
                      "Description",
                      "FundsOut",
                      "FundsIn",
                      "Balance")
    n_pages <- tryCatch(
        get_n_pages(file_path),
        error = function(e)
            0
    )
    
    if (n_pages < 2) {
        message("Skipping single-page file for Chequing account:",
                file_path)
        return(NULL)
    }
    
    tables <- tryCatch(
        tabulapdf::extract_tables(
            file_path,
            pages = 1:(n_pages - 1),
            method = "stream"
        ),
        error = function(e)
            NULL
    )
    
    if (is.null(tables)) {
        message("No tables extracted from Chequing account PDF:",
                file_path)
        return(NULL)
    }
    
    df_list <- lapply(tables, function(tbl) {
        if (nrow(tbl) > 2) {
            # Assuming first two rows are headers to be skipped
            df <- as.data.frame(tbl, stringsAsFactors = FALSE)
            df <- df[-c(1, 2), ] # Skip header rows
            colnames(df) <- column_names
            df %>%
                select(Date,
                       EffectiveDate,
                       Description,
                       FundsOut,
                       FundsIn,
                       Balance) %>%
                mutate(across(
                    c(FundsOut, FundsIn, Balance),
                    ~ suppressWarnings(as.numeric(gsub(
                        ",", "", .
                    ))) # Clean and convert numeric columns
                ))
        } else {
            NULL
        }
    })
    
    df_list <- Filter(Negate(is.null), df_list)
    if (length(df_list) == 0) {
        message("No usable data tables in Chequing account PDF:",
                file_path)
        return(NULL)
    }
    
    df <- bind_rows(df_list)
    df$Date <- as.Date(paste(year, df$Date), format = "%Y %b %d") # Parse date
    df$EffectiveDate <- as.Date(paste(year, df$EffectiveDate), format = "%Y %b %d")
    df$account_id <- 1
    
    #for testing
    df <- head(df)
    
    return(df)
}

