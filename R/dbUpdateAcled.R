#' Update ACLED Data with Upsert Logic
#'
#' Performs upsert operation on ACLED database table, keeping only the latest
#' timestamp for each event_id_cnty. This function handles duplicate records
#' by keeping only the most recent timestamp for each unique event identifier.
#'
#' @param connection A database connection object created with \code{\link[DBI]{dbConnect}}
#' @param acled.new.df A data.frame containing new ACLED data. Must include
#'   columns 'event_id_cnty' and 'timestamp'
#' @return Integer. Number of records successfully processed and upserted
#' @export
#' @importFrom DBI dbIsValid
#' @importFrom dplyr group_by slice_max ungroup mutate
#' @importFrom dbx dbUpsert
#' @examples
#' \dontrun{
#' # Create database connection
#' con <- DBI::dbConnect(odbc::odbc(), "connection_string")
#' 
#' # Load new ACLED data
#' acled.new.df <- read.csv("new_acled_data.csv")
#' 
#' # Update database
#' records.updated <- dbUpdateAcled(
#'   connection = con, 
#'   acled.new.df = acled.new.df
#' )
#' 
#' # Clean up
#' DBI::dbDisconnect(con)
#' }
dbUpdateAcled <- function(connection, acled.new.df) {
  
  # Validate inputs
  if (!dbIsValid(dbObj = connection)) {
    stop("Database connection is not valid. Please check your connection.")
  }
  
  if (!is.data.frame(acled.new.df)) {
    stop(
      "Argument 'acled.new.df' must be a data.frame, got ", 
      class(acled.new.df)[1]
    )
  }
  
  if (nrow(acled.new.df) == 0) {
    stop("Input data is empty. Cannot proceed with upsert operation.")
  }
  
  required.columns <- c("event_id_cnty", "timestamp")
  missing.columns <- setdiff(required.columns, names(acled.new.df))
  
  if (length(missing.columns) > 0) {
    stop(
      "Required column(s) missing from acled.new.df: ",
      paste(missing.columns, collapse = ", ")
    )
  }
  
  message("Processing ", nrow(acled.new.df), " ACLED records...")
  
  # Pre-process: Handle duplicates in incoming data
  # Keep only the latest timestamp for each event_id_cnty
  clean.data.df <- acled.new.df |>
    group_by(event_id_cnty) |>
    slice_max(
      order_by = timestamp, 
      n = 1, 
      with_ties = FALSE
    ) |>
    ungroup() |>
    mutate(last_updated = Sys.time())
  
  # Check for duplicates removed
  duplicates.removed <- nrow(acled.new.df) - nrow(clean.data.df)
  if (duplicates.removed > 0) {
    warning(
      "Removed ", duplicates.removed, 
      " duplicate records based on event_id_cnty and timestamp."
    )
  }
  
  # Perform upsert operation
  result <- tryCatch(
    expr = {
      dbUpsert(
        conn = connection,
        table = "data_input.acled",
        records = clean.data.df,
        where_cols = "event_id_cnty",
        no_update_cols = c("event_id_cnty")
      )
      
      records.processed <- nrow(clean.data.df)
      message("Successfully processed ", records.processed, " records.")
      
      records.processed
    },
    error = function(e) {
      error.message <- paste(
        "Failed to upsert ACLED data:", 
        e$message
      )
      stop(error.message, call. = FALSE)
    }
  )
  
  return(result)
}