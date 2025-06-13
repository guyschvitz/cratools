#' Export data.frame to SQL database
#'
#' This function exports a data.frame to a specified SQL Server database table.
#' It includes validation checks for the database connection, table existence,
#' and column availability before performing the export operation.
#'
#' @param con A valid DBI database connection object
#' @param input.df A data.frame to be exported to the database
#' @param export.cols A character vector of column names to export. If NULL, all columns are exported
#' @param schema A character string specifying the database schema name
#' @param table A character string specifying the database table name
#' @param create.if.missing Logical. If TRUE, creates the table if it doesn't exist. If FALSE, stops with error. Default is FALSE
#' @param overwrite Logical. If TRUE, overwrite the existing database table. If FALSE, append rows of 'input.df' to the existing table.
#'
#' @returns A data.frame containing metadata on the export operation including schema, table, date, number of rows exported, and success status
#' @export
#'
#' @examples
#' \dontrun{
#' exportToDataBase(con = con,
#'                  input.df = dcrm.6m.pred.df,
#'                  export.cols = NULL,
#'                  schema = "dcrm",
#'                  table = "ResultsTemp")
#' }
exportToDataBase <- function(con, input.df, export.cols = NULL, schema, table, create.if.missing = FALSE, overwrite = FALSE){
  
  # Input validation
  if (!is.character(schema) || length(schema) != 1 || nchar(schema) == 0) {
    stop("Schema must be a non-empty character string")
  }
  
  if (!is.character(table) || length(table) != 1 || nchar(table) == 0) {
    stop("Table must be a non-empty character string")
  }
  
  # Validate schema and table names to prevent SQL injection
  if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", schema)) {
    stop("Schema name contains invalid characters. Only letters, numbers, and underscores are allowed, starting with a letter")
  }
  
  if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", table)) {
    stop("Table name contains invalid characters. Only letters, numbers, and underscores are allowed, starting with a letter")
  }
  
  # Convert input.df to standard R data.frame to avoid unexpected behavior
  input.df <- data.frame(input.df)
  
  # If export.cols not specified, export all columns
  if(is.null(export.cols)){
    export.cols <- names(input.df)
  }
  
  # Stop export if not all export columns are found in input data.frame
  na.cols1 <- setdiff(export.cols, names(input.df))
  if(length(na.cols1) > 0){
    stop(sprintf("Columns not found in input dataset: %s", paste(sQuote(na.cols1), collapse = ", ")))
  }
  
  # Stop export if not all export columns are found in database table
  col.query <- sprintf("SELECT * FROM %s.%s WHERE 1 = 0", schema, table)
  col.response <- DBI::dbGetQuery(con, col.query)
  na.cols2 <- setdiff(export.cols, names(col.response))
  if(length(na.cols2) > 0){
    stop(sprintf("Columns not found in input dataset: %s", paste(sQuote(na.cols2), collapse = ", ")))
  }
  
  # Stop if connection is invalid
  if(!DBI::dbIsValid(con)){
    stop("Database connection is invalid.")
  }
  
  # Check if schema and table exist
  query <- sprintf("SELECT COUNT(*) as table_count FROM information_schema.tables WHERE table_schema = '%s' AND table_name = '%s'",
                   schema, table)
  
  response <- tryCatch({
    DBI::dbGetQuery(con, query)
  }, error = function(e) {
    stop(sprintf("Error checking table existence: %s", e$message))
  })
  
  tbl.exists <- response$table_count > 0
  
  if(!tbl.exists && !create.if.missing){
    stop(sprintf("Database table not found: '%s.%s'. Set create.if.missing = TRUE to create the table automatically.", 
                 schema, table))
  }
  
  # If table doesn't exist and we're trying to append, we need to create it first
  if(!tbl.exists && !overwrite){
    message(sprintf("Table '%s.%s' does not exist. Creating new table (append mode will act as overwrite for new tables).", 
                    schema, table))
  }
  
  # Prepare data for export
  export.df <- input.df[, export.cols, drop = FALSE]
  n.rows <- nrow(export.df)
  
  # Export table with error handling
  export.job <- tryCatch({
    if (overwrite) {
      DBI::dbWriteTable(con, 
                        value = export.df,
                        name = DBI::Id(schema = schema, table = table),
                        overwrite = TRUE)
    } else {
      DBI::dbWriteTable(con, 
                        value = export.df,
                        name = DBI::Id(schema = schema, table = table),
                        append = TRUE)
    }
  }, error = function(e) {
    stop(sprintf("Error writing to database: %s", e$message))
  })
  
  # Return export log
  result <- data.frame(
    schema = schema,
    table = table,
    date = Sys.Date(),
    datetime = Sys.time(),
    rows.exported = n.rows,
    columns.exported = length(export.cols),
    success = export.job,
    stringsAsFactors = FALSE
  )
  
  return(result)
}
