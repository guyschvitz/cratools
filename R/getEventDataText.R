#' Extract and Format Text from Event Data
#'
#' This function extracts and formats narrative descriptions from event data,
#' with customizable column selection, deduplication, and text formatting.
#' Designed to work with output from getEventDataByPeriods() or any event data.frame.
#'
#' @param event.df A data.frame containing event data
#' @param text.cols Character vector specifying columns to include in text output (default: c("event_date", "event_type", "notes"))
#' @param dedup.cols Character vector specifying columns to use for deduplication (default: same as text.cols)
#' @param text.format Character string with glue-style formatting template (default: ACLED format)
#' @param sort.col Character string specifying column to sort by (default: "event_date")
#' @param sort.ascending Logical indicating sort direction (default: TRUE)
#'
#' @return Character vector containing formatted text descriptions, one per unique event
#'
#' @examples
#' \dontrun{
#' # Example 1: Default ACLED formatting
#' text.output <- getEventDataText(event.df = period.data)
#'
#' # Example 2: Custom ACLED format with more details
#' text.output <- getEventDataText(
#'   event.df = period.data,
#'   text.cols = c("event_date", "event_type", "actor1", "actor2", "fatalities", "notes"),
#'   text.format = "Date: {event_date}\nType: {event_type}\nActors: {actor1} vs {actor2}\nFatalities: {fatalities}\nDescription: {notes}"
#' )
#'
#' # Example 3: UCDP-GED format
#' text.output <- getEventDataText(
#'   event.df = ucdp.data,
#'   text.cols = c("date_start", "conflict_name", "deaths_total", "adm_1"),
#'   dedup.cols = c("date_start", "conflict_name", "adm_1"),
#'   text.format = "Date: {date_start}\nConflict: {conflict_name}\nDeaths: {deaths_total}\nLocation: {adm_1}",
#'   sort.col = "date_start"
#' )
#'
#' # Example 4: Simple format for quick overview
#' text.output <- getEventDataText(
#'   event.df = event.data,
#'   text.cols = c("event_date", "event_type"),
#'   text.format = "{event_date}: {event_type}"
#' )
#'
#' # Usage with getEventDataByPeriods output
#' period.result <- getEventDataByPeriods(...)
#' period.texts.ls <- purrr::map(period.result$period.data, getEventDataText)
#' }
#'
#' @importFrom data.table as.data.table
#' @importFrom glue glue_data
#' @export
getEventDataText <- function(event.df,
                             text.cols = c("event_date", "event_type", "notes"),
                             dedup.cols = NULL,
                             text.format = "Date: {event_date}\nType: {event_type}\nDescription: {notes}",
                             sort.col = "event_date",
                             sort.ascending = TRUE) {

  # Input validation
  if (missing(event.df) || !is.data.frame(event.df)) {
    stop("event.df must be a valid data.frame")
  }

  if (nrow(event.df) == 0) {
    return(character(0))
  }

  if (!is.character(text.cols) || length(text.cols) == 0) {
    stop("text.cols must be a non-empty character vector")
  }

  if (!is.character(text.format) || length(text.format) != 1) {
    stop("text.format must be a single character string")
  }

  if (!is.character(sort.col) || length(sort.col) != 1) {
    stop("sort.col must be a single character string")
  }

  if (!is.logical(sort.ascending) || length(sort.ascending) != 1) {
    stop("sort.ascending must be a single logical value")
  }

  # Use text.cols for deduplication if dedup.cols not specified
  if (is.null(dedup.cols)) {
    dedup.cols <- text.cols
  }

  # Check that required columns exist
  all.required.cols <- unique(c(text.cols, dedup.cols, sort.col))
  missing.cols <- all.required.cols[!all.required.cols %in% names(event.df)]
  if (length(missing.cols) > 0) {
    stop("Missing required columns in event.df: ", paste(missing.cols, collapse = ", "))
  }

  message("Processing ", nrow(event.df), " events for text extraction")
  message("  - Using columns for text: ", paste(text.cols, collapse = ", "))
  message("  - Using columns for deduplication: ", paste(dedup.cols, collapse = ", "))

  tryCatch({

    # Convert to data.table for efficient processing
    requireNamespace("data.table", quietly = TRUE)
    dt <- data.table::as.data.table(event.df)

    # Ensure sort column is appropriate type for sorting
    if (sort.col %in% names(dt) && is.character(dt[[sort.col]])) {
      # Try to convert date-like strings to Date objects
      if (grepl("date", sort.col, ignore.case = TRUE)) {
        tryCatch({
          dt[[sort.col]] <- as.Date(dt[[sort.col]])
          message("  - Converted ", sort.col, " to Date for sorting")
        }, error = function(e) {
          message("  - Warning: Could not convert ", sort.col, " to Date, using as character")
        })
      }
    }

    # Remove duplicates based on specified columns
    dt.unique <- unique(dt, by = dedup.cols)

    if (nrow(dt.unique) < nrow(dt)) {
      duplicates.removed <- nrow(dt) - nrow(dt.unique)
      message("  - Removed ", duplicates.removed, " duplicate events")
    }

    # Sort by specified column
    if (sort.ascending) {
      dt.unique <- dt.unique[order(get(sort.col))]
    } else {
      dt.unique <- dt.unique[order(-get(sort.col))]
    }

    # Create formatted text output
    message("  - Formatting text using glue template")

    # Convert data.table back to data.frame for glue compatibility
    df.unique <- as.data.frame(dt.unique)

    # Use glue to format text with the data.frame
    formatted.text.vec <- glue::glue_data(df.unique, text.format)

    # Convert glue output to character vector
    result.vec <- as.character(formatted.text.vec)

    message("  * Successfully extracted and formatted ", length(result.vec), " unique events")

    return(result.vec)

  }, error = function(e) {
    error.msg <- paste("Error in getEventDataText:", e$message)
    message(error.msg)
    stop(error.msg, call. = FALSE)
  })
}
