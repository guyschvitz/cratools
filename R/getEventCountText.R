#' Process Conflict Event Data for LLM Analysis
#'
#' This function preprocesses conflict event datasets (e.g., ACLED, UCDP) by filtering,
#' aggregating, and formatting the data into either structured tables or human-readable
#' text summaries suitable for Large Language Model (LLM) processing.
#'
#' @param event.df data.frame. Conflict event data.
#' @param start.date Date or YYYY-MM-DD. Start date for filtering events.
#' @param end.date Date, YYYY-MM-DD, or NULL. End date for filtering events.
#'   If NULL, all events from start.date onwards are included.
#' @param event.date.col character. Column name containing event dates.
#' @param ctry.id.col character. Column name containing country identifiers.
#' @param ctry.id character vector. Country identifier(s) to filter by.
#' @param event.type.col character. Column name containing event type information.
#' @param use.ctry.regex logical. If TRUE, uses regex matching for country filtering;
#'   if FALSE, uses exact matching. Default TRUE.
#' @param by.event.type logical. If TRUE, results include event counts by event type.
#'   If FALSE, only return total event counts (across all event types). Default TRUE.
#' @param output.as.text logical. If TRUE (default), returns formatted text summaries.
#'   If FALSE, returns the aggregated data.frame.
#'
#' @return
#' If `output.as.text = TRUE`, a named character vector where each element contains a
#' formatted text summary of event counts by type and country for the specified time period.
#' If `output.as.text = FALSE`, a tidy data.frame with event counts by country, type, and date range.
#'
#' @examples
#' \dontrun{
#' # Example with ACLED data
#' result <- getEventCountText(
#'   event.df = acled.df,
#'   start.date = "2023-01-01",
#'   end.date = "2023-12-31",
#'   event.date.col = "event_date",
#'   ctry.id.col = "country",
#'   ctry.id = "Nigeria",
#'   event.type.col = "event_type"
#' )
#' }
#'
#' @importFrom dplyr filter group_by summarise rename mutate left_join n bind_rows arrange
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom rlang sym
#' @export
getEventCountText <- function(event.df, start.date, end.date = NULL,
                              event.date.col, ctry.id.col, ctry.id,
                              event.type.col, use.ctry.regex = TRUE,
                              by.event.type = TRUE, output.as.text = TRUE) {

  # Input validation
  if (!is.data.frame(event.df)) {
    stop("'event.df' must be a data.frame")
  }
  if (nrow(event.df) == 0) {
    stop("'event.df' is empty")
  }

  required.cols <- c(event.date.col, ctry.id.col, event.type.col)
  missing.cols <- setdiff(required.cols, names(event.df))
  if (length(missing.cols) > 0) {
    stop(glue::glue("Missing required columns: {paste(missing.cols, collapse = ', ')}"))
  }

  if (length(ctry.id) == 0 || any(is.na(ctry.id)) || any(ctry.id == "")) {
    stop("'ctry.id' cannot be empty, NA, or contain empty strings")
  }

  # Date validation and coercion
  start.date <- tryCatch({
    as.Date(start.date)
  }, error = function(e) {
    stop(glue::glue("Invalid 'start.date' format: {start.date}. Use YYYY-MM-DD format."))
  })

  if (!is.null(end.date)) {
    end.date <- tryCatch({
      as.Date(end.date)
    }, error = function(e) {
      stop(glue::glue("Invalid 'end.date' format: {end.date}. Use YYYY-MM-DD format."))
    })
    if (end.date < start.date) {
      stop("'end.date' cannot be earlier than 'start.date'")
    }
  }

  # Coerce event date column
  if (!inherits(event.df[[event.date.col]], "Date")) {
    event.df[[event.date.col]] <- tryCatch({
      as.Date(event.df[[event.date.col]])
    }, error = function(e) {
      stop(glue::glue("Cannot convert '{event.date.col}' to Date format."))
    })
  }

  # Filter by countries
  if (use.ctry.regex) {
    regex.pattern <- paste(ctry.id, collapse = "|")
    event.sub.df <- tryCatch({
      event.df |>
        dplyr::filter(grepl(regex.pattern, !!rlang::sym(ctry.id.col), ignore.case = TRUE))
    }, error = function(e) {
      stop(glue::glue("Error in regex filtering: {e$message}"))
    })
  } else {
    event.sub.df <- event.df |>
      dplyr::filter(!!rlang::sym(ctry.id.col) %in% ctry.id)
  }

  if (nrow(event.sub.df) == 0) {
    warning(glue::glue("No events found for country identifier(s): {paste(sQuote(ctry.id), collapse = ', ')}"))
    return(if (output.as.text) character(0) else data.frame())
  }

  # Filter by date range
  if (is.null(end.date)) {
    event.sub.df <- event.sub.df |>
      dplyr::filter(!!rlang::sym(event.date.col) >= start.date)
  } else {
    event.sub.df <- event.sub.df |>
      dplyr::filter(!!rlang::sym(event.date.col) >= start.date &
                      !!rlang::sym(event.date.col) <= end.date)
  }

  if (nrow(event.sub.df) == 0) {
    return(if (output.as.text) character(0) else data.frame())
  }

  # Calculate date ranges for each country
  date.ranges.df <- event.sub.df |>
    dplyr::group_by(!!rlang::sym(ctry.id.col)) |>
    dplyr::summarise(
      start_date = min(!!rlang::sym(event.date.col), na.rm = TRUE),
      end_date = max(!!rlang::sym(event.date.col), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(country = !!rlang::sym(ctry.id.col))

  # Aggregate by event type
  event.counts.df <- event.sub.df |>
    dplyr::group_by(!!rlang::sym(ctry.id.col), !!rlang::sym(event.type.col)) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::rename(country = !!rlang::sym(ctry.id.col), event_type = !!rlang::sym(event.type.col))

  # Calculate total counts
  total.counts.df <- event.counts.df |>
    dplyr::group_by(country) |>
    dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(event_type = "Total events")

  # Combine results based on by.event.type parameter
  if (by.event.type) {
    combined.counts.df <- dplyr::bind_rows(total.counts.df, event.counts.df)
  } else {
    combined.counts.df <- total.counts.df
  }

  # Add date ranges and create event text
  results.df <- combined.counts.df |>
    dplyr::left_join(date.ranges.df, by = "country") |>
    dplyr::mutate(event_text = paste(event_type, n, sep = ": ")) |>
    dplyr::arrange(country,
                   event_type != "Total events",
                   event_type)

  # Return data.frame if requested
  if (!output.as.text) {
    return(results.df)
  }

  # Generate text output
  country.names <- unique(results.df$country)

  result <- purrr::map_chr(country.names, function(country) {
    country.data <- results.df |>
      dplyr::filter(country == !!country)

    start_date <- unique(country.data$start_date)[1]
    end_date <- unique(country.data$end_date)[1]

    header <- glue::glue("Country: {country}")
    period <- glue::glue("Period:  {start_date} - {end_date}")
    body <- paste(country.data$event_text, collapse = "\n")

    glue::glue("{header}\n{period}\n{body}")
  })

  names(result) <- country.names
  return(result)
}
