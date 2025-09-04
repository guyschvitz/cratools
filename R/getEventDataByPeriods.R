#' Extract Event Data by Time Periods
#'
#' This function subsets event data into specified time periods using explicit
#' start dates and either calculated or user-defined end dates. Returns both
#' the data subsets and corresponding metadata.
#'
#' @param event.df Data frame containing event data
#' @param ctry.id Character string specifying the country identifier
#' @param period.start.dates Vector of start dates for periods (Date objects or character strings)
#' @param period.end.dates Vector of end dates for periods (optional). If not provided,
#'   will calculate end dates using period.unit
#' @param period.unit Character string specifying period length when end dates not provided:
#'   "week", "month", "quarter", or "year" (default: "month")
#' @param period.labels Character vector of labels for periods (optional). If not provided,
#'   will generate labels as "{start.date} - {end.date}"
#' @param ctry.id.col Character string specifying the column name for country identifiers (default: "country")
#' @param event.date.col Character string specifying the column name for event dates (default: "event_date")
#' @param use.ctry.regex Logical indicating whether to use regex matching for country names (default: FALSE)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{period.data} - Named list of data.frames, one for each period
#'     \item \code{period.metadata} - Named list of metadata for each period containing:
#'       \itemize{
#'         \item \code{period.start} - Start date of period
#'         \item \code{period.end} - End date of period
#'         \item \code{period.label} - Human-readable period label
#'         \item \code{event.count} - Number of events in period
#'         \item \code{period.unit} - Unit used for this period (if applicable)
#'       }
#'     \item \code{country} - Country identifier used
#'     \item \code{total.events} - Total number of events across all periods
#'   }
#'
#' @examples
#' \dontrun{
#' # Example 1: Monthly periods using period.unit
#' start_dates <- c("2024-01-01", "2024-03-01", "2024-06-01")
#' result <- getEventDataByPeriods(
#'   event.df = acled_data,
#'   ctry.id = "Nigeria",
#'   period.start.dates = start_dates,
#'   period.unit = "month"
#' )
#'
#' # Example 2: Custom periods with explicit end dates
#' start_dates <- c("2007-06-01", "2011-02-01", "2015-01-01")
#' end_dates <- c("2008-12-31", "2012-06-30", "2016-03-31")
#' labels <- c("2007 Election Cycle", "2011 Election Cycle", "2015 Election Cycle")
#'
#' result <- getEventDataByPeriods(
#'   event.df = acled_data,
#'   ctry.id = "Nigeria",
#'   period.start.dates = start_dates,
#'   period.end.dates = end_dates,
#'   period.labels = labels
#' )
#'
#' # Example 3: Quarterly periods
#' start_dates <- as.Date(c("2024-01-01", "2024-04-01", "2024-07-01"))
#' result <- getEventDataByPeriods(
#'   event.df = acled_data,
#'   ctry.id = "Nigeria",
#'   period.start.dates = start_dates,
#'   period.unit = "quarter"
#' )
#'
#' # Access results
#' first_period_data <- result$period.data[[1]]
#' first_period_info <- result$period.metadata[[1]]
#' }
#'
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @importFrom purrr pmap map
#' @importFrom lubridate period
#' @export
getEventDataByPeriods <- function(event.df,
                                 ctry.id,
                                 period.start.dates,
                                 period.end.dates = NULL,
                                 period.unit = "month",
                                 period.labels = NULL,
                                 ctry.id.col = "country",
                                 event.date.col = "event_date",
                                 use.ctry.regex = FALSE) {

  # Input validation
  if (missing(event.df) || !is.data.frame(event.df)) {
    stop("event.df must be a valid data frame")
  }

  if (missing(ctry.id) || !is.character(ctry.id) || length(ctry.id) != 1) {
    stop("ctry.id must be a single character string")
  }

  if (missing(period.start.dates) || length(period.start.dates) == 0) {
    stop("period.start.dates must be a non-empty vector of dates")
  }

  # Validate period.unit parameter
  if (!period.unit %in% c("week", "month", "quarter", "year")) {
    stop("period.unit must be one of: 'week', 'month', 'quarter', 'year'. Got: ", period.unit)
  }

  # Check required columns exist
  required.cols <- c(ctry.id.col, event.date.col)
  missing.cols <- required.cols[!required.cols %in% names(event.df)]
  if (length(missing.cols) > 0) {
    stop("Missing required columns in event.df: ", paste(missing.cols, collapse = ", "))
  }

  # Convert start dates to Date objects
  if (is.character(period.start.dates)) {
    tryCatch({
      period.start.dates <- as.Date(period.start.dates)
    }, error = function(e) {
      stop("Could not convert period.start.dates to dates. Please ensure they contain valid date strings. Error: ", e$message)
    })
  }

  # Handle end dates
  if (is.null(period.end.dates)) {
    # Generate end dates based on period.unit
    if (period.unit == "week") {
      period.end.dates <- period.start.dates + lubridate::period(1, units = "weeks") - 1
    } else if (period.unit == "month") {
      period.end.dates <- period.start.dates + lubridate::period(1, units = "months") - 1
    } else if (period.unit == "quarter") {
      period.end.dates <- period.start.dates + lubridate::period(3, units = "months") - 1
    } else if (period.unit == "year") {
      period.end.dates <- period.start.dates + lubridate::period(1, units = "years") - 1
    }
    using.calculated.end.dates <- TRUE
  } else {
    # Convert provided end dates to Date objects
    if (is.character(period.end.dates)) {
      tryCatch({
        period.end.dates <- as.Date(period.end.dates)
      }, error = function(e) {
        stop("Could not convert period.end.dates to dates. Please ensure they contain valid date strings. Error: ", e$message)
      })
    }

    # Validate that start and end dates have same length
    if (length(period.start.dates) != length(period.end.dates)) {
      stop("period.start.dates and period.end.dates must have the same length")
    }

    # Validate that start dates are before end dates
    if (any(period.start.dates >= period.end.dates)) {
      stop("All start dates must be before their corresponding end dates")
    }

    using.calculated.end.dates <- FALSE
  }

  # Handle period labels
  if (is.null(period.labels)) {
    period.labels <- paste0(period.start.dates, " - ", period.end.dates)
    message("No period labels provided. Using date range labels: ", paste(period.labels[1:min(3, length(period.labels))], collapse = ", "), if(length(period.labels) > 3) "..." else "")
  } else {
    if (length(period.labels) != length(period.start.dates)) {
      stop("period.labels must have the same length as period.start.dates")
    }
  }

  # Status message: Starting analysis
  message("Starting event data extraction by periods for ", ctry.id)
  message("  - Extracting data for ", length(period.start.dates), " periods")
  if (using.calculated.end.dates) {
    message("  - Using calculated end dates based on period.unit: ", period.unit)
  } else {
    message("  - Using user-provided end dates")
  }

  tryCatch({

    # Subset event data to country
    message("  - Filtering events for ", ctry.id)

    if (use.ctry.regex) {
      event.sub.df <- event.df |>
        dplyr::filter(grepl(ctry.id, !!sym(ctry.id.col), ignore.case = TRUE))
    } else {
      event.sub.df <- event.df |>
        dplyr::filter(!!sym(ctry.id.col) == ctry.id)
    }

    # Check if any data exists
    if (nrow(event.sub.df) == 0) {
      stop("No event data found for ", ctry.id)
    }

    message("  - Found ", nrow(event.sub.df), " total events for ", ctry.id)

    # Extract data for each period
    message("  - Extracting event data for each period")

    period.results.ls <- purrr::pmap(
      list(period.start.dates, period.end.dates, period.labels),
      function(period.start, period.end, period.label) {

        message("    - Processing ", period.label, " (", period.start, " to ", period.end, ")")

        # Extract events for this period
        period.events.df <- event.sub.df |>
          dplyr::filter(
            !!sym(event.date.col) >= period.start &
              !!sym(event.date.col) <= period.end
          ) |>
          dplyr::mutate(period_label = period.label)

        event.count <- nrow(period.events.df)
        message("    - Found ", event.count, " events for ", period.label)

        # Create metadata object
        period.metadata <- list(
          period.start = period.start,
          period.end = period.end,
          period.label = period.label,
          event.count = event.count
        )

        # Add period.unit if using calculated end dates
        if (using.calculated.end.dates) {
          period.metadata$period.unit <- period.unit
        }

        # Return both data and metadata
        return(list(
          data = period.events.df,
          metadata = period.metadata
        ))
      }
    )

    # Extract data and metadata into separate lists
    period.data.ls <- purrr::map(period.results.ls, ~ .x$data)
    period.metadata.ls <- purrr::map(period.results.ls, ~ .x$metadata)

    # Set names for the lists using period labels
    names(period.data.ls) <- period.labels
    names(period.metadata.ls) <- period.labels

    # Calculate total events across all periods
    total.events <- sum(sapply(period.metadata.ls, function(x) x$event.count))

    # Create final result object
    result.ls <- list(
      period.data = period.data.ls,
      period.metadata = period.metadata.ls,
      country = ctry.id,
      total.events = total.events
    )

    # Status message: Success
    message("  * Successfully extracted data for ", length(period.data.ls), " periods")
    message("  * Total events across all periods: ", total.events)

    return(result.ls)

  }, error = function(e) {
    error.msg <- paste("Error in getEventDataByPeriods for", ctry.id, ":", e$message)
    message(error.msg)
    stop(error.msg, call. = FALSE)
  })
}
