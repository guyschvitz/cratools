#' getSeriesDf: Generate a Series Data Frame
#'
#' Generates a column of values between start and end columns within a data frame,
#' either numeric or date-based, at a specified step interval.
#'
#' @param data    data.frame   Input dataset.
#' @param start   character    Name of the column containing start values.
#' @param end     character    Name of the column containing end values.
#' @param step    numeric      Step size for the generated series (default: 1).
#' @param timeint character    Time interval for date-based series
#'                   (one of "sec", "min", "hour", "day", "week", "month", "quarter", "year"; default: "day").
#' @param varname character    Name of the output series column (default: "series").
#'
#' @return A data.frame with the original rows replicated per sequence element,
#'         and one new column (`varname`) containing the generated series.
#'
#' @examples
#' # Numeric series
#' df1 <- data.frame(start = c(1, 4), end = c(3, 6))
#' getSeriesDf(df1, "start", "end")
#'
#' # Date series
#' df2 <- data.frame(
#'   start = as.Date(c("2021-01-01", "2021-02-01")),
#'   end   = as.Date(c("2021-01-05", "2021-02-05"))
#' )
#' getSeriesDf(df2, "start", "end", step = 2, timeint = "day", varname = "dateSeq")
#'
#' @export
getSeriesDf <- function(data, start, end, step = 1, timeint = "day", varname = "series") {
  ## Input must be a data.frame
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  ## start, end, and varname must be single character strings
  if (!is.character(start) || length(start) != 1 ||
      !is.character(end)   || length(end)   != 1 ||
      !is.character(varname) || length(varname) != 1) {
    stop("`start`, `end`, and `varname` must each be single character strings.")
  }
  ## start and end columns must exist
  if (!start %in% names(data)) {
    stop(sprintf("Start column '%s' not found in data.", start))
  }
  if (!end %in% names(data)) {
    stop(sprintf("End column '%s' not found in data.", end))
  }
  ## timeint must be valid
  valid_timeints <- c("sec","min","hour","day","week","month","quarter","year")
  if (!timeint %in% valid_timeints) {
    stop("`timeint` must be one of: ", paste(valid_timeints, collapse = ", "))
  }
  ## step must be positive numeric
  if (!is.numeric(step) || length(step) != 1 || step <= 0) {
    stop("`step` must be a positive numeric value.")
  }
  ## warn if varname already exists
  if (varname %in% names(data)) {
    warning(sprintf("Output column '%s' exists and will be overwritten.", varname))
  }
  ## Determine if columns are Date or numeric
  start.is.date <- inherits(data[[start]], "Date")
  end.is.date   <- inherits(data[[end]],   "Date")
  if (start.is.date != end.is.date) {
    stop("`start` and `end` must both be Date or both numeric.")
  }
  is.date <- start.is.date
  ## no NAs allowed in start or end
  if (any(is.na(data[[start]])) || any(is.na(data[[end]]))) {
    stop("`start` and `end` columns must not contain NAs.")
  }
  ## warn if end < start
  if (any(data[[end]] < data[[start]])) {
    warning("Some end < start; those rows will yield empty sequences.")
  }
  ## prepare step argument for seq
  step.arg <- if (is.date) {
    paste(step, timeint)
  } else {
    step
  }
  ## generate sequences
  seq.list <- tryCatch(
    Map(seq, data[[start]], data[[end]], MoreArgs = list(by = step.arg)),
    error = function(e) stop("Error generating sequences: ", e$message)
  )
  ## get lengths of sequences
  lengths <- lengths(seq.list)
  ## return empty if all sequences empty
  if (all(lengths == 0)) {
    warning("All sequences empty; returning empty data.frame.")
    return(data.frame())
  }
  ## expand original data
  expanded <- tryCatch(
    as.data.frame(lapply(data, rep, lengths)),
    error = function(e) stop("Error expanding data: ", e$message)
  )
  ## bind sequence values
  seq.values <- unlist(seq.list)
  if (is.date) {
    seq.values <- as.Date(seq.values, origin = "1970-01-01")
  }
  expanded[[varname]] <- seq.values
  ## drop original start and end columns
  expanded[ , setdiff(names(expanded), c(start, end)), drop = FALSE]
}
