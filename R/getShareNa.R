#' Get Share of NA Values in a Vector
#'
#' This function calculates the proportion of missing (NA) values in a vector.
#' It works with any vector type (numeric, character, logical, etc.) and returns
#' the fraction of missing values rounded to the specified number of digits.
#'
#' @param x A vector of any type (numeric, character, logical, etc.).
#' @param digits Integer specifying the number of decimal places to round to (default: 3).
#'   Must be a non-negative integer.
#'
#' @return A numeric value between 0 and 1 representing the proportion of NA values,
#'   rounded to the specified number of digits. Returns 0 if the vector contains no NAs,
#'   and 1 if all values are NA.
#' @export
#'
#' @examples
#' # Numeric vector with some NAs
#' x.numeric <- c(1, 2, NA, 4, NA, 6)
#' getShareNa(x.numeric)
#'
#' # Character vector with NAs
#' x.char <- c("a", "b", NA, "d", NA)
#' getShareNa(x.char)
#'
#' # Vector with no NAs
#' x.complete <- c(1, 2, 3, 4, 5)
#' getShareNa(x.complete)
#'
#' # Vector with all NAs
#' x.all.na <- c(NA, NA, NA)
#' getShareNa(x.all.na)
#'
#' # Custom number of digits
#' x.few.na <- c(rep(1, 97), rep(NA, 3))  # 3% missing
#' getShareNa(x.few.na, digits = 5)
#'
#' # Logical vector
#' x.logical <- c(TRUE, FALSE, NA, TRUE, NA)
#' getShareNa(x.logical)
getShareNa <- function(x, digits = 3) {
  # Input validation
  if (missing(x)) {
    stop("Argument 'x' is required")
  }

  if (!is.vector(x) && !is.factor(x)) {
    stop("Argument 'x' must be a vector or factor")
  }

  if (!is.numeric(digits) || length(digits) != 1 || digits < 0 || digits != round(digits)) {
    stop("Argument 'digits' must be a single non-negative integer")
  }

  # Handle empty vector
  if (length(x) == 0) {
    warning("Input vector is empty")
    return(NaN)
  }

  # Calculate proportion of NAs
  na.count <- sum(is.na(x))
  total.count <- length(x)
  na.proportion <- na.count / total.count

  # Round to specified digits
  return(round(na.proportion, digits))
}
