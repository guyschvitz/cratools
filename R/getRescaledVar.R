#' Rescale Variable to New Range
#'
#' This function rescales a numeric variable to a new range, with optional log transformation
#' and custom minimum/maximum bounds. Values are linearly transformed to fit within the
#' specified range (0 to new.max).
#'
#' @param x A numeric vector to be rescaled.
#' @param min Optional minimum value for rescaling. If NULL, uses the minimum of x.
#'   Values below this threshold will be capped at this minimum.
#' @param max Optional maximum value for rescaling. If NULL, uses the maximum of x.
#'   Values above this threshold will be capped at this maximum.
#' @param new.max The maximum value of the rescaled range (default: 10).
#'   The rescaled values will range from 0 to new.max.
#' @param log.transform Logical indicating whether to apply log transformation
#'   before rescaling (default: FALSE). Uses log(x + 1) to handle zero values.
#'
#' @return A numeric vector of rescaled values ranging from 0 to new.max.
#' @export
#'
#' @examples
#' # Basic rescaling
#' x <- c(1, 5, 10, 15, 20)
#' getRescaledVar(x)
#'
#' # Rescaling with custom range
#' getRescaledVar(x, new.max = 100)
#'
#' # Rescaling with custom min/max bounds
#' getRescaledVar(x, min = 5, max = 15)
#'
#' # Log transformation before rescaling
#' x.skewed <- c(1, 10, 100, 1000)
#' getRescaledVar(x.skewed, log.transform = TRUE)
#'
#' # Handling data with outliers
#' x.outliers <- c(1, 2, 3, 4, 100)
#' getRescaledVar(x.outliers, max = 10)  # Cap outlier at 10
getRescaledVar <- function(x, min = NULL, max = NULL, new.max = 10, log.transform = FALSE) {
  # Input validation
  if (missing(x) || is.null(x)) {
    stop("Argument 'x' is required and cannot be NULL")
  }

  if (!is.numeric(x)) {
    stop("Argument 'x' must be numeric")
  }

  if (length(x) == 0) {
    warning("Input vector is empty")
    return(numeric(0))
  }

  if (all(is.na(x))) {
    warning("All values in x are NA")
    return(rep(NA_real_, length(x)))
  }

  if (!is.null(min) && (!is.numeric(min) || length(min) != 1)) {
    stop("Argument 'min' must be a single numeric value or NULL")
  }

  if (!is.null(max) && (!is.numeric(max) || length(max) != 1)) {
    stop("Argument 'max' must be a single numeric value or NULL")
  }

  if (!is.numeric(new.max) || length(new.max) != 1 || new.max <= 0) {
    stop("Argument 'new.max' must be a single positive numeric value")
  }

  if (!is.logical(log.transform) || length(log.transform) != 1) {
    stop("Argument 'log.transform' must be a single logical value")
  }

  # Check for negative values when log transformation is requested
  if (log.transform && any(x < 0, na.rm = TRUE)) {
    stop("Cannot apply log transformation to negative values")
  }

  # Store original NA positions
  na.positions <- is.na(x)

  # Work with non-NA values
  x.work <- x[!na.positions]

  if (length(x.work) == 0) {
    return(x)  # All NA, return as-is
  }

  # Apply log transformation if requested
  if (log.transform) {
    x.work <- log(x.work + 1)
  }

  # Determine and validate max value
  if (!is.null(max)) {
    if (log.transform) {
      max.transformed <- log(max + 1)
    } else {
      max.transformed <- max
    }
    x.work <- pmin(x.work, max.transformed)
  } else {
    max.transformed <- max(x.work, na.rm = TRUE)
  }

  # Determine and validate min value
  if (!is.null(min)) {
    if (log.transform) {
      min.transformed <- log(min + 1)
    } else {
      min.transformed <- min
    }
    x.work <- pmax(x.work, min.transformed)
  } else {
    min.transformed <- min(x.work, na.rm = TRUE)
  }

  # Check for valid range
  if (max.transformed <= min.transformed) {
    if (max.transformed == min.transformed) {
      warning("Min and max are equal; returning constant values")
      result <- rep(new.max / 2, length(x))
      result[na.positions] <- NA_real_
      return(result)
    } else {
      stop("Maximum value must be greater than minimum value")
    }
  }

  # Rescale the values
  x.rescaled <- ((x.work - min.transformed) / (max.transformed - min.transformed)) * new.max

  # Reconstruct full vector with NAs in original positions
  result <- rep(NA_real_, length(x))
  result[!na.positions] <- x.rescaled

  return(result)
}
