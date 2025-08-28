#' Get Data Frame Chunk List
#'
#' Splits a data frame into a list of chunks with specified maximum rows per chunk.
#' This function is useful for processing large datasets in smaller batches.
#'
#' @param df A data.frame to be split into chunks
#' @param max.rows An integer specifying the maximum number of rows per chunk
#' @return A list of data.frames, each containing at most max.rows rows
#' @export
#' @examples
#' # Split data frame into chunks of 100 rows
#' test.df <- data.frame(x = 1:250, y = letters[1:250])
#' chunks.ls <- getDfChunkList(df = test.df, max.rows = 100)
#' length(chunks.ls)  # Returns 3
getDfChunkList <- function(df, max.rows) {

  # Validate inputs
  if (!is.data.frame(df)) {
    stop(
      "Argument 'df' must be a data.frame, got ",
      class(df)[1]
    )
  }

  if (!is.numeric(max.rows) || length(max.rows) != 1 || max.rows <= 0) {
    stop("Argument 'max.rows' must be a positive integer.")
  }

  if (nrow(df) == 0) {
    warning("Input data frame is empty. Returning empty list.")
    empty.list <- list()
    return(empty.list)
  }

  # Calculate number of chunks needed
  n.chunks <- ceiling(nrow(df) / max.rows)

  # Create group indices
  groups <- rep(1:n.chunks, each = max.rows, length.out = nrow(df))

  # Split the dataframe
  result.ls <- split(df, groups)

  return(result.ls)
}
