#' Copy Directory Structure
#'
#' Recursively copies all directories, subdirectories, and files from a source
#' directory to a target directory, with option to overwrite existing files.
#' Progress messages are displayed during the copy operation, and a summary
#' is provided upon completion.
#'
#' @param source.dir Character string. Path to the source directory to copy from.
#' @param target.dir Character string. Path to the target directory to copy to.
#'   Will be created if it doesn't exist.
#' @param overwrite Logical. Whether to overwrite existing files in the target
#'   directory. Default is \code{TRUE}.
#'
#' @return Logical. Returns \code{TRUE} if all items were copied successfully,
#'   \code{FALSE} if any items failed to copy. The result is returned invisibly.
#'
#' @details
#' This function performs a complete recursive copy of a directory structure,
#' including all subdirectories and files. It will:
#' \itemize{
#'   \item Validate that the source directory exists
#'   \item Create the target directory if it doesn't exist
#'   \item Recursively copy all files and directories
#'   \item Display progress messages for each copied item
#'   \item Provide a summary of the operation upon completion
#' }
#'
#' If any files or directories fail to copy, warnings will be displayed and
#' the function will return \code{FALSE}, but it will continue attempting
#' to copy remaining items.
#'

#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' copyDirectoryContent(
#'   source.dir = "/path/to/source",
#'   target.dir = "/path/to/target"
#' )
#'
#' # Copy without overwriting existing files
#' copyDirectoryContent(
#'   source.dir = "~/documents",
#'   target.dir = "~/backup",
#'   overwrite = FALSE
#' )
#'
#' # Check if copy was successful
#' success <- copyDirectoryContent(
#'   source.dir = "./data",
#'   target.dir = "./backup/data"
#' )
#' if (success) {
#'   cat("All files copied successfully!\n")
#' } else {
#'   cat("Some files failed to copy. Check warnings.\n")
#' }
#' }
copyDirectoryContent <- function(source.dir, target.dir, overwrite = TRUE) {

  # Validate source directory exists
  if (!dir.exists(source.dir)) {
    stop(
      "Source directory does not exist: '", source.dir, "'. ",
      "Please provide a valid source directory path."
    )
  }

  # Create target directory if it doesn't exist
  if (!dir.exists(target.dir)) {
    dir.create(target.dir, recursive = TRUE)
    message("Created target directory: ", target.dir)
  }

  # Get all files and directories in source (recursive)
  all.items <- list.files(
    path = source.dir,
    full.names = TRUE,
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE
  )

  # Get relative paths for progress reporting
  relative.paths <- list.files(
    path = source.dir,
    full.names = FALSE,
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE
  )

  # Create target paths
  target.paths <- file.path(target.dir, relative.paths)

  # Pre-allocate results vector for tracking success
  copy.results <- vector("logical", length = length(all.items))

  # Process each item
  for (i in seq_along(all.items)) {
    source.item <- all.items[i]
    target.item <- target.paths[i]

    if (dir.exists(source.item)) {
      # Handle directory creation
      if (!dir.exists(target.item)) {
        dir.success <- dir.create(target.item, recursive = TRUE)
        if (dir.success) {
          message("Created directory: ", relative.paths[i])
          copy.results[i] <- TRUE
        } else {
          warning("Failed to create directory: ", relative.paths[i])
          copy.results[i] <- FALSE
        }
      } else {
        copy.results[i] <- TRUE
      }
    } else {
      # Handle file copying
      parent.dir <- dirname(target.item)
      if (!dir.exists(parent.dir)) {
        dir.create(parent.dir, recursive = TRUE)
      }

      # Copy the file
      file.success <- file.copy(
        from = source.item,
        to = target.item,
        overwrite = overwrite
      )

      if (file.success) {
        message("Copied file: ", relative.paths[i])
        copy.results[i] <- TRUE
      } else {
        warning("Failed to copy file: ", relative.paths[i])
        copy.results[i] <- FALSE
      }
    }
  }

  # Summary reporting
  total.items <- length(copy.results)
  successful.items <- sum(copy.results)

  if (successful.items == total.items) {
    message(
      "Directory copy completed successfully! Processed ",
      total.items, " items."
    )
  } else {
    failed.items <- total.items - successful.items
    warning(
      "Directory copy completed with issues. ",
      successful.items, " of ", total.items, " items processed successfully. ",
      failed.items, " items failed."
    )
  }

  result <- all(copy.results)
  return(invisible(result))
}
