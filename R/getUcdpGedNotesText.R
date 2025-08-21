#' Extract conflict event descriptions from UCDP GED data
#'
#' This function extracts and formats event narratives from UCDP GED data
#' for a specified country and time range. Can optionally group results by country
#' when multiple countries are provided.
#'
#' @param ged.df A data.frame or data.table containing GED data
#' @param ctry.id Vector of country identifiers (character)
#' @param sdate Start date (exclusive)
#' @param edate End date (exclusive)
#' @param use.regex Logical. Whether to interpret ctry.id as a regex pattern. Default is TRUE
#' @param group.by.country Logical. Whether to group results by country when multiple countries match. Default is TRUE
#'
#' @return A named list of character strings. Each element is a formatted text block for one group.
#'         If group.by.country is TRUE, list names indicate the country. If FALSE, returns single element.
#'
#' @examples
#' \dontrun{
#' # Single country for one time period
#' notes.ls <- getUcdpGedNotesText(
#'   ged.df = ged.df,
#'   ctry.id = "Nigeria",
#'   sdate = as.Date("2025-05-01"),
#'   edate = as.Date("2025-08-01")
#' )
#' }
#'
#' @importFrom data.table as.data.table set fifelse
#' @export
getUcdpGedNotesText <- function(ged.df,
                                ctry.id,
                                sdate,
                                edate,
                                use.regex = TRUE,
                                group.by.country = TRUE) {

  # Ensure data.table is loaded
  requireNamespace("data.table", quietly = TRUE)

  dt <- data.table::as.data.table(ged.df)

  # Validate input
  if (!("country" %in% names(dt)) || !("date_start" %in% names(dt))) {
    stop("GED data must contain 'country' and 'date_start' columns")
  }

  # Coerce date column to Date if not already
  if (!inherits(dt$date_start, "Date")) {
    data.table::set(dt, j = "date_start", value = as.Date(dt$date_start))
  }

  # Filter by date
  dt <- dt[date_start > sdate & date_start < edate]

  # Filter by country
  if (use.regex) {
    dt <- dt[grepl(paste(ctry.id, collapse = "|"), country, ignore.case = TRUE)]
  } else {
    dt <- dt[country %in% ctry.id]
  }

  if (nrow(dt) == 0) {
    return(list("No events recorded in this period."))
  }

  # Recode type_of_violence as labelled factor
  data.table::set(dt, j = "type_of_violence", value = factor(
    dt$type_of_violence,
    levels = 1:3,
    labels = c("State-based conflict", "Non-state conflict", "One-sided violence")
  ))

  # Replace unknown actor names based on "XXX" pattern
  data.table::set(dt, j = "side_a", value = data.table::fifelse(grepl("XXX", dt$side_a), "TBD", dt$side_a))
  data.table::set(dt, j = "side_b", value = data.table::fifelse(grepl("XXX", dt$side_b), "TBD", dt$side_b))

  # Create group ID based on country grouping preference
  if (group.by.country) {
    data.table::set(dt, j = "group.id", value = dt$country)
  } else {
    data.table::set(dt, j = "group.id", value = "all_countries")
  }

  # Remove duplicates based on the text-relevant fields before summarizing
  dt.unique <- unique(dt, by = c("group.id", "date_start", "type_of_violence", "side_a", "side_b", "best", "source_headline"))
  dt.unique <- dt.unique[order(date_start)]

  # Create text output per group
  out.ls <- dt.unique[, .(text = list(sprintf(
    "Event date: %s\nEvent Type: %s\nActor 1: %s\nActor 2: %s\nFatalities: %s (best estimate)\nDescription: %s.",
    date_start, type_of_violence, side_a, side_b, best, source_headline
  ))), by = group.id]

  # Convert to named list: one list entry per group.id
  output.list <- setNames(out.ls$text, out.ls$group.id)

  return(output.list)
}
