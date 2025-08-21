#' Extract conflict event descriptions from ACLED data
#'
#' This function extracts and formats narrative descriptions from ACLED data
#' for a given country and time period. Can optionally group results by country
#' when multiple countries are provided.
#'
#' @param acled.df A data.frame or data.table containing ACLED event data
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
#' notes.ls <- getAcledNotesText(
#'   acled.df = acled.df,
#'   ctry.id = "Nigeria",
#'   sdate = as.Date("2025-05-01"),
#'   edate = as.Date("2025-08-01")
#' )
#' }
#'
#' @importFrom data.table as.data.table set
#' @export
getAcledNotesText <- function(acled.df,
                              ctry.id,
                              sdate,
                              edate,
                              use.regex = TRUE,
                              group.by.country = TRUE) {

  # Ensure data.table is loaded
  requireNamespace("data.table", quietly = TRUE)

  dt <- data.table::as.data.table(acled.df)

  # Validate input
  if (!("country" %in% names(dt)) || !("event_date" %in% names(dt))) {
    stop("ACLED data must contain 'country' and 'event_date' columns")
  }

  # Coerce date column to Date if not already
  if (!inherits(dt$event_date, "Date")) {
    data.table::set(dt, j = "event_date", value = as.Date(dt$event_date))
  }

  # Filter by date
  dt <- dt[event_date > sdate & event_date < edate]

  # Filter by country
  if (use.regex) {
    dt <- dt[grepl(paste(ctry.id, collapse = "|"), country, ignore.case = TRUE)]
  } else {
    dt <- dt[country %in% ctry.id]
  }

  if (nrow(dt) == 0) {
    return(list("No events recorded in this period."))
  }

  # Create group ID based on country grouping preference
  if (group.by.country) {
    data.table::set(dt, j = "group.id", value = dt$country)
  } else {
    data.table::set(dt, j = "group.id", value = "all_countries")
  }

  # Remove duplicates based on the text-relevant fields before summarizing
  dt.unique <- unique(dt, by = c("group.id", "event_date", "event_type", "actor1", "actor2", "fatalities", "notes"))
  dt.unique <- dt.unique[order(event_date)]

  # Create text output per group
  out.ls <- dt.unique[, .(text = list(sprintf(
    "Event date: %s\nEvent Type: %s\nActor 1: %s\nActor 2: %s\nFatalities: %s\nDescription: %s.",
    event_date, event_type, actor1, actor2, fatalities, notes
  ))), by = group.id]

  # Convert to named list: one list entry per group.id
  output.list <- setNames(out.ls$text, out.ls$group.id)

  return(output.list)
}

