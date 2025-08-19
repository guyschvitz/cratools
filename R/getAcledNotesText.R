#' Extract conflict event descriptions from ACLED data
#'
#' This function extracts and formats narrative descriptions from ACLED data
#' for a given country and time period. It can optionally group results by country
#' and time unit (month, quarter, or year).
#'
#' @param acled.df A data.frame or data.table containing ACLED event data
#' @param ctry.id Vector of country identifiers (character)
#' @param sdate Start date (exclusive)
#' @param edate End date (exclusive)
#' @param use.regex Logical. Whether to interpret ctry.id as a regex pattern. Default is TRUE
#' @param group.by Optional. Time unit to group by: "month", "quarter", or "year". Default is NULL (no grouping)
#'
#' @return A named list of character strings. Each element is a formatted text block for one group.
#'
#' @examples
#' # Example with sample ACLED data
#' notes.ls <- getAcledNotesText(
#'   acled.df = acled.df,
#'   ctry.id = "Nigeria",
#'   sdate = as.Date("2025-05-01"),
#'   edate = as.Date("2025-08-01"),
#'   group.by = "month"
#' )
#'
#' cat(notes.ls[[1]])
#'
#' @export
getAcledNotesText <- function(acled.df,
                              ctry.id,
                              sdate,
                              edate,
                              use.regex = TRUE,
                              group.by = NULL) {
  dt <- data.table::as.data.table(acled.df)

  # Validate input
  if (!("country" %in% names(dt)) || !("event_date" %in% names(dt))) {
    stop("ACLED data must contain 'country' and 'event_date' columns")
  }

  # Coerce date column to Date if not already
  if (!inherits(dt$event_date, "Date")) {
    dt[, event_date := as.Date(event_date)]
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

  # Grouping
  if (!is.null(group.by)) {
    if (!is.null(group.by)) {
      if (group.by == "month") {
        dt[, time.grp := format(event_date, "%Y-%m")]
      } else if (group.by == "quarter") {
        dt[, time.grp := paste0(lubridate::year(event_date), " Q", lubridate::quarter(event_date))]
      } else if (group.by == "year") {
        dt[, time.grp := as.character(lubridate::year(event_date))]
      } else {
        stop("Invalid value for group.by. Use 'month', 'quarter', or 'year'.")
      }
    } else {
      dt[, time.grp := NA_character_]
    }
  } else {
    dt[, time.grp := NA_character_]
  }

  # Create group ID
  dt[, group.id := ifelse(is.na(time.grp), country, paste(country, time.grp, sep = " - "))]

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
