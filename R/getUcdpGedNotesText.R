#' Extract conflict event descriptions from UCDP GED data
#'
#' This function extracts and formats event narratives from UCDP GED data
#' for a specified country and time range, optionally grouped by time period.
#'
#' @param ged.df A data.frame or data.table containing GED data
#' @param ctry.id Vector of country identifiers (character)
#' @param sdate Start date (exclusive)
#' @param edate End date (exclusive)
#' @param use.regex Logical. Whether to interpret ctry.id as a regex pattern. Default is TRUE
#' @param group.by Optional. Time unit to group by: "month", "quarter", or "year". Default is NULL (no grouping)
#'
#' @return A named list of character strings. Each element is a formatted text block for one group.
#'
#' @examples
#' # Example with sample GED data
#' \dontrun{
#' notes.ls <- getUcdpGedNotesText(
#'   ged.df = ged.df,
#'   ctry.id = "Nigeria",
#'   sdate = as.Date("2025-05-01"),
#'   edate = as.Date("2025-08-01"),
#'   group.by = "month"
#' )
#' }
#'
#' @export
getUcdpGedNotesText <- function(ged.df,
                                ctry.id,
                                sdate,
                                edate,
                                use.regex = TRUE,
                                group.by = NULL) {
  dt <- data.table::as.data.table(ged.df)

  # Validate input
  if (!("country" %in% names(dt)) || !("date_start" %in% names(dt))) {
    stop("GED data must contain 'country' and 'date_start' columns")
  }

  # Coerce date column to Date if not already
  if (!inherits(dt$date_start, "Date")) {
    dt[, date_start := as.Date(date_start)]
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
  dt[, type_of_violence := factor(
    type_of_violence,
    levels = 1:3,
    labels = c("State-based conflict", "Non-state conflict", "One-sided violence")
  )]

  # Replace unknown actor names based on "XXX" pattern
  dt[, side_a := data.table::fifelse(grepl("XXX", side_a), "TBD", side_a)]
  dt[, side_b := data.table::fifelse(grepl("XXX", side_b), "TBD", side_b)]

  # Grouping
  if (!is.null(group.by)) {
    if (!is.null(group.by)) {
      if (group.by == "month") {
        dt[, time.grp := format(date_start, "%Y-%m")]
      } else if (group.by == "quarter") {
        dt[, time.grp := paste0(lubridate::year(date_start), " Q", lubridate::quarter(date_start))]
      } else if (group.by == "year") {
        dt[, time.grp := as.character(lubridate::year(date_start))]
      } else {
        stop("Invalid value for group.by. Use 'month', 'quarter', or 'year'.")
      }
    } else {
      dt[, time.grp := NA_character_]
    }
  } else {
    dt[, time.grp := NA_character_]
  }

  dt[, group.id := ifelse(is.na(time.grp), country, paste(country, time.grp, sep = " - "))]

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
