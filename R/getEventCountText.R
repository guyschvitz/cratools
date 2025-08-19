#' Analyze Conflict Event Trends
#'
#' Computes recent change (baseline -> target, normalized by interval) and long-term trend
#' (slope over a specified window). Returns either a structured data.frame or a single
#' text block summarizing results per country.
#'
#' @param event.df data.frame. Conflict event data.
#' @param event.date.col character. Column name with event dates.
#' @param ctry.id.col character. Column name with country identifiers.
#' @param ctry.id character vector. Country identifier(s) to filter by.
#' @param event.type.col character. Column name with event type.
#' @param target.start.date Date or YYYY-MM-DD. Start of target period.
#' @param target.end.date Date or YYYY-MM-DD. End of target period.
#' @param baseline.start.date Date or YYYY-MM-DD. Start of baseline period.
#' @param baseline.end.date Date or YYYY-MM-DD. End of baseline period.
#' @param trend.period.months integer. Number of months for the trend window. Default 12.
#' @param trend.end.date Date or YYYY-MM-DD or NULL. End of trend window. Default = target.end.date.
#' @param use.ctry.regex logical. If TRUE, regex-match countries; else exact match. Default TRUE.
#' @param by.event.type logical. If TRUE, compute by event type; else totals. Default TRUE.
#' @param aggregate.by character. One of "month","quarter","year". Default "month".
#' @param output.as.text logical. If TRUE (default) return single character string; else data.frame.
#'
#' @return If output.as.text = FALSE, a data.frame with:
#'   country, (event_type), baseline_start/end, target_start/end, trend_start/end,
#'   baseline_count, target_count, baseline_intervals, target_intervals,
#'   baseline_avg, target_avg, pct_change, pct_change_label,
#'   trend_slope, trend_label, and the two interpretation columns.
#'   If output.as.text = TRUE, a single character string with one block per country.
#'
#' @examples
#' \dontrun{
#' # Example with ACLED data
#' result <- getEventTrendText(
#'   event.df = acled.df,
#'   event.date.col = "event_date",
#'   ctry.id.col = "country",
#'   ctry.id = "Nigeria",
#'   event.type.col = "event_type",
#'   target.start.date = "2023-07-01",
#'   target.end.date = "2023-12-31",
#'   baseline.start.date = "2023-01-01",
#'   baseline.end.date = "2023-06-30"
#' )
#' }
#'
#' @import dplyr
#' @import glue
#' @import lubridate
#' @import stringr
#' @import purrr
#' @import rlang
#' @export
getEventTrendText <- function(event.df, event.date.col, ctry.id.col, ctry.id, event.type.col,
                              target.start.date, target.end.date,
                              baseline.start.date, baseline.end.date,
                              trend.period.months = 12, trend.end.date = NULL,
                              use.ctry.regex = TRUE, by.event.type = TRUE,
                              aggregate.by = "month", output.as.text = TRUE) {
  
  # Input validation 
  if (!is.data.frame(event.df)) {
    stop("'event.df' must be a data.frame")
  }
  if (nrow(event.df) == 0) {
    stop("'event.df' is empty")
  }
  
  required.cols <- c(event.date.col, ctry.id.col, event.type.col)
  missing.cols <- setdiff(required.cols, names(event.df))
  if (length(missing.cols) > 0) {
    stop(glue::glue("Missing required columns: {paste(missing.cols, collapse = ', ')}"))
  }
  
  if (!aggregate.by %in% c("month", "quarter", "year")) {
    stop("'aggregate.by' must be one of: 'month', 'quarter', 'year'")
  }
  
  if (length(ctry.id) == 0 || any(is.na(ctry.id)) || any(ctry.id == "")) {
    stop("'ctry.id' cannot be empty, NA, or contain empty strings")
  }
  
  if (!is.numeric(trend.period.months) || trend.period.months <= 0) {
    stop("'trend.period.months' must be positive")
  }
  
  # Coerce date columns and validate
  date.params <- list(
    target.start.date = target.start.date,
    target.end.date = target.end.date,
    baseline.start.date = baseline.start.date,
    baseline.end.date = baseline.end.date,
    trend.end.date = trend.end.date
  )
  
  for (param.name in names(date.params)) {
    if (!is.null(date.params[[param.name]])) {
      tryCatch({
        assign(param.name, as.Date(date.params[[param.name]]), envir = environment())
      }, error = function(e) {
        stop(glue::glue("Invalid '{param.name}' format. Use YYYY-MM-DD format."))
      })
    }
  }
  
  if (is.null(trend.end.date)) {
    trend.end.date <- target.end.date
  }
  
  if (target.end.date < target.start.date) {
    stop("'target.end.date' cannot be earlier than 'target.start.date'")
  }
  if (baseline.end.date < baseline.start.date) {
    stop("'baseline.end.date' cannot be earlier than 'baseline.start.date'")
  }
  
  trend.start.date <- trend.end.date - lubridate::period(trend.period.months, "months")
  
  # Coerce event date column
  if (!inherits(event.df[[event.date.col]], "Date")) {
    event.df[[event.date.col]] <- tryCatch({
      as.Date(event.df[[event.date.col]])
    }, error = function(e) {
      stop(glue::glue("Cannot convert '{event.date.col}' to Date format."))
    })
  }
  
  # Filter by countries 
  if (use.ctry.regex) {
    regex.pattern <- paste(ctry.id, collapse = "|")
    event.sub.df <- tryCatch({
      event.df |>
        dplyr::filter(grepl(regex.pattern, !!rlang::sym(ctry.id.col), ignore.case = TRUE))
    }, error = function(e) {
      stop(glue::glue("Error in regex filtering: {e$message}"))
    })
  } else {
    event.sub.df <- event.df |>
      dplyr::filter(!!rlang::sym(ctry.id.col) %in% ctry.id)
  }
  
  if (nrow(event.sub.df) == 0) {
    warning(glue::glue("No events found for country identifier(s): {paste(sQuote(ctry.id), collapse = ', ')}"))
    return(if (output.as.text) "" else data.frame())
  }
  
  # Helper functions 
  countEventsInPeriod <- function(df, start.date, end.date) {
    period.df <- df |>
      dplyr::filter(!!rlang::sym(event.date.col) >= start.date & 
                      !!rlang::sym(event.date.col) <= end.date)
    
    if (nrow(period.df) == 0) {
      if (by.event.type) {
        return(data.frame(country = character(0), event_type = character(0), count = numeric(0)))
      }
      return(data.frame(country = character(0), count = numeric(0)))
    }
    
    if (by.event.type) {
      period.df |>
        dplyr::group_by(!!rlang::sym(ctry.id.col), !!rlang::sym(event.type.col)) |>
        dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
        dplyr::rename(country = !!rlang::sym(ctry.id.col), event_type = !!rlang::sym(event.type.col))
    } else {
      period.df |>
        dplyr::group_by(!!rlang::sym(ctry.id.col)) |>
        dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
        dplyr::rename(country = !!rlang::sym(ctry.id.col))
    }
  }
  
  nIntervals <- function(start.date, end.date, unit) {
    s <- lubridate::floor_date(as.Date(start.date), unit)
    e <- lubridate::floor_date(as.Date(end.date), unit)
    if (e < s) return(0L)
    length(seq(from = s, to = e, by = unit))
  }
  
  labelTrendFromSlope <- function(slope) {
    if (is.na(slope)) return("Insufficient data")
    abs.slope <- abs(slope)
    if (abs.slope < 0.1) return("Stable")
    if (abs.slope < 0.5) return(if (slope > 0) "Minor increase" else "Minor decrease")
    if (abs.slope < 1.5) return(if (slope > 0) "Moderate increase" else "Moderate decrease")
    return(if (slope > 0) "Major increase" else "Major decrease")
  }
  
  labelChangeFromPct <- function(pct) {
    if (is.infinite(pct)) return("Major increase")
    abs.pct <- abs(pct)
    if (abs.pct < 5) return("No change")
    if (abs.pct < 20) return(if (pct > 0) "Minor increase" else "Minor decrease")
    if (abs.pct < 50) return(if (pct > 0) "Moderate increase" else "Moderate decrease")
    return(if (pct > 0) "Major increase" else "Major decrease")
  }
  
  # Calculate baseline & target counts
  target.counts.df <- countEventsInPeriod(event.sub.df, target.start.date, target.end.date)
  baseline.counts.df <- countEventsInPeriod(event.sub.df, baseline.start.date, baseline.end.date)
  
  # Trend analysis (for slope calculation)
  trend.df <- event.sub.df |>
    dplyr::filter(!!rlang::sym(event.date.col) >= trend.start.date & 
                    !!rlang::sym(event.date.col) <= trend.end.date)
  
  if (nrow(trend.df) == 0) {
    warning("No events found in the trend analysis period")
    return(if (output.as.text) "" else data.frame())
  }
  
  trend.df <- trend.df |>
    dplyr::mutate(time_period = dplyr::case_when(
      aggregate.by == "month" ~ lubridate::floor_date(!!rlang::sym(event.date.col), "month"),
      aggregate.by == "quarter" ~ lubridate::floor_date(!!rlang::sym(event.date.col), "quarter"),
      aggregate.by == "year" ~ lubridate::floor_date(!!rlang::sym(event.date.col), "year")
    ))
  
  if (by.event.type) {
    trend.agg.df <- trend.df |>
      dplyr::group_by(!!rlang::sym(ctry.id.col), !!rlang::sym(event.type.col), time_period) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::rename(country = !!rlang::sym(ctry.id.col), event_type = !!rlang::sym(event.type.col))
    group.vars <- c("country", "event_type")
  } else {
    trend.agg.df <- trend.df |>
      dplyr::group_by(!!rlang::sym(ctry.id.col), time_period) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::rename(country = !!rlang::sym(ctry.id.col))
    group.vars <- "country"
  }
  
  trend.results.df <- trend.agg.df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group.vars))) |>
    dplyr::arrange(time_period) |>
    dplyr::mutate(time_index = dplyr::row_number()) |>
    dplyr::summarise(
      trend_slope = if (dplyr::n() >= 3) {
        tryCatch(stats::coef(stats::lm(count ~ time_index))[2], error = function(e) NA_real_)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
  
  # Merge all components
  if (by.event.type) {
    all.combinations.df <- base::expand.grid(
      country = unique(event.sub.df[[ctry.id.col]]),
      event_type = unique(event.sub.df[[event.type.col]]),
      stringsAsFactors = FALSE
    )
    results.df <- all.combinations.df |>
      dplyr::left_join(target.counts.df, by = c("country", "event_type")) |>
      dplyr::rename(target_count = count) |>
      dplyr::left_join(baseline.counts.df, by = c("country", "event_type")) |>
      dplyr::rename(baseline_count = count) |>
      dplyr::left_join(trend.results.df, by = c("country", "event_type"))
  } else {
    all.combinations.df <- data.frame(country = unique(event.sub.df[[ctry.id.col]]))
    results.df <- all.combinations.df |>
      dplyr::left_join(target.counts.df, by = "country") |>
      dplyr::rename(target_count = count) |>
      dplyr::left_join(baseline.counts.df, by = "country") |>
      dplyr::rename(baseline_count = count) |>
      dplyr::left_join(trend.results.df, by = "country")
  }
  
  # Replace missing counts with 0
  results.df$target_count[is.na(results.df$target_count)] <- 0
  results.df$baseline_count[is.na(results.df$baseline_count)] <- 0
  
  # Add period metadata columns
  results.df <- results.df |>
    dplyr::mutate(
      baseline_start = baseline.start.date,
      baseline_end = baseline.end.date,
      target_start = target.start.date,
      target_end = target.end.date,
      trend_start = trend.start.date,
      trend_end = trend.end.date
    )
  
  # Calculate interval-normalized percentage change
  results.df <- results.df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      interval_unit = aggregate.by,
      baseline_intervals = nIntervals(baseline_start, baseline_end, aggregate.by),
      target_intervals = nIntervals(target_start, target_end, aggregate.by),
      baseline_avg = ifelse(baseline_intervals > 0, baseline_count / baseline_intervals, NA_real_),
      target_avg = ifelse(target_intervals > 0, target_count / target_intervals, NA_real_)
    ) |>
    dplyr::ungroup()
  
  results.df <- results.df |>
    dplyr::mutate(
      pct_change = dplyr::case_when(
        is.na(baseline_avg) | is.na(target_avg) ~ NA_real_,
        baseline_avg == 0 & target_avg == 0 ~ 0,
        baseline_avg == 0 & target_avg > 0 ~ Inf,
        TRUE ~ (target_avg - baseline_avg) / baseline_avg * 100
      ),
      pct_change_label = vapply(pct_change, labelChangeFromPct, character(1)),
      trend_label = vapply(trend_slope, labelTrendFromSlope, character(1)),
      interpretation_trend = dplyr::case_when(
        trend_label == "Insufficient data" ~ "Long-term trend — Insufficient data.",
        TRUE ~ glue::glue(
          "Long-term trend — {trend_label} (slope {ifelse(is.na(trend_slope), 'NA', sprintf('%+.2f', trend_slope))} events/period; {trend_start} – {trend_end})."
        )
      ),
      interpretation_pct = dplyr::case_when(
        is.na(pct_change) ~ "Recent change — Insufficient data to compute normalized change.",
        is.infinite(pct_change) ~ glue::glue(
          "Recent change — Major increase from baseline ({baseline_start} – {baseline_end}) to target ({target_start} – {target_end}): from 0 to {round(target_avg, 1)} events per {interval_unit}."
        ),
        TRUE ~ glue::glue(
          "Recent change — {pct_change_label} from baseline ({baseline_start} – {baseline_end}) to target ({target_start} – {target_end}): {sprintf('%+.1f', pct_change)}% (baseline {round(baseline_avg, 1)} → target {round(target_avg, 1)} per {interval_unit})."
        )
      )
    )
  
  # Round for readability
  results.df$pct_change <- round(results.df$pct_change, 2)
  results.df$trend_slope <- round(results.df$trend_slope, 4)
  
  if (!output.as.text) {
    return(results.df)
  }
  
  # Generate text output: single block per country
  country.names <- unique(results.df$country)
  
  blocks <- purrr::map_chr(country.names, function(country) {
    country.results <- results.df |>
      dplyr::filter(country == !!country)
    
    header <- glue::glue(
      "Country: {country}\n",
      "Overall period: {country.results$trend_start[1]} – {country.results$trend_end[1]}\n",
      "Baseline period: {country.results$baseline_start[1]} – {country.results$baseline_end[1]}\n",
      "Target period: {country.results$target_start[1]} – {country.results$target_end[1]}"
    )
    
    # Generate body lines
    if (by.event.type && "event_type" %in% names(country.results)) {
      body.lines <- apply(country.results, 1, function(row) {
        glue::glue("{row[['event_type']]}:\n  {row[['interpretation_trend']]}\n  {row[['interpretation_pct']]}")
      })
    } else {
      body.lines <- apply(country.results, 1, function(row) {
        glue::glue("All events:\n  {row[['interpretation_trend']]}\n  {row[['interpretation_pct']]}")
      })
    }
    
    glue::glue("{header}\n\n{paste(body.lines, collapse = '\n\n')}")
  })
  
  # Return single string with blocks separated by blank lines
  return(paste(blocks, collapse = "\n\n"))
}


#' Process Conflict Event Data for LLM Analysis
#'
#' This function preprocesses conflict event datasets (e.g., ACLED, UCDP) by filtering,
#' aggregating, and formatting the data into either structured tables or human-readable
#' text summaries suitable for Large Language Model (LLM) processing.
#'
#' @param event.df data.frame. Conflict event data.
#' @param start.date Date or YYYY-MM-DD. Start date for filtering events.
#' @param end.date Date, YYYY-MM-DD, or NULL. End date for filtering events.
#'   If NULL, all events from start.date onwards are included.
#' @param event.date.col character. Column name containing event dates.
#' @param ctry.id.col character. Column name containing country identifiers.
#' @param ctry.id character vector. Country identifier(s) to filter by.
#' @param event.type.col character. Column name containing event type information.
#' @param use.ctry.regex logical. If TRUE, uses regex matching for country filtering;
#'   if FALSE, uses exact matching. Default TRUE.
#' @param by.event.type logical. If TRUE, results include event counts by event type.
#'   If FALSE, only return total event counts (across all event types). Default TRUE.
#' @param output.as.text logical. If TRUE (default), returns formatted text summaries.
#'   If FALSE, returns the aggregated data.frame.
#'
#' @return 
#' If `output.as.text = TRUE`, a named character vector where each element contains a 
#' formatted text summary of event counts by type and country for the specified time period.  
#' If `output.as.text = FALSE`, a tidy data.frame with event counts by country, type, and date range.
#'
#' @examples
#' \dontrun{
#' # Example with ACLED data
#' result <- getEventCountText(
#'   event.df = acled.df,
#'   start.date = "2023-01-01",
#'   end.date = "2023-12-31",
#'   event.date.col = "event_date",
#'   ctry.id.col = "country",
#'   ctry.id = "Nigeria",
#'   event.type.col = "event_type"
#' )
#' }
#'
#' @import dplyr
#' @import glue
#' @import purrr
#' @import rlang
#' @export
getEventCountText <- function(event.df, start.date, end.date = NULL, 
                              event.date.col, ctry.id.col, ctry.id,
                              event.type.col, use.ctry.regex = TRUE,
                              by.event.type = TRUE, output.as.text = TRUE) {
  
  # Input validation 
  if (!is.data.frame(event.df)) {
    stop("'event.df' must be a data.frame")
  }
  if (nrow(event.df) == 0) {
    stop("'event.df' is empty")
  }
  
  required.cols <- c(event.date.col, ctry.id.col, event.type.col)
  missing.cols <- setdiff(required.cols, names(event.df))
  if (length(missing.cols) > 0) {
    stop(glue::glue("Missing required columns: {paste(missing.cols, collapse = ', ')}"))
  }
  
  if (length(ctry.id) == 0 || any(is.na(ctry.id)) || any(ctry.id == "")) {
    stop("'ctry.id' cannot be empty, NA, or contain empty strings")
  }
  
  # Date validation and coercion
  start.date <- tryCatch({
    as.Date(start.date)
  }, error = function(e) {
    stop(glue::glue("Invalid 'start.date' format: {start.date}. Use YYYY-MM-DD format."))
  })
  
  if (!is.null(end.date)) {
    end.date <- tryCatch({
      as.Date(end.date)
    }, error = function(e) {
      stop(glue::glue("Invalid 'end.date' format: {end.date}. Use YYYY-MM-DD format."))
    })
    if (end.date < start.date) {
      stop("'end.date' cannot be earlier than 'start.date'")
    }
  }
  
  # Coerce event date column
  if (!inherits(event.df[[event.date.col]], "Date")) {
    event.df[[event.date.col]] <- tryCatch({
      as.Date(event.df[[event.date.col]])
    }, error = function(e) {
      stop(glue::glue("Cannot convert '{event.date.col}' to Date format."))
    })
  }
  
  # Filter by countries
  if (use.ctry.regex) {
    regex.pattern <- paste(ctry.id, collapse = "|")
    event.sub.df <- tryCatch({
      event.df |>
        dplyr::filter(grepl(regex.pattern, !!rlang::sym(ctry.id.col), ignore.case = TRUE))
    }, error = function(e) {
      stop(glue::glue("Error in regex filtering: {e$message}"))
    })
  } else {
    event.sub.df <- event.df |>
      dplyr::filter(!!rlang::sym(ctry.id.col) %in% ctry.id)
  }
  
  if (nrow(event.sub.df) == 0) {
    warning(glue::glue("No events found for country identifier(s): {paste(sQuote(ctry.id), collapse = ', ')}"))
    return(if (output.as.text) character(0) else data.frame())
  }
  
  # Filter by date range
  if (is.null(end.date)) {
    event.sub.df <- event.sub.df |>
      dplyr::filter(!!rlang::sym(event.date.col) >= start.date)
  } else {
    event.sub.df <- event.sub.df |>
      dplyr::filter(!!rlang::sym(event.date.col) >= start.date & 
                      !!rlang::sym(event.date.col) <= end.date)
  }
  
  if (nrow(event.sub.df) == 0) {
    return(if (output.as.text) character(0) else data.frame())
  }
  
  # Calculate date ranges for each country
  date.ranges.df <- event.sub.df |>
    dplyr::group_by(!!rlang::sym(ctry.id.col)) |>
    dplyr::summarise(
      start_date = min(!!rlang::sym(event.date.col), na.rm = TRUE),
      end_date = max(!!rlang::sym(event.date.col), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(country = !!rlang::sym(ctry.id.col))
  
  # Aggregate by event type
  event.counts.df <- event.sub.df |>
    dplyr::group_by(!!rlang::sym(ctry.id.col), !!rlang::sym(event.type.col)) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::rename(country = !!rlang::sym(ctry.id.col), event_type = !!rlang::sym(event.type.col))
  
  # Calculate total counts
  total.counts.df <- event.counts.df |>
    dplyr::group_by(country) |>
    dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(event_type = "Total events")
  
  # Combine results based on by.event.type parameter
  if (by.event.type) {
    combined.counts.df <- dplyr::bind_rows(total.counts.df, event.counts.df)
  } else {
    combined.counts.df <- total.counts.df
  }
  
  # Add date ranges and create event text
  results.df <- combined.counts.df |>
    dplyr::left_join(date.ranges.df, by = "country") |>
    dplyr::mutate(event_text = paste(event_type, n, sep = ": ")) |>
    dplyr::arrange(country, 
                   event_type != "Total events", 
                   event_type)
  
  # Return data.frame if requested
  if (!output.as.text) {
    return(results.df)
  }
  
  # Generate text output
  country.names <- unique(results.df$country)
  
  result <- purrr::map_chr(country.names, function(country) {
    country.data <- results.df |>
      dplyr::filter(country == !!country)
    
    start_date <- unique(country.data$start_date)[1]
    end_date <- unique(country.data$end_date)[1]
    
    header <- glue::glue("Country: {country}")
    period <- glue::glue("Period:  {start_date} - {end_date}")
    body <- paste(country.data$event_text, collapse = "\n")
    
    glue::glue("{header}\n{period}\n{body}")
  })
  
  names(result) <- country.names
  return(result)
}