#' getAcledData: Retrieve ACLED data from the API: https://api.acleddata.com/
#'
#' @param api.email character. ACLED user email account
#' @param api.key character: ACLED API key
#' @param start.date date: Desired start-date of data. Default: NULL
#' @param end.date date: Desired end-date of ACLED data. Default: NULL
#' @param n.months numeric: Number of months of acled data needed. Default: 24
#'
#' @returns A data.frame with ACLED data from the API
getAcledData <- function(api.email, api.key, start.date = NULL, end.date = NULL,
                         n.months = NULL){
  ## Load ACLED data
  if(is.null(end.date)){
    end.date <- Sys.Date()
  }

  if(!is.null(n.months)){
    if(!is.null(start.date)){
      warning("Both 'start.date' and 'n.months' are provided. Ignoring 'start.date'.")
    }
    start.date <- end.date - lubridate::period(n.months, units = "months")
  }

  ## Split query into smaller "chunks" to avoid hitting API bandwith limits
  acled.region.df <- acled.api::get.api.regions()[[1]]

  ## Loop over each row in ACLED region dataset and fetch data for each region
  acled.df <- lapply(1:nrow(acled.region.df), function(x){
    ## ... Subset region names and region codes
    region.nm <- acled.region.df$region[x]

    ## Print status message
    message(sprintf("Querying ACLED data for region %s, %s to %s",
                    region.nm, start.date, end.date))

    ## Main query
    acled.api::acled.api(email.address = api.email,
                         access.key = api.key,
                         ## Start of time window: Beginning of this year
                         start.date = as.character(start.date),
                         ## End: Most recent date (ideally today)
                         end.date = as.character(end.date),
                         ## Query region
                         region = region.nm,
                         ## download all vars
                         all.variables = T)
  }) |>
    ## ... Collect query result into single data.frame
    dplyr::bind_rows()
}
