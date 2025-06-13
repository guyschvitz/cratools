#' getDataTimeSpan: Get time span (start-date, end-date) for each variable 
#' with at least one non-missing value.
#' 
#' @param df data.frame
#' @param time.var character: Name of the "time" variable in the dataset (e.g. year)
#' @param vars character: Name(s) of variables for which time span is needed. If 
#' left empty, function returns time spans of all variables in the dataset
#'
#' @returns a data.frame with 3 columns: variable name, start-date, end-date
getDataTimeSpan <- function(df, time.var = "YEAR", vars = NULL){
  
  if(is.null(vars)){
    vars <- names(df)
  }
  
  range.df <- lapply(vars, function(x){
    rng <- range(df[[time.var]][!is.na(df[[x]])])
    data.frame(var = x,
               start = rng[1],
               end = rng[2])
  }) |> dplyr::bind_rows()
  
  return(range.df)
}


