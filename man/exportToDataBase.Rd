% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportToDataBase.R
\name{exportToDataBase}
\alias{exportToDataBase}
\title{Export data.frame to SQL database}
\usage{
exportToDataBase(
  con,
  input.df,
  export.cols = NULL,
  schema,
  table,
  create.if.missing = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{con}{A valid DBI database connection object}

\item{input.df}{A data.frame to be exported to the database}

\item{export.cols}{A character vector of column names to export. If NULL, all columns are exported}

\item{schema}{A character string specifying the database schema name}

\item{table}{A character string specifying the database table name}

\item{create.if.missing}{Logical. If TRUE, creates the table if it doesn't exist. If FALSE, stops with error. Default is FALSE}

\item{overwrite}{Logical. If TRUE, overwrite the existing database table. If FALSE, append rows of 'input.df' to the existing table.}
}
\value{
A data.frame containing metadata on the export operation including schema, table, date, number of rows exported, and success status
}
\description{
This function exports a data.frame to a specified SQL Server database table.
It includes validation checks for the database connection, table existence,
and column availability before performing the export operation.
}
\examples{
\dontrun{
exportToDataBase(con = con,
                 input.df = dcrm.6m.pred.df,
                 export.cols = NULL,
                 schema = "dcrm",
                 table = "ResultsTemp")
}
}
