#' Connect to Database
#'
#' Establishes a connection to a database using ODBC driver with provided
#' connection parameters.
#'
#' @param db.info.ls A named list containing database connection parameters.
#'   Must include: driver, server, database, username, password
#' @param drv Database driver object. Default is odbc::odbc()
#' @return A database connection object
#'
#' @importFrom DBI dbConnect
#' @export
#'
#' @examples
#' \dontrun{
#' # Create connection info list
#' db.config.ls <- list(
#'   driver = "SQL Server",
#'   server = "localhost",
#'   database = "mydb",
#'   username = "user",
#'   password = "pass"
#' )
#'
#' # Connect using default ODBC driver
#' con <- connect2Db(db.info.ls = db.config.ls)
#'
#' # Connect using specific driver
#' con <- connect2Db(
#'   db.info.ls = db.config.ls,
#'   drv = RPostgreSQL::PostgreSQL()
#' )
#' }
connect2Db <- function(db.info.ls, drv = odbc::odbc()) {

  # Validate required parameters
  required.fields <- c("driver", "server", "database", "username", "password")
  missing.fields <- setdiff(required.fields, names(db.info.ls))

  if (length(missing.fields) > 0) {
    stop("Missing required fields in db.info.ls: ", paste(missing.fields, collapse = ", "))
  }

  # Create database connection
  con <- DBI::dbConnect(
    drv = drv,
    Driver = db.info.ls$driver,
    Server = db.info.ls$server,
    Database = db.info.ls$database,
    UID = db.info.ls$username,
    PWD = db.info.ls$password,
    TrustServerCertificate = "yes"
  )

  return(con)
}
