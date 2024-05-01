
#' Connect to EpiVault
#' 
#' Connects to a specified EpiVault instance and returns the connection for use in other `epivaultr` functions. `ev_server` and `ev_database` are required and can either be passed as function parameters or can be placed in global options (see examples). If passed as function parameters, this will override the global options.
#'
#' @param ev_server Name of SQL Server instance
#' @param ev_database Name of EpiVault database
#' @param ev_driver Name of SQL Server drive (default: "SQL Server")
#'
#' @return An EpiVault connection object
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # using global options
#' 
#' options(ev_server = "BHTS-RESEARC22A")
#' options(ev_database = "EpiVault1")
#' 
#' con <- ev_connect()
#' 
#' # using parameters - global options get overridden
#' 
#' con <- ev_connect(ev_server = "BHTS-RESRCH22DV", ev_database = "EpiVaultDV")
#' 
#' }
ev_connect <- function(ev_server = getOption("ev_server"), 
                       ev_database = getOption("ev_database"),
                       ev_driver = dplyr::coalesce(getOption("ev_driver"), "SQL Server")) {
  
  if(!is.character(ev_server) | !is.character(ev_database)) stop("You must specify `ev_server` and `ev_database` using `options(...)`")
  
  con <- DBI::dbConnect(odbc::odbc(), driver = ev_driver, server = ev_server, 
                        database = ev_database, Trusted_Connection = "True")
  
  return(con)
  
}

#' Disconnect from EpiVault
#' 
#' Disconnects from EpiVault instance identified by `con`
#'
#' @param con EpiVault connection
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' con <- ev_connect()
#' 
#' ev_disconnect(con)
#' 
#' }
ev_disconnect <- function(con) {
  
  DBI::dbDisconnect(con)
  
}


ev_message <- function(..., file = NULL) {
  
  message <- paste0(Sys.time(), " - ", ...)
  
  message(message)
  
}


ev_var_headers <- function() {
  
  var_headers <- c("variable",
                   "var",
                   "variables",
                   "vars",
                   "n",
                   "name",
                   "names",
                   "project.table.variable",
                   "project_name.table_name.variable_name")
  
  return(var_headers)
  
}


ev_db_columns <- function() {
  
  db_cols <- c("ev_row_index")
  
  return(db_cols)
  
}



