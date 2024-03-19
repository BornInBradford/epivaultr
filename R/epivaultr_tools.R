
ev_connect <- function(ev_server = getOption("ev_server"), 
                       ev_database = getOption("ev_database"),
                       ev_driver = dplyr::coalesce(getOption("ev_driver"), "SQL Server")) {
  
  if(!is.character(ev_server) | !is.character(ev_database)) stop("`ev_server` and `ev_database` must be passed either as function parameters or using `options(...)`")
  
  con <- DBI::dbConnect(odbc::odbc(), driver = ev_driver, server = ev_server, 
                        database = ev_database, Trusted_Connection = "True")
  
  return(con)
  
}

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
