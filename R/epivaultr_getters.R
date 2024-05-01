
#' Get project names
#' 
#' Returns project names from an `ev_variables` container. An `ev_variables` container can be created using `read_ev_variables`.
#'
#' @param ev_vars An `ev_variables` container
#'
#' @return A character vector of project names
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' vars <- read_ev_variables("path-to-variables-file")
#' 
#' get_ev_projects(vars)
#' 
#' }
#' 
get_ev_projects <- function(ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  projects <- ev_vars$projects
  
  return(projects)
  
}


get_ev_tables <- function(ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  tables <- ev_vars$tables
  
  return(tables)
  
  
}


get_ev_variables <- function(ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  vars <- ev_vars$variables
  
  return(vars)
  
}


get_ev_vars_df <- function(ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  vars_df <- ev_vars$vars_df
  
  return(vars_df)
  
}


get_ev_data <- function(ev_data, df_name = character(0), df_index = integer(0)) {
  
  if(class(ev_data) != "ev_data") stop("`ev_data` must be of class `ev_data` e.g. created using the `fetch_ev_data` function")
  
  dat <- data.frame()
  
  if(length(df_name) == 1) dat <- ev_data$data[[df_name]]
  
  if(length(df_index) == 1) dat <- ev_data$data[[df_index]]
  
  return(dat)
  
}


get_ev_data_names <- function(ev_data) {
  
  if(class(ev_data) != "ev_data") stop("`ev_data` must be of class `ev_data` e.g. created using the `fetch_ev_data` function")
  
  data_names <- names(ev_data$data)
  
  return(data_names)
  
}


get_ev_metadata <- function(ev_data, type = character(0)) {
  
  if(class(ev_data) != "ev_data") stop("`ev_data` must be of class `ev_data` e.g. created using the `fetch_ev_data` function")
  
  dat <- data.frame()
  
  if(!type %in% c("variable", "category", "table")) warning(paste0("Metadata type not recognised: ", type))
  
  if(type == "variable") dat <- ev_data$metadata$variable
  
  if(type == "category") dat <- ev_data$metadata$category
  
  if(type == "table") dat <- ev_data$metadata$table
  
  return(dat)
  
}

