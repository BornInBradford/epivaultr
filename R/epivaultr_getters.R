
#' @export
get_ev_projects <- function(x, ...) {
  
  UseMethod("get_ev_projects", x)
  
}


#' @export
get_ev_projects.ev_variables <- function(ev_vars) {
  
  projects <- ev_vars$projects
  
  return(projects)
  
}


#' @export
get_ev_tables <- function(x, ...) {
  
  UseMethod("get_ev_tables", x)
  
}


#' @export
get_ev_tables.ev_variables <- function(ev_vars) {
  
  tables <- ev_vars$tables
  
  return(tables)
  
}


#' @export
get_ev_variables <- function(x, ...) {
  
  UseMethod("get_ev_variables", x)
  
}


#' @export
get_ev_variables.ev_variables <- function(ev_vars) {
  
  vars <- ev_vars$variables
  
  return(vars)
  
}


#' @export
get_ev_vars_df <- function(x, ...) {
  
  UseMethod("get_ev_vars_df", x)
  
}


#' @export
get_ev_vars_df.ev_variables <- function(ev_vars) {
  
  vars_df <- ev_vars$vars_df
  
  return(vars_df)
  
}


#' @export
get_ev_data <- function(x, ...) {
  
  UseMethod("get_ev_data", x)
  
}


#' @export
get_ev_data.ev_data <- function(ev_data, df_name = character(0), df_index = integer(0)) {
  
  dat <- data.frame()
  
  if(length(df_name) == 1) dat <- ev_data$data[[df_name]]
  
  if(length(df_index) == 1) dat <- ev_data$data[[df_index]]
  
  return(dat)
  
}


get_ev_data_names <- function(x, ...) {
  
  UseMethod("get_ev_data_names", x)
  
}


#' @export
get_ev_data_names.ev_data <- function(ev_data) {
  
  data_names <- names(ev_data$data)
  
  return(data_names)
  
}


#' @export
get_ev_metadata <- function(x, ...) {
  
  UseMethod("get_ev_metadata", x)
  
}


#' @export
get_ev_metadata.ev_data <- function(ev_data, type = character(0)) {
  
  dat <- data.frame()
  
  if(!type %in% c("variable", "category", "table")) warning(paste0("Metadata type not recognised: ", type))
  
  if(type == "variable") dat <- ev_data$metadata$variable
  
  if(type == "category") dat <- ev_data$metadata$category
  
  if(type == "table") dat <- ev_data$metadata$table
  
  return(dat)
  
}

