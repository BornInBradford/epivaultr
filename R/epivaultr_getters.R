
get_ev_projects <- function(x, ...) {
  
  UseMethod("get_ev_projects", x)
  
}


get_ev_projects.ev_variables <- function(ev_vars) {
  
  projects <- ev_vars$projects
  
  return(projects)
  
}



get_ev_tables <- function(x, ...) {
  
  UseMethod("get_ev_tables", x)
  
}


get_ev_tables.ev_variables <- function(ev_vars) {
  
  tables <- ev_vars$tables
  
  return(tables)
  
}



get_ev_vars_requested <- function(x, ...) {
  
  UseMethod("get_ev_vars_requested", x)
  
}


get_ev_vars_requested.ev_variables <- function(ev_vars) {
  
  vars <- ev_vars$vars_requested$variables
  
  return(vars)
  
}


get_ev_vars_df <- function(x, ...) {
  
  UseMethod("get_ev_vars_df", x)
  
}


get_ev_vars_df.ev_variables <- function(ev_vars) {
  
  vars_df <- ev_vars$vars_df
  
  return(vars_df)
  
}


get_ev_data <- function(x, ...) {
  
  UseMethod("get_ev_data", x)
  
}


get_ev_data.ev_data <- function(ev_data, df_name = character(0), df_index = integer(0)) {
  
  dat <- data.frame()
  
  if(length(df_name) == 1) dat <- ev_data$data[[df_name]]
  
  if(length(df_index) == 1) dat <- ev_data$data[[df_index]]
  
  return(dat)
  
}


get_ev_metadata <- function(x, ...) {
  
  UseMethod("get_ev_metadata", x)
  
}


get_ev_metadata.ev_data <- function(ev_data, type = character(0)) {
  
  dat <- data.frame()
  
  if(!type %in% c("variable", "category", "table")) warning(paste0("Metadata type not recognised: ", type))
  
  if(type == "variable") dat <- ev_data$metadata$variable
  
  if(type == "category") dat <- ev_data$metadata$category
  
  if(type == "table") dat <- ev_data$metadata$table
  
  return(dat)
  
}

