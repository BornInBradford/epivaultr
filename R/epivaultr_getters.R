
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
  
  UseMethod("get_bb_vars_requested", x)
  
}


get_ev_vars_requested.ev_variables <- function(ev_vars) {
  
  vars <- ev_vars$vars_requested$variables
  
  return(vars)
  
}


get_ev_vars_df <- function(x, ...) {
  
  UseMethod("get_bb_vars_df", x)
  
}


get_ev_vars_df.ev_variables <- function(ev_vars) {
  
  vars_df <- ev_vars$vars_df
  
  return(vars_df)
  
}
