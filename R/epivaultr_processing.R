
ev_read_variables <- function(file) {
  
  vars <- character(0)
  
  ext <- tools::file_ext(file)
  
  if(ext %in% c("txt", "csv", "tsv")) {
    
    vars_tab <- tryCatch(vroom::vroom(file, show_col_types = FALSE, col_names = FALSE), 
                         error = function(e) read.table(file))
    
  } else { # treat as excel
    
    vars_tab <- readxl::read_excel(file, col_names = FALSE)
    
  }
  
  if(length(vars_tab) == 1) vars <- trimws(vars_tab[[1]])
  
  if(length(vars_tab) == 3) vars <- paste0(trimws(vars_tab[[1]]), ".", trimws(vars_tab[[2]]), ".", trimws(vars_tab[[3]]))
  
  # remove any that look like a header
  vars <- vars[!vars %in% ev_var_headers()]
  
  if(length(vars) == 0) stop("Variables not found or format of input file cannot be determined")
    
  vars_df <- data.frame(varfullname = vars)
  vars_df <- tidyr::separate(vars_df, varfullname, c("project", "table", "variable"), sep = "\\.")
  vars_df <- dplyr::mutate(vars_df, proj_table = paste(project, table, sep = "."))
  
  projects <- unique(vars_df$project)
  tables <- unique(vars_df$proj_table)
  
  me <- list(vars_requested = vars,
             projects = projects,
             tables = tables,
             vars_df = vars_df)
  
  class(me) <- "ev_variables"
  
  return(me)
  
}


fetch_ev_data <- function(x, ...) {
  
  UseMethod("fetch_ev_data", x)
  
}

fetch_ev_data.ev_variables <- function(ev_vars) {
  
  me <- list(data = list(),
             metadata = list(),
             request = ev_vars)
  
  tables <- get_ev_tables(ev_vars)
  vars_df <- get_ev_vars_df(ev_vars)
  
  con <- ev_connect()
  
  
  
  ev_disconnect(con)
  
}
