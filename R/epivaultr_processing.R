
read_ev_variables <- function(file) {
  
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


sql_make_select <- function(cols) {
  
  if(!is.null(cols) & !any(is.na(cols)) & length(cols) > 0) {
    
    sql_select <- paste0(cols, collapse = ", ")
    
  } else {
    
    sql_select <- "*"
    
  }
  
  return(sql_select)
  
}


sql_make_filter <- function(vals) {
  
  if(!is.null(vals) & !any(is.na(vals)) & length(vals) > 0) {
    
    sql_filter <- paste0("'", vals, "'", collapse = ", ")
    
  } else {
    
    sql_filter <- "''"
    
  }
  
  return(sql_filter)
  
}


fetch_ev_meta_vars <- function(con, ev_vars, cats = FALSE) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  tables <- get_ev_tables(ev_vars)
  vars_df <- get_ev_vars_df(ev_vars)
  
  tab_meta <- fetch_ev_meta_tabs(con, ev_vars)
  
  if(!is.data.frame(tab_meta) | nrow(tab_meta) == 0) stop("Cannot find valid table metadata for request!")
  
  var_meta <- data.frame()
  
  # create unions of selects over each table by table
  for(t in 1:nrow(tab_meta)) {
    
    sql <- paste0("select * from metadata.",
                  tab_meta$project_name[t], "__", tab_meta$table_name[t],
                  "__", ifelse(cats, "categories", "variables"))
    
    var_meta_t <- DBI::dbGetQuery(con, sql)
    
    if(!is.data.frame(var_meta_t) | nrow(var_meta_t) == 0) stop(paste0("Cannot find valid variable metadata for table ", tab_meta$table_id[t]))
    
    vars_t <- var_meta_t$variable
    
    vars_t_required <- var_meta_t |> dplyr::filter(required == 1) |> dplyr::pull(variable)
    
    vars_search <- vars_df |> dplyr::filter(proj_table == tab_meta$table_id[t]) |>
      dplyr::select(variable) |> dplyr::pull()
    
    # simple search to start with
    vars_found <- intersect(vars_t, vars_search)
    
    vars_not_found <- setdiff(vars_search, vars_t)
    
    # check any that might have wildcards
    for(v in vars_not_found) {
      
      vf <- grep(pattern = glob2rx(v),
                 x = vars_t,
                 value = TRUE)
      
      vars_found <- c(vars_found, vf)
      
    }
    
    vars_found <- c(vars_t_required, vars_found) |> unique()
    
    var_meta_t <- var_meta_t |> dplyr::filter(variable %in% vars_found) |>
      dplyr::mutate(varfullname = paste0(tab_meta$table_id[t], ".", variable),
                    table_id = tab_meta$table_id[t],
                    project = tab_meta$project_name[t],
                    table = tab_meta$table_name[t],
                    .before = 1)
    
    var_meta <- var_meta |> dplyr::bind_rows(var_meta_t)
    
  }
  
  return(var_meta)
  
  
}


fetch_ev_meta_tabs <- function(con, ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  tables <- get_ev_tables(ev_vars)
  
  sql <- paste0("select * from data_vis0.DataDictionary__metadata_table where table_id in (",
                sql_make_filter(tables), 
                ");")
  
  tab_meta <- DBI::dbGetQuery(con, sql)
  
  return(tab_meta)
  
}


fetch_ev_meta_cats <- function(ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
}


fetch_ev_data <- function(con, ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  me <- list(data = list(),
             metadata = list(),
             request = ev_vars)
  
  meta_vars <- fetch_ev_meta_vars(ev_vars)
  meta_tabs <- fetch_ev_meta_tabs(ev_vars)
  meta_cats <- fetch_ev_meta_cats(ev_vars)
  
  
  tables <- get_ev_tables(ev_vars)
  vars_df <- get_ev_vars_df(ev_vars)
  
}



