
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


fetch_ev_meta_vars <- function(con, ev_vars, visibility = 0, cats = FALSE) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  tables <- get_ev_tables(ev_vars)
  vars_df <- get_ev_vars_df(ev_vars)
  
  ev_message("Fetching ", ifelse(cats, "categorical", "variable"), " metadata for ", length(tables), " table(s)")
  
  var_meta <- data.frame()
  
  # create unions of selects over each table by table
  for(t in 1:length(tables)) {
    
    vars_df_t <- vars_df |> dplyr::filter(proj_table == tables[t])
    
    tab_table_id <- tables[t]
    tab_project <- strsplit(tab_table_id, ".", fixed = TRUE)[[1]][1]
    tab_table <- strsplit(tab_table_id, ".", fixed = TRUE)[[1]][2]
    
    sql <- paste0("select * from metadata.",
                  tab_project, "__", tab_table,
                  "__variables where visibility <= ", visibility)
    
    var_meta_t <- DBI::dbGetQuery(con, sql)
    
    if(!is.data.frame(var_meta_t) | nrow(var_meta_t) == 0) stop(paste0("Cannot find valid variable metadata for table ", tab_table_id,
                                                                       " with visibility ", visibility))
    
    vars_t <- var_meta_t$variable
    
    vars_t_required <- var_meta_t |> dplyr::filter(required == 1) |> dplyr::pull(variable)
    
    vars_search <- vars_df_t |> dplyr::select(variable) |> dplyr::pull()
    
    # simple search to start with
    vars_found <- intersect(vars_t, vars_search)
    
    # check any that might have wildcards
    for(v in grep(pattern = "\\*|\\?", x = vars_search, value = TRUE)) {
      
      vf <- grep(pattern = glob2rx(v),
                 x = vars_t,
                 value = TRUE)
      
      vars_found <- c(vars_found, vf)
      
    }
    
    vars_found <- c(vars_t_required, vars_found) |> unique()
    
    vars_not_found <- grep(pattern = "\\*|\\?", x = vars_search, value = TRUE, invert = TRUE) |>
      setdiff(vars_found)
    
    if(length(vars_not_found) > 0) warning(paste0(length(vars_not_found), " variable(s) not found in `", tab_table_id, "` with visibility ",
                                                  visibility, ": ",
                                                  paste0("`", vars_not_found, "`", collapse = ", ")))
      
    var_meta_t <- var_meta_t |> dplyr::filter(variable %in% vars_found) |>
      dplyr::mutate(varfullname = paste0(tab_table_id, ".", variable),
                    table_id = tab_table_id,
                    project = tab_project,
                    table = tab_table,
                    .before = 1)
    
    if(cats) {
      
      sql <- paste0("select * from metadata.",
                    tab_project, "__", tab_table,
                    "__categories")
      
      cats_tab <- DBI::dbGetQuery(con, sql)
      
      var_meta_t <- var_meta_t |> dplyr::select(varfullname, table_id, project, table, variable) |>
        dplyr::inner_join(cats_tab, by = "variable")
      
    }
    
    
    var_meta <- var_meta |> dplyr::bind_rows(var_meta_t)
    
  }
  
  return(var_meta)
  
  
}


fetch_ev_meta_tabs <- function(con, ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  tables <- get_ev_tables(ev_vars)
  
  ev_message("Fetching table metadata for ", length(tables), " table(s)")
  
  sql <- paste0("select * from data_vis0.DataDictionary__metadata_table where table_id in (",
                sql_make_filter(tables), 
                ");")
  
  tab_meta <- DBI::dbGetQuery(con, sql)
  
  if(!is.data.frame(tab_meta) | nrow(tab_meta) == 0) stop(paste0("Cannot find valid table metadata"))
  
  return(tab_meta)
  
}


fetch_ev_data <- function(con, ev_vars, visibility = 0) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be of class `ev_variables` e.g. created using the `read_ev_variables` function")
  
  me <- list(data = list(),
             metadata = list(),
             request = ev_vars)
  
  meta_vars <- fetch_ev_meta_vars(con, ev_vars, visibility)
  meta_tabs <- fetch_ev_meta_tabs(con, ev_vars)
  meta_cats <- fetch_ev_meta_vars(con, ev_vars, visibility, cats = TRUE)
  
  for(t in 1:nrow(meta_tabs)) {
    
    ev_message("Fetching data from ", meta_tabs$table_id[t])
    
    tab_data <- data.frame()
    
    tab_table_id <- meta_tabs$table_id[t]
    tab_project <- meta_tabs$project_name[t]
    tab_table <- meta_tabs$table_name[t]
    tab_nvars <- meta_tabs$n_variables[t]
    
    tab_cats <- meta_cats |> dplyr::filter(table_id == tab_table_id)
    
    tab_vars <- meta_vars |> dplyr::filter(table_id == tab_table_id)
    
    tab_var_filter <- tab_vars |> dplyr::pull(variable)
    
    # what visibility level do we need to query to get all variables we can see
    query_vis <- max(tab_vars$visibility)
    
    type_col <- paste0("sql_type_vis", query_vis)
    sql_type <- dplyr::select(meta_tabs, !!!type_col)[t, 1]
    
    # do we need to filter?
    if(length(tab_var_filter) == tab_nvars) tab_var_filter <- character(0)
    
    if(sql_type == "table") tab_data <- fetch_ev_table(con, project = tab_project, table = tab_table, 
                                                       visibility = query_vis, variables = tab_var_filter)
    if(sql_type == "procedure") tab_data <- fetch_ev_procedure(con, project = tab_project, table = tab_table, 
                                                               visibility = query_vis, variables = tab_var_filter)
    
    
    # NB do value type transforms THEN label otherwise labels get dropped
    tab_data <- tab_data |> set_ev_val_types(tab_vars) |>
      label_ev_variables(tab_vars, tab_cats)
    
    me$data <- append(me$data, list(tab_data))
    
  }
  
  names(me$data) <- meta_tabs$table_id
  
  me$metadata$variable <- meta_vars
  me$metadata$category <- meta_cats
  me$metadata$table <- meta_tabs
  
  class(me) <- "ev_data"
  
  return(me)

}


fetch_ev_table <- function(con, project, table, visibility = 0, variables = character(0)) {
  
  query_tab <- paste0("data_vis", visibility, ".", project, "__", table)
  
  sql <- paste0("select ", sql_make_select(variables),
                " from ", query_tab)
  
  tab_data <- DBI::dbGetQuery(con, sql)
  
  if(!is.data.frame(tab_data) | nrow(tab_data) == 0) stop(paste0("Failed to receive valid data from ", query_tab))
  
  return(tab_data)
  
}


fetch_ev_procedure <- function(con, project, table, visibility = 0, variables = character(0)) {
  
  query_tab <- paste0("data_vis", visibility, ".", project, "__", table, "__wide")
  
  sql <- paste0("exec ", query_tab)
  
  tab_data <- DBI::dbGetQuery(con, sql)
  
  if(!is.data.frame(tab_data) | nrow(tab_data) == 0) stop(paste0("Failed to receive valid data from ", query_tab))
  
  if(length(variables) > 0) tab_data <- tab_data |> dplyr::select(all_of(variables))
  
  return(tab_data)
  
}


label_ev_variables <- function(df, vars_df, cats_df) {
  
  if(nrow(cats_df) > 0) {
    for(vlab in 1:nrow(cats_df)) {
      
      new_vlab <- cats_df$value[vlab]
      names(new_vlab) <- cats_df$label[vlab]
      
      df <- df |> labelled::add_value_labels(!!cats_df$variable[vlab] := new_vlab)
      
    }
  }
  
  if(nrow(vars_df) > 0) {
    
    new_labs <- vars_df$label
    names(new_labs) <- vars_df$variable
    
    df <- df |> labelled::set_variable_labels(.labels = new_labs)
    
  }
  
  return(df)
  
}


set_ev_val_types <- function(df, vars_df) {
  
  if(nrow(vars_df) > 0) {
    
    for(v in 1:nrow(vars_df)) {
      
      if(vars_df$value_type[v] == "text" & class(df[[v]]) != "character") df <- df |> dplyr::mutate(!!vars_df$variable[v] := as.character(df[[v]]))
      if(vars_df$value_type[v] == "integer" & class(df[[v]]) != "integer") df <- df |> dplyr::mutate(!!vars_df$variable[v] := as.integer(df[[v]]))
      if(vars_df$value_type[v] == "float" & class(df[[v]]) != "numeric") df <- df |> dplyr::mutate(!!vars_df$variable[v] := as.numeric(df[[v]]))
      if(vars_df$value_type[v] == "categorical" & class(df[[v]]) != "integer") df <- df |> dplyr::mutate(!!vars_df$variable[v] := as.integer(df[[v]]))
      if(vars_df$value_type[v] == "date" & class(df[[v]]) != "Date") df <- df |> dplyr::mutate(!!vars_df$variable[v] := as.Date(df[[v]]))
      if(vars_df$value_type[v] == "boolean" & class(df[[v]]) != "integer") df <- df |> dplyr::mutate(!!vars_df$variable[v] := as.integer(df[[v]]))
      
    }
    
  }
  
  return(df)
  
}

