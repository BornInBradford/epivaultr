
#' Make ev_variables container
#' 
#' Converts a character vector of fully qualified variable names into an \code{ev_variables} container.
#' 
#' This is called by data request functions such as \code{\link{read_ev_variables}} and \code{\link{ev_simple_fetch}} and you would not normally call it directly. However, there may be circumstances in which all you have is a list of variable names and you wish to convert these into an \code{ev_variables} container.
#'
#' @param vars Character vector of fully qualified variable names
#'
#' @return An \code{ev_variables} container
#' @export
#' 
#' @family {data request functions}
#'
#' @examples
#' \dontrun{
#' 
#' vars <- c("proj1.tab1.var1", "proj1.tab1.var2", "proj2.tab2.var1")
#' make_ev_variables(vars)
#' 
#' }
#' 
#' @importFrom utils read.table
make_ev_variables <- function(vars) {
  
  vars_df <- data.frame(varfullname = vars)
  vars_df <- tidyr::separate(vars_df, varfullname, c("project", "table", "variable"), sep = "\\.")
  vars_df <- dplyr::mutate(vars_df, proj_table = paste(project, table, sep = "."))
  
  projects <- unique(vars_df$project)
  tables <- unique(vars_df$proj_table)
  
  me <- list(variables = vars,
             projects = projects,
             tables = tables,
             vars_df = vars_df)
  
  class(me) <- "ev_variables"
  
  return(me)
  
}


#' Read variables from a file
#' 
#' Reads variables from a file into an \code{ev_variables} container. Supports delimited or fixed width text and Excel formats. Will try to guess format based on file extension and first few lines.
#' 
#' Builds a character vector of fully qualified variable names and passes this to \code{\link{make_ev_variables}}.
#'
#' @param file Path to the file to be read
#'
#' @return An \code{ev_variables} container
#' @export
#' 
#' @family {data request functions}
#' @seealso If you want to create an \code{ev_variables} container directly from a vector of variable names, see \code{\link{make_ev_variables}}.
#'
#' @examples
#' \dontrun{
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#' 
#' }
#' 
#' @importFrom utils read.table
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
  
  vars <- make_ev_variables(vars)
  
  return(vars)
  
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


#' Fetch variable metadata from EpiVault
#' 
#' Interprets an \code{ev_variables} container, fetches the variable metadata from EpiVault and returns it in a data frame. Can fetch either variable level information or categorical value labels depending on the \code{cats} parameter. See Arguments and Details.
#' 
#' If \code{cats=TRUE} then it returns category metadata, otherwise (default) it returns variable metadata:
#' 
#' \describe{
#'   \item{\emph{variable (default)}}{Variable level information such as name, label, description, keywords and summary stats.}
#'   \item{\emph{category}}{Value labels for categorical variables, one row per value. Returns variable name, value and label.}
#' }
#'
#' @param con An EpiVault connection object
#' @param ev_vars An \code{ev_variables} container
#' @param visibility (default=0) Maximum visibility level requested. See examples.
#' @param cats (default=FALSE) If \code{TRUE}, return categorical value labels, otherwise return variable level metadata
#' 
#'
#' @return A data frame
#' @export
#' 
#' @family {data request functions}
#'
#' @examples
#' \dontrun{
#' 
#' con <- ev_connect()
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#'
#' # fetch variable metadata from EpiVault - default visibility for standard data requests
#' meta <- fetch_ev_meta_vars(con, vars)
#' 
#' # fetch variable metadata from EpiVault - highest visibility to access sensitive variables
#' meta <- fetch_ev_meta_vars(con, vars, visibility = 9)
#'
#' # fetch categorical metadata from EpiVault
#' meta <- fetch_ev_meta_vars(con, vars, cats = TRUE)
#'
#' }
#' 
fetch_ev_meta_vars <- function(con, ev_vars, visibility = 0, cats = FALSE) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be an `ev_variables` container e.g. created using the `read_ev_variables` function")
  
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
      
      vf <- grep(pattern = utils::glob2rx(v),
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
  
  var_meta <- var_meta |> dplyr::select(-dplyr::any_of(ev_db_columns()))
  
  return(var_meta)
  
  
}


#' Fetch table metadata from EpiVault
#' 
#' Interprets an \code{ev_variables} container, fetches the table metadata from EpiVault and returns it in a data frame.
#'
#' @param con An EpiVault connection object
#' @param ev_vars An \code{ev_variables} container
#' 
#' @return A data frame
#' @export
#' 
#' @family {data request functions}
#'
#' @examples
#' \dontrun{
#' 
#' con <- ev_connect()
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#'
#' # fetch table metadata from EpiVault
#' meta <- fetch_ev_meta_tabs(con, vars)
#' 
#' }
#' 
fetch_ev_meta_tabs <- function(con, ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be an `ev_variables` container e.g. created using the `read_ev_variables` function")
  
  tables <- get_ev_tables(ev_vars)
  
  ev_message("Fetching table metadata for ", length(tables), " table(s)")
  
  sql <- paste0("select * from data_vis0.DataDictionary__metadata_table where table_id in (",
                sql_make_filter(tables), 
                ");")
  
  tab_meta <- DBI::dbGetQuery(con, sql)
  
  if(!is.data.frame(tab_meta) | nrow(tab_meta) == 0) stop(paste0("Cannot find valid table metadata"))
  
  tab_meta <- tab_meta |> dplyr::select(-dplyr::any_of(ev_db_columns()))
  
  return(tab_meta)
  
}


#' Fetch data request from EpiVault
#' 
#' Interprets an \code{ev_variables} container, fetches the data from EpiVault and returns it in an \code{ev_data} container.
#' 
#' Passes \code{ev_vars} to \code{\link{fetch_ev_meta_vars}} and \code{\link{fetch_ev_meta_tabs}} to get the metadata ready for processing, fetches the tabular data from EpiVault before applying data labels. All fetched data and metadata is returned in an \code{ev_data} container.
#'
#' @param con An EpiVault connection object
#' @param ev_vars An \code{ev_variables} container
#' @param visibility (default=0) Maximum visibility level requested. See examples.
#' 
#'
#' @return An \code{ev_data} container
#' @export
#' 
#' @family {data request functions}
#'
#' @examples
#' \dontrun{
#' 
#' con <- ev_connect()
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#'
#' # fetch the data from EpiVault - default visibility for standard data requests
#' ev <- fetch_ev_data(con, vars)
#' 
#' # fetch the data from EpiVault - highest visibility to access sensitive variables
#' ev <- fetch_ev_data(con, vars, visibility = 9)
#'
#' }
#' 
fetch_ev_data <- function(con, ev_vars, visibility = 0) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be an `ev_variables` container e.g. created using the `read_ev_variables` function")
  
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
  
  tab_data <- tab_data |> dplyr::select(-dplyr::any_of(ev_db_columns()))
    
  return(tab_data)
  
}


fetch_ev_procedure <- function(con, project, table, visibility = 0, variables = character(0)) {
  
  query_tab <- paste0("data_vis", visibility, ".", project, "__", table, "__wide")
  
  sql <- paste0("exec ", query_tab)
  
  tab_data <- DBI::dbGetQuery(con, sql)
  
  if(!is.data.frame(tab_data) | nrow(tab_data) == 0) stop(paste0("Failed to receive valid data from ", query_tab))
  
  if(length(variables) > 0) tab_data <- tab_data |> dplyr::select(dplyr::all_of(variables))
  
  return(tab_data)
  
}


#' Simple data fetcher
#' 
#' Fetches data from a single table in EpiVault and returns it as a data frame.
#'
#' @param con An EpiVault connection object
#' @param project Project name
#' @param table Table name
#' @param visibility (default=0) Maximum visibility level requested. See examples.
#' @param variables (optional) Character vector of variable names. These should just be the variable (column) names, i.e. not fully qualified. If omitted, all variables are returned.
#'
#' @return A data frame containing the data requested
#' @export
#' 
#' @family {data request functions}
#'
#' @examples
#' \dontrun{
#' 
#' con <- ev_connect()
#' 
#' # fetch the whole table
#' dat <- ev_simple_fetch(con,
#'                        project = "project1",
#'                        table = "table1",
#'                        visibility = 0
#' )
#' 
#' # just fetch a couple of variables
#' # NB. variables in the table tagged as 'required' will also be returned
#' dat <- ev_simple_fetch(con,
#'                        project = "project1",
#'                        table = "table1",
#'                        visibility = 0,
#'                        variables = c("var11", "var13", "var44")
#' )
#' 
#' # use higher visibility level to access sensitive variables
#' # special database permissions may be required 
#' dat <- ev_simple_fetch(con,
#'                        project = "project1",
#'                        table = "table1",
#'                        visibility = 9,
#'                        variables = c("date_of_birth")
#' )
#'                        
#' }
#' 
#' @importFrom utils read.table
ev_simple_fetch <- function(con, project, table, visibility = 0, variables = character(0)) {
  
  if(is.null(variables) | any(is.na(variables)) | length(variables) == 0) variables <- "*"
  
  variables <- variables[!is.na(variables)]
  
  vars <- paste0(project, ".", table, ".", variables)
  
  vars <- make_ev_variables(vars)
  
  dat <- fetch_ev_data(con, vars, visibility)
  
  dat <- get_ev_data(dat, df_index = 1)
  
  return(dat)
  
}


#' @importFrom rlang :=
label_ev_variables <- function(df, vars_df, cats_df) {
  
  if(nrow(cats_df) > 0) {
    for(vlab in 1:nrow(cats_df)) {
      
      new_vlab <- cats_df$value[vlab]
      names(new_vlab) <- ifelse(is.na(cats_df$label[vlab]), "", cats_df$label[vlab])
      
      df <- df |> labelled::add_value_labels(!!cats_df$variable[vlab] := new_vlab)
      
    }
  }
  
  if(nrow(vars_df) > 0) {
    
    new_labs <- ifelse(is.na(vars_df$label), "", vars_df$label)
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


#' Write data files
#' 
#' Writes the data and metadata from an \code{ev_data} container to the specified file format.
#' 
#' @param ev_data An \code{ev_data} container
#' @param path Path to the folder to save files to
#' @param name (default='ev_data') A name for the data request. Used as the prefix for every output file.
#' @param format (default='stata') File format to write to. One of \code{c('stata', 'csv')}.
#' @param metadata (default=FALSE) If \code{TRUE}, also output files for variable, category and table metadata.
#' 
#' @export
#' 
#' @family {data request functions}
#'
#' @examples
#' \dontrun{
#' 
#' con <- ev_connect()
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#'
#' # fetch the data from EpiVault
#' ev <- fetch_ev_data(con, vars)
#' 
#' # write the data files in default stata format, excluding metadata
#' write_ev_data(ev_data = ev,
#'               path = "path/to/data/request/folder",
#'               name = "request101"
#' )
#' 
#' # write the data files in csv format, including metadata
#' write_ev_data(ev_data = ev,
#'               path = "path/to/data/request/folder",
#'               name = "request101",
#'               format = 'csv',
#'               metadata = TRUE
#' )
#' 
#' }
#' 
write_ev_data <- function(ev_data, 
                          path,
                          name = "ev_data",
                          format = "stata",
                          metadata = FALSE) {
  
  data_names <- get_ev_data_names(ev_data)
  
  for(d in 1:length(data_names)) {
    
    ev_message("Writing ", data_names[d])
    
    if(format == "stata") {
      
      filename <- file.path(path, paste0(name, "_", data_names[d], ".dta"))
      
      haven::write_dta(data = get_ev_data(ev_data, df_index = d),
                       path = filename)
      
    }
    
    if(format == "csv") {
      
      filename <- file.path(path, paste0(name, "_", data_names[d], ".csv"))
      
      readr::write_csv(x = get_ev_data(ev_data, df_index = d),
                       file = filename,
                       na = "")
      
    }
    
  }
  
  var_search <- ev_data$request |> get_ev_variables()
  
  if(length(var_search) > 0) {
    
    filename <- file.path(path, paste0(name, "_variable_search.csv"))
    
    vars <- data.frame(variable = var_search)
    
    readr::write_csv(x = vars,
                     file = filename,
                     na = "")
    
    
  }
  
  if(metadata) {
  
    ev_message("Writing metadata files")
    
    if(format == "stata") {
      
      filename <- file.path(path, paste0(name, "_variable_metadata.dta"))
      
      haven::write_dta(data = get_ev_metadata(ev_data, type = "variable"),
                       path = filename)
      
      filename <- file.path(path, paste0(name, "_category_metadata.dta"))
      
      haven::write_dta(data = get_ev_metadata(ev_data, type = "category"),
                       path = filename)
      
      filename <- file.path(path, paste0(name, "_table_metadata.dta"))
      
      haven::write_dta(data = get_ev_metadata(ev_data, type = "table"),
                       path = filename)
      
    }
    
    if(format == "csv") {
      
      filename <- file.path(path, paste0(name, "_variable_metadata.csv"))
      
      readr::write_csv(x = get_ev_metadata(ev_data, type = "variable"),
                       file = filename,
                       na = "")
      
      filename <- file.path(path, paste0(name, "_category_metadata.csv"))
      
      readr::write_csv(x = get_ev_metadata(ev_data, type = "category"),
                       file = filename,
                       na = "")
      
      filename <- file.path(path, paste0(name, "_table_metadata.csv"))
      
      readr::write_csv(x = get_ev_metadata(ev_data, type = "table"),
                       file = filename,
                       na = "")
      
    }
    
  }
  
}
