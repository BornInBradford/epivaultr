
#' Get project names
#' 
#' Returns project names from an \code{ev_variables} container. 
#' 
#' An \code{ev_variables} container can be created using \code{\link{read_ev_variables}}.
#'
#' @param ev_vars An \code{ev_variables} container
#'
#' @return A character vector of project names
#' @export
#' 
#' @seealso \code{\link{read_ev_variables}} to create an \code{ev_variables} container.
#' @family {getters}
#'
#' @examples
#' \dontrun{
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#' 
#' # get the project names from the request
#' get_ev_projects(vars)
#' 
#' }
#' 
get_ev_projects <- function(ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be an `ev_variables` container e.g. created using the `read_ev_variables` function")
  
  projects <- ev_vars$projects
  
  return(projects)
  
}


#' Get table names
#' 
#' Returns table names from an \code{ev_variables} container. 
#' 
#' An \code{ev_variables} container can be created using \code{\link{read_ev_variables}}.
#'
#' @param ev_vars An \code{ev_variables} container
#'
#' @return A character vector of table names
#' @export
#' 
#' @seealso \code{\link{read_ev_variables}} to create an \code{ev_variables} container.
#' @family {getters}
#'
#' @examples
#' \dontrun{
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#' 
#' # get the table names from the request
#' get_ev_tables(vars)
#' 
#' }
#'
get_ev_tables <- function(ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be an `ev_variables` container e.g. created using the `read_ev_variables` function")
  
  tables <- ev_vars$tables
  
  return(tables)
  
  
}


#' Get variables names
#' 
#' Returns fully qualified variable names from an \code{ev_variables} container. 
#' 
#' An \code{ev_variables} container can be created using \code{\link{read_ev_variables}}.
#'
#' @param ev_vars An \code{ev_variables} container
#'
#' @return A character vector of variable names
#' @export
#' 
#' @seealso \code{\link{read_ev_variables}} to create an \code{ev_variables} container.
#' @family {getters}
#'
#' @examples
#' \dontrun{
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#' 
#' # get the variable names from the request
#' get_ev_variables(vars)
#' 
#' }
#'
get_ev_variables <- function(ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be an `ev_variables` container e.g. created using the `read_ev_variables` function")
  
  vars <- ev_vars$variables
  
  return(vars)
  
}


#' Get variables data frame
#' 
#' Returns the variable data frame from an \code{ev_variables} container. 
#' 
#' The variable data frame contains four columns:
#' 
#' \describe{
#'   \item{\emph{varfullname}}{Fully qualified variable name, i.e. \code{project.table.variable}}
#'   \item{\emph{project}}{Project name}
#'   \item{\emph{table}}{Table name}
#'   \item{\emph{variable}}{Variable name}
#' }
#' 
#' An \code{ev_variables} container can be created using \code{\link{read_ev_variables}}.
#'
#' @param ev_vars An \code{ev_variables} container
#'
#' @return A data frame containing fully qualified variable name, project name, table name and variable name.
#' @export
#' 
#' @seealso \code{\link{read_ev_variables}} to create an \code{ev_variables} container.
#' @family {getters}
#'
#' @examples
#' \dontrun{
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#' 
#' # get the variable data frame from the request
#' get_ev_vars_df(vars)
#' 
#' }
#'
get_ev_vars_df <- function(ev_vars) {
  
  if(class(ev_vars) != "ev_variables") stop("`ev_vars` must be an `ev_variables` container e.g. created using the `read_ev_variables` function")
  
  vars_df <- ev_vars$vars_df
  
  return(vars_df)
  
}


#' Get single data frame
#' 
#' Returns a selected data frame from an \code{ev_data} container.
#' 
#' The data frame can be selected by index (1, 2, 3 etc.) or by name (\code{project.table}). The names of data frames within an \code{ev_data} container can be queried using \code{\link{get_ev_data_names}}.
#' 
#' An \code{ev_data} container can be created using \code{\link{fetch_ev_data}}.
#'
#' @param ev_data An \code{ev_data} container
#' @param df_name The name of the data frame. Only used if \code{df_index} is not supplied
#' @param df_index The numerical index of the data frame within the \code{ev_data} container. Overrides \code{df_name} if both are supplied.
#'
#' @return A data frame
#' @export
#' 
#' @seealso \code{\link{fetch_ev_data}} to create an \code{ev_data} container.
#' @family {getters}
#'
#' @examples
#' \dontrun{
#' 
#' con <- ev_connect()
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#' # fetch the data from EpiVault
#' dat <- fetch_ev_data(con, vars)
#' 
#' # get a data frame by name
#' get_ev_data(dat, df_name = "proj1.tab1")
#' # get a data frame by index
#' get_ev_data(dat, df_index = 1)
#' 
#' }
#'
get_ev_data <- function(ev_data, df_name = character(0), df_index = integer(0)) {
  
  if(class(ev_data) != "ev_data") stop("`ev_data` must be an `ev_data` container e.g. created using the `fetch_ev_data` function")
  
  dat <- data.frame()
  
  if(length(df_name) == 1) dat <- ev_data$data[[df_name]]
  
  if(length(df_index) == 1) dat <- ev_data$data[[df_index]]
  
  return(dat)
  
}


#' Get table/data frame names
#' 
#' Returns the names of the data frames in an \code{ev_data} container.
#' 
#' The returned values can be used in a call to \code{\link{get_ev_data}}. 
#' 
#' An \code{ev_data} container can be created using \code{\link{fetch_ev_data}}.
#'
#' @param ev_data An \code{ev_data} container
#' 
#' @return A character vector of data names
#' @export
#' 
#' @seealso \code{\link{fetch_ev_data}} to create an \code{ev_data} container.
#' @family {getters}
#'
#' @examples
#' \dontrun{
#' 
#' con <- ev_connect()
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#' # fetch the data from EpiVault
#' dat <- fetch_ev_data(con, vars)
#' 
#' # get the data frame names
#' get_ev_data_names(dat)
#' 
#' }
#'
get_ev_data_names <- function(ev_data) {
  
  if(class(ev_data) != "ev_data") stop("`ev_data` must be an `ev_data` container e.g. created using the `fetch_ev_data` function")
  
  data_names <- names(ev_data$data)
  
  return(data_names)
  
}


#' Get single metadata table
#' 
#' Returns metadata of a selected type from an \code{ev_data} container. 
#' 
#' Three types of metadata can be returned: 
#' 
#' \describe{
#'   \item{\emph{variable}}{Variable level information such as name, label, description, keywords and summary stats.}
#'   \item{\emph{category}}{Value labels for categorical variables, one row per value. Returns variable name, value and label.}
#'   \item{\emph{table}}{Table level information such as name, cohort membership, entity type, subjects and respondents and table description.}
#' }
#' 
#' An \code{ev_data} container can be created using \code{\link{fetch_ev_data}}.
#'
#' @param ev_data An \code{ev_data} container
#' @param type One of \code{c('variable', 'category', 'table')}. If not recognised, returns an empty data frame.
#'
#' @return A data frame of metadata
#' @export
#' 
#' @seealso \code{\link{fetch_ev_data}} to create an \code{ev_data} container.
#' @family {getters}
#'
#' @examples
#' \dontrun{
#' 
#' con <- ev_connect()
#' 
#' # read a variable data request file
#' vars <- read_ev_variables("path/to/variables/file")
#' # fetch the data from EpiVault
#' dat <- fetch_ev_data(con, vars)
#' 
#' # get variable metadata
#' get_ev_metadata(dat, type = "variable")
#' # get category metadata
#' get_ev_metadata(dat, type = "category")
#' # get table metadata
#' get_ev_metadata(dat, type = "table")
#' 
#' }
#'
get_ev_metadata <- function(ev_data, type = character(0)) {
  
  if(class(ev_data) != "ev_data") stop("`ev_data` must be an `ev_data` container e.g. created using the `fetch_ev_data` function")
  
  dat <- data.frame()
  
  if(!type %in% c("variable", "category", "table")) warning(paste0("Metadata type not recognised: ", type))
  
  if(type == "variable") dat <- ev_data$metadata$variable
  
  if(type == "category") dat <- ev_data$metadata$category
  
  if(type == "table") dat <- ev_data$metadata$table
  
  return(dat)
  
}

