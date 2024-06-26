% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epivaultr_processing.R
\name{fetch_ev_meta_vars}
\alias{fetch_ev_meta_vars}
\title{Fetch variable metadata from EpiVault}
\usage{
fetch_ev_meta_vars(con, ev_vars, visibility = 0, cats = FALSE)
}
\arguments{
\item{con}{An EpiVault connection object}

\item{ev_vars}{An \code{ev_variables} container}

\item{visibility}{(default=0) Maximum visibility level requested. See examples.}

\item{cats}{(default=FALSE) If \code{TRUE}, return categorical value labels, otherwise return variable level metadata}
}
\value{
A data frame
}
\description{
Interprets an \code{ev_variables} container, fetches the variable metadata from EpiVault and returns it in a data frame. Can fetch either variable level information or categorical value labels depending on the \code{cats} parameter. See Arguments and Details.
}
\details{
If \code{cats=TRUE} then it returns category metadata, otherwise (default) it returns variable metadata:

\describe{
  \item{\emph{variable (default)}}{Variable level information such as name, label, description, keywords and summary stats.}
  \item{\emph{category}}{Value labels for categorical variables, one row per value. Returns variable name, value and label.}
}
}
\examples{
\dontrun{

con <- ev_connect()

# read a variable data request file
vars <- read_ev_variables("path/to/variables/file")

# fetch variable metadata from EpiVault - default visibility for standard data requests
meta <- fetch_ev_meta_vars(con, vars)

# fetch variable metadata from EpiVault - highest visibility to access sensitive variables
meta <- fetch_ev_meta_vars(con, vars, visibility = 9)

# fetch categorical metadata from EpiVault
meta <- fetch_ev_meta_vars(con, vars, cats = TRUE)

}

}
\seealso{
Other {data request functions}: 
\code{\link{ev_simple_fetch}()},
\code{\link{fetch_ev_data}()},
\code{\link{fetch_ev_meta_tabs}()},
\code{\link{make_ev_variables}()},
\code{\link{read_ev_variables}()},
\code{\link{write_ev_data}()}
}
\concept{{data request functions}}
