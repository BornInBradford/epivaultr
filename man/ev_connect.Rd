% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epivaultr_tools.R
\name{ev_connect}
\alias{ev_connect}
\title{Connect to EpiVault}
\usage{
ev_connect(
  ev_server = getOption("ev_server"),
  ev_database = getOption("ev_database"),
  ev_driver = dplyr::coalesce(getOption("ev_driver"), "SQL Server")
)
}
\arguments{
\item{ev_server}{Name of SQL Server instance}

\item{ev_database}{Name of EpiVault database}

\item{ev_driver}{(default="SQL Server") Name of SQL Server driver}
}
\value{
An EpiVault connection object
}
\description{
Connects to a specified EpiVault instance and returns the connection for use in other \code{epivaultr} functions.
}
\details{
\code{ev_server} and \code{ev_database} are required and can either be passed as function parameters or can be placed in global options (see examples). If passed as function parameters, this will override the global options.
}
\examples{
\dontrun{

# using global options
options(ev_server = "BHTS-RESEARC22A")
options(ev_database = "EpiVault1")

con <- ev_connect()

# using parameters - global options get overridden
con <- ev_connect(ev_server = "BHTS-RESRCH22DV", ev_database = "EpiVaultDV")

}
}
\seealso{
Other {EpiVault connectors}: 
\code{\link{ev_disconnect}()}
}
\concept{{EpiVault connectors}}
