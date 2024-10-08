% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_project_flags.R
\name{get_project_flags}
\alias{get_project_flags}
\title{Get important boolean flags that describe projects}
\usage{
get_project_flags(rc_conn)
}
\arguments{
\item{rc_conn}{A DBI connection object to a REDCap Database on a system that uses the UF extensions to REDCap Project Ownership}
}
\value{
a dataframe of boolean flags for every REDCap project in the redcap_projects table
}
\description{
Get important boolean flags that describe projects
}
\examples{
\dontrun{
  get_project_flags(rc_conn)
}
}
