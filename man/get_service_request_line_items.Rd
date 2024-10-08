% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_service_request_line_items.R
\name{get_service_request_line_items}
\alias{get_service_request_line_items}
\title{Assemble line items for service requests billing}
\usage{
get_service_request_line_items(service_requests, rc_billing_conn, rc_conn)
}
\arguments{
\item{service_requests}{A data frame of service requests, REDCap Service Request PID 1414.}

\item{rc_billing_conn}{A connection to REDCap billing database containing an invoice_line_items table. \code{\link{connect_to_rcc_billing_db}}}

\item{rc_conn}{A connection to REDCap database. \code{\link{connect_to_redcap_db}}}
}
\value{
A data frame of line items for service requests billing.
}
\description{
Assemble line items for service requests billing
}
\examples{
\dontrun{
line_items <- get_service_request_line_items(service_requests, rc_billing_conn, rc_conn)
}

}
