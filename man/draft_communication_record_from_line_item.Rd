% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{draft_communication_record_from_line_item}
\alias{draft_communication_record_from_line_item}
\title{Adds metadata necessary for sending emails to an invoice_line_item dataframe, e.g. \code{\link{transform_invoice_line_items_for_csbt}}}
\usage{
draft_communication_record_from_line_item(invoice_line_items)
}
\arguments{
\item{invoice_line_items}{A dataframe from the invoice_line_item table}
}
\value{
The input dataframe with the following columns added:
\itemize{
  \item updated - A timestamp provided by \code{\link[redcapcustodian]{get_script_run_time}}
  \item sender - The value set in \code{Sys.getenv("EMAIL_FROM")}
  \item recipient - The value set in \code{Sys.getenv("EMAIL_TO")}
  \item date_sent - A timestamp provided by \code{\link[redcapcustodian]{get_script_run_time}}
  \item date_received - A placeholder timestamp, \code{as.POSIXct(NA)}
  \item script_name - The script name returned by \code{\link[redcapcustodian]{get_script_name}}
}
}
\description{
Adds metadata necessary for sending emails to an invoice_line_item dataframe, e.g. \code{\link{transform_invoice_line_items_for_csbt}}
}
\examples{
\dontrun{
tbl(conn, "invoice_line_item") |>
  collect() |>
  draft_communication_record_from_line_item()
}
}
