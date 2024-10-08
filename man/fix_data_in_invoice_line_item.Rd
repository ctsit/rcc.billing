% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{fix_data_in_invoice_line_item}
\alias{fix_data_in_invoice_line_item}
\title{fix_data_in_invoice_line_item}
\usage{
fix_data_in_invoice_line_item(data)
}
\arguments{
\item{data}{- a dataframe with data from the invoice_line_item table}
}
\value{
The input dataframe with revised data types
}
\description{
Fixes column data types that can vary between MySQL/MariaDB and SQLite3.
This allows testing in SQLite3 while production is MariaDB
}
\examples{
\dontrun{
fix_data_in_invoice_line_item(invoice_line_item_test_data)
}
}
