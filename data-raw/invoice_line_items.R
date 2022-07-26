library(dplyr)

invoice_line_item_reasons <- tibble::tribble(
  ~code, ~label,
  "new_item", "New item to be invoiced",
  "deleted", "Project deleted",
  "reassigned", "PI reassigned"
)

usethis::use_data(invoice_line_item_reasons, overwrite = TRUE)

invoice_line_item_statuses <- tribble(
  ~status, ~description,
  "draft", "draft invoices line items that have not yet been sent",
  "sent", "the invoice line item has been sent to CSBT",
  "canceled", "the invoice line item does not need to be paid",
  "unreconciled", "CSBT says the line item is paid, but this has not yet been verified by CTS-IT",
  "paid", "CTS-IT has verified the line item has been paid"
)

usethis::use_data(invoice_line_item_statuses, overwrite = TRUE)

# # If you need to revise invoice_line_item_test_data, use this code
# library(rcc.billing)
# my_file = "output/invoice_line_item_test_data.xlsx"
# invoice_line_item_test_data %>%
#   writexl::write_xlsx(my_file)
# # make manual edits to the Excel file
# invoice_line_item_test_data <- readxl::read_xlsx(my_file)
# usethis::use_data(invoice_line_item_test_data, overwrite = TRUE)
