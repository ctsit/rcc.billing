# create_and_send_new_invoice_line_items

`create_and_send_new_invoice_line_items` decides what services to bill
at the beginning of each month. It reads service request data and redcap
project data to create a dataset of records that will be used by the
fiscal team to create line items on invoices. The dataset will include
records that describe service request work completed in the previous
month. The dataset will also have one record for each REDCap project
with an anniversary in the previous month and also meets several other
criteria.

create_and_send_new_invoice_line_items data processing workflow
