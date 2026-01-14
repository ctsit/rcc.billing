# update_invoice_line_items_with_invoicing_details

`update_invoice_line_items_with_invoicing_details.R` reads downloaded
invoicing details provided by the fiscal team and applies them to
billing tables and redcap tables. It does these things:

- Update invoice_line_item records with payment details.
- Insert invoice_line_item_communication records for each revised
  invoice_line_item
- Update service_instance records with new CTSI Study IDs
- Mirror invoice_line_item data to REDCap table of the same name
- Update banned_owners table for each PI who left the university
