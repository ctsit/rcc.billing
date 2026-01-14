# update_free_support_time_remaining

`update_free_support_time_remaining` enforces a business rule to provide
a free customer service consultation of up to one hour for each REDCap
project. It reads the customer service database, groups the data by
project ID, and determines which projects have logged hours which should
receive hourly rate adjust down to \$0/hour. This marks the records as
*pro bono*. Those records will be invoiced like other work but at the
\$0/hour rate and the “pro bono” label.

update_free_support_time_remaining data processing workflow
