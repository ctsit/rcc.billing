# When jobs run

The scripts in the billing system are dependent upon one another to
clean data, backfill new records, communicate with customers, decide
what to bill, acknowledge receipt of funds, and enforce policy. Each is
dependent on the other tasks. To assure everything happens on schedule,
almost all tasks in the billing system are run on a regular basis via
cron. Data cleaning and updating happen on a daily or weekly cycle.
Internal reporting happens on a weekly cycle. Everything that decides
which projects will get billed happens on a monthly cycle. The detailed
schedule of jobs is shown below.

## Daily

- 00:47 -
  [cleanup_project_ownership_table](https://ctsit.github.io/rcc.billing/articles/cleanup_project_ownership_table.md)
- 1:05 -
  [update_project_billable_attribute](https://ctsit.github.io/rcc.billing/articles/update_project_billable_attribute.md)

## Weekday business hours, every two hours, 7:00 - 19:00

- 7:03 - 17:03 -
  [update_probono_service_request_records](https://ctsit.github.io/rcc.billing/articles/update_probono_service_request_records.md)
- 7:07 - 17:07 -
  [update_free_support_time_remaining](https://ctsit.github.io/rcc.billing/articles/update_free_support_time_remaining.md)

## Weekly

- Monday at 3:11 - update uf fiscal orgs data (temporarily disabled)
- Monday at 7:03 - Send
  [billable_candidates](https://ctsit.github.io/rcc.billing/articles/billable_candidates.md)
  report
- Tuesday at 10:00 -
  [cleanup_bad_email_addresses](https://ctsit.github.io/rcc.billing/articles/cleanup_bad_email_addresses.md)
- Tuesday at 10:03 -
  [cleanup_bad_project_pi_email_addresses](https://ctsit.github.io/rcc.billing/articles/cleanup_bad_project_pi_email_addresses.md)

## Monthly

- First Tuesday of the month -
  [sequester_unpaid_projects](https://ctsit.github.io/rcc.billing/articles/sequester_unpaid_projects.md)
- Day 1 at 8:02 -
  [sequester_orphans](https://ctsit.github.io/rcc.billing/articles/sequester_orphans.md)
- Day 1 at 10:03 -
  [warn_owners_of_impending_bill](https://ctsit.github.io/rcc.billing/articles/warn_owners_of_impending_bill.md)
  in the next month
- Day 1 at 10:05 -
  [request_correction_of_bad_ownership_data](https://ctsit.github.io/rcc.billing/articles/request_correction_of_bad_ownership_data.md)
  by the study team
- Day 5 at 8:05 -
  [create_and_send_new_invoice_line_items](https://ctsit.github.io/rcc.billing/articles/create_and_send_new_invoice_line_items.md)
- Day 23 at 8:02 -
  [sequester_orphans](https://ctsit.github.io/rcc.billing/articles/sequester_orphans.md)
- Day 23 at 10:03 -
  [warn_owners_of_impending_bill](https://ctsit.github.io/rcc.billing/articles/warn_owners_of_impending_bill.md)
  in the next month
- Day 28 at 8:02 -
  [sequester_orphans](https://ctsit.github.io/rcc.billing/articles/sequester_orphans.md)
