# cleanup_project_ownership_table

`cleanup_project_ownership_table.R` identifies projects with ownership
issues, nominates a new owner if possible, and writes that owner to the
project ownership data. It cannot always determine a new owner. It logs
the changes it makes to ownership data and the projects where it cannot
fix the ownership data.
