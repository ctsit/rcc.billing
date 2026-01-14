# update_project_billable_attribute

`update_project_billable_attribute` identifies and new projects that do
not have the `billable` attribute set and sets that attribute according
to business rules. It marks projects as billable once they are one month
old. It marks projects as non-billable if they are owned by the REDCap
admin team.
