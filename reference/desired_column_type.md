# desired_column_type

Determines the column type `redcap_projects_history` should use for a
given `redcap_projects` column type. `ENUM` columns are converted to a
generic `VARCHAR(255)` rather than mirrored verbatim: REDCap's own live
`redcap_projects` table can already contain data outside an `ENUM`'s
declared member list (e.g. a stray `""` left over from a legacy
non-strict-mode write), and REDCap upgrades continue to add new legal
`ENUM` values over time. Because `redcap_projects_history` exists purely
to record what REDCap sent rather than to enforce REDCap's own value
constraints, `VARCHAR` sidesteps both failure modes for good instead of
chasing each newly-drifted or already-invalid value with a fresh
`ALTER TABLE`. All other column types are left as-is.

## Usage

``` r
desired_column_type(source_column_type)
```

## Arguments

- source_column_type, :

  a character vector of `information_schema.columns.COLUMN_TYPE` values
  from the source table (e.g. `"enum('1','2','3')"`, `"varchar(191)"`)

## Value

a character vector of the column type `redcap_projects_history` should
use

## Examples

``` r
desired_column_type(c("enum('1','2','3')", "varchar(191)"))
#> [1] "varchar(255)" "varchar(191)"
```
