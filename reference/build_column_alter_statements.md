# build_column_alter_statements

Builds the `ALTER TABLE ... MODIFY COLUMN` statements needed to bring a
target table's drifted columns to their desired column type, while
preserving each column's existing nullability on the target (see
[`get_column_type_drift`](https://ctsit.github.io/rcc.billing/reference/get_column_type_drift.md)
for why nullability is never tightened to match the source). Pure
string-building function with no database access, given the output of
[`get_column_type_drift`](https://ctsit.github.io/rcc.billing/reference/get_column_type_drift.md).

## Usage

``` r
build_column_alter_statements(drift, target_table)
```

## Arguments

- drift, :

  a dataframe as returned by
  [`get_column_type_drift`](https://ctsit.github.io/rcc.billing/reference/get_column_type_drift.md)

- target_table, :

  the name of the table the generated statements will alter

## Value

a character vector of `ALTER TABLE` statements, one per drifted column

## Examples

``` r
if (FALSE) { # \dontrun{
build_column_alter_statements(drift, "redcap_projects_history")
} # }
```
