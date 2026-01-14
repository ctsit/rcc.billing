# mutate_columns_to_posixct

Mutates column data types to POSIXct

## Usage

``` r
mutate_columns_to_posixct(data, column_names)
```

## Arguments

- data:

  \- a dataframe to mutate

- column_names:

  \- a vector of column names to mutate

## Value

The input dataframe with revised data types

## Examples

``` r
if (FALSE) { # \dontrun{
time_columns <- c("created", "updated")
mutate_columns_to_posixct(data, time_columns)
} # }
```
