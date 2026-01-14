# previous_n_months

Return the month number that would occur n months before the integer in
\`month\`

## Usage

``` r
previous_n_months(month, n = 1)
```

## Arguments

- month:

  \- an integer month number

- n:

  \- the number of months to subtract from current month (default = 1)

## Value

the nth previous month number as in integer

## Examples

``` r
previous_n_months(9, 2)
#> [1] 7
previous_n_months(1, 1)
#> [1] 12
```
