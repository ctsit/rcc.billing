# next_n_months

Return the month number that would occur n months after the integer in
\`month\`

## Usage

``` r
next_n_months(month, n = 1)
```

## Arguments

- month:

  \- an integer month number

- n:

  \- the number of months to add to the current month (default = 1)

## Value

the nth next month number as in integer

## Examples

``` r
next_n_months(9, 2)
#> [1] 11
next_n_months(1, 1)
#> [1] 2
```
