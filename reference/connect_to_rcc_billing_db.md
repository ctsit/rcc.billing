# Connect to the rcc_billing DB

Connect to the rcc_billing DB

## Usage

``` r
connect_to_rcc_billing_db(drv = RMariaDB::MariaDB(), continue_on_error = FALSE)
```

## Arguments

- drv, :

  an object that inherits from DBIDriver (e.g. RMariaDB::MariaDB()), or
  an existing DBIConnection object (in order to clone an existing
  connection).

- continue_on_error:

  if TRUE then continue execution on error, if FALSE then quit non
  interactive sessions on error

## Value

An S4 object. Run ?dbConnect for more information

## Examples

``` r
if (FALSE) { # \dontrun{
# connect to the RCC Billing database using RCCBILLING_* environment variables
con <- connect_to_rcc_billing_db()

# connect to sqlite RCC Billing db
con <- connect_to_rcc_billing_db(drv = RSQLite::SQLite())
} # }
```
