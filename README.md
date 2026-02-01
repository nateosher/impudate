# impudate

Utilities for working with partially missing dates.

## Example

```r
library(impudate)

df <- tibble::tibble(date = c("2024", "2024-07", "2024-07-31", NA))
add_date_parts(df, date)
```
