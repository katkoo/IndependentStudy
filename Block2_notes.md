Block2\_notes
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts -------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
sales <- read_csv(here::here("data/hdb_resale_2015_onwards.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   month = col_character(),
    ##   town = col_character(),
    ##   flat_type = col_character(),
    ##   block = col_character(),
    ##   street_name = col_character(),
    ##   storey_range = col_character(),
    ##   floor_area_sqm = col_double(),
    ##   flat_model = col_character(),
    ##   lease_commence_date = col_double(),
    ##   remaining_lease = col_double(),
    ##   resale_price = col_double()
    ## )

``` r
# library(readr)
# sales <- read_csv("data/hdb_resale_2015_onwards.csv")
# View(sales)
```

``` r
sales <- read_csv(here::here("data/hdb_resale_2015_onwards.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   month = col_character(),
    ##   town = col_character(),
    ##   flat_type = col_character(),
    ##   block = col_character(),
    ##   street_name = col_character(),
    ##   storey_range = col_character(),
    ##   floor_area_sqm = col_double(),
    ##   flat_model = col_character(),
    ##   lease_commence_date = col_double(),
    ##   remaining_lease = col_double(),
    ##   resale_price = col_double()
    ## )

``` r
sales %>% glimpse()
```

    ## Rows: 79,100
    ## Columns: 11
    ## $ month               <chr> "2015-01", "2015-01", "2015-01", "2015-01", "20...
    ## $ town                <chr> "ANG MO KIO", "ANG MO KIO", "ANG MO KIO", "ANG ...
    ## $ flat_type           <chr> "3 ROOM", "3 ROOM", "3 ROOM", "3 ROOM", "3 ROOM...
    ## $ block               <chr> "174", "541", "163", "446", "557", "603", "709"...
    ## $ street_name         <chr> "ANG MO KIO AVE 4", "ANG MO KIO AVE 10", "ANG M...
    ## $ storey_range        <chr> "07 TO 09", "01 TO 03", "01 TO 03", "01 TO 03",...
    ## $ floor_area_sqm      <dbl> 60, 68, 69, 68, 68, 67, 68, 68, 67, 68, 67, 68,...
    ## $ flat_model          <chr> "Improved", "New Generation", "New Generation",...
    ## $ lease_commence_date <dbl> 1986, 1981, 1980, 1979, 1980, 1980, 1980, 1981,...
    ## $ remaining_lease     <dbl> 70, 65, 64, 63, 64, 64, 64, 65, 62, 69, 60, 64,...
    ## $ resale_price        <dbl> 255000, 275000, 285000, 290000, 290000, 290000,...

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(forcats)

sales <- sales %>%
  mutate(
    month = ymd(month, truncated = 1),
    flat_type = as_factor(flat_type),
    storey_range = as_factor(storey_range),
    flat_model = as_factor(flat_model)
  )
sales %>% glimpse()
```

    ## Rows: 79,100
    ## Columns: 11
    ## $ month               <date> 2015-01-01, 2015-01-01, 2015-01-01, 2015-01-01...
    ## $ town                <chr> "ANG MO KIO", "ANG MO KIO", "ANG MO KIO", "ANG ...
    ## $ flat_type           <fct> 3 ROOM, 3 ROOM, 3 ROOM, 3 ROOM, 3 ROOM, 3 ROOM,...
    ## $ block               <chr> "174", "541", "163", "446", "557", "603", "709"...
    ## $ street_name         <chr> "ANG MO KIO AVE 4", "ANG MO KIO AVE 10", "ANG M...
    ## $ storey_range        <fct> 07 TO 09, 01 TO 03, 01 TO 03, 01 TO 03, 07 TO 0...
    ## $ floor_area_sqm      <dbl> 60, 68, 69, 68, 68, 67, 68, 68, 67, 68, 67, 68,...
    ## $ flat_model          <fct> Improved, New Generation, New Generation, New G...
    ## $ lease_commence_date <dbl> 1986, 1981, 1980, 1979, 1980, 1980, 1980, 1981,...
    ## $ remaining_lease     <dbl> 70, 65, 64, 63, 64, 64, 64, 65, 62, 69, 60, 64,...
    ## $ resale_price        <dbl> 255000, 275000, 285000, 290000, 290000, 290000,...

``` r
saveRDS(sales, here::here("data/sales.rds"))
```
