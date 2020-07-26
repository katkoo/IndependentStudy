R Notebook
================

``` r
# Use tidyverse staples filter, arrange, group_by, and summarize. 
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
library(here)
```

    ## here() starts at C:/Users/Kateryna/Documents/2020/IndependentStudy/Rstudio

``` r
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
sales <- readRDS(here::here("data/sales.rds"))

"1. What is the earlier lease commencement date and where are these flats located?"
```

    ## [1] "1. What is the earlier lease commencement date and where are these flats located?"

``` r
ans1 <- sales %>% 
  filter(lease_commence_date == min(sales$lease_commence_date) ) %>%
  group_by(town) %>% 
  summarise_at(vars(lease_commence_date),funs(mean(.,na.rm=TRUE))) %>% 
  select(matches("town") | matches("lease_commence_date"))
```

    ## Warning: `funs()` is deprecated as of dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
toString(ans1)
```

    ## [1] "JURONG EAST, 1966"

``` r
"2. What are the largest HDB flats in Singapore? How much did they sell for?"
```

    ## [1] "2. What are the largest HDB flats in Singapore? How much did they sell for?"

``` r
ans2 <- sales %>% 
  filter(floor_area_sqm == max(sales$floor_area_sqm) ) %>% 
  select(matches("town") | matches( "resale_price"))
toString(ans2)
```

    ## [1] "KALLANG/WHAMPOA, 1060000"

``` r
"3. What is the most expensive flat in Punggol?"
```

    ## [1] "3. What is the most expensive flat in Punggol?"

``` r
ans3 <- sales %>% 
  filter(town == "PUNGGOL") %>% 
  summarize(stat = max(resale_price))
toString(ans3)
```

    ## [1] "870000"

``` r
# to verify:
ans3_chk <- sales %>% 
  filter(town == "PUNGGOL") %>% 
  arrange(-resale_price) 
  # %>% View()
```

``` r
"4. Which town has, on average, the largest flats (by floor area)?"
```

    ## [1] "4. Which town has, on average, the largest flats (by floor area)?"

``` r
ans4 <- sales %>% 
  group_by(town) %>% summarise_at(vars(floor_area_sqm),funs(mean(.,na.rm=TRUE))) %>%
  filter( floor_area_sqm == sales %>% group_by(town) %>% summarise_at(vars(floor_area_sqm),funs(mean(.,na.rm=TRUE))) %>% select(matches("floor_area_sqm")) %>% max() ) %>% 
  select(matches("town") | matches( "floor_area_sqm"))
toString(ans4) 
```

    ## [1] "PASIR RIS, 123.090423345664"

``` r
"5. Which town has, on average, the cheapest flats per square meter?"
```

    ## [1] "5. Which town has, on average, the cheapest flats per square meter?"

``` r
sales <- transform(sales, price_per_sqm = ifelse(resale_price==floor_area_sqm, resale_price/floor_area_sqm, resale_price/floor_area_sqm))
ans5 <- sales %>% 
  group_by(town) %>% summarise_at(vars(price_per_sqm),funs(mean(.,na.rm=TRUE))) %>%
  filter( price_per_sqm == sales %>% group_by(town) %>% summarise_at(vars(price_per_sqm),funs(mean(.,na.rm=TRUE))) %>% select(matches("price_per_sqm")) %>% min() ) %>% 
  select(matches("town") | matches( "price_per_sqm"))
toString(ans5) 
```

    ## [1] "CHOA CHU KANG, 3529.02122737768"
