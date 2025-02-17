Block 3: Descriptive statistics and visualisations
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ---------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(here)
```

    ## here() starts at C:/Users/Kateryna/Documents/2020/IndependentStudy/Rstudio

``` r
library(e1071)
library(skimr)
#library(lubridate)
#library(forcats)
#library(ggplot2)
sales <- readRDS(here::here("data/sales.rds"))
```

## Chapter3: 2 different types of descriptive statistics: measures of the central tendency and measures of dispersion.

<https://02522-cua.github.io/lecturenotes/descriptive-statistics-and-visualisations.html#assignment-monday-february-17th-2359>

### 3.2 Central Tendency

``` r
mean(sales$floor_area_sqm)
```

    ## [1] 97.58903

``` r
median(sales$floor_area_sqm)
```

    ## [1] 96

``` r
# from https://stackoverflow.com/a/25635740
manual_mode <- function(x, na.rm = FALSE) { # we don't use 'mode' as a function name because it already exists
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

manual_mode(sales$floor_area_sqm)
```

    ## [1] 67

### 3.3 Dispersion

``` r
"Range"
```

    ## [1] "Range"

``` r
max(sales$floor_area_sqm) - min(sales$floor_area_sqm)
```

    ## [1] 249

``` r
"Interquartile Range"
```

    ## [1] "Interquartile Range"

``` r
IQR(sales$floor_area_sqm)
```

    ## [1] 36

``` r
"Standard Deviation"
```

    ## [1] "Standard Deviation"

``` r
sd(sales$floor_area_sqm)
```

    ## [1] 24.22276

``` r
"Coefficient of variation"
```

    ## [1] "Coefficient of variation"

``` r
sd(sales$floor_area_sqm) / mean(sales$floor_area_sqm)
```

    ## [1] 0.2482119

``` r
"Kurtosis and Skewness from the 'e1071` library"
```

    ## [1] "Kurtosis and Skewness from the 'e1071` library"

``` r
kurtosis(sales$floor_area_sqm)
```

    ## [1] -0.1450646

``` r
skewness(sales$floor_area_sqm)
```

    ## [1] 0.2770161

``` r
"Summary"
```

    ## [1] "Summary"

``` r
summary(sales$floor_area_sqm)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   31.00   76.00   96.00   97.59  112.00  280.00

``` r
"Skim"
```

    ## [1] "Skim"

``` r
skim(sales$floor_area_sqm)
```

|                                                  |                        |
| :----------------------------------------------- | :--------------------- |
| Name                                             | sales$floor\_area\_sqm |
| Number of rows                                   | 79100                  |
| Number of columns                                | 1                      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                        |
| Column type frequency:                           |                        |
| numeric                                          | 1                      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                        |
| Group variables                                  | None                   |

Data summary

**Variable type:
numeric**

| skim\_variable | n\_missing | complete\_rate |  mean |    sd | p0 | p25 | p50 | p75 | p100 | hist  |
| :------------- | ---------: | -------------: | ----: | ----: | -: | --: | --: | --: | ---: | :---- |
| data           |          0 |              1 | 97.59 | 24.22 | 31 |  76 |  96 | 112 |  280 | ▃▇▁▁▁ |

### 3.4 Visualization

``` r
ggplot(sales, aes(x = floor_area_sqm)) +
  geom_histogram(binwidth = 5)
```

![](Block3_notes_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(sales, aes(x = floor_area_sqm)) +
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  stat_function(fun = dnorm, args = list(mean = mean(sales$floor_area_sqm), sd = sd(sales$floor_area_sqm)))
```

![](Block3_notes_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
sales %>%
  filter(floor_area_sqm == 67) 
```

    ## # A tibble: 4,904 x 11
    ##    month      town  flat_type block street_name storey_range floor_area_sqm
    ##    <date>     <chr> <fct>     <chr> <chr>       <fct>                 <dbl>
    ##  1 2015-01-01 ANG ~ 3 ROOM    603   ANG MO KIO~ 07 TO 09                 67
    ##  2 2015-01-01 ANG ~ 3 ROOM    109   ANG MO KIO~ 01 TO 03                 67
    ##  3 2015-01-01 ANG ~ 3 ROOM    218   ANG MO KIO~ 07 TO 09                 67
    ##  4 2015-01-01 ANG ~ 3 ROOM    471   ANG MO KIO~ 07 TO 09                 67
    ##  5 2015-01-01 ANG ~ 3 ROOM    434   ANG MO KIO~ 07 TO 09                 67
    ##  6 2015-01-01 ANG ~ 3 ROOM    560   ANG MO KIO~ 07 TO 09                 67
    ##  7 2015-01-01 ANG ~ 3 ROOM    631   ANG MO KIO~ 07 TO 09                 67
    ##  8 2015-01-01 ANG ~ 3 ROOM    442   ANG MO KIO~ 10 TO 12                 67
    ##  9 2015-01-01 ANG ~ 3 ROOM    558   ANG MO KIO~ 10 TO 12                 67
    ## 10 2015-01-01 ANG ~ 3 ROOM    212   ANG MO KIO~ 10 TO 12                 67
    ## # ... with 4,894 more rows, and 4 more variables: flat_model <fct>,
    ## #   lease_commence_date <dbl>, remaining_lease <dbl>, resale_price <dbl>

``` r
  #%>% View()
```

``` r
ggplot(sales, aes(x = 1, y = floor_area_sqm)) +
  geom_boxplot()
```

![](Block3_notes_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(sales, aes(x = 1, y = floor_area_sqm)) +
  geom_violin()
```

![](Block3_notes_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(sales, aes(x = floor_area_sqm)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(vars(flat_type), scales = "free_y")
```

![](Block3_notes_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(sales, aes(x = flat_type, y = floor_area_sqm)) +
  geom_violin()
```

![](Block3_notes_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
