Lab 05 - Wrangling spatial data
================
Fiona Wang
Feb 19

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

Filter both data frames and only select Alaska.

``` r
dn <- dennys
dn_ak <- dn %>% 
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

There are 3 Denny’s locations in Alaska.

``` r
lq <- laquinta
lq_ak <- lq %>% 
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

There are 2 La Quinta locations in Alaska.

### Exercise 2

We want to calculate how many pairs there are.

``` r
npair <- nrow(dn_ak) * nrow(lq_ak)
npair
```

    ## [1] 6

There are 6 pairings between Denny’s and La Quinta locations in Alaska.

### Exercise 3

Let’s join the two data frames.

``` r
dn_lq_ak <- full_join(dn_ak, lq_ak,
                      by = "state")
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x     city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##   <chr>         <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ## 1 2900 Denali   Ancho… AK    99503       -150.       61.2 3501 Min… "\nAn… 99503
    ## 2 2900 Denali   Ancho… AK    99503       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 3 3850 Debarr … Ancho… AK    99508       -150.       61.2 3501 Min… "\nAn… 99503
    ## 4 3850 Debarr … Ancho… AK    99508       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 5 1929 Airport… Fairb… AK    99701       -148.       64.8 3501 Min… "\nAn… 99503
    ## 6 1929 Airport… Fairb… AK    99701       -148.       64.8 4920 Dal… "\nFa… 99709
    ## # ℹ 2 more variables: longitude.y <dbl>, latitude.y <dbl>

### Exercise 4

We would like to know how many observations there are in this newly
joined data frame, as well as the names of the variables.

``` r
nrow(dn_lq_ak)
```

    ## [1] 6

``` r
names(dn_lq_ak)
```

    ##  [1] "address.x"   "city.x"      "state"       "zip.x"       "longitude.x"
    ##  [6] "latitude.x"  "address.y"   "city.y"      "zip.y"       "longitude.y"
    ## [11] "latitude.y"

As the two data frames have the same variables, the names have .x or .y
to help distinguish.

### Exercise 5

Haversine

``` r
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  long1 <- long1 * pi / 180
  lat1 <- lat1 * pi / 180
  long2 <- long2 * pi / 180
  lat2 <- lat2 * pi / 180
  R <- 6371
  a <- sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1) / 2)^2
  d <- R * 2 * asin(sqrt(a))
  return(round(d, round))
}
```

### Exercise 6

Calculate the distance in each pair.

``` r
dn_lq_ak <- dn_lq_ak %>% 
  mutate(distance = haversine(
    longitude.x, latitude.x, longitude.y, latitude.y, round = 3
  ))
```

Interestingly, I looked up Google map to see if this calculation is
correct. However, the distance on google map is always less than what is
calculated here. I think it is because we are calculating the sphere, so
it’s larger.

### Exercise 7

Find the minimum distance between a Denny’s and La Quinta for each
Denny’s location.

``` r
dn_lq_ak_mindist <- dn_lq_ak %>% 
  group_by(address.x) %>% 
  summarize(closest = min(distance))
print(dn_lq_ak_mindist)
```

    ## # A tibble: 3 × 2
    ##   address.x        closest
    ##   <chr>              <dbl>
    ## 1 1929 Airport Way    5.20
    ## 2 2900 Denali         2.04
    ## 3 3850 Debarr Road    6.00

I realized that these distances are rounded to second.

### Exercise 8
