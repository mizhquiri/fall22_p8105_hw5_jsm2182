HW 5
================

## Problem 2

Load data

This dataset overviews demographic and geographic data for homicide
victims across various cities in the United States.

``` r
homicide_rawdf %>% 
  mutate(
    city_state = str_c(city,", ", state)
  ) %>% 
  distinct(city_state)
```

    ## # A tibble: 51 × 1
    ##    city_state     
    ##    <chr>          
    ##  1 Albuquerque, NM
    ##  2 Atlanta, GA    
    ##  3 Baltimore, MD  
    ##  4 Baton Rouge, LA
    ##  5 Birmingham, AL 
    ##  6 Boston, MA     
    ##  7 Buffalo, NY    
    ##  8 Charlotte, NC  
    ##  9 Chicago, IL    
    ## 10 Cincinnati, OH 
    ## # … with 41 more rows
