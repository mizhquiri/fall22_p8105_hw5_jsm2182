HW 5
================

## Problem 2

Load data

This dataset overviews demographic and geographic data for homicide
victims across various cities in the United States.

``` r
homicide_cleandf = 
  homicide_rawdf %>% 
  mutate(
    city_state = str_c(city,", ", state)
  )
```

``` r
all_homicides = 
  homicide_cleandf %>% 
  group_by(city_state) %>% 
  summarize(n_obs = n())
```

``` r
unsolved_homicides = 
  homicide_cleandf %>% 
  group_by(city_state) %>% 
  filter(
    disposition != "Closed by arrest"
  ) %>% 
  summarize(
    n_obs_unsolved = n()
  )
```

``` r
homicide_city_state_data = 
  left_join(all_homicides, unsolved_homicides, by = "city_state")
```

``` r
prop.test(5, 10) %>% 
  broom::tidy() %>% 
  select(
    estimate, conf.low, conf.high
  )
```

    ## # A tibble: 1 × 3
    ##   estimate conf.low conf.high
    ##      <dbl>    <dbl>     <dbl>
    ## 1      0.5    0.237     0.763

``` r
sample = rnorm(30, mean = 0)

test_results = t.test(sample) %>% 
  broom::tidy()
```

``` r
sim_t_test = function(true_mean) {
  sample = rnorm(30, mean = 0)
  
  test_results = t.test(sample) %>% 
  broom::tidy()
}

expand_grid(
  true_mean = 0:6, 
  iter = 1:5
)
```

    ## # A tibble: 35 × 2
    ##    true_mean  iter
    ##        <int> <int>
    ##  1         0     1
    ##  2         0     2
    ##  3         0     3
    ##  4         0     4
    ##  5         0     5
    ##  6         1     1
    ##  7         1     2
    ##  8         1     3
    ##  9         1     4
    ## 10         1     5
    ## # … with 25 more rows

Above, diff sample/diff sample mean but consistent true mean which is
the input

Once you have the plot, it’s all after you run the simulation group_by +
summarize for the true mean & summarize how often the null was rejected

group_by averages

If all you see are results where p\< 0.5 –\> publication bias; + low
power You end up seeing results which are different than true
