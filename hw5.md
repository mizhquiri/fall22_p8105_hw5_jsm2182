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

head(homicide_city_state_data)
```

    ## # A tibble: 6 × 3
    ##   city_state      n_obs n_obs_unsolved
    ##   <chr>           <int>          <int>
    ## 1 Albuquerque, NM   378            146
    ## 2 Atlanta, GA       973            373
    ## 3 Baltimore, MD    2827           1825
    ## 4 Baton Rouge, LA   424            196
    ## 5 Birmingham, AL    800            347
    ## 6 Boston, MA        614            310

*Baltimore 1 - sample Proportion of Unsolved Homicides to All Homicides*

``` r
  baltimore_homicides = 
  homicide_city_state_data %>% 
  filter(
    city_state == "Baltimore, MD"
  ) 


  prop.test(baltimore_homicides %>% pull(n_obs_unsolved), baltimore_homicides %>% pull(n_obs)) %>% 
  broom::tidy() %>% 
  select(
    estimate, conf.low, conf.high
  )
```

    ## # A tibble: 1 × 3
    ##   estimate conf.low conf.high
    ##      <dbl>    <dbl>     <dbl>
    ## 1    0.646    0.628     0.663

``` r
  prop.test(baltimore_homicides %>% pull(n_obs_unsolved), baltimore_homicides %>% pull(n_obs)) %>% 
  broom::tidy() %>% 
  select(
    estimate, conf.low, conf.high
  )
```

    ## # A tibble: 1 × 3
    ##   estimate conf.low conf.high
    ##      <dbl>    <dbl>     <dbl>
    ## 1    0.646    0.628     0.663

We are 95% confident that the true proportion unsolved homicide rates in
Baltimore is between 0.627 and 0.663.

1.  Create a function that computes estimate, conf.low, conf.high

%\>% version

``` r
prop_output = function(df) {
  
data = df
  prop.test(x = df %>% pull(n_obs_unsolved), n = df %>% pull(n_obs)) %>% 
    broom::tidy() %>% 
    select(
      estimate, conf.low, conf.high
    )
  

}
```

2.  map the data

VERSION 1

Using prop custom function

``` r
map(homicide_city_state_data, prop_output)
```

``` r
map2(x = homicide_city_state_data %>% n_obs_unsolved, y =  homicide_city_state_data %>% n_obs, ~prop_output(homicide_city_state_data))
```

Using basic prop.test

``` r
homicide_city_state_data %>% 
  mutate(
    proportion_test = purrr::map2( x = homicide_city_state_data %>% pull(n_obs_unsolved), y = homicide_city_state_data %>% pull(n_obs), .f = ~prop.test(x = .x, n = .y))
    ) 
```

``` r
homicide_city_state_data %>% 
  mutate(
    prop = map2(.x = n_obs_unsolved, .y = n_obs, ~prop_test_output(x = .x, n = .y))
  )
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

## Problem 3

VERSION 1: looks up to date

``` r
sim_mean_sd = function(mu) {

   x = rnorm(n = 30, mean = mu, sd = 5) 
   
  output = t.test(x) %>% broom::tidy()
  
 }

a = sim_mean_sd(0)
a
```

    ## # A tibble: 1 × 8
    ##   estimate statistic p.value parameter conf.low conf.high method         alter…¹
    ##      <dbl>     <dbl>   <dbl>     <dbl>    <dbl>     <dbl> <chr>          <chr>  
    ## 1    0.412     0.489   0.629        29    -1.31      2.14 One Sample t-… two.si…
    ## # … with abbreviated variable name ¹​alternative

``` r
sim_results_df = 
  expand_grid(
    iter = 1:10,
    mean = c(1,2,3,4,5,6)) %>% 
  mutate(
    estimate_df = 
      map(mean, sim_mean_sd)
  ) %>% unnest(estimate_df) %>% 
  select(p.value, estimate)

sim_results_df
```

    ## # A tibble: 60 × 2
    ##        p.value estimate
    ##          <dbl>    <dbl>
    ##  1 0.0294         1.66 
    ##  2 0.00687        2.55 
    ##  3 0.000124       3.57 
    ##  4 0.00924        2.35 
    ##  5 0.000000508    6.19 
    ##  6 0.000000540    6.33 
    ##  7 0.839         -0.190
    ##  8 0.0184         2.12 
    ##  9 0.000493       3.68 
    ## 10 0.00000216     5.09 
    ## # … with 50 more rows

``` r
sim_results_df = 
  expand_grid(
    sample_size = c(30, 60, 120, 240),
    true_sd = c(6, 3),
    iter = 1:1000
  ) %>% 
  mutate(
    estimate_df = 
      map2(.x = sample_size, .y = true_sd, ~sim_mean_sd(n = .x, sigma = .y))
  ) %>% 
  unnest(estimate_df)
```
