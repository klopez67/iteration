Writing Functions
================
Kimberly Lopez
2024-10-24

If you use the same code twice, you need a function – this will improve
code readability, facilitate troubleshooting, and reduce chances for
mistakes. This content looks at the best approaches for writing R
functions.

# Example

**setting the seed so that the output on this page is fixed**

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
set.seed(1)
```

## My First Function

Generating random numbers from the normal distribution and finidng the
standard deviation.

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

To create a function to do this we create a new function that takes the
sample as an argument, computes the vector of Z scores in the body, and
returns the result.

- x is the generic input of the sample
- can be explicit and state to return(z)

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  z
  
}

z_scores(x=x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

Checking to see if it works:

``` r
z_scores(x=3)
```

    ## [1] NA

To update the code so it works always:

- set a condition if x is numeric, and a minimum of 1 vector to compute
  the SD

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = (x- mean(x)) / sd(x)
  
  return(z)
}
```

## Multiple Outputs

In some cases it might be better to return the mean and standard
deviation instead of the Z scores. A first option is to store each of
the values in a named list, and to return that list.

- creating a new mean and standard deviation
- saves mean and sd in a tibble dataframe and returns if

``` r
mean_and_sd= function(x){
  mean_x=mean(x)
  sd_x=sd(x)
  out_df = tibble(mean= mean_x,
                  sd= sd_x)
  return(out_df)
}
```

Apply to our vector x_vec

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.51  2.85

## Checking code using simulation

``` r
sim_df = 
  tibble(
    x=rnorm(30,10,5)
    )

sim_df|>
  summarize(
    mean= mean(x),
    sd= sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.2  3.71

\*\* **Creating a simulation function to check sample mean and sd:**
\*\*

We want a function to take inputs:

- sample size n
- mean mu
- standard deviation sigma
- check code by running n = 30, mu= 4, sigma=12

``` r
sim_mean_sd = function(n, mu, sigma ) {
  
  sim_df = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  out_df=
    sim_df |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
  
  return(out_df)
}

sim_mean_sd( n=30,mu=4, sigma=12)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   5.03      12.4
