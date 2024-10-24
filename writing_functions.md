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

**You can set paramaters equal to default values by setting function
(n=30,mu=2,sigma=2) but you can always override it by putting different
values in when you run the code on inputs**

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

# Revisiting past examples: LOTR DATA

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything()) 
```

**Write a function that can be used to abstract the data loading and
cleaning process. Use this function to recreate the tidied LoTR
dataset**

–\> if we start from loading dataset, the input values have to be path,
range, and movie_name

–\> body of the function should tidy the data clean, pivot, mutate

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
  
  movie_df = 
    readxl::read_excel(path, range = range) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words") |>
    mutate(
      race = str_to_lower(race),
      movie = movie_name) |> 
    select(movie, everything())
  
  return(movie_df)
  
}
```

To check apply the function to create **lotr_tidy** dataframe by binding
the output of the functions

``` r
lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("data/LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "J3:L6", "return_king"))

head(lotr_tidy)
```

    ## # A tibble: 6 × 4
    ##   movie           race   sex    words
    ##   <chr>           <chr>  <chr>  <dbl>
    ## 1 fellowship_ring elf    female  1229
    ## 2 fellowship_ring elf    male     971
    ## 3 fellowship_ring hobbit female    14
    ## 4 fellowship_ring hobbit male    3644
    ## 5 fellowship_ring man    female     0
    ## 6 fellowship_ring man    male    1995

## \# Revisiting past examples: NSDUH Data

This is a reading data from the web code that scrapes the information
for us.

We could take the first table “nth(1)” by: **read_html(html)**

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

We can also create a function that does the data import for us:

–\> takes the input html for the link –\> take the input table_num that
we put into “nth()” to ddenote which table from the link we want –\>
takes the input “table_name” to know which

``` r
nsduh_table <- function(html, table_num, table_name) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
  
}
```

``` r
nsduh_results = 
  bind_rows(
    nsduh_table(nsduh_html, 1, "marj_one_year"),
    nsduh_table(nsduh_html, 4, "cocaine_one_year"),
    nsduh_table(nsduh_html, 5, "heroin_one_year")
  )

head(nsduh_results)
```

    ## # A tibble: 6 × 5
    ##   State   age   year      percent name         
    ##   <chr>   <chr> <chr>       <dbl> <chr>        
    ## 1 Alabama 12+   2013-2014    9.98 marj_one_year
    ## 2 Alabama 12+   2014-2015    9.6  marj_one_year
    ## 3 Alabama 12-17 2013-2014    9.9  marj_one_year
    ## 4 Alabama 12-17 2014-2015    9.71 marj_one_year
    ## 5 Alabama 18-25 2013-2014   27.0  marj_one_year
    ## 6 Alabama 18-25 2014-2015   26.1  marj_one_year

**We do not want to put the html link within our function because it
will constantly download the data from the internet multiple times, so
name this outside the functions**

We can save this nsduh function into a r script paste it there save that
into a “source folder” and then call on the function whenevre in this
project and it does the same thing.

``` r
source("source/nsduh_table_format.R")

bind_rows(
    nsduh_table(nsduh_html, 1, "marj_one_year"),
    nsduh_table(nsduh_html, 4, "cocaine_one_year"),
    nsduh_table(nsduh_html, 5, "heroin_one_year")
  )
```

    ## # A tibble: 1,530 × 5
    ##    State   age   year      percent name         
    ##    <chr>   <chr> <chr>       <dbl> <chr>        
    ##  1 Alabama 12+   2013-2014    9.98 marj_one_year
    ##  2 Alabama 12+   2014-2015    9.6  marj_one_year
    ##  3 Alabama 12-17 2013-2014    9.9  marj_one_year
    ##  4 Alabama 12-17 2014-2015    9.71 marj_one_year
    ##  5 Alabama 18-25 2013-2014   27.0  marj_one_year
    ##  6 Alabama 18-25 2014-2015   26.1  marj_one_year
    ##  7 Alabama 26+   2013-2014    7.1  marj_one_year
    ##  8 Alabama 26+   2014-2015    6.81 marj_one_year
    ##  9 Alabama 18+   2013-2014    9.99 marj_one_year
    ## 10 Alabama 18+   2014-2015    9.59 marj_one_year
    ## # ℹ 1,520 more rows
