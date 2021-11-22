
<!-- README.md is generated from README.Rmd. Please edit that file -->

# anthroplus

<!-- badges: start -->

[![R-CMD-check](https://github.com/WorldHealthOrganization/anthroplus/workflows/R-CMD-check/badge.svg)](https://github.com/WorldHealthOrganization/anthroplus/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/anthroplus)](https://CRAN.R-project.org/package=anthroplus)
<!-- badges: end -->

The goal of `anthroplus` is to provide R functions for the application
of the WHO Reference 2007 for 5-19 years to monitor the growth of
school-age children and adolescents.

It is modeled after the [R Macros of the WHO Reference
2007](https://www.who.int/tools/growth-reference-data-for-5to19-years/application-tools).

## Installation

You can install the released version of anthroplus from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("anthroplus")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("worldhealthorganization/anthroplus")
```

## Example

### Z-scores

This function calculates z-scores for the three anthropometric
indicators, weight-for-age, height-for-age and body mass index
(BMI)-for-age.

``` r
library(anthroplus)
anthroplus_zscores(
  sex = c("1", "f"),
  age_in_months = c(100, 110),
  height_in_cm = c(100, 90),
  weight_in_kg = c(30, 40)
)
#>   age_in_months csex coedema     cbmi  zhfa zwfa zbfa fhfa fwfa fbfa
#> 1           100    1       n 30.00000 -5.04 0.87 5.03    0    0    1
#> 2           110    2       n 49.38272 -7.06 1.78 7.37    1    0    1
```

The returned value is a `data.frame` that can further be processed or
saved as a `.csv` file.

You can also use the function with a given dataset with `with`

``` r
your_data_set <- read.csv("my_survey.csv")
with(
  your_data_set,
  anthroplus_zscores(
    sex = sex_column, age_in_months = age_column,
    weight_in_kg = weight_column, height_in_cm = height_column,
    oedema = oedema_column
  )
)
```

### Prevalence estimates

The function to compute the prevalence estimates is similar to
`anthroplus_zscores` in terms of the parameters.

``` r
set.seed(1)
anthroplus_prevalence(
  sex = c(1, 2),
  age_in_months = rpois(100, 100),
  height_in_cm = rnorm(100, 100, 10),
  weight_in_kg = rnorm(100, 40, 10)
)[, c(1, 4, 5, 6)]
#>                           Group HAZ_pop HAZ_unwpop   HA_3_r
#> 1                           All      64         64  79.6875
#> 2                   Sex: Female      32         32  81.2500
#> 3                     Sex: Male      32         32  78.1250
#> 4         Age Group 1: 61-71 mo       0          0       NA
#> 5         Age Group 1: 72-83 mo       2          2   0.0000
#> 6         Age Group 1: 84-95 mo      16         16  75.0000
#> 7        Age Group 1: 96-107 mo      35         35  80.0000
#> 8       Age Group 1: 108-119 mo      11         11 100.0000
#> 9       Age Group 1: 120-131 mo       0          0       NA
#> 10      Age Group 1: 132-143 mo       0          0       NA
#> 11      Age Group 1: 144-155 mo       0          0       NA
#> 12      Age Group 1: 156-167 mo       0          0       NA
#> 13      Age Group 1: 168-179 mo       0          0       NA
#> 14      Age Group 1: 180-191 mo       0          0       NA
#> 15      Age Group 1: 192-203 mo       0          0       NA
#> 16      Age Group 1: 204-215 mo       0          0       NA
#> 17      Age Group 1: 216-227 mo       0          0       NA
#> 18      Age Group 1: 228-228 mo       0          0       NA
#> 19       Age Group 2: 61-119 mo      64         64  79.6875
#> 20      Age Group 2: 120-179 mo       0          0       NA
#> 21      Age Group 2: 180-228 mo       0          0       NA
#> 22  Age + Sex: Female.61-119 mo      32         32  81.2500
#> 23    Age + Sex: Male.61-119 mo      32         32  78.1250
#> 24 Age + Sex: Female.120-179 mo       0          0       NA
#> 25   Age + Sex: Male.120-179 mo       0          0       NA
#> 26 Age + Sex: Female.180-228 mo       0          0       NA
#> 27   Age + Sex: Male.180-228 mo       0          0       NA
```

Using the function `with` it is easy to apply `anthroplus_prevalence` to
a full dataset.

To look at all parameters, type `?anthroplus_prevalence`.

### Contributions

Contributions in the form of issues are very welcome. In particular if
you find any bugs or cannot reproduce results obtained with other
implementations.
