
<!-- README.md is generated from README.Rmd. Please edit that file -->

# autumn: Fast, Modern, and Tidy Raking <img src="man/figures/autumn.png" align="right" width="120" />

> *“And as to me, I know nothing else but miracles”* - Walt Whitman,
> probably talking about this package.

[![Travis-CI build
status](https://travis-ci.org/r-lib/pkgdown.svg?branch=master)](https://travis-ci.org/aaronrudkin/autumn)
[![Coverage
Status](https://coveralls.io/repos/github/aaronrudkin/autumn/badge.svg?branch=master)](https://coveralls.io/github/aaronrudkin/autumn?branch=master)

Iterative proportional fitting (raking) is a straightforward and fast
way to generate weights which ensure a dataset reflects known target
marginal distributions: put simply, survey professionals use raking to
ensure that samples represent the population they are drawn from.

Existing `R` implementations of raking are frustrating to use, have
antiquated syntax, require external dependencies or compilation, have
inadequate documentation, generate difficult to understand errors, run
slowly, and don’t support “tidy” workflows. **autumn** is a modern
package built from the ground up to fix these problems.

## Installation

**autumn** will be submitted to CRAN in ~January 2020~ June 2020. In the meantime,
you can install it using the following command:

``` r
# Install GitHub version:
devtools::install_github("aaronrudkin/autumn")
```

## Usage

The workhorse function of **autumn** is `harvest()`, which takes at
minimum two arguments: 1) a data.frame (or tibble) containing data; 2)
target proportions. At its simplest, a call to `harvest()` works as
follows:

``` r
# Standard R function call
harvest(respondent_data, ns_target)

# Using `magrittr`'s pipe operator
respondent_data %>% harvest(ns_target)
```

It just works\! This function call will iteratively weight observations
to match the target proportions and add a column `weights` to the data
frame (it is also possible to rename the column or return the weights as
a vector). Default parameters are helpful and sane: weights are
guaranteed mean 1 and maximum 5.

### Specifying a Target

The main challenge when running `harvest()` is to correctly specify
target proportions. Two formats are supported: 1) a list of named
vectors; 2) a data.frame or tibble.

When supplying targets as a list of named vectors, it looks like this:

``` r
list(
  gender = c(Male = 0.4829, Female = 0.5171), 
  region = c(Midwest = 0.2086, 
             Northeast = 0.1764, 
             South = 0.3775, 
             West = 0.2374)
)
```

Each list element should match the name of a single variable in the
data, and each vector name should match a value the variable can take.
The numeric values should be positive and sum to 1 within each variable.

When supplying data as a data.frame or tibble, the data.frame should
have three columns (by default `harvest()` looks for columns named
“variable”, “level”, and “proportion” – although these names can be
overridden):

``` r
target_tbl
#> # A tibble: 6 x 3
#>   variable level     proportion
#>   <chr>    <chr>          <dbl>
#> 1 gender   Male           0.483
#> 2 gender   Female         0.517
#> 3 region   Midwest        0.209
#> 4 region   Northeast      0.176
#> 5 region   South          0.378
#> 6 region   West           0.237
```

### Advanced Usage

**autumn** supports a variety of advanced features including:

  - Supplying starting weights
  - Adjusting maximum weights
  - Adjusting convergence and iteration criteria
  - Adjusting variable selection and error calculation criteria
  - Handling missing data appropriately
  - Calculating design effects for produced weights
  - Summarizing raking results

Interested in doing something fancy? Check out our R vignettes for more
details: TODO VIGNETTES GO HERE

## Speed 🚀

How fast is **autumn**? Fast.

Below, we present results of three different benchmark scenarios, each
using real data (the first two benchmarks use the `respondent_data` and
`ns_target` datasets included with **autumn**). All of these benchmarks
use identical data and default parameterizations, and were run on a low
power 2016-vintage personal computer. The larger the the dataset and the
more complicated the rake, the more you benefit from using **autumn**.
Customizing convergence criteria to allow for earlier termination can
result in further speed improvements over existing software.

*Note:*

### Small scale

This benchmark generates weights for a dataset of 6,691 observations,
raking on 10 variables. Compared with the implementation in
**anesrake**, **autumn** is about *67% faster* and allocates one third
less memory. Compared with the implementation in **survey**, **autumn**
is about *4X as fast* and allocates 20% more memory.

    #> # A tibble: 3 x 6
    #>   expression      min   median `itr/sec` mem_alloc `gc/sec`
    #>   <chr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    #> 1 autumn        1.35s    1.73s     0.572  738.35MB    3.01 
    #> 2 anesrake      2.46s    2.89s     0.315    1.11GB    2.42 
    #> 3 survey        4.57s    6.76s     0.148  614.84MB    0.866

### Medium scale

Consider a raking task that is more difficult to converge: the same
dataset (6,691 observations) raked on 17 variables. The extra variables
involve interactions which greatly complicate convergence. **autumn** is
*three times as fast* as **anesrake** and uses almost two thirds less
memory (**survey** will not complete the rake):

    #> # A tibble: 2 x 6
    #>   expression      min   median `itr/sec` mem_alloc `gc/sec`
    #>   <chr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    #> 1 autumn         2.3s    3.02s    0.327     1.22GB     6.58
    #> 2 anesrake      8.41s    9.99s    0.0945    3.11GB     4.77

### Large scale

Finally, consider an extremely resource intensive problem: raking a much
larger dataset of 108,660 observations on 17 variables. In this
scenario, **autumn** is 11 times faster and uses 92% less memory. (This
benchmark is limited to 10 iterations):

    #> # A tibble: 2 x 6
    #>   expression      min   median `itr/sec` mem_alloc `gc/sec`
    #>   <chr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    #> 1 autumn        47.1s    48.8s   0.0200     20.8GB     2.44
    #> 2 anesrake      8.15m     8.8m   0.00189   238.6GB     2.52

## Why is the package called “autumn”?

<p align="center">

<img src="man/figures/raking_leaves.jpg" align="center" width="480" />

</p>

## Authorship and Funding

**autumn** is written and maintained by [Aaron
Rudkin](https://github.com/aaronrudkin/). Target proportions in the
included `ns_target` data were developed by [Alex
Rossell-Hayes](https://github.com/rossellhayes).

If you have any comments, issues, or concerns, please [open a GitHub
issue](https://github.com/aaronrudkin/autumn/issues). Contributions are
welcome. Please see our [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md) for details.

**autumn** was developed in conjunction with [Democracy Fund + UCLA
Nationscape](https://www.voterstudygroup.org/nationscape), one of the
largest public opinion surveys ever conducted. UCLA’s Nationscape team
are: [Tyler Reny](http://tylerreny.github.io/), [Alex
Rossell-Hayes](http://alexander.rossellhayes.com/), [Aaron
Rudkin](https://github.com/aaronrudkin/), [Chris
Tausanovitch](http://www.ctausanovitch.com/), and [Lynn
Vavreck](https://www.lynnvavreck.com/). Funding for this project was
provided by [Democracy Fund](https://www.democracyfund.org/), part of
the [Omidyar Group](http://omidyargroup.com/).

![UCLA + Democracy Fund](man/figures/logo_ucla_demfund.png
"UCLA + Democracy Fund")

Package hex logo adapted from art by
[Freepik](https://www.flaticon.com/authors/Freepik) from
[flaticon.com](https://www.flaticon.com/)
