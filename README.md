
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multispat

<!-- badges: start -->
<!-- badges: end -->

The goal of multispat is to …

## Installation

You can install the development version of multispat from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("clsong/multispat")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.6     ✓ dplyr   1.0.7
#> ✓ tidyr   1.1.4     ✓ stringr 1.4.0
#> ✓ readr   2.1.1     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(multispat)

# set total number of species
gamma <- 3

# set species associations
spatial_association <- matrix(NA, nrow = gamma, ncol = gamma)
diag(spatial_association) <- 0
spatial_association[1, 2] <- -.8
spatial_association[1, 3] <- 0.7
spatial_association[2, 3] <- 0.7
spatial_association[lower.tri(spatial_association)] <- t(spatial_association)[lower.tri(spatial_association)]
 
# Visualize the spatial associations
plot_spatial_association(spatial_association)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
# Simulate the given point processes
df <- simulate_point_process(spatial_association, gamma, radius = .2)

# plot the spatial pattern
df %>%
  ggplot(aes(x, y, color = species)) +
  geom_point() +
  theme_minimal() +
  theme(
    aspect.ratio = 1
  )
```

<img src="man/figures/README-example-2.png" width="100%" />
