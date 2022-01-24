# multispat

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of multispat like so:

``` r
remotes::install_github("clsong/multispat")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyverse)
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
#  spatial_association %>%
#    plot_spatial_association(plot_type = "ggraph")

# initialize species locations
 num_initial <- 2
 species_location <- 1:gamma %>%
   map(~ matrix(runif(2 * num_initial, min = 0, max = 1),
                nrow = num_initial, ncol = 2
   ))
 names(species_location) <- LETTERS[1:gamma]
 for (i in 1:length(species_location)) {
   colnames(species_location[[i]]) <- c("x", "y")
 }

# simulation parameters
 num_candidates <- 1000
 radius <- .2
 
# Number of simualted points
 Nsteps <- 50
 for (i in 1:Nsteps) {
   invader <- i %% gamma + 1

   winner <- sample_spatial_point(
     species_location,
     spatial_association,
     gamma,
     invading_species = invader, 
     num_candidates, 
     radius
   )
   species_location[[invader]] <- species_location[[invader]] %>%
     rbind(winner)
 }

 df <- species_location %>%
   map(as_tibble) %>%
   bind_rows(.id = "species")

 # plot
 df %>%
   ggplot(aes(x, y, color = species)) +
   geom_point() +
   theme(
     aspect.ratio = 1
   )
```

