openmindR
================

Install package like this:

``` r
devtools::install_github("favstats/openmindR")
```

Load package:

``` r
library(openmindR)
```

## Openmind ggplot2 theme

There are three functions for the ggplot2 theme:

  - `theme_om`
  - `scale_fill_om`
  - `scale_color_om`

Make sure you have the Poppins font installed\!

``` r
windowsFonts(`Poppins` = windowsFont("Poppins"))
```

[Good tutorial on how install custom fonts in
R](https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/)

**Example**

``` r
## Load tidyverse
library(tidyverse)
```

    ## -- Attaching packages ------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.3.0
    ## v tibble  2.0.1     v dplyr   0.8.0
    ## v tidyr   0.8.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.3.0

    ## -- Conflicts ---------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
titanic_dat <- Titanic %>% as_tibble()

titanic_dat %>% 
  ggplot(aes(Class, n, fill = Class)) +
  geom_col() +
  theme_om(legend_position = c(0.9, 0.75)) +
  scale_fill_om() +
  facet_wrap(~Survived)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

**Adapt `theme_om`**
