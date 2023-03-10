DLUHC style for R graphics
================
Last updated: 2023-02-08

<!-- README.md is generated from README.Rmd. Please edit that file -->

### Credit to BBC data team

This package has been copied from the much better BBC bbplot, and
reapplied to work for creating graphics in a DLUHC style. Credit must go
to the BBC data team for their fantastic product which the team at DLUHC
have essentially copied for our own use.

### Install the dluhctheme package

`dluhctheme` is not on CRAN, so you will have to install it directly
from Github using `devtools`.

If you do not have the `devtools` package installed, you will have to
run the first line in the code below as well.

``` r
# install.packages('devtools')
devtools::install_github('communitiesuk/dluhctheme')
```

For more info on `dluhctheme` check out the [package’s Github
repo](https://github.com/communitiesuk/dluhctheme), but most of the
details about how to use the package and its functions are detailed
below.

When you have downloaded the package and successfully installed it you
are good to go and create charts.

### How does the dluctheme package work?

The package has two types of functions;

-   Generic functions: `dluhc_style()` and `finalise_plot()` are
    functions which are designed to be generic themes which can be added
    to any ggplot2 object. They only edit the axes and the grid lines
    and define the text style, they do not set the colour of lines or
    fill on the graphs.

-   Specific functions: The other functions are used as a quick tool for
    instantly creating a particular type of formatted graph based on a
    set shape of data inputted. These are less adaptable, but allow a
    user to create a graph in one line of code: `one_line_timeseries()`,
    `multi_line_timeseries()`, `facet_highlight_timeseries()`,
    `LA_map()`, `facet_barchart()`, `forecast_timeseries()`

These functions allow you to create a data visualization in a set theme
instantly but are more fixed in some of their formatting. Examples of
these functions can all be found in the “Reference” tab at the top of
this page
