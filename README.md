
# Manyenviron <img src="man/figures/manyenviron_hexlogo.png" align="right" width="220"/>

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![GitHub release (latest by
date)](https://img.shields.io/github/v/release/globalgov/manyenviron)
![GitHub Release
Date](https://img.shields.io/github/release-date/globalgov/manyenviron)
<!-- badges: end -->

Manyenviron is a data package in the
[qData](https://github.com/globalgov/qData) universe of many packages.
It currently includes an ensemble of datasets on international
environmental agreements, and
[states](https://github.com/globalgov/qStates)’ membership or other
relationships to those agreements.

Please also check out [`{qData}`](https://github.com/globalgov) for more
information about the other packages in the `{qData}` ecosystem.

## How to install

We’ve made it easier than ever to install and start analysing global
governance data in R. Simply install the core package,
[qData](https://github.com/globalgov/qData), as follows, and then you
can discover, install and update various “many” packages from the
console.

``` r
# install.packages(remotes)
remotes::install_github("globalgov/qData") # this installs our core package, the only one you need to do independently
qData::get_packages() # this prints a list of the publicly available data packages currently available
qData::get_packages("manyenviron") # this downloads and installs the named package
```

## Data included

Once you have installed the package, you can see the different databases
and datasets included in the `{manyenviron}` package using the following
function.

``` r
qData::data_contrast("manyenviron")
```

Working with an ensemble of related data has many advantages for robust
analysis. Just take a look at our vignettes
[here](https://globalgov.github.io/qData/articles/user.html).

## Many Packages

The [`{qData}`](https://github.com/globalgov/qData) ecosystem is aimed
at collecting, connecting and correcting network data across
issue-domains of global governance.

While some many packages can and do include novel data, much of what
they offer involves standing on the shoulders of giants. Many packages
endeavour to be as transparent as possible about where data comes from,
how it has been coded and/or relabeled, and who has done the work. As
such, we make it easy to cite both the particular datasets you use by
listing the official references in the function above, as well as the
package providers for their work assembling the data by using the
function below.

``` r
citation("manyenviron")
```

    ## 
    ## To cite manyenviron in publications use:
    ## 
    ##   J. Hollway. Environmental agreements for manydata. 2021.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {Manyenviron: Environmental agreements for manydata},
    ##     author = {James Hollway},
    ##     year = {2021},
    ##     url = {https://github.com/globalgov/manyenviron},
    ##   }

## Contributing

[`{manypkgs}`](https://github.com/globalgov/manypkgs) also makes it easy
to contribute in lots of different ways.

If you have already developed a dataset salient to this package, please
reach out by flagging this as an
[issue](https://github.com/globalgov/manyenviron/issues) for us, or by
forking, further developing the package yourself, and opening a [pull
request](https://github.com/globalgov/manyenviron/pulls) so that your
data can be used easily.

If you have collected or developed other data that may not be best for
this package, but could be useful within the wider universe,
[`{manypkgs}`](https://github.com/globalgov/qData) includes a number of
functions that make it easy to create a new “many” package and populate
with clean, consistent global governance data.

If you have any other ideas about how this package or the qData universe
more broadly might better facilitate your empirical analysis, we’d be
very happy to hear from you.
