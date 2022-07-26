# manyenviron <img src="man/figures/manyenviron_hexlogo.png" align="right" width="220"/>

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![GitHub release (latest by
date)](https://img.shields.io/github/v/release/globalgov/manyenviron)
![GitHub Release
Date](https://img.shields.io/github/release-date/globalgov/manyenviron)
<!-- badges: end -->

manyenviron is a data package in the many universe of packages. It
currently includes an ensemble of datasets on international
environmental agreements, and
[states](https://github.com/globalgov/qStates)’ membership or other
relationships to those agreements.

Please also check out [`{manydata}`](https://github.com/globalgov) for
more information about the other packages in the many universe.

How to install
--------------

We’ve made it easier than ever to install and start analysing global
governance data in R. Simply install the core package,
[manydata](https://github.com/globalgov/manydata), as follows, and then
you can discover, install and update various “many” packages from the
console.

``` r
manydata::get_packages() # this prints a list of the publicly available data packages currently available
```

    ## # A tibble: 7 × 6
    ##   name        full_name            
    ##   <chr>       <chr>                
    ## 1 manydata    globalgov/manydata   
    ## 2 manyenviron globalgov/manyenviron
    ## 3 manyhealth  globalgov/manyhealth 
    ## 4 manypkgs    globalgov/manypkgs   
    ## 5 manystates  globalgov/manystates 
    ## 6 manytrade   globalgov/manytrade  
    ## 7 messydates  globalgov/messydates 
    ##   description                                                          
    ##   <chr>                                                                
    ## 1 An R portal for ensembled global governance data                     
    ## 2 R Package for ensembled data on environmental agreements             
    ## 3 An R package for ensembled data on international health organisations
    ## 4 Support for creating new manyverse packages                          
    ## 5 An R package for ensembled data on sovereign states                  
    ## 6 An R package for ensembled data on trade agreements                  
    ## 7 An R package for ISO's Extended Date/Time Format (EDTF)              
    ##   installed latest updated   
    ##   <chr>     <chr>  <date>    
    ## 1 0.7.5     0.7.5  2022-06-07
    ## 2 0.1.3     0.1.2  2022-03-16
    ## 3 0.1.1     0.1.1  2022-02-15
    ## 4 0.2.2     0.2.2  2022-07-21
    ## 5 0.1.1     0.0.6  2021-12-06
    ## 6 0.1.2     0.1.2  2022-07-14
    ## 7 0.3.1     0.3.1  2022-07-21

``` r
#manydata::get_packages("manyenviron") # this downloads and installs the named package
```

Data included
-------------

Once you have installed the package, you can see the different databases
and datasets included in the `{manyenviron}` package using the following
function.

    manydata::data_contrast("manyenviron")

    ## agreements :
    ##        Unique ID Missing Data Rows Columns        Beg        End
    ## IEADB          0       2.72 % 3666      10 1351-08-01         NA
    ## GNEVAR         0      50.64 % 7273      14 1351-08-01 1371-07-31
    ## ECOLEX         0       5.32 % 2174      10 1868-10-17         NA
    ## CIESIN         0          0 %  666       7 1868-01-01         NA
    ## HEIDI          0       0.34 % 2280       7 1900-05-11         NA
    ##                                                   URL
    ## IEADB     https://iea.uoregon.edu/base-agreement-list
    ## GNEVAR                                             NA
    ## ECOLEX     https://www.ecolex.org/result/?type=treaty
    ## CIESIN       https://sedac.ciesin.columbia.edu/entri/
    ## HEIDI  https://www.chaire-epi.ulaval.ca/en/data/heidi
    ## 
    ## memberships :
    ##            Unique ID Missing Data  Rows Columns        Beg End
    ## ECOLEX_MEM         0      22.47 % 25003      10 1192-06-12  NA
    ## GNEVAR_MEM         0       26.4 % 35671      13 1192-06-12  NA
    ## IEADB_MEM          0      13.94 % 15466      12 1901-02-22  NA
    ## TFDD_MEM           0       1.77 %  2118       8 1900-01-03  NA
    ##                                                             URL
    ## ECOLEX_MEM           https://www.ecolex.org/result/?type=treaty
    ## GNEVAR_MEM                                                   NA
    ## IEADB_MEM               https://iea.uoregon.edu/country-members
    ## TFDD_MEM   https://transboundarywaters.science.oregonstate.edu/
    ## 
    ## organizations :
    ##     Unique ID Missing Data  Rows Columns        Beg End
    ## MIA         0         25 %    78       4 1831-01-01  NA
    ## YIO         0       26.8 % 75115       4 1997-01-01  NA
    ##                                                             URL
    ## MIA https://garymarks.web.unc.edu/data/international-authority/
    ## YIO                                        https://uia.org/ybio
    ## 
    ## references :
    ##            Unique ID Missing Data Rows Columns Beg End URL
    ## ECOLEX_REF         0          0 % 1164       3  NA  NA  NA
    ## 
    ## regimes :
    ##     Unique ID Missing Data Rows Columns        Beg  End
    ## IRD         0       33.7 %   92       7 1946-01-01 -Inf
    ##                                                                                                             URL
    ## IRD https://direct.mit.edu/glep/article-abstract/6/3/121/14360/The-International-Regimes-Database-Designing-and
    ## 
    ## texts :
    ##            Unique ID Missing Data Rows Columns        Beg End URL
    ## GNEVAR_TXT         0      38.03 % 6377       8 1351-08-01  NA  NA

Working with an ensemble of related data has many advantages for robust
analysis. Just take a look at our vignettes
[here](https://globalgov.github.io/manydata/articles/user.html).

The many packages universe
--------------------------

The [many universe of packages](https://github.com/globalgov/manydata)
is aimed at collecting, connecting, and correcting network data across
issue-domains of global governance.

While some many packages can and do include novel data, much of what
they offer involves standing on the shoulders of giants. Many packages
endeavour to be as transparent as possible about where data comes from,
how it has been coded and/or relabeled, and who has done the work. As
such, we make it easy to cite both the particular datasets you use by
listing the official references in the function above, as well as the
package providers for their work assembling the data by using the
function below.

    citation("manyenviron")

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

Contributing
------------

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
[`{manypkgs}`](https://github.com/globalgov/manypkgs) includes a number
of functions that make it easy to create a new “many” package and
populate with clean, consistent global governance data.

If you have any other ideas about how this package or the manydata
universe more broadly might better facilitate your empirical analysis,
we’d be very happy to hear from you.
