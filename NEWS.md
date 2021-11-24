# manyenviron 0.1.0

2021-11-23

## Package

* Closed #39 by renaming qEnviron to manyenviron
* Added a vignette about connecting environmental treaties
  * Closed #38 by adding memberships plot to vignette
  * Closed #37 by adding lineage plot to vignette
  * Closed #35 by fixing issues with lineage plots
* Fixed #32 by deleting "Title.rda" file

## Data

* Modified the ways in which treaty texts are gathered and stored
  * Closed #41 by setting up a scraping script for treaty text to avoid storing raw data
  * Treaty texts data are stored in one dataset from multiple sources to avoid redundancy
* Fixed #40 by correcting the class and order in which variables appear in datasets
* Closed #36 by re-ordering the datasets columns in all databases
* Fixed #34 by putting id column first in references dataset
* Fixed #33 by making sure all datasets are in tibble format

# qEnviron 0.0.4

2021-10-06

## Package

* Closed #26 by adding a website using pkgdown package

## Data

* Closed #24 by adding IEADB texts using web scraping functions
* Closed #25 by translating treaty titles using key API in `standardise_titles()`
* Closed #27 by adding a qID_ref column using `condense_qID()` from qCreate
* Closed #20 by preparing coding scheme based on treaty titles and texts

# qEnviron 0.0.3

2021-07-30

## Package changes

* Updated LICENSE.md with `qCreate::update_package()` function

## Data changes

* Used new version of `standardise_dates()` to transform dates column in `messydt` class
* Used new version of `standardise_titles()` on title columns to better standardise specific words spelling
* Simplified the tests of the databases according to these changes

## Organizations database

* Closed #6 by adding MIA dataset into organizations database

## Agreements database

* Closed #21 by joining numerous GNEVAR datasets with the `consolidate()` function
* Added CIESIN dataset into agreements database

## References database

* Closed #22 by adding REF dataset into references database

# qEnviron 0.0.2

2021-02-22

## Package

* Closed #9 by fixing GitHub PushRelease actions
* Fixed logo aspect ratio and added globalgov/ direction
* Added a LICENSE.md file

## Data

* Added variables in databases such as Signature, Force, Rat, Term, L, J and D
* Updated the tests when export the datasets to be more in line with new variables
* Added `standardise_titles()` function to the Title variables 
* Closed #14 by adding TFDD in membership database
* Closed #8 by changing the citation for ECOLEX with a more recent one
* Fixed #17 by adding one table of reference for IDs and ID columns in agreements database


# qEnvironDev 0.0.1

2020-02-10

## Package

* Set up qEnvironDev package using `qData::setup_package()`
  * Added core package files
  * Added `.github` folder and files
  * Added `tests` folder and files

## Data

* Imported initial datasets in qEnviron, cleaned them and export them in the databases.
  * Added `IEADB` agreement data
  * Added `IEADB` membership data
  * Closed #2 by adding `ECOLEX` agreement data
  * Closed #3 by adding `ECOLEX` membership data
  * Closed #4 by adding `GNEVAR` agreement data 
  * Closed #5 by adding `GNEVAR` membership data
* Organized datasets in the two databases so the names are consistent
* Modified description file to add pointblank as dependency and precise authors' role
* Cleaned the datasets by making some long instead of wide and by standardizing the dates
