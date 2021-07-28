# qEnvironDev 0.0.3

2021-07-28

## Package changes

* Updated LICENSE.md with `update_package()` function

## Data changes

* Closes #6 by adding MIA dataset into organizations database
* Closes #21 by joining numerous GNEVAR datasets with the `consolidate()` function
* Closes #22 by adding REF dataset into references database
* Added CIESIN dataset into agreements database
* Used new version of `standardise_dates()` to transform dates column in `messydt` class
* Used new version of `standardise_titles()` on title columns to better standardise specific words spelling
* Simplified the tests of the databases according to these changes

# qEnvironDev 0.0.2

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
