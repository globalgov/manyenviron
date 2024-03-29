# manyenviron 0.3.0

2023-03-21

## Package

* Closed #61 by adding a ´generate_memberships()´ function for programmatically creating random, fictional, environmental agreements and members to those agreements

## Data

* Closed #67 by verifying `Beg`, `End`, `Signature`, and `Force` in HUGGO dataset
* Verified `Title`, `Beg`, `End`, `Signature`, `Force`, and `url` for 465 treaties in HUGGO dataset (see HUGGO_verified.csv)
* Verified `Title`, `Beg`, `End`, `Signature`, `Force`, and `url` for 196 treaties present in ECOLEX, IEADB, CIEISIN, and HEIDI datasets added to HUGGO dataset (see HUGGO_additional.csv)
* Added 70 formatted treaty texts in 'TreatyTexts' folder
* Added 469 new raw treaty texts, along with other 876 unformatted texts from previous versions of HUGGO, in 'TreatyTexts' folder

# manyenviron 0.2.1

2022-10-11

## Data

* Corrected coding mistakes with HUGGO and AIGGO datasets in 'agreements' database
* Corrected coding mistakes with HUGGO_MEM in 'memberships' database

# manyenviron 0.2.0

2022-10-10

## Package

* Added ´generate_agreements()´ function for programmatically creating random, fictional, environmental agreement titles

## Data

* Closed #59 by dividing coded datasets into HUGGO and AIGGO
* Added MEA edges and nodes data to HUGGO_MEM and HUGGO respectively

# manyenviron 0.1.3

2022-08-16

## Package

* Closed #57 by switching back to original logo size.
* Updated workflow files to include package caching.

## Data

* Changed class from 'messydt' to 'mdate' for date variables (`Beg`, `Signature`, `Force`, `End`, `Rat`, `SignatureCountry`, and `Term`) across all databases using `messydates::as_messydate()`.
* Re-exported treaty texts after standardising with `manypkgs::standardise_treaty_text()`.
* Added `accessionC` and `accessionP` variables in `GNEVAR` dataset in `agreements` database using `manypkgs::code_accession_terms()`. These variables code the accession conditions (such as whether it is open to all states) and procedures (for example by majority voting) respectively that are stipulated in treaties for states acceding to a treaty.

# manyenviron 0.1.2

2022-03-16

## Package

* Closed #53 by adding Shinyapp folders, dashboard vignette and connecting to a Shinyapp account.

## Data

* Closed #15 by adding `International Regimes Database` to the `regimes` database.
* Fixed #55 by renaming variables in database to avoid ambiguity
  * Renamed variables "L", "D", and "J" in the agreements database "DocType", "AgreementType", and "GeogArea" respectively
  * Renamed variables "L" and "SignatureC" in memberships database "DocType" and "SignatureCountry" respectively

# manyenviron 0.1.1

2022-02-03

## Package

* Fixed #46 by simplifying tests for texts database
* Fixed #48 by moving to bootstrap 5 in pkgdown file
* Closed #43 by using treaty texts to create `code_lineage`

## Data

**Added datasets**
* Closed #13 by web scraping YIO pages
* Added HEIDI dataset

**Added columns in datasets**
* Closed #42 by adding Lineage column in datasets in agreements database
* Closed #44 by adding membership column

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
