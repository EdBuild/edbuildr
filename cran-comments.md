## Resubmission
This is a resubmission. In this version I have:
* updated all functions with 2019 data

## Test environments
* local OS X install, R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission
This is a resubmission. In this version I have:
* fixed bug in function `<big_borders()>` 
* added variables median household income and median property value to `<sd_table_xlsx()>` 

## Test environments
* local OS X install, R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission
This is a resubmission. In this version I have:
* added new function `<big_borders()>` to model county and state level local revenue pooling
* added new function`<sd_neighbor()>` to create a dataframe of neighboring school districts
* added state school district ids and school district levels to `<masterpull()>` 
* padded unique school ids to a standard seven digits in `<masterpull()>` 
* added parameter to `<masterpull()>` to view disaggregated 2018 data for school districts in Vermont
* corrected mispelling of variable name in `<sd_neighbor_xlsx()>` 
* updated all functions with 2018 data

## Test environments
* local OS X install, R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* New maintainer

## Previous cran-comments

## Resubmission
This is a resubmission. In this version I have: 
* added undirected single quotes around 'EdBuild' and 'EdBuild's' in the description file
* added a reference describing the methods in the package in the description field of the description file
* replaced \dontrun{} with \donttest{} for examples that take more than 5 seconds to execute 
* added \value to all .Rd files and explained the function's results

## Test environments
* local OS X install, R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments
* local OS X install, R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

There was a message about possibly mis-spelled words in DESCRIPTION: ("EdBuild" at 15:60 and "EdBuild's" at 10:21 and 18:43).
* Those words are spelled correctly. 
