# edbuildr 0.1.1

* added undirected single quotes around 'EdBuild' and 'EdBuild's' in the description file
* added a reference describing the methods in the package in the description field of the description file
* replaced \dontrun{} with \donttest{} for examples that take more than 5 seconds to execute 
* added \value to all .Rd files and explained the function's results
* added an error message in `<masterpull()>` if a user enters the wrong data type parameter
* adjusted note to more easily prompt users how to save output from `<sd_table_xlsx()>`
* shortened excel file sheet names in `<sd_table_xlsx>` and `<sd_neighbor_xlsx()>` to a maximum of 30 characters
* adjusted rounding of percentages to nearest whole number in `<sd_table_xlsx()>` and `<sd_neighbor_xlsx()>`

# edbuildr 0.1.0

* Initial release of `edbuildr` to automated the downloading and processing of school district data. 
