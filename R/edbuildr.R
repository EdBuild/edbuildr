#'edbuildr: A package for automated downloading and processing of school
#'district data
#'
#'This package allows users to import EdBuild's master dataset of school
#'district finance, student demographics, and community economic indicators for
#'every school district in the United States. The master dataset is built from
#'the US Census, Annual Survey of School System Finances (F33) and joins data
#'from the National Center for Education Statistics, Common Core of Data (CCD);
#'the US Census, Small Area Income and Poverty Estimates (SAIPE); and the US
#'Census, Education Demographic and Geographic Estimates (EDGE). We apply
#'EdBuild's standard processing to the dataset and provide the option to select
#'from four different exclusion criteria - see the masterpull help file for full
#'details. The master dataset is available for any school year from 2013 to 2019 or
#'longitudinally for all years 2013-2019. School year is identified by the end year.
#'For example, the 2018-19 school year is 2019. Additional functions in the package use
#'EdBuild's master data to analyze the difference between neighboring school
#'districts and create formatted excel tables of school district data. For full
#'details about EdBuild's data processing please see: EdBuild (2020)
#'<http://data.edbuild.org/>.
#'
#'@section edbuildr functions: The edbuildr functions are: \describe{
#'  \item{\code{\link{big_borders}}}{Pools local revenue for school districts at
#'  either the county or state level to see the effects on individual
#'  districts.}\item{\code{\link{f33pull}}}{Pulls in the US Census's Annual Survey of
#'  School System Finances (F33) and processes according to EdBuild's
#'  adjustments. Available for 2006-2019.}
#'  \item{\code{\link{f33pull_raw}}}{Pulls in the the full US Census's Annual
#'  Survey of School System Finances (F33) with no EdBuild adjustments.
#'  Available for 1990-2019.} \item{\code{\link{long_masterpull}}}{Pulls in
#'  EdBuild's master dataset longitudinally for years 2013-2019.}
#'  \item{master_codebook}{Reads in a codebook for EdBuild's master data. The
#'  codebook tells the user what each variable name represents and the source of
#'  each variable.} \item{\code{\link{masterpull}}}{Pulls in EdBuild's master
#'  dataset which is a compilation of national level school district data from
#'  the Census's Annual Survey of School System Finances (F33) and Small Area
#'  Income and Poverty Estimates (SAIPE) and NCES's Common Core of Data (CCD)
#'  and Education Demographic and Geographic Estimates (EDGE). Cost adjustments
#'  were calculated using C2ER. Available for 2013-2019.}
#'  \item{\code{\link{neigh_diff}}}{Calculates the difference and national rank
#'  for a selected variable between all school district neighbors.}
#'  \item{\code{\link{round2}}}{Rounds 0.5 up.}
#'  \item{\code{\link{sd_neighbor}}}{Creates a dataframe of any
#'  school district and their neighbors with selected variables.}
#'  \item{\code{\link{sd_neighbor_xlsx}}}{Creates a formatted table (.xlsx) of any
#'  school district and their neighbors with selected variables.}
#'  \item{\code{\link{sd_table_xlsx}}}{Creates a formatted excel table (.xlsx)
#'  of school districts in a state or county with selected variables.} }
#'@author \itemize{ \item {Megan Brodzik (meganbrodzik@gmail.com), maintainer} \item
#'  {Cecilia Depman (cecilia.depman@gmail.com), author} \item {Sara Hodges
#'  (sara.hodges@gmail.com), author} \item {Kailey Spencer
#'  (kailey.spencer.depoe@gmail.com), contributor} }
#'@docType package
#'@name edbuildr
NULL
#> NULL
