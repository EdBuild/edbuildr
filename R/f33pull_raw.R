#'A function to pull raw F33 data
#'
#'This function imports raw data from the US Census, Annual Survey of School
#'System Finances (F33).
#'@param data_year Four digit year of F33 data you would like to pull. Available
#'  for 1990-2019.
#'@keywords F33 data EdBuild
#'@seealso \code{\link{f33pull}}
#'@importFrom utils read.csv
#'@import magrittr
#'@export
#'@usage f33pull_raw(data_year = "2019")
#'@return A dataframe where each observation is a school district.
#'@format A data frame with 40 variables. Definitions of each variable name can
#'  be found in the
#'  \href{https://www.census.gov/programs-surveys/school-finances.html}{US
#'  Census's Annual Survey of School System Finances} technical documentation.
#'@source
#'\url{https://s3.amazonaws.com/data.edbuild.org/public/Raw+Data/F33/csv/2019.csv}
#'
#' @examples
#' \donttest{f33_2019 <- f33pull_raw('2019')}

f33pull_raw = function(data_year = "2019") {
  if (as.numeric(data_year)<1990) {
    message("Error: f33pull_raw cannot be used for data before the year 1990")
  }

  else if (as.numeric(data_year)>2019) {
    message("Error: The most recent year of F33 data is for 2019; data_year > 2019 is not valid")
  }

  else {
    url = paste("https://s3.amazonaws.com/data.edbuild.org/public/Raw+Data/F33/csv", paste(data_year, ".csv", sep=""), sep ="/")
    dataset <- read.csv(file = url, stringsAsFactors = FALSE)
    #print out a note about the processing steps that were applied to the F33 data
    message("NOTE:: No processing was done to this data after being downloaded from the Census. To pull EdBuild's processed data use f33pull.
            For full survey documentation and file layout see: https://www.census.gov/programs-surveys/school-finances/technical-documentation/complete/2018.html")

    return(dataset)
  }

}

