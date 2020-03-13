#' A function to import a longitudinal version of EdBuild's master dataset
#'
#' This function imports a longitudinal dataset of EdBuild's master data for the
#' years 2013- 2017. The master dataset is a compilation of national level
#' school district data from the US Census Annual Survey of School System
#' Finances (F33); US Census Small Area Income and Poverty Estimates (SAIPE);
#' National Center for Education Statistics (NCES) Common Core of Data (CCD);
#' and Education Demographic and Geographic Estimates (EDGE). Cost adjustments
#' were calculated using C2ER.
#' @keywords master data EdBuild F33 CCD SAIPE EDGE
#' @import dplyr magrittr
#' @usage long_masterpull()
#' @return A dataframe where each observation is a school district.
#' @seealso \code{\link{master_codebook}}, \code{\link{masterpull}}
#' @export
#' @format A data frame with 92,636 observations and 41 variables. To view
#'   descriptions of variable names and sources for each use
#'   \code{master_codebook()}
#' @examples
#' \donttest{long_master <- long_masterpull()}


long_masterpull = function() {

      url_13 = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2013/full_data_13.csv"
      url_14 = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2014/full_data_14.csv"
      url_15 = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2015/full_data_15.csv"
      url_16 = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2016/full_data_16.csv"
      url_17 = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2017/full_data_17.csv"

      master_13 <- read.csv(file = url_13, stringsAsFactors = FALSE) %>%
        dplyr::mutate(CONUM = as.character(CONUM),
                      STATE_FIPS = as.character(STATE_FIPS),
                      FRL_rate = dFRL/dEnroll_district,
                      year = "2013")

      master_14 <- read.csv(file = url_14, stringsAsFactors = FALSE) %>%
        dplyr::mutate(CONUM = as.character(CONUM),
                      STATE_FIPS = as.character(STATE_FIPS),
                      FRL_rate = dFRL/dEnroll_district,
                      year = "2014")

      master_15 <- read.csv(file = url_15, stringsAsFactors = FALSE) %>%
        dplyr::mutate(CONUM = as.character(CONUM),
                      STATE_FIPS = as.character(STATE_FIPS),
                      FRL_rate = dFRL/dEnroll_district,
                      year = "2015")

      master_16 <- read.csv(file = url_16, stringsAsFactors = FALSE) %>%
        dplyr::mutate(CONUM = as.character(CONUM),
                      STATE_FIPS = as.character(STATE_FIPS),
                      FRL_rate = dFRL/dEnroll_district,
                      year = "2016")

      master_17 <- read.csv(file = url_17, stringsAsFactors = FALSE) %>%
        dplyr::mutate(CONUM = as.character(CONUM),
                      STATE_FIPS = as.character(STATE_FIPS),
                      FRL_rate = dFRL/dEnroll_district,
                      year = "2017")

      long_master <- dplyr::bind_rows(master_13, master_14, master_15, master_16, master_17) %>%
        dplyr::select(-Region, -X, -COLIn) %>%
        dplyr::select(NCESID, year, everything())

    return(long_master)

    }
