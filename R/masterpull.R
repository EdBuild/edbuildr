#' A function to import EdBuild's master dataset
#'
#' This function allows you to read in EdBuild's master dataset for the years
#' 2013- 2018. The master dataset is a compilation of national level school
#' district data from the US Census Annual Survey of School System Finances
#' (F33); US Census Small Area Income and Poverty Estimates (SAIPE); National
#' Center for Education Statistics (NCES) Common Core of Data (CCD); and
#' Education Demographic and Geographic Estimates (EDGE). Cost adjustments were
#' calculated using C2ER.
#' @param data_year Four digit year of master data to pull in. Options include
#'   2013- 2018. Defaults to 2018.
#' @param data_type Type of master data to pull in \itemize{ \item \code{geo}
#'   pulls in all school districts that have physical school district
#'   boundaries. To be used for map-based analysis and other analyses that
#'   pertain to school districts with geographic boundaries. For instance, an
#'   analysis using median owner-occupied property value would use the geography
#'   exclusion. \item \code{fin} pulls in all school districts that meet
#'   EdBuild’s criteria for financial analysis. To be used for finance analysis.
#'   \item \code{gen} pulls in all school districts that meet enrollment and
#'   district type requirements. To be used for all other, non-finance analysis.
#'   \item \code{full} pulls in all school districts in the given year. To be
#'   used with great care, or to find a district that does not appear in any
#'   other exclusion, for example charter school districts. } Defaults to
#'   \code{gen}
#' @param disaggregated For the full, general, and finance exclusions in 2018,
#'   users have the option to view disaggregated Vermont school district data
#'   without EdBuild's aggregation processing.
#' @keywords master data EdBuild F33 CCD SAIPE EDGE
#' @usage masterpull(data_year = "2018", data_type = "gen", disaggregated =
#'   FALSE)
#' @import dplyr magrittr
#' @importFrom utils read.csv
#' @return A dataframe where each observation is a school district.
#' @seealso \code{\link{master_codebook}}, \code{\link{long_masterpull}}
#' @export
#' @note There are three types of exclusions that can be applied to the master
#'   dataset:
#'
#'   1. Geography based: \itemize{ \item Excludes districts that do not have
#'   physical school district boundaries and thus are not included in the US
#'   Census, EDGE shapefile. }
#'
#'   2. Finance based: \itemize{ \item Excludes districts that are of types 5
#'   (vocational or special education), 6 (non-operating) or 7 (educational
#'   service agency) in the F33 data.
#'
#'   \item If F33 school type is missing, excludes districts that are of types 4
#'   (regional education service agency), 5 (state agency), 6 (federal agency),
#'   7 (charter agency) or 8 (other education agency) based on CCD.
#'
#'   \item Excludes districts that zero missing or total enrollments.
#'
#'   \item Excludes districts that have missing or zero operational schools.
#'
#'   \item Excludes districts that have missing revenues.
#'
#'   \item Excludes districts that have very low revenues (<$500).
#'
#'   \item Excludes districts that have very high revenues (>$100,000 in
#'   inflation-adjusted 2017 dollars).
#'
#'   \item Excludes districts from the US territories. }
#'
#'   3. General: \itemize{ \item Excludes districts that are of types 5
#'   (vocational or special education), 6 (nonoperating) or 7 (educational
#'   service agency) in the F33 data.
#'
#'   \item If F33 school type is missing, excludes districts that are of types 4
#'   (regional education service agency), 5 (state agency), 6 (federal agency),
#'   7 (charter agency) or 8 (other education agency) based on CCD.
#'
#'   \item Excludes districts that have missing or zero total enrollments.
#'
#'   \item Excludes districts from the US territories. }
#' @format A data frame with 42 variables. To view descriptions of variable
#'   names and sources for each use \code{master_codebook()}
#' @source
#' \url{https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/full_data_18_type_exc.csv}
#'
#' @examples
#' \donttest{master18_geo <- masterpull("2018", data_type = "geo")}


masterpull = function(data_year = "2018", data_type = "gen", disaggregated = FALSE) {
  if (as.numeric(data_year)<2013) {
    message("Error: Master datasets are only available for years 2013 through 2018")
  }

  else if (as.numeric(data_year)>2018) {
    message("Error: Master datasets are only available for years 2013 through 2018")
  }

  else if (data_type != "full" & data_type != "geo" & data_type != "gen" & data_type != "fin"){
    message("Error: please select a valid data_type ('gen', 'geo', 'fin', or 'full')")
  }
  else if (data_year != 2018 & disaggregated == TRUE | data_year == 2018 & data_type == "geo" & disaggregated == TRUE){
    message("Error: disagregated school district data for Vermont is only avaiable
            for the 2018 finance, general, and full master datasets")
  }

  else  {
    if(as.numeric(data_year)==2013 & data_type == "full") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2013/full_data_13.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::select(-X, -Region, -COLIn)

      }
      else if(as.numeric(data_year)==2014 & data_type == "full") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2014/full_data_14.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::select(-X)

      }
      else if(as.numeric(data_year)==2015 & data_type == "full") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2015/full_data_15.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::select(-X)

      }
      else if(as.numeric(data_year)==2016 & data_type == "full") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2016/full_data_16.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::select(-X)

      }
      else if(as.numeric(data_year)==2017 & data_type == "full") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2017/full_data_17.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
        dplyr::select(-X)

      }
      else if(as.numeric(data_year)==2018 & data_type == "full" & disaggregated== FALSE) {
      url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/full_data_18.csv"
      master <- read.csv(file = url, stringsAsFactors = FALSE)
      }
      else if(as.numeric(data_year)==2018 & data_type == "full" & disaggregated== TRUE) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/disaggregated/disag_full_data_18.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)
      }

      else if (as.numeric(data_year)==2013 & data_type == "geo"){
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2013/full_data_13_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2014 & data_type == "geo") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2014/full_data_14_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2015 & data_type == "geo") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2015/full_data_15_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2016 & data_type == "geo")  {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2016/full_data_16_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2017 & data_type == "geo") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2017/full_data_17_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2018 & data_type == "geo") {
      url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/full_data_18_geo_exc.csv"
      master <- read.csv(file = url, stringsAsFactors = FALSE)

      }

      else if (as.numeric(data_year)==2013 & data_type == "fin") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2013/full_data_13_fin_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2014 & data_type == "fin") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2014/full_data_14_fin_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2015 & data_type == "fin") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2015/full_data_15_fin_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2016 & data_type == "fin") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2016/full_data_16_fin_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2017 & data_type == "fin") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2017/full_data_17_fin_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)
      }
      else if(as.numeric(data_year)==2018 & data_type == "fin" & disaggregated== FALSE) {
      url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/full_data_18_fin_exc.csv"
      master <- read.csv(file = url, stringsAsFactors = FALSE)
      }
      else if(as.numeric(data_year)==2018 & data_type == "fin" & disaggregated== TRUE) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/disaggregated/disag_data_18_fin_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)
      }

      else if (as.numeric(data_year)==2013 & data_type == "gen"){
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2013/full_data_13_type_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2014 & data_type == "gen") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2014/full_data_14_type_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2015 & data_type == "gen") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2015/full_data_15_type_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if(as.numeric(data_year)==2016 & data_type == "gen") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2016/full_data_16_type_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)

      }
      else if (as.numeric(data_year)==2017 & data_type == "gen") {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2017/full_data_17_type_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)
      }
      else if (as.numeric(data_year)==2018 & data_type == "gen" & disaggregated== FALSE) {
      url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/full_data_18_type_exc.csv"
      master <- read.csv(file = url, stringsAsFactors = FALSE)
      }
      else if (as.numeric(data_year)==2018 & data_type == "gen" & disaggregated== TRUE) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/disaggregated/disag_data_18_type_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE)
      }

    master <- master %>%
      dplyr::mutate(FRL_rate = dFRL/dEnroll_district) %>%
      dplyr::mutate(NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"),
                    STATE_FIPS = stringr::str_pad(STATE_FIPS, width = 2, pad = "0"),
                    CONUM = stringr::str_pad(CONUM, width = 2, pad = "0")) %>%
      dplyr::select(NCESID, state_id, State:NAME, County, CONUM:dFRL, FRL_rate, dIEP, everything())
    return(master)

  }

}
