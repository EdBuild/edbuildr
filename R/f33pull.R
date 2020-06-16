#' A function to import and process F33 data
#'
#' This function imports the US Census, Annual Survey of School System Finances
#' (F33) data with standard EdBuild processing steps applied to revenues.
#' @param data_year Four digit year of F33 data you would like to pull.
#'   Available for 2006-2018.
#' @param additional_var List any additional F33 variables you would like to
#'   pull.
#' @param path Path name of F33 file if data is stored on local computer.
#'   Defaults to NULL to pull in F33 data from EdBuild's server.
#' @param keep_calcs Do you want to keep all of the variables used to calculate
#'   adjusted revenues? Defaults to FALSE.
#' @keywords F33 data EdBuild
#' @import stringr dplyr magrittr
#' @importFrom utils read.csv
#' @usage f33pull(data_year = "2018", path=NULL, additional_var=NULL, keep_calcs=FALSE)
#' @seealso \code{\link{f33pull_raw}}
#' @export
#' @return A dataframe where each observation is a school district.
#' @note The following processing was made to state and local revenues for each
#'   school district:
#'
#'   \enumerate{ \item Revenues are multiplied by 1,000.
#'
#'   \item Sale of property (U11) is removed from local revenue.
#'
#'   \item Capital outlay and debt service programs (C11) is removed from state
#'   revenue.
#'
#'   \item In Arkansas, the Census state NCES local revenue (C24) is subtracted
#'   from state revenues and added to local revenues for all districts in the
#'   state.
#'
#'   \item In Texas, recapture, reported as payments to state governments (L12),
#'   is subtracted from local revenue.
#'
#'   \item Charter passthrough dollars, V92, is subtracted proportionately from
#'   state and local revenues since students attending charter schools are not
#'   included in F33 enrollment. }
#'
#'   For full details about F33 processing please visit
#'   \href{https://edbuild.org/content/dividing-lines/main/methodology}{EdBuild's
#'   dividing lines map site}.
#'
#' @format A data frame with 14 variables: \describe{ \item{year}{data year}
#'   \item{State}{State name} \item{STATE_FIPS}{State code} \item{NCEISD}{NCES
#'   school district ID} \item{NAME}{School district name} \item{CONUM}{County
#'   number} \item{ENROLL}{School district enrollment} \item{TFEDREV}{Total
#'   federal revenue- no EdBuild adjustments} \item{LOCREV_adj}{Local revenue -
#'   with EdBuild adjustments} \item{LOCREV_adj_PP}{Local revenue per pupil-
#'   with EdBuild adjustments} \item{STREV_adj}{State revenue - with EdBuild
#'   adjustments} \item{STREV_adj_PP}{State revenue per pupil - with EdBuild
#'   adjustments} \item{STLOCREV_adj}{State and local revenue - with EdBuild
#'   adjustments} \item{STLOCREV_adj_PP}{State and local revenue per pupil -
#'   with EdBuild adjustments} }
#' @source
#' \url{https://s3.amazonaws.com/data.edbuild.org/public/Raw+Data/F33/csv/2018.csv}
#'
#' @examples
#' \donttest{f33_2018 <- f33pull(data_year="2018",
#'                     additional_var=c("V40", "TCAPOUT"),
#'                     keep_calcs=FALSE)}


f33pull = function(data_year = "2018", path=NULL, additional_var=NULL, keep_calcs=FALSE) {

  fips <- masterpull(data_year) %>%
    dplyr::select(State, STATE_FIPS) %>%
    dplyr::mutate(STATE_FIPS = stringr::str_pad(as.character(STATE_FIPS), width = 2, pad = "0"))

  if (as.numeric(data_year)<2006) {
    #because we don't have local property sale revenue before 2006, which is a standard part of the processing, f33pull can't be used before that year
    message("Error: f33pull cannot be used for data before the year 2006 because local property sale was not measured before this year")
  }

  else if (as.numeric(data_year)>2018) {
    #the most current year of F33 data is 2018, so a data_year of more than that is not valid
    message("Error: The most recent year of F33 data is for 2018; data_year > 2018 is not valid")
  }

  else {
    if (is.null(path)) {
      url = paste("https://s3.amazonaws.com/data.edbuild.org/public/Raw+Data/F33/csv", paste(data_year, ".csv", sep=""), sep ="/")
      dataset <- read.csv(file = url, stringsAsFactors = FALSE) %>%
        dplyr::mutate(NCEISD =  stringr::str_pad(NCESID, width = 7, pad = "0"), # format district and county IDs
                      CONUM = stringr::str_pad(CONUM, width = 5, pad = "0"), # format district and county IDs
                      STATE = stringr::str_pad(STATE, width = 2, pad = "0"), # format district and county IDs
                      STATE_FIPS = substr(NCESID, start = 1, stop  = 2),# create State FIPS column
                      year = as.numeric(data_year)) %>%  # add year variable
        dplyr::filter(NCESID!="0000000") %>% # drop rows with NCESID of all zeros
        dplyr::select(-IDCENSUS, -YRDATA) %>% # drop IDCENSUS and YRDATA columns
        dplyr::rename(ST_CAPITAL = C11,
                      LOC_PROPERTY_SALE = U11,
                      ENROLL = V33,
                      IMPACT_AID = B11)  # rename property sales and capital and impact aid

      #this is the list of variables to pull everytime
      to_pull <- c("year", "STATE_FIPS", "NCESID", "NAME", "CONUM", "ENROLL", "TFEDREV", "TSTREV",
                   "ST_CAPITAL", "TLOCREV", "C24", "V92", "L12", "LOC_PROPERTY_SALE")

      to_pull <- if (is.null(additional_var)) {
        to_pull
      }
      else {
        append(to_pull, additional_var)
      }

      #this is the subset of the possible use-specified additional variables that should not be made numeric and multiplied by 1000
      dont_multiply <- to_pull[to_pull %in% c("SCHLEV", "CSA", "CBSA")]

      dataset <- dataset %>%
        dplyr::select(one_of(to_pull)) %>% #select the variables from to_pull
        dplyr::left_join((fips %>%
                            unique()), by = "STATE_FIPS") %>% #add state name onto data
        dplyr::select(year, State, everything()) %>% #reorder the variables
        dplyr::mutate_at(.vars=vars(-c(year:ENROLL), -one_of(dont_multiply)), ~(as.numeric(.)*1000)) %>% #make the revenue variables numeric and multiply by 1000
        dplyr::mutate(NCESID = stringr::str_pad(NCESID, 7, pad="0"), #make sure that NCESID is properly formatted
                      ENROLL = as.numeric(ENROLL), #make sure enrollment is numeric
                      LOCREV_adj_noV92adj = ifelse(grepl("^05", NCESID),
                                                   TLOCREV - LOC_PROPERTY_SALE + C24, #remove property sale and add C24 to local revenues for Arkansas
                                                   ifelse(grepl("^48", NCESID),
                                                          ifelse(TLOCREV - LOC_PROPERTY_SALE - L12>0, TLOCREV - LOC_PROPERTY_SALE - L12, NA), #remove property sale revenues and L12 from local revenue in Texas and don't let the adjust figure be less than 0
                                                          TLOCREV - LOC_PROPERTY_SALE)), #remove property sale revenues from local for other states
                      STREV_adj_noV92adj = ifelse(grepl("^05", NCESID),
                                                  TSTREV - ST_CAPITAL - C24, #remove capital and C24 from state revenue for Arkansas
                                                  TSTREV - ST_CAPITAL), #remove capital from state revenue for all other states
                      pctFSL_Local = LOCREV_adj_noV92adj/(LOCREV_adj_noV92adj + STREV_adj_noV92adj + TFEDREV), #compute the percent of TOTAL (excluding property sales, capital & after TX and AR adjustments) revenue that's local
                      pctFSL_State = STREV_adj_noV92adj/(LOCREV_adj_noV92adj + STREV_adj_noV92adj + TFEDREV), #compute the percent of TOTAL (excluding property sales, capital & after TX and AR adjustments) revenue that's state
                      LOCREV_adj = LOCREV_adj_noV92adj - (pctFSL_Local*V92), #remove total-revenue-proportionate share of V92
                      LOCREV_adj_PP = ifelse(ENROLL==0, NA, LOCREV_adj/ENROLL), #create a per-pupil local revenue adjusted variable
                      STREV_adj = STREV_adj_noV92adj - (pctFSL_State*V92), #remove total-revenue-proportionate share of V92
                      STREV_adj_PP = ifelse(ENROLL==0, NA, STREV_adj/ENROLL), #create a per-pupil state revenue adjusted variable
                      STLOCREV_adj = LOCREV_adj + STREV_adj, #compute a total state + local revenue adjusted variable
                      STLOCREV_adj_PP = LOCREV_adj_PP + STREV_adj_PP) #computer a per-pupil state + local revenue adjusted variable


      #print out a note about the processing steps that were applied to the F33 data
      message("NOTE::revenues are adjusted by:
            1) multiplying by 1000,
            2) excluding property sale from local,
            3) excluding capital from state,
            4) adjusting C24 in Arkansas (removed from state, added to local),
            5) removing L12 (payments to state governments) from Texas local revenues
            6) subtracting V92 proportionately from state and local revenues (charter passthrough dollars)")

    }
    else {
      dataset <- utils::read.csv(path) %>%
        dplyr::mutate(NCEISD =  stringr::str_pad(NCESID, width = 7, pad = "0"), # format district and county IDs
                      CONUM = stringr::str_pad(CONUM, width = 5, pad = "0"), # format district and county IDs
                      STATE = stringr::str_pad(STATE, width = 2, pad = "0"), # format district and county IDs
                      STATE_FIPS = substr(NCESID, start = 1, stop  = 2),# create State FIPS column
                      year = as.numeric(data_year),
                      IMPACT_AID = B11) %>%  # add year variable
        dplyr::filter(NCESID!="0000000") %>% # drop rows with NCESID of all zeros
        dplyr::select(-IDCENSUS, -YRDATA) %>% # drop IDCENSUS and YRDATA columns
        dplyr::rename(ST_CAPITAL = C11,
                      LOC_PROPERTY_SALE = U11,
                      ENROLL = V33)  # rename property sales and capital and impact aid

      #this is the list of variables to pull everytime
      to_pull <- c("year", "STATE_FIPS", "NCESID", "NAME", "CONUM", "ENROLL", "TFEDREV", "TSTREV",
                   "ST_CAPITAL", "TLOCREV", "C24", "V92", "L12", "LOC_PROPERTY_SALE")

      to_pull <- if (is.null(additional_var)) {
        to_pull
      }
      else {
        append(to_pull, additional_var)
      }

      #this is the subset of the possible use-specified additional variables that should not be made numeric and multiplied by 1000
      dont_multiply <- to_pull[to_pull %in% c("SCHLEV", "CSA", "CBSA")]

      dataset <- dataset %>%
        dplyr::select(one_of(to_pull)) %>% #select the variables from to_pull
        dplyr::left_join((fips %>%
                          unique()), by = "STATE_FIPS") %>% #add state name onto data
        dplyr::select(year, State, everything()) %>% #reorder the variables
        dplyr::mutate_at(.vars=vars(-c(year:ENROLL), -one_of(dont_multiply)), ~(as.numeric(.)*1000)) %>% #make the revenue variables numeric and multiply by 1000
        dplyr::mutate(NCESID = stringr::str_pad(NCESID, 7, pad="0"), #make sure that NCESID is properly formatted
                      ENROLL = as.numeric(ENROLL), #make sure enrollment is numeric
                      LOCREV_adj_noV92adj = ifelse(grepl("^05", NCESID),
                                                   TLOCREV - LOC_PROPERTY_SALE + C24, #remove property sale and add C24 to local revenues for Arkansas
                                                   ifelse(grepl("^48", NCESID),
                                                          ifelse(TLOCREV - LOC_PROPERTY_SALE - L12>0, TLOCREV - LOC_PROPERTY_SALE - L12, NA), #remove property sale revenues and L12 from local revenue in Texas and don't let the adjust figure be less than 0
                                                          TLOCREV - LOC_PROPERTY_SALE)), #remove property sale revenues from local for other states
                      STREV_adj_noV92adj = ifelse(grepl("^05", NCESID),
                                                  TSTREV - ST_CAPITAL - C24, #remove capital and C24 from state revenue for Arkansas
                                                  TSTREV - ST_CAPITAL), #remove capital from state revenue for all other states
                      pctFSL_Local = LOCREV_adj_noV92adj/(LOCREV_adj_noV92adj + STREV_adj_noV92adj + TFEDREV), #compute the percent of TOTAL (excluding property sales, capital & after TX and AR adjustments) revenue that's local
                      pctFSL_State = STREV_adj_noV92adj/(LOCREV_adj_noV92adj + STREV_adj_noV92adj + TFEDREV), #compute the percent of TOTAL (excluding property sales, capital & after TX and AR adjustments) revenue that's state
                      LOCREV_adj = LOCREV_adj_noV92adj - (pctFSL_Local*V92), #remove total-revenue-proportionate share of V92
                      LOCREV_adj_PP = ifelse(ENROLL==0, NA, LOCREV_adj/ENROLL), #create a per-pupil local revenue adjusted variable
                      STREV_adj = STREV_adj_noV92adj - (pctFSL_State*V92), #remove total-revenue-proportionate share of V92
                      STREV_adj_PP = ifelse(ENROLL==0, NA, STREV_adj/ENROLL), #create a per-pupil state revenue adjusted variable
                      STLOCREV_adj = LOCREV_adj + STREV_adj, #compute a total state + local revenue adjusted variable
                      STLOCREV_adj_PP = LOCREV_adj_PP + STREV_adj_PP) #computer a per-pupil state + local revenue adjusted variable


      #print out a note about the processing steps that were applied to the F33 data
      message("NOTE::revenues are adjusted by:
            1) multiplying by 1000,
            2) excluding property sale from local,
            3) excluding capital from state,
            4) adjusting C24 in Arkansas (removed from state, added to local),
            5) removing L12 (payments to state governments) from Texas local revenues
            6) subtracting V92 proportionately from state and local revenues (charter passthrough dollars)")
    }

    if (keep_calcs==FALSE) {
      #if the user has chosen not to keep all the variables used to compute the adjusted revenues, drop them here
      dplyr::select(dataset, -c(TSTREV:LOC_PROPERTY_SALE, LOCREV_adj_noV92adj:pctFSL_State))
    }
    else {
      #if the user has chosen to keep all the variables used to compute the adjuste revenues, just keep the entire dataset
      dataset
    }

  }

}
