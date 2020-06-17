#' A function to model local revenue pooling
#'
#' This function allows you to pool local revenue for school districts at either
#' the county or state level to see the effects on individual districts.
#' @param data_year Four digit year of master data to pull in. Options include
#'   2013- 2018. Defaults to 2018.
#' @param pooling_level Character string indicating the level of local revenue
#'   pooling. Either county or state. Defaults to county.
#' @keywords borders revenue pooling EdBuild
#' @usage big_borders(data_year= "2018", pooling_level = "county")
#' @import dplyr magrittr stringr
#' @return A dataframe with 17 variables where each observation is a school district.
#' @seealso \code{\link{masterpull}}, \code{\link{master_codebook}}
#' @export
#' @examples
#' \donttest{state_pool <- big_borders(
#'            data_year = "2018",
#'            pooling_level = "state"
#'            )}


big_borders = function(data_year = "2018", pooling_level="county") {

  if (as.numeric(data_year)<2013) {
    message("Error: Master datasets are only available for years 2013 through 2018")
  }

  else if (as.numeric(data_year)>2018) {
    message("Error: Master datasets are only available for years 2013 through 2018")
  }

  else if (as.numeric(data_year)==2013){
    url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2013/full_data_13_geo_exc.csv"
    master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
      dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

  }

  else if(as.numeric(data_year)==2014) {
    url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2014/full_data_14_geo_exc.csv"
    master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
      dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

  }

  else if(as.numeric(data_year)==2015) {
    url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2015/full_data_15_geo_exc.csv"
    master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
      dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

  }

  else if(as.numeric(data_year)==2016)  {
    url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2016/full_data_16_geo_exc.csv"
    master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
      dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

  }

  else if(as.numeric(data_year)==2017) {
    url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2017/full_data_17_geo_exc.csv"
    master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
      dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

  }
  else if(as.numeric(data_year)==2018) {
    url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/full_data_18_geo_exc.csv"
    master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
      dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

  }

  county_master <- master %>% ## assigning counties to the districts that do not have CCD counties but that have F33 revenue data, the assigned counties come from the geo intersection
    dplyr::mutate(County = case_when (NAME == "Exeter Unified School District" ~ "Tulare County",
                             NAME == "Bonsall Unified School District" ~ "San Diego  County",
                             NAME == "Santa Paula Unified School District" ~ "Ventura County",
                             NAME == "Washington Twp Sch Dist" & NCESID == 3417040 ~ "Burlington County",
                             NAME == "Twin Buttes School District #37" ~ "Dunn County",
                             NAME == "Mandaree Sch Dist 36" ~ "Dunn County",
                             NAME == "Sigel Twp Sch Dist 6" ~ "Huron County",
                             NAME == "White Shield Sch Dist 85" ~ "Mclean County",
                             NAME == "Belcourt Sch District 7" ~ "Rolette County",
                             TRUE ~ as.character(County)))

  if(pooling_level == "county") {    ##### districts with county- level local revenue pooling #####
    county_districts <- county_master %>%
      dplyr::group_by(State, County) %>%
      dplyr::summarise(LR_cnty = sum(LR, na.rm = TRUE),
              enroll_f33 = sum(ENROLL, na.rm = TRUE),
              LRPP_cnty = LR_cnty/enroll_f33) %>%
      dplyr::right_join(master, by = c("State", "County")) %>%
      dplyr::ungroup() %>%
      dplyr:: mutate(SR_cnty = mean(SRPP, na.rm =TRUE),
           SRPP_cnty = SRPP,
           SLRPP_cnty = LRPP_cnty + SRPP) %>% ## total revenue per pupil is the local revenue per pupil pooled at the county level plus the state rev per pupil they were getting before
      dplyr::mutate(SLRPP_diff = round2(SLRPP_cnty - SLRPP, 0)) %>% ### calculate difference in total revenue between current and county-level, local pool system
      dplyr::filter(!is.na(SLRPP_diff)) %>%
      dplyr::mutate(outcome = case_when(SLRPP_diff < 0 ~ "less",
                            SLRPP_diff >= 0 ~ "same or more",
                            TRUE ~ as.character(NA))) %>%
      dplyr::select(NCESID, State, County, NAME, LRPP, LRPP_cnty, SRPP_cnty, SRPP, SLRPP_cnty, SLRPP, SLRPP_diff, outcome,
                    dEnroll_district, pctNonwhite, FRL_rate, StPovRate, MHI) %>%
      dplyr::mutate_at(vars(LRPP:SLRPP_diff), ~(round2(.,0))) %>%
      dplyr::mutate(NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"))

    return(county_districts)

  }

  else if (pooling_level == "state") {  ##### districts with state- level local revenue pooling #####
    state_districts <- county_master %>%
      dplyr::group_by(State) %>%
      dplyr::summarise(LR_state = sum(LR, na.rm = TRUE),
                enroll_f33 = sum(ENROLL, na.rm = TRUE),
                LRPP_state = LR_state/enroll_f33) %>%
      dplyr::right_join(master, by = c("State")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(SR_state = mean(SRPP, na.rm =TRUE),
             SRPP_state = SRPP,
             SLRPP_state = LRPP_state + SRPP) %>% ## total revenue per pupil is the local revenue per pupil pooled at the state level plus the state rev per pupil they were getting before
      dplyr::mutate(SLRPP_diff = round2(SLRPP_state - SLRPP, 0)) %>% ### calculate difference in total revenue between current and state-level, local pool system
      dplyr::filter(!is.na(SLRPP_diff)) %>%
      dplyr::mutate(outcome = case_when(SLRPP_diff < 0 ~ "less",
                                 SLRPP_diff >= 0 ~ "same or more",
                                 TRUE ~ as.character(NA))) %>%
      dplyr::select(NCESID, State, County, NAME, LRPP, LRPP_state, SRPP_state, SRPP, SLRPP_state, SLRPP, SLRPP_diff, outcome,
                    dEnroll_district, pctNonwhite, FRL_rate, StPovRate, MHI) %>%
      dplyr::mutate_at(vars(LRPP:SLRPP_diff), ~(round2(.,0))) %>%
      dplyr::mutate(NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"))

    return(state_districts)

  }

  else{
    message("Error: Revenue pooling is only available at the state or county level")
  }

}
