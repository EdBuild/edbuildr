#' A function to find the difference between school district neighbors
#'
#' This function allows you to find the difference between each pair of school
#' district neighbors and calculate the national rank from largest to smallest.
#' @param data_year Four digit year of master data to pull in. Options include
#'   2013- 2018. Defaults to 2018.
#' @param diff_var Character string on which to rank the difference between
#'   school district neighbors. Use diff_var = “options” to print a list of the variables.
#'   Defaults to Percentage Point Difference in
#'   Poverty Rate.
#' @param type Character string to indicate which types of neighbors to return.
#'   Defaults to "like" which returns a list of neighbors that are the same
#'   district type (that is, unified to unified, elementary to elementary and
#'   secondary to secondary). To view all neighbors use "all". This selection
#'   becomes important for districts like Chicago which have upwards of 50
#'   neighboring school districts, but only 1 type-like neighbor. Chicago is a unified
#'   district and it has 1 neighbor that is also unified, 16 neighbors that are secondary
#'   districts, and 32 neighbors that are elementary districts.
#' @keywords neighbors difference rank EdBuild
#' @usage neigh_diff(data_year= "2018",
#'  diff_var="Percentage Point Difference in Poverty Rate", type= "like")
#' @import dplyr magrittr
#' @return A dataframe where each observation is a pair of neighboring school
#'   districts.
#' @seealso \code{\link{masterpull}}, \code{\link{master_codebook}},
#'   \code{\link{sd_neighbor_xlsx}}
#' @export
#' @examples
#' \donttest{tr_diff <- neigh_diff(
#'            data_year = "2018",
#'            diff_var = "Difference in Total Revenue Per Pupil"
#'            )}

neigh_diff = function(data_year = "2018", diff_var="Percentage Point Difference in Poverty Rate", type= "like") {

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

      pairs_url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Pairs/pairs_1314.csv"
      pairs <- read.csv(file = pairs_url, stringsAsFactors = FALSE)

    }

    else if(as.numeric(data_year)==2014) {
      url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2014/full_data_14_geo_exc.csv"
      master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
        dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

      pairs_url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Pairs/pairs_1314.csv"
      pairs <- read.csv(file = pairs_url, stringsAsFactors = FALSE)

    }

    else if(as.numeric(data_year)==2015) {
      url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2015/full_data_15_geo_exc.csv"
      master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
        dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

      pairs_url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Pairs/pairs_1516.csv"
      pairs <- read.csv(file = pairs_url, stringsAsFactors = FALSE)

    }

    else if(as.numeric(data_year)==2016)  {
      url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2016/full_data_16_geo_exc.csv"
      master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
        dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

      pairs_url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Pairs/pairs_1516.csv"
      pairs <- read.csv(file = pairs_url, stringsAsFactors = FALSE)

    }

    else if(as.numeric(data_year)==2017) {
      url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2017/full_data_17_geo_exc.csv"
      master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
        dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

      pairs_url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Pairs/pairs_1617.csv"
      pairs <- read.csv(file = pairs_url, stringsAsFactors = FALSE)

    }

  else if(as.numeric(data_year)==2018) {
    url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/full_data_18_geo_exc.csv"
    master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
      dplyr::mutate(FRL_rate = dFRL/dEnroll_district)

    pairs_url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Pairs/pairs_1617.csv"
    pairs <- read.csv(file = pairs_url, stringsAsFactors = FALSE)

  }

    master_names <- names(master) ## making vector of names in master to apply below with either _1 or _2

    master1 <- master %>%
      magrittr::set_colnames(paste(master_names, "_1", sep = "")) ## making a master file where variables have _1 after name

    master2 <- master %>%
      magrittr::set_colnames(paste(master_names, "_2", sep = "")) ## making a master file where variables have _2 after name

    pair_data <- pairs %>% ## Combine variables + neighbors
      dplyr::left_join(master1, by = c("GEOID" = "NCESID_1")) %>% ## First, join variables to neighbor list (base) - ncesid_1
      dplyr::left_join(master2, by = c("GEOID.1" = "NCESID_2")) %>% ## Then, join variables to neighbor list (pair) - the neighbor pair
      dplyr::filter(State_1 == State_2,      ## filtering out pairs where they are across state lines
           length >= 152.4) %>%             ## filter so districts are only neighbors if they share more than 500ft of border line
      dplyr::select(-STATE_FIPS_2, -State_2, -CONUM_1, -CONUM_2, -SRPP_cola_1, -LRPP_cola_1, -SLRPP_cola_1,
             -SRPP_cola_2, -LRPP_cola_2, -SLRPP_cola_2) %>%
      dplyr::rename(State = State_1,
             FIPS = STATE_FIPS_1,
             `border_length (meters)` = length)

    pairs_data_diff <- pair_data %>% ## Calculate difference between neighbors
      dplyr::mutate(lrpp_diff_pct = ((LRPP_1 - LRPP_2) / LRPP_1),
             srpp_diff_pct = ((SRPP_1 - SRPP_2) / SRPP_1),
             slrpp_diff_pct = ((SLRPP_1 - SLRPP_2) / SLRPP_1),
             lrpp_diff_abs = (LRPP_1 - LRPP_2),
             srpp_diff_abs = (SRPP_1 - SRPP_2),
             slrpp_diff_abs = (SLRPP_1 - SLRPP_2),
             mhi_diff_pct = ((MHI_1 - MHI_2) / MHI_1),
             mpv_diff_pct = ((MPV_1 - MPV_2) / MPV_1),
             mhi_diff_abs = (MHI_1 - MHI_2),
             mpv_diff_abs = (MPV_1 - MPV_2),
             pctnonwhite_diff = pctNonwhite_1 - pctNonwhite_2,
             povrate_diff = StPovRate_1 - StPovRate_2,
             enroll_f33_diff = (ENROLL_1 - ENROLL_2) / ENROLL_1,
             lr_diff = (LR_1 - LR_2) / LR_1,
             sr_diff = (SR_1 - SR_2) / SR_1,
             slr_diff = (SLR_1 - SLR_2) / SLR_1,
             op_schools_diff = (dOperational_schools_1 - dOperational_schools_2) / dOperational_schools_1,
             enroll_cdd_diff = (dEnroll_district_1 - dEnroll_district_2) / dEnroll_district_1,
             frl_diff = (dFRL_1 - dFRL_2) / dFRL_1,
             frl_rate_diff = FRL_rate_1 - FRL_rate_2,
             iep_diff = (dIEP_1 - dIEP_2) / dIEP_1,
             lep_diff = (dLEP_1 - dLEP_2) / dLEP_1,
             white_diff = (dWhite_1 - dWhite_2) / dWhite_1,
             hispanic_diff = (dHispanic_1 - dHispanic_2) / dHispanic_1,
             black_diff = (dBlack_1 - dBlack_2) / dBlack_1,
             asian_pi_diff = (dAsian_PI_1 - dAsian_PI_2) /dAsian_PI_1,
             hawaiian_pi_diff = (dHawaiian_PI_1 - dHawaiian_PI_2) / dHawaiian_PI_1,
             amInd_AKK_diff = (dAmIndian_Aknative_1 - dAmIndian_Aknative_2) / dAmIndian_Aknative_1,
             mult_race_diff = (d2plus_races_1 - d2plus_races_2) / d2plus_races_1,
             tpop_diff = (TPop_1 - TPop_2) / TPop_1,
             stpop_diff = (StPop_1 - StPop_2) / StPop_1,
             stpov_diff = (StPov_1 - StPov_2) / StPov_1,
             sd_area_diff = (sd_area_1 - sd_area_2) / sd_area_1,
             student_per_sq_mile_diff = (student_per_sq_mile_1 - student_per_sq_mile_2) / student_per_sq_mile_1) %>%
      dplyr::rename(NCESID_1 = GEOID,
           NCESID_2 = GEOID.1,
            `District Name` = NAME_1,
           `District County` = County_1,
             `District Enrollment, F33` = ENROLL_1,
             `District Enrollment, CCD` = dEnroll_district_1,
             `District Local Revenue` = LR_1,
             `District State Revenue` = SR_1,
             `District Total Revenue` = SLR_1,
             `District Local Revenue PP` = LRPP_1,
             `District State Revenue PP` = SRPP_1,
             `District Total Revenue PP` = SLRPP_1,
             `District MHI` = MHI_1,
             `District MPV` = MPV_1,
              `District Percent FRL` = FRL_rate_1,
             `District Percent Nonwhite` = pctNonwhite_1,
             `District Poverty Rate` = StPovRate_1,
             `District Type` = dType_1,
             `District Urbanicity` = dUrbanicity_1,
             `District Number of Operational Schools` = dOperational_schools_1,
             `District Number of FRL Students` = dFRL_1,
             `District Number of LEP Students` = dLEP_1,
             `District Number of IEP Students` = dIEP_1,
             `District Number of White Students` = dWhite_1,
             `District Number of Black Students` = dBlack_1,
             `District Number of Hispanic Students` = dHispanic_1,
             `District Number of Asian and PI Students` = dAsian_PI_1,
             `District Number of Hawaiian and PI Students` = dHawaiian_PI_1,
             `District Number of American Indian and AK Native Students` = dAmIndian_Aknative_1,
             `District Number of Students 2 or More Races` = d2plus_races_1,
             `District Total Population` = TPop_1,
             `District Student Population` = StPop_1,
             `District Number of Students in Poverty` = StPov_1,
             `District Area (square miles)` = sd_area_1,
             `District Students per Square Mile` = student_per_sq_mile_1,
             `District Level` = sd_type_1,
             `Neighbor Name` = NAME_2,
             `Neighbor County` = County_2,
             `Neighbor Enrollment, F33` = ENROLL_2,
             `Neighbor Enrollment, CCD` = dEnroll_district_2,
             `Neighbor Local Revenue` = LR_2,
             `Neighbor State Revenue` = SR_2,
             `Neighbor Total Revenue` = SLR_2,
             `Neighbor Local Revenue PP` = LRPP_2,
             `Neighbor State Revenue PP` = SRPP_2,
             `Neighbor Total Revenue PP` = SLRPP_2,
             `Neighbor Percent FRL` = FRL_rate_2,
             `Neighbor MHI` = MHI_2,
             `Neighbor MPV` = MPV_2,
             `Neighbor Percent Nonwhite` = pctNonwhite_2,
             `Neighbor Poverty Rate` = StPovRate_2,
             `Neighbor Type` = dType_2,
             `Neighbor Urbanicity` = dUrbanicity_2,
             `Neighbor Number of Operational Schools` = dOperational_schools_2,
             `Neighbor Number of FRL Students` = dFRL_2,
             `Neighbor Number of LEP Students` = dLEP_2,
             `Neighbor Number of IEP Students` = dIEP_2,
             `Neighbor Number of White Students` = dWhite_2,
             `Neighbor Number of Black Students` = dBlack_2,
             `Neighbor Number of Hispanic Students` = dHispanic_2,
             `Neighbor Number of Asian and PI Students` = dAsian_PI_2,
             `Neighbor Number of Hawaiian and PI Students` = dHawaiian_PI_2,
             `Neighbor Number of American Indian and AK Native Students` = dAmIndian_Aknative_2,
             `Neighbor Number of Students 2 or More Races` = d2plus_races_2,
             `Neighbor Total Population` = TPop_2,
             `Neighbor Student Population` = StPop_2,
             `Neighbor Number of Students in Poverty` = StPov_2,
             `Neighbor Area (square miles)` = sd_area_2,
             `Neighbor Students per Square Mile` = student_per_sq_mile_2,
             `Neighbor Level` = sd_type_2,
            `Percent Difference in Local Revenue Per Pupil` = lrpp_diff_pct,
            `Percent Difference in State Revenue Per Pupil` = srpp_diff_pct,
            `Percent Difference in Total Revenue Per Pupil` = slrpp_diff_pct,
            `Percent Difference in Median Household Income` = mhi_diff_pct,
            `Percent Difference in Median Property Value` = mpv_diff_pct,
            `Difference in Local Revenue Per Pupil` = lrpp_diff_abs,
            `Difference in State Revenue Per Pupil` = srpp_diff_abs,
            `Difference in Total Revenue Per Pupil` = slrpp_diff_abs,
            `Difference in Median Household Income` = mhi_diff_abs,
            `Difference in Median Property Value` = mpv_diff_abs,
            `Difference in Percent FRL` = frl_rate_diff,
            `Percentage Point Difference in Percent Nonwhite Enrollment` = pctnonwhite_diff,
            `Percentage Point Difference in Poverty Rate` = povrate_diff,
            `Percent Difference in Enrollment, F33`= enroll_f33_diff,
            `Percent Difference in Local Revenue` = lr_diff,
            `Percent Difference in State Revenue` =sr_diff,
            `Percent Difference in Total Revenue` = slr_diff,
            `Percent Difference in Number of Operational Schools`=  op_schools_diff,
            `Percent Difference in Enrollment, CCD`  = enroll_cdd_diff,
            `Percent Difference in FRL Students` = frl_diff,
            `Percent Difference in IEP Students` =  iep_diff,
            `Percent Difference in LEP Students` = lep_diff,
            `Percent Difference in White Enrollment` = white_diff,
            `Percent Difference in Hispanic Enrollment` = hispanic_diff,
            `Percent Difference in Black Enrollment` = black_diff,
            `Percent Difference in Asian Enrollment` = asian_pi_diff,
            `Percent Difference in Hawaiian and PI Enrollment` = hawaiian_pi_diff,
            `Percent Difference in American Indian and AK Native Enrollment` = amInd_AKK_diff,
            `Percent Difference in Multi-Racial Enrollment` = mult_race_diff,
            `Percent Difference in Total Population` =tpop_diff,
            `Percent Difference in Student Population` =stpop_diff,
            `Percent Difference in Students Living in Poverty` =stpov_diff,
            `Percent Difference in District Area` = sd_area_diff,
            `Percent Difference in Students Per Square Mile` = student_per_sq_mile_diff)

    if(type == "like") {

      pairs_data_diff <- pairs_data_diff %>%
        dplyr::filter(`District Level` == `Neighbor Level`)   ## only compare districts of same type (uni-uni, elem-elem, sec-sec)

              if(tidyselect::all_of(diff_var) %in% names(pairs_data_diff) == TRUE) {
                pairs_data_diff <- pairs_data_diff %>%
                  dplyr::filter(dplyr::case_when(get(diff_var) > 0 ~ get(diff_var) > 0, ## filter reciprocals based on diff_var first
                                          get(diff_var) == 0 ~ NCESID_1 > NCESID_2, ### then if diff_var is the same across neighbors, filter by larger GEOID
                                          TRUE ~ get(diff_var) > 0)) %>%
                  dplyr::arrange(desc(get(diff_var))) %>% ### get() converts the diff_var from string to variable name
                  dplyr::mutate(rank = row_number()) %>%
                  dplyr::select(NCESID_1, NCESID_2, u_id, rank, State, `District Name`,
                                `Neighbor Name`, tidyselect::all_of(diff_var), `District Enrollment, F33`:`Neighbor Percent FRL`)

                colnames(pairs_data_diff)[4] <- paste("Rank", tidyselect::all_of(diff_var), sep = "_")

                return (pairs_data_diff)
              }

            else if(tidyselect::all_of(diff_var) == "options") {
              diff_list <- pairs_data_diff %>%
                dplyr::select(`Percent Difference in Local Revenue Per Pupil`:`Percent Difference in Students Per Square Mile`)
              print(names(diff_list)) ## runs if diff_var is not in variable name list of pairs_data_diff
              message("Please see above for variables you can use to rank neighbors.")
            }

            else {
              diff_list <- pairs_data_diff %>%
                dplyr::select(`Percent Difference in Local Revenue Per Pupil`:`Percent Difference in Students Per Square Mile`)
              print(names(diff_list)) ## runs if diff_var is not in variable name list of pairs_data_diff
              message("Error: The difference variable you selected is not included in the master dataset.
                      Please see above for variables you can use to rank neighbors.")

            }
    }

    else if (type == "all") {
        if(tidyselect::all_of(diff_var) %in% names(pairs_data_diff) == TRUE) {
          pairs_data_diff <- pairs_data_diff %>%
            dplyr::filter(dplyr::case_when(get(diff_var) > 0 ~ get(diff_var) > 0, ## filter reciprocals based on diff_var first
                                    get(diff_var) == 0 ~ NCESID_1 > NCESID_2, ### then if diff_var is the same across neighbors, filter by larger GEOID
                                    TRUE ~ get(diff_var) > 0)) %>%
            dplyr::arrange(desc(get(diff_var))) %>% ### get() converts the diff_var from string to variable name
            dplyr::mutate(rank = row_number()) %>%
            dplyr::select(NCESID_1, NCESID_2, u_id, rank, State, `District Name`,
                          `Neighbor Name`, tidyselect::all_of(diff_var), `District Enrollment, F33`:`Neighbor Percent FRL`)

          colnames(pairs_data_diff)[4] <- paste("Rank", tidyselect::all_of(diff_var), sep = "_")

          return (pairs_data_diff)
        }

      else if(tidyselect::all_of(diff_var) == "options") {
        diff_list <- pairs_data_diff %>%
          dplyr::select(`Percent Difference in Local Revenue Per Pupil`:`Percent Difference in Students Per Square Mile`)
        print(names(diff_list)) ## runs if diff_var is not in variable name list of pairs_data_diff
        message("Please see above for variables you can use to rank neighbors.")
      }

      else {
        diff_list <- pairs_data_diff %>%
          dplyr::select(`Percent Difference in Local Revenue Per Pupil`:`Percent Difference in Students Per Square Mile`)
        print(names(diff_list)) ## runs if diff_var is not in variable name list of pairs_data_diff
        message("Error: The difference variable you selected is not included in the master dataset.
                      Please see above for variables you can use to rank neighbors.")

      }
    }
}
