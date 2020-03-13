#' @title A function to create a table of a school district and all its neighbors
#' @name sd_neighbor_xlsx
#' @description This function allows you to write out a table of any school
#'   district and its neighbors with selected data from EdBuild's master
#'   datafile, ready to export as a formatted excel file.
#' @param data_year Four digit year of master data to pull in. Options include
#'   2013- 2017. Defaults to 2017.
#' @param school_district Seven digit NCESID of the school district. Default is
#'   NULL. To find the NCESID for any school district, use
#'   \code{\link{masterpull}} to search for your district.
#' @param table_vars Variable or list of variables to include in the table. Use
#'   tables_vars = “options” to print a list of the variables. Defaults to:
#'   Name; Enrollment; Poverty Rate; Percent Nonwhite; Local Revenue, per Pupil;
#'   State Revenue, per Pupil
#' @keywords neighbors difference table EdBuild
#' @import dplyr openxlsx scales stringr magrittr
#' @usage sd_neighbor_xlsx(data_year = "2017", school_district = NULL,
#'   table_vars = c('Name', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite',
#'   'Local Revenue PP', 'State Revenue PP', 'Type'))
#' @return An excel workbook which can be written out with
#'   \code{openxlsx::saveWorkbook(my_table, file =
#'   '~/Documents/neighbor_table.xlsx', overwrite = TRUE)}
#' @seealso \code{\link{sd_table_xlsx}}, \code{\link{master_codebook}},
#'   \code{\link{masterpull}} \code{\link{neigh_diff}}
#' @export
#' @examples
#' \donttest{table <- sd_neighbor_xlsx(
#'          data_year = "2017",
#'          school_district = "3402640",
#'          table_vars = c("Name",
#'                        "Percent FRL",
#'                        "Median Household Income",
#'                         "State Revenue PP")
#'         )}


sd_neighbor_xlsx = function (data_year = "2017", school_district = NULL, table_vars = c('Name', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite', 'Local Revenue PP', 'State Revenue PP', 'Type')) {


  dist_list <- neigh_diff(data_year='2017', diff_var="Percentage Point Difference in Poverty Rate", type = "all") %>%
    dplyr::mutate(NCESID_1 = stringr::str_pad(NCESID_1, 7, pad="0"),
                  NCESID_2 = stringr::str_pad(NCESID_2, 7, pad="0"))

  dist_list_1 <- dist_list %>%
    dplyr::select(NCESID = NCESID_1) %>%
    dplyr::distinct()

  dist_list_2 <- dist_list %>%
    dplyr::select(NCESID = NCESID_2) %>%
    dplyr::distinct()

  listid <- dplyr::bind_rows(dist_list_1, dist_list_2) %>%
    dplyr::distinct()

  ncesid <- listid$NCESID

  # this is the default list of variables to pull
  to_pull <- c("District Name",
               "District Enrollment, CCD",
               "District Poverty Rate",
               "District Percent Nonwhite",
               "District Local Revenue PP",
               "District State Revenue PP",
               "District Level",
               "Neighbor Name",
               "Neighbor Enrollment, CCD",
               "Neighbor Poverty Rate",
               "Neighbor Percent Nonwhite",
               "Neighbor Local Revenue PP",
               "Neighbor State Revenue PP",
               "Neighbor Level")

  to_default <- c('Name', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite', 'Local Revenue PP', 'State Revenue PP', "Type")

  print_names <- c("Name",
                   "County",
                   "Enrollment",
                   "Percent FRL",
                   "Poverty Rate",
                   "Percent Nonwhite",
                   "Local Revenue PP",
                   "State Revenue PP",
                   "Total Revenue PP",
                   "Median Household Income",
                   "Median Property Value",
                   "Type")

  good <- c('Name', 'County', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite', 'Percent FRL',
            'Local Revenue PP', 'State Revenue PP', 'Total Revenue PP',
            'Median Household Income', 'Median Property Value', 'Type')

  if (as.numeric(data_year)<2013) {
    message("Error: Master datasets are only available for years 2013 through 2017")
  }

  else if (as.numeric(data_year)>2017) {
    message("Error: Master datasets are only available for years 2013 through 2017")
  }

  else if (is.null(school_district)) {
    message( "To generate a table, specify a school district by its NCESID.
    If you do not know the NCESID of a school district, use masterpull() to search for your district.")
  }

  else if(school_district %in% ncesid == FALSE) {
    message( "The district you specified is not available. To generate a table, specify a school district by its NCESID.
    If you do not know the NCESID of a school district, use masterpull() to search for your district.")
  }

  else if(setequal(intersect(table_vars, good), table_vars) == FALSE){
    message("Error: Use one or more of the following variables to generate a table:
              table_vars = c('Name', 'County', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite', 'Percent FRL',
                     'Local Revenue PP', 'State Revenue PP', 'Total Revenue PP',
                     'Median Property Value', 'Median Houshold Income', 'Type')")
  }

  else  {
    school_district

    message("NOTE:: save your workbook to the desired location using:
          openxlsx::saveWorkbook(my_table, file =  '~/Documents/neighbor_table.xlsx', overwrite = TRUE)")


  select_cols <-  if (!setequal(table_vars, to_default)) {

    for (i in 1:length(table_vars)) {

      table_iterate <- table_vars[i]

      if ("Name" %in% table_iterate) {
        temp_vars <- c("District Name", "Neighbor Name")
      }
      else if ("Enrollment" %in% table_iterate) {
        temp_vars <- c("District Enrollment, CCD", "Neighbor Enrollment, CCD")
      }
      else if ("County" %in% table_iterate) {
        temp_cars <- c("District County", "Neighbor County")
      }
      else if ("Percent FRL" %in% table_iterate) {
        temp_vars <- c("District Percent FRL", "Neighbor Percent FRL")
      }
      else if ("Poverty Rate" %in% table_iterate) {
        temp_vars <- c("District Poverty Rate", "Neighbor Poverty Rate")
      }
      else if ("Percent Nonwhite" %in% table_iterate) {
        temp_vars <- c("District Percent Nonwhite", "Neighbor Percent Nonwhite")
      }
      else if ("Local Revenue PP" %in% table_iterate) {
        temp_vars <- c("District Local Revenue PP", "Neighbor Local Revenue PP")
      }
      else if ("State Revenue PP" %in% table_iterate) {
        temp_vars <- c("District State Revenue PP", "Neighbor State Revenue PP")
      }
      else if ("Total Revenue PP" %in% table_iterate) {
        temp_vars <- c("District Total Revenue PP", "Neighbor Total Revenue PP")
      }
      else if ("Median Household Income" %in% table_iterate){
        temp_vars <- c("District MHI", "Neighbor MHI")
      }
      else if("Median Property Value" %in% table_iterate) {
        temp_vars <- c("District MPV", "Neighbor MPV")
      }
      else if("Type" %in% table_iterate) {
        temp_vars <- c("District Level", "Neighbor Level")
      }
      else {
        print(print_names)
        message("Please see above for variables you can use to generate a table")
      }

      if(i == 1) {
        additional_vars <- temp_vars
        }
      else{
        additional_vars <- rbind(additional_vars, temp_vars)
      }
    }

    additional_vars <- as.vector(t(additional_vars))
  }
  else {
    to_pull
  }

  table_data <- neigh_diff(data_year, diff_var="Percentage Point Difference in Poverty Rate", type = "all") %>%
    dplyr::mutate(NCESID_1 = stringr::str_pad(NCESID_1, 7, pad="0"),
                  NCESID_2 = stringr::str_pad(NCESID_2, 7, pad="0")) %>%  #make sure that NCESID is properly formatted
    dplyr::filter(NCESID_1 == school_district | NCESID_2 == school_district) %>%
    dplyr::mutate(code = ifelse(NCESID_1 == school_district, 1,
                              ifelse(NCESID_2 == school_district, 2, 0))) %>%
    dplyr::select(all_of(select_cols), code)

  dist1 <- table_data %>%
    dplyr::filter(code==1) %>%
    dplyr::select(matches("District") | matches("code")) %>%
    dplyr::distinct()

  dist2 <- table_data %>%
    dplyr::filter(code==2) %>%
    dplyr::select(matches("Neighbor") | matches("code")) %>%
    dplyr::distinct()

  colnames(dist1) <- sub("District ", "", colnames(dist1))
  colnames(dist2) <- sub("Neighbor ", "", colnames(dist2))

  dist <- bind_rows(dist1, dist2)

  table_data_dist <- table_data %>%
    dplyr::select(-matches("District")) %>%
    dplyr::select(-code, everything())

  colnames(table_data_dist) <- sub("Neighbor ", "", colnames(table_data_dist))

  table_data_neighb <- table_data %>%
    dplyr::select(-matches("Neighbor")) %>%
    dplyr::select(-code, everything())

  colnames(table_data_neighb) <- sub("District ", "", colnames(table_data_neighb))

  table_reform <- bind_rows(dist, table_data_dist, table_data_neighb) %>%
    dplyr::distinct() %>%
    dplyr::select(-code) %>%
    dplyr::distinct()

  if (nrow(table_reform)==1) {

    message("NOTE:: Your table has one row because the school district does not have any neighbors of the same type.  For more information on neighbor types, see neigh_diff documentation.")
  }
  else(
    message("")
  )

  # format all potential columns
  # percent variables
  table_reform <-  if (!is.null(table_reform$`Percent Nonwhite`)) {

    table_reform <- table_reform %>%
      dplyr::mutate(`Percent Nonwhite` = scales::percent(round2(`Percent Nonwhite`, 2), accuracy = 1L))
  } else {
    table_reform
  }

  table_reform <- if (!is.null(table_reform$`Poverty Rate`)) {

    table_reform <- table_reform %>%
      dplyr::mutate(`Poverty Rate` = scales::percent(round2(`Poverty Rate`, 2), accuracy = 1L ))
  } else {
    table_reform
  }

  table_reform <- if (!is.null(table_reform$`Percent FRL`)) {

    table_reform <- table_reform %>%
      dplyr::mutate(`Percent FRL` = scales::percent(round2(`Percent FRL`, 2), accuracy = 1L ))
  } else {
    table_reform
  }

  # currency variables
  table_reform <- if (!is.null(table_reform$`Local Revenue PP`)) {

    table_reform <- table_reform %>%
      dplyr::mutate(`Local Revenue PP` = scales::dollar(round2(`Local Revenue PP`, 0)))
  } else {
    table_reform
  }

  table_reform <- if (!is.null(table_reform$`State Revenue PP`)) {

    table_reform <- table_reform %>%
      dplyr::mutate(`State Revenue PP` = scales::dollar(round2(`State Revenue PP`, 0)))
  } else {
    table_reform
  }

  table_reform <- if (!is.null(table_reform$`Total Revenue PP`)) {

    table_reform <- table_reform %>%
      dplyr::mutate(`Total Revenue PP` = scales::dollar(round2(`Total Revenue PP`, 0)))
  } else {
    table_reform
  }

  table_reform <- if (!is.null(table_reform$`MHI`)) {

    table_reform <- table_reform %>%
      dplyr::mutate(MHI = scales::dollar(round2(MHI, 0)))
  } else {
    table_reform
  }

  table_reform <- if (!is.null(table_reform$`MPV`)) {

    table_reform <- table_reform %>%
      dplyr::mutate(MPV = scales::dollar(round2(MPV, 0)))
  } else {
    table_reform
  }


  # numeric variables
  table_reform <- if (!is.null(table_reform$`Enrollment, CCD`)) {

    table_reform <- table_reform %>%
      dplyr::mutate(`Enrollment, CCD` = scales::comma(`Enrollment, CCD`))
  }

  else {
    table_reform
  }

  search_for_these <- c("Name", "County",
                        "Local Revenue PP", "State Revenue PP", "Total Revenue PP",
                        "Enrollment, CCD",
                        "Percent FRL",
                        "Percent Nonwhite",
                        "Poverty Rate",
                        "MPV", "MHI", "Level")


  replace_with_these <- c("Name", "County",
                          "Local Revenue, Per Pupil", "State Revenue, Per Pupil", "Total Revenue, Per Pupil",
                          "Enrollment",
                          "Percent FRL",
                          "Percent Nonwhite",
                          "Poverty Rate",
                          "Median Property Value",  "Median Household Income", "Type")

  found <- match(colnames(table_reform), search_for_these, nomatch = 0)
  colnames(table_reform)[colnames(table_reform) %in% search_for_these] <- replace_with_these[found]

  NameId = table_reform$Name[1]
  NameId = substr(NameId, 0, 30)

  # create formatted excel
  Workbook_ = openxlsx::createWorkbook()
  sheet_name <- paste0(NameId)
  ### make sheet
  openxlsx::addWorksheet(wb = Workbook_, sheetName = sheet_name)
  ### Create header format
  col_names = openxlsx::createStyle(wrapText = TRUE, textDecoration = 'Bold', halign = 'center', valign = 'center', border = 'TopBottomLeftRight',
                                    fgFill = "#5a8898", fontColour = "white")
  ### Create cell format.
  col_values = openxlsx::createStyle(halign = 'center', valign = 'center', border = 'TopBottomLeftRight')

  openxlsx::writeData(wb = Workbook_, sheet = sheet_name, x = table_reform, headerStyle = col_names)
  openxlsx::addStyle(wb = Workbook_, sheet = sheet_name, style = col_values, rows = 1:nrow(table_reform) + 1, cols = 2:ncol(table_reform),
                     gridExpand = TRUE, stack = TRUE)

  Workbook_
  return(Workbook_)

  }
}




