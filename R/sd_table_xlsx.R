#' @title A function to create a table of school districts in a selected state
#'   or county
#' @name sd_table_xlsx
#' @description This function allows you to write out a table of any school
#'   districts with selected data as a formatted excel file.
#' @param data_year Four digit year of master data to pull in. Options include
#'   2013- 2018. Defaults to 2018.
#' @param state State name. Defaults to New Jersey.
#' @param county County name. Defaults to NULL.
#' @param table_vars Variable or list of variables to include in the table. Use
#'   tables_vars = “options” to print a list of the variables. Defaults to:
#'   Name; Enrollment; Poverty Rate; Percent Nonwhite; Local Revenue, Per Pupil;
#'   State Revenue, Per Pupil; Total Revenue, Per Pupil.
#' @keywords neighbors difference table EdBuild
#' @import dplyr openxlsx scales stringr magrittr
#' @usage sd_table_xlsx(data_year = '2018', state = "New Jersey", county = NULL,
#'   table_vars = c('Name', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite',
#'   'Local Revenue PP', 'State Revenue PP', 'Total Revenue PP'))
#' @return An excel workbook which can be written out with
#'   \code{openxlsx::saveWorkbook(my_table, file =
#'   '~/Documents/state_year.xlsx', overwrite = TRUE)}
#' @seealso \code{\link{sd_neighbor_xlsx}}, \code{\link{master_codebook}},
#'   \code{\link{masterpull}}
#' @export
#' @examples
#' \donttest{my_table <- sd_table_xlsx(data_year = "2018",
#'             state = "Maryland",
#'             county = c("Baltimore County", "Baltimore City", "Howard County", "Carroll County"),
#'             table_vars = c("Name", "Poverty Rate")
#'              )}


sd_table_xlsx = function (data_year = '2018', state = "New Jersey", county = NULL, table_vars = c('Name', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite', 'Local Revenue PP', 'State Revenue PP', 'Total Revenue PP')) {

  good <- c('NCESID', 'Name', 'County', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite', 'Percent FRL',
            'Local Revenue PP', 'State Revenue PP', 'Total Revenue PP',
            'MPV', 'MHI')

  county_name <- masterpull(data_year='2018', data_type = "fin") %>%
      dplyr::mutate(NCESID = stringr::str_pad(NCESID, 7, pad="0"),
                    County = stringr::str_trim(County)) %>%  #make sure that NCESID is properly formatted
      dplyr::filter(State == state) %>%
      dplyr::select(County)

  county_check <- county_name$County

  if (as.numeric(data_year)<2013) {
    message("Error: Master datasets are only available for years 2013 through 2018")
  }

  else if (as.numeric(data_year)>2018) {
    message("Error: Master datasets are only available for years 2013 through 2018")
  }

  else if (length(county) > 0 & setequal(intersect(county, county_check), county) == FALSE) {
    message( "The county you specified is not available. Use masterpull() to see the list of counties available in your state.")
  }

  else if(setequal(intersect(table_vars, good), table_vars) == FALSE){
    message("Error: Use one or more of the following variables to generate a table:
              table_vars = c('NCESID', 'Name', 'County', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite', 'Percent FRL',
                     'Local Revenue PP', 'State Revenue PP', 'Total Revenue PP',
                     'MPV', 'MHI')")
  }

  else{

  message("NOTE:: save your workbook to the desired location using:
          openxlsx::saveWorkbook(my_table, file =  '~/Documents/state_year.xlsx', overwrite = TRUE)")

  # this is the default list of variables to pull

  to_default <- c('Name', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite', 'Local Revenue PP', 'State Revenue PP', 'Total Revenue PP')

  to_pull <- c("NAME",
               "dEnroll_district",
               "StPovRate",
               "pctNonwhite",
               "SRPP",
               "LRPP",
               "SLRPP")

  if_county <- c("County")


  select_cols <-  if (!setequal(table_vars, to_default)) {

    for (i in 1:length(table_vars)) {

      table_iterate <- table_vars[i]

      if ("Name" %in% table_iterate) {
        temp_vars <- c("NAME")
      }
      else if ("Enrollment" %in% table_iterate) {
        temp_vars <- c("dEnroll_district")
      }
      else if ("County" %in% table_iterate) {
        temp_vars <- c("County")
      }
      else if ("Percent FRL" %in% table_iterate) {
        temp_vars <- c("FRL_rate")
      }
      else if ("Poverty Rate" %in% table_iterate) {
        temp_vars <- c("StPovRate")
      }
      else if ("Percent Nonwhite" %in% table_iterate) {
        temp_vars <- c("pctNonwhite")
      }
      else if ("Local Revenue PP" %in% table_iterate) {
        temp_vars <- c("LRPP")
      }
      else if ("State Revenue PP" %in% table_iterate) {
        temp_vars <- c("SRPP")
      }
      else if ("Total Revenue PP" %in% table_iterate) {
        temp_vars <- c("SLRPP")
      }
      else if ("Median Household Income" %in% table_iterate){
        temp_vars <- c("MHI")
      }
      else if("Median Property Value" %in% table_iterate) {
        temp_vars <- c("MPV")
      }
      else {
        message("Error: Use one or more of the following variables to generate a table:
                     table_vars = c('NCESID', 'Name', 'County', 'Enrollment', 'Poverty Rate', 'Percent Nonwhite', 'Percent FRL',
                     'Local Revenue PP', 'State Revenue PP', 'Total Revenue PP',
                     'MPV', 'MHI')")
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

  select_cols <- if (length(county) > 0) {
    append(select_cols, if_county)
  }
  else {
    select_cols
  }

  if (length(county) > 0) {
    table_data <- masterpull(data_year, data_type = "fin") %>%
      dplyr::mutate(NCESID = stringr::str_pad(NCESID, 7, pad="0"),
                    County = stringr::str_trim(County)) %>%  #make sure that NCESID is properly formatted
      dplyr::filter(State == state) %>%
      dplyr::filter(County %in% county) %>%
      dplyr::select(all_of(select_cols))
  }
  else {
    table_data <- masterpull(data_year, data_type = "fin") %>%
      dplyr::mutate(NCESID = stringr::str_pad(NCESID, 7, pad="0")) %>%  #make sure that NCESID is properly formatted
      dplyr::filter(State == state) %>%
      dplyr::select(all_of(select_cols))
  }


  # format all potential columns
  # percent variables
  table_data <-  if (!is.null(table_data$pctNonwhite)) {

    table_data <- table_data %>%
      dplyr::mutate(pctNonwhite = scales::percent(round2(pctNonwhite, 2), accuracy = 1L))

  } else {
    table_data
  }

  table_data <- if (!is.null(table_data$StPovRate)) {

    table_data <- table_data %>%
      dplyr::mutate(StPovRate = scales::percent(round2(StPovRate, 2), accuracy = 1L ))
  } else {
    table_data
  }

  table_data <- if (!is.null(table_data$dFRL)) {

    table_data <- table_data %>%
      dplyr::mutate(`Percent FRL` = dFRL/dEnroll_district) %>%
      dplyr::mutate(`Percent FRL` = dplyr::if_else(`Percent FRL`>=1, 1, `Percent FRL`)) %>%
      dplyr::mutate(`Percent FRL` = scales::percent(round2(`Percent FRL`, 2), accuracy = 1L )) %>%
      dplyr::select(-dFRL)
  } else {
    table_data
  }

  # currency variables
  table_data <- if (!is.null(table_data$LRPP)) {

    table_data <- table_data %>%
      dplyr::mutate(LRPP = scales::dollar(round2(LRPP, 0)))
  } else {
    table_data
  }

  table_data <- if (!is.null(table_data$SRPP)) {

    table_data <- table_data %>%
      dplyr::mutate(SRPP = scales::dollar(round2(SRPP, 0)))
  } else {
    table_data
  }

  table_data <- if (!is.null(table_data$SLRPP)) {

    table_data <- table_data %>%
      dplyr::mutate(SLRPP = scales::dollar(round2(SLRPP, 0)))
  } else {
    table_data
  }

  table_data <- if (!is.null(table_data$MPV)) {

    table_data <- table_data %>%
      dplyr::mutate(MPV = scales::dollar(round2(MPV, 0)))
  } else {
    table_data
  }

  table_data <- if (!is.null(table_data$MHI)) {

    table_data <- table_data %>%
      dplyr::mutate(MHI = scales::dollar(round2(MHI, 0)))
  } else {
    table_data
  }

  # numeric variables
  table_data <- if (!is.null(table_data$dEnroll_district)) {

    table_data <- table_data %>%
      dplyr::mutate(dEnroll_district = scales::comma(dEnroll_district))
  } else {
    table_data
  }

  search_for_these <- c("NCESID", "State", "NAME",
                        "LRPP", "SRPP", "SLRPP",
                        "County", "dEnroll_district",
                        "pctNonwhite", "StPovRate",
                        "MPV", "MHI", "FRL_rate")


  replace_with_these <- c("NCESID", "State", "Name",
                          "Local revenue, per pupil", "State revenue, per pupil", "Total revenue, per pupil",
                          "County", "Enrollment",
                          "Percent nonwhite", "Poverty rate",
                          "Median property value", "Median household income", "Percent FRL")

  found <- match(colnames(table_data), search_for_these, nomatch = 0)
  colnames(table_data)[colnames(table_data) %in% search_for_these] <- replace_with_these[found]

  NameIdSt = data.frame(col = state)
  NameIdSt$col = substr(NameIdSt$col, 0, 30)

  if (length(county > 0)) {

    ## Split data apart by a grouping variable;
    ##   makes a named list of tables
    dat <- split(table_data, table_data$County)
    dat

    new <- table_data %>%
      dplyr::mutate(County = substr(County, 0, 30))

    dat_name <- split(new, new$County)
    dat_name

    # create formatted excel
    wb = openxlsx::createWorkbook()

    ### Create header format
    col_names = openxlsx::createStyle(wrapText = TRUE, textDecoration = 'Bold', halign = 'center', valign = 'center',
                            border = 'TopBottomLeftRight', fgFill = "#5a8898", fontColour = "white")

    ### Create cell format.
    col_values = openxlsx::createStyle(halign = 'center', valign = 'center', border = 'TopBottomLeftRight')

    ## Loop through the list of split tables as well as their names
    ##   and add each one as a sheet to the workbook
    Map(function(data, name){

      openxlsx::addWorksheet(wb, name)
      openxlsx::writeData(wb, name, data, headerStyle = col_names)
      openxlsx::addStyle(wb, name, style = col_values, rows = 1:nrow(data) + 1, cols = 2:ncol(data),
               gridExpand = TRUE, stack = TRUE)

    }, dat, names(dat))

    wb
    return(wb)

    message("NOTE:: save your workbook to the desired location using:
          openxlsx::saveWorkbook(my_table, file =  '~/Documents/county_state_year.xlsx', overwrite = TRUE)")
  }
  else {

    # create formatted excel
    Workbook_ = openxlsx::createWorkbook()
    sheet_name <- paste0(NameIdSt$col)
    ### make sheet
    openxlsx::addWorksheet(wb = Workbook_, sheetName = sheet_name)
    ### Create header format
    col_names = openxlsx::createStyle(wrapText = TRUE, textDecoration = 'Bold', halign = 'center', valign = 'center', border = 'TopBottomLeftRight',
                            fgFill = "#5a8898", fontColour = "white")
    ### Create cell format.
    col_values = openxlsx::createStyle(halign = 'center', valign = 'center', border = 'TopBottomLeftRight')

    openxlsx::writeData(wb = Workbook_, sheet = sheet_name, x = table_data, headerStyle = col_names)
    openxlsx::addStyle(wb = Workbook_, sheet = sheet_name, style = col_values, rows = 1:nrow(table_data) + 1, cols = 2:ncol(table_data),
             gridExpand = TRUE, stack = TRUE)

    Workbook_
    return(Workbook_)

    message("NOTE:: save your workbook to the desired location using:
          openxlsx::saveWorkbook(my_table, file =  '~/Documents/county_state_year.xlsx', overwrite = TRUE)")

  }
  }
}
