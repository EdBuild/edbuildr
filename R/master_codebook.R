#' A function to view EdBuild's data sources and variable names
#'
#' This function imports a codebook for EdBuild's master data. The
#' codebook tells the user what each variable name represents and the source of
#' each variable.
#' @keywords master data codebook EdBuild F33 CCD SAIPE EDGE
#' @return A dataframe where each observation is a variable from the master dataset.
#' @usage master_codebook()
#' @import magrittr
#' @importFrom utils read.csv
#' @seealso \link{long_masterpull}, \code{\link{masterpull}}
#' @export
#' @examples
#' codebook <- master_codebook()



master_codebook = function() {

  url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/master_codebook.csv"
  codebook <- read.csv(file = url, stringsAsFactors = FALSE)
  return(codebook)
}
