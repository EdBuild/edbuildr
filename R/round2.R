#' A function to round 0.5 down
#'
#' This function rounds values based on mathematical rules. That is, numbers
#' ending below 0.5 round down and numbers ending with 0.5 and greater round up.
#' @keywords round
#' @usage round2(x, n)
#' @param x a numeric vector
#' @param n number of decimal places
#' @return The value returned is a numeric vector, x, rounded to n decimals
#'   places.
#' @export
#' @references
#'   \url{https://stackoverflow.com/questions/12688717/round-up-from-5}
#' @examples
#' round2(143.05, 1)
#' round2(143.048, 2)

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
