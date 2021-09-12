#' Euclidean algorithm
#'
#' @param a An integer
#' @param b An integer
#' @return The greatest common divisor of \code{a} and \code{b}
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export
euclidean <- function(a, b) {
  while(b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}