#' Generate random mac address
#'
#' This function generates a random mac address. So far no naming conventions
#' are taken into account.
#'
#' @note This feature is only implemented for linux systems
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param n An integer indicating the number of addresses to generate.
#'
#' @examples
#'
#' rmac(3)
#'

rmac <- function(n = 1L){
  bytes <- as.raw(0:255)

  one_mac <- function(){
    paste(sample(bytes, 8L, replace = TRUE), collapse = ":")
  }

  vapply(seq_len(n), function(i) one_mac(), character(1))
}
