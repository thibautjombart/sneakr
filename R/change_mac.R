#' Change mac address of an interface with a random one
#'
#'
#' @param x The name of an interface to be changed.
#'
change_mac <- function(x) {

  new_mac <- rmac()
  system(paste("sudo ifconfig", x, "down"))
  system(paste("sudo ifconfig", x, "hw ether", new_mac()))
  system(paste("sudo ifconfig", x, "up"))
}
