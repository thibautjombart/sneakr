#' List networking interfaces
#'
#' This function generates a random mac address. So far no naming conventions
#' are taken into account.
#'
#' @note This feature is only implemented for linux systems and requires 'ip'.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @examples
#'
#' rmac(3)
#'
ifconfig <- function(fields = c("inet", "inet6", "ether", "broadcast")){

  ## We use 'ifconfig' system call to get the interfaces info and discard the
  ## one on 'lo'.

  info <- system("ifconfig", intern = TRUE)
  info <- info[-length(info)] # remove last line
  split_at <- which(info=="")


  get_field <- function(what, where) {
    temp <- unlist(strsplit(where, " "))
    match_posi <- match(what, temp)
    if (length(match_posi) > 0) {
      return(temp[match_posi + 1])
    } else {
      return(NULL)
    }
  }


  process_entry <- function(begin, end){
    txt <- info[begin:end]

    out <- lapply(fields, get_field, txt)
    names(out) <- fields
    out <- out[!sapply(out, is.na)]

    out$name <- sub(":.*", "", txt[1])

    return(out)
  }


  ## find where to start and stop reading info
  N <- length(split_at) + 1
  from <- c(1, split_at + 1)
  to <- c(split_at, length(info))

  out <- list()
  for (i in 1:N) {
    out[[i]] <- process_entry(from[i], to[i])
    names(out)[i] <- out[[i]]$name
    out[[i]]$name <- NULL
  }

  return(out)
}
