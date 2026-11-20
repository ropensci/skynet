#' Get path to skynet examples
#'
#' To access csv examples from SKYNET
#'
#' @param path File name.
#'
#' @return A character vector with the names of the example files when
#' \code{path} is \code{NULL}, otherwise the full path to the requested file.
#'
#' @examples
#' skynet_example()
#' skynet_example("Coupon_2001Q1.csv")
#'
#' @export
#'

skynet_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "skynet"))
  } else {
    system.file("extdata", path, package = "skynet", mustWork = TRUE)
  }
}
