#' Get path to skynet examples
#'
#' To access csv examples from SKYNET
#'
#' @param path File name.
#'
#' @examples
#' \dontrun{
#' skynet_example()
#' skynet_example("Coupon 2001Q1.csv")
#' }
#' @export
#'

skynet_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "skynet"))
  } else {
    system.file("extdata", path, package = "skynet", mustWork = TRUE)
  }
}
