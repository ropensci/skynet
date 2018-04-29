#' Import Data
#'
#' Imports data from BTS/RITA/Transtats website
#' File order doesn't matter, but it is recommended to name the files using the following
#' syntax: \emph{"Coupon YearQuarter.csv", "Ticket YearQuarter.csv", "T100 Year".}
#' Note: We do recommend sparklyr to be used for larger sets of data.
#'
#' @param x First csv file to be imported, in case of DB1B database, or in case of using
#' the T-100 database, the only file to be included.
#' @param y Second csv file to be imported.
#' @param zip Should equal TRUE if original file comes from the BTS prezipped option.
#' @param nonsch Should equal TRUE to include non-scheduled flights
#' @examples
#' \dontrun{
#'
#' netImport(skynet_example("Coupon_2001Q1.csv"), skynet_example("Ticket_2001Q1.csv"))
#'
#' }
#' @export
#'
#'

netImport <- function(x = NULL){
  warning("netImport function is deprecated, please use `import_db1b()`, or `import_t100()`.")
}
