#' International Data
#'
#' Imports International data to complement to the DB1B data set.
#' NOTE: When using this function, certain variables will be skewed as the T100 dataset does not contain
#' all the data the DB1B dataset contains.
#'
#' @param x T-100 International Segment csv file
#' @param m Data set to merge with
#' @param Q Desired T-100 Quarter. Should be equal to 1, 2, 3 or 4.
#'
#' @examples
#' \dontrun{
#'
#' make.netInt(skynet_example("T100_2011_int.csv"), OD_Sample, 1)
#'
#' }
#'
#' @export
#'
#'

make.netInt <- function(x = NULL, m = NULL, Q = NULL){

warning("netInt function is deprecated, please use `import_t100()`,
        generate your desired network and rbind() with the db1b network of choice.")

}

globalVariables(c("ORIGIN", "DEST", "ORIGIN_CITY_MARKET_ID",
                  "DEST_CITY_MARKET_ID", "PASSENGERS", "CARRIER",
                  "origin_city_mkt_id", "dest_city_mkt_id", "quarter",
                  "DEST_CITY_NAME", "ORIGIN_CITY_NAME",
                  "dest_city", "origin_city"))
