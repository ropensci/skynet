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
#' netInt(skynet_example("T100_2011_int.csv"), OD_Sample, 1)
#'
#' }
#'
#' @export
#'

make.netInt <- function(x = NULL, m, Q = NULL){

  if(is.null(Q))
    stop("Please select desired Quarter")
  if(is.null(x))
    stop("Please select T-100 International Segment file")

# International option
  International <- fread(x, header = TRUE, sep = ",", stringsAsFactors = FALSE,
      integer64 = "numeric")

  International <- International %>%
    filter(CLASS == "F" | CLASS == "L", PASSENGERS > 0, QUARTER == Q) %>%
    select(ORIGIN, ORIGIN_CITY_MARKET_ID, DEST, DEST_CITY_MARKET_ID,
           PASSENGERS, ORIGIN_CITY_NAME, DEST_CITY_NAME) %>%
    rename(origin = ORIGIN, dest = DEST, passengers = PASSENGERS,
           origin_city_mkt_id = ORIGIN_CITY_MARKET_ID,
           origin_city = ORIGIN_CITY_NAME,
           dest_city_mkt_id = DEST_CITY_MARKET_ID,
           dest_city = DEST_CITY_NAME) %>%
    mutate(itin_fare = NA, itin_yield = NA, fare_sd = NA,
           passengers = as.integer(passengers)) %>%
    select(origin, dest, passengers, fare_sd, itin_fare, itin_yield,
           origin_city_mkt_id, origin_city, dest_city_mkt_id, dest_city)

  m <- as.data.frame(m)
  netMergedInt <- rbind(m, International)

  return(netMergedInt)

}

globalVariables(c("ORIGIN", "DEST", "ORIGIN_CITY_MARKET_ID",
                  "DEST_CITY_MARKET_ID", "PASSENGERS", "CARRIER",
                  "origin_city_mkt_id", "dest_city_mkt_id", "quarter",
                  "DEST_CITY_NAME", "ORIGIN_CITY_NAME",
                  "dest_city", "origin_city"))
