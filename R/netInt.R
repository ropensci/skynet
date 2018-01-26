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
#' @export
#'

make.netInt <- function(x = NULL, m, Q = NULL){

  if(is.null(Q))
    stop("Please select desired Quarter")
  if(!is.null(x)){

# International option
  International <- fread(x, header = TRUE, sep = ",", stringsAsFactors = FALSE,
      integer64 = "numeric")

  International <- International %>%
    select(ORIGIN, DEST, ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID,
           PASSENGERS, CARRIER, QUARTER, YEAR) %>%
    rename(origin = ORIGIN ,dest = DEST ,origin_mkt_id = ORIGIN_CITY_MARKET_ID,
           dest_mkt_id = DEST_CITY_MARKET_ID, passengers = PASSENGERS,
           op_carrier = CARRIER, year = YEAR, quarter = QUARTER)

 }else{

   # Create Filter
   IntFilter <- International %>%
      filter(passengers > 0, quarter == Q)

 # Merges netMerged with international filter

  netMergedInt <- m %>%
    select(origin, dest, origin_mkt_id, dest_mkt_id, passengers)

  netMergedInt <- rbind(netMergedInt, IntFilter)
  attr(netMergedInt, "status") <- "int"

  return(netMergedInt)

}
}

globalVariables(c("ORIGIN", "DEST", "ORIGIN_CITY_MARKET_ID", "DEST_CITY_MARKET_ID",
                  "PASSENGERS", "CARRIER", "origin_mkt_id", "dest_mkt_id", "quarter"))
