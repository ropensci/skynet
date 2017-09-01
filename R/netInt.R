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
#' make.netInt("T100 2016", OD_2016Q1, 1)
#'
#' @export
#'

make.netInt <- function(x = NULL, m, Q = NULL){

  if(is.null(Q))
    stop("Please select desired Quarter")
  if(!is.null(x)){

# International option
  International <- fread(x, header = TRUE, sep = ",", stringsAsFactors = TRUE,
      integer64 = "numeric")

 }else{

   # Create Filter
   IntFilter <- International %>%
      select(origin, dest, origin_city_mkt_id, dest_city_mkt_id,
         passengers, quarter) %>%
      filter(passengers > 0, quarter == Q) %>%
      select(origin, dest, origin_city_mkt_id, dest_city_mkt_id, passengers)

 # Merges netMerged with international filter

  netMergedInt <- m %>%
    select(origin, dest, origin_city_mkt_id, dest_city_mkt_id, passengers)

  netMergedInt <- rbind(netMergedInt, IntFilter)


  return(netMergedInt)

}
}
