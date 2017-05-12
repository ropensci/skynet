#' International Data
#'
#' Imports International data to complement to the DB1B data set.
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
      select(ORIGIN, DEST, ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID,
         PASSENGERS, QUARTER) %>%
      filter(PASSENGERS > 0, QUARTER == Q) %>%
      select(ORIGIN, DEST, ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID, PASSENGERS)

 # Merges netMerged with international filter

  netMergedInt <- m %>%
    select(ORIGIN, DEST, ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID, PASSENGERS)

  netMergedInt <- rbind(netMergedInt, IntFilter)

  assign(paste(deparse(substitute(m)), "Int", sep = "_"), netMergedInt, .GlobalEnv)

# airLookup <- airportCode %>%
#  select(ORIGIN) %>%
#  mutate(ORIGIN_chr = as.character(ORIGIN))

# netMergedInt <- left_join(netMergedInt, airLookup, by = c("ORIGIN" = "ORIGIN_chr"))
# netMergedInt <- left_join(netMergedInt, airLookup, by = c("DEST" = "ORIGIN_chr"))

# netMergedInt <- netMergedInt %>%
#  select(ORIGIN.y, ORIGIN.y.y, PASSENGERS) %>%
#  rename(ORIGIN = ORIGIN.y, DEST = ORIGIN.y.y)


# Creates nodes for netInt

nodesTemp <- netMergedInt %>%
  data.frame() %>%
  group_by(DEST) %>%
  summarize(PASSENGERS = sum(PASSENGERS)) %>%
  rename(ORIGIN = DEST)

nodesInt <- netMergedInt %>%
  select(ORIGIN, PASSENGERS) %>%
  group_by(ORIGIN) %>%
  summarize(PASSENGERS = sum(PASSENGERS))

nodesInt <- nodesInt %>%
  merge(nodesTemp, by = "ORIGIN", all = TRUE) %>%
  mutate(PASSENGERS.x = replace(PASSENGERS.x, is.na(PASSENGERS.x), 0),
         PASSENGERS.y = replace(PASSENGERS.y, is.na(PASSENGERS.y), 0),
         freq = (PASSENGERS.x + PASSENGERS.y)) %>%
  select(ORIGIN, freq) %>%
 # merge(airLookup, by = "ORIGIN", all.x = TRUE) %>%
  merge(airportCode, by = "ORIGIN", all.x = TRUE) %>%
  select(ORIGIN, freq, Latitude, Longitude)

assign("nodesInt", nodesInt, .GlobalEnv)

}
}
