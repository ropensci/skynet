#' netInt
#' Imports International data
#' @export

netInt <- function(x = NULL, Q = NULL){

  if(is.null(Q))
    stop("Please select desired Quarter")
  if(!exists("netMerged"))
    stop("Please import BTS data first with netImport")
  if(!is.null(x))

# International option
International <<- fread(x, header = TRUE, sep = ",", stringsAsFactors = TRUE,
      integer64 = "numeric")

  else
# Create Filter
InterFilter <- International %>%
  select(ORIGIN, DEST, ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID,
         PASSENGERS, YEAR, QUARTER) %>%
  filter(PASSENGERS > 0, QUARTER == Q) %>%
  select(ORIGIN, DEST, ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID, PASSENGERS)

# Merges netMerged with international filter
netMergedInt <- netMerged %>%
  select(ORIGIN, DEST, ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID, PASSENGERS) %>%
  rbind(InterFilter) %>%
  mutate(ORIGIN = as.character(ORIGIN), DEST = as.character(DEST))

airLookup <- airportCode %>%
  select(ORIGIN) %>%
  mutate(ORIGIN_chr = as.character(ORIGIN))

netMergedInt <- left_join(netMergedInt, airLookup, by = c("ORIGIN" = "ORIGIN_chr"))
netMergedInt <- left_join(netMergedInt, airLookup, by = c("DEST" = "ORIGIN_chr"))

netMergedInt <- netMergedInt %>%
  select(ORIGIN.y, ORIGIN.y.y, PASSENGERS) %>%
  rename(ORIGIN = ORIGIN.y, DEST = ORIGIN.y.y)

netMergedInt <<- netMergedInt

# Creates nodes for netInt

nodesTemp <- netMergedInt %>%
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
  merge(airLookup, by = "ORIGIN", all.x = TRUE) %>%
  merge(airportCode, by = "ORIGIN", all.x = TRUE) %>%
  select(ORIGIN, freq, Latitude, Longitude)

nodesInt <<- nodesInt
}
