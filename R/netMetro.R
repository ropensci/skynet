#netMetro
netMetro <- function(x = netMerged){
  netMet <- x %>%
    select(ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID, PASSENGERS) %>%
    group_by(ORIGIN_CITY_MARKET_ID, DEST_CITY_MARKET_ID) %>%
    summarise(weight = sum(PASSENGERS)) %>%
    rename(ORIGIN = ORIGIN_CITY_MARKET_ID, DEST = DEST_CITY_MARKET_ID)

  netMet <<- netMet
}
