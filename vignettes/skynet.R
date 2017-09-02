## ---- eval=FALSE---------------------------------------------------------
#   library(skynet)
#   net.Import("/folder1/Coupon 2016Q1.csv", "/folder/Ticket 2016Q1.csv")

## ---- echo=FALSE, message=FALSE, warning=FALSE, results='asis'-----------
knitr::kable(data.frame(Coupon = c("ITIN_ID", "MKT_ID", "SEQ_NUM", "ORIGIN_CITY_MARKET_ID",  
"ORIGIN", "YEAR", "QUARTER", "DEST_CITY_MARKET_ID", "DEST", "TRIP_BREAK", "OPERATING_CARRIER", 
"DISTANCE", "GATEWAY"),
Ticket = c("ITIN_ID", "ROUNDTRIP", "ITIN_YIELD", "PASSENGERS",
"ITIN_FARE", "BULKFARE", "DISTANCE_FULL","","","","","","")))

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(data.frame(origin = c("ABI", "ABI", "ADK", "ADQ", "AKN"),
dest = c("ABQ", "AMA", "ANC", "ANC", "ANC"),
passengers = c(2, 4, 88, 1768, 55),
fare_sd = c(13.79, 12.525, 411.815, 293.44, 338.4),
itin_fare = c(236.25, 262.5, 427.69, 329.64, 482.265),
itin_yield = c(0.19, 0.695, 0.27, 0.36,0.265),
origin_city_mkt_id = c(30136, 30136, 30165, 30070, 30245),
origin_city = c("Abilene, TX", "Abilene, TX", "Adak Island, AK", "Kodiak, AK", "King Salmon, AK"),
dest_city_mkt_id = c(30140, 30279, 30299, 30299, 30299),
dest_city = c("Albuquerque, NM", "Amarillo, TX", "Anchorage, AK", "Anchorage, AK", "Anchorage, AK"))
)

