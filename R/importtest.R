#' Import T-100 Data
#'
#' #Don't forget to add information on segment or market (to test)
#'
#' @param x T100 csv
#'
#' @examples
#' \dontrun{
#' importT100(x)
#' }
#' @export
#'


importT100 <- function(x){

  t100 <- fread(x, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                integer64 = "numeric")

  t100 <- t100 %>%
    filter(AIRCRAFT_CONFIG == 1 | AIRCRAFT_CONFIG == 3, PASSENGERS > 0) %>%
    select(ORIGIN, ORIGIN_CITY_MARKET_ID, DEST, DEST_CITY_MARKET_ID, UNIQUE_CARRIER,
           PASSENGERS, QUARTER, YEAR, DISTANCE) %>%
    rename(origin_mkt_id = ORIGIN_CITY_MARKET_ID, origin = ORIGIN, year = YEAR, quarter = QUARTER,
           dest_mkt_id = DEST_CITY_MARKET_ID , dest = DEST,
           op_carrier = UNIQUE_CARRIER, distance = DISTANCE, passengers = PASSENGERS) %>%
    mutate(itin_fare = NA, itin_yield = NA, roundtrip = NA)

  filename <- gsub(" ", "",
                   tools::file_path_sans_ext(
                     basename(x)))
  assign(paste(filename), t100, envir = envir)

}

# For CRAN submission
pos = 1
envir = as.environment(pos)
