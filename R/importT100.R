#' Import T-100 Data
#'
#' Imports T-100 Data directly from zipped file
#' Please try to name original csv as e.g. "T100 2011Q1mkt.csv" or "T100 2011Q1seg.csv"
#' respectively
#'
#' @param x T100 csv
#'
#' @examples
#' NA
#'
#' @export
#'


importT100 <- function(x){

  T100 <- fread(x, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                integer64 = "numeric")

  if(exists(x["AIRCRAFT_CONFIG"])){

    # Segment
    T100 <- T100 %>%
      filter(CLASS == "F" | CLASS == "L", PASSENGERS > 0) %>%
      select(ORIGIN, ORIGIN_CITY_MARKET_ID, DEST, DEST_CITY_MARKET_ID, UNIQUE_CARRIER,
             PASSENGERS, QUARTER, YEAR, DISTANCE) %>%
      rename(origin_mkt_id = ORIGIN_CITY_MARKET_ID, origin = ORIGIN, year = YEAR, quarter = QUARTER,
             dest_mkt_id = DEST_CITY_MARKET_ID , dest = DEST,
             op_carrier = UNIQUE_CARRIER, distance = DISTANCE, passengers = PASSENGERS) %>%
      mutate(itin_fare = NA, itin_yield = NA, roundtrip = NA)

    T100 <- data.frame(T100)

    #extracts name from file
    filename <- gsub(" ", "",
                     tools::file_path_sans_ext(
                       basename(x)))
    assign(paste(filename), T100, envir = envir)

  }else{

    #Market
    T100 <- T100 %>%
      filter(CLASS == "F" | CLASS == "L", PASSENGERS > 0) %>%
      select(ORIGIN, ORIGIN_CITY_MARKET_ID, DEST, DEST_CITY_MARKET_ID, UNIQUE_CARRIER,
             PASSENGERS, QUARTER, YEAR, DISTANCE) %>%
      rename(origin_mkt_id = ORIGIN_CITY_MARKET_ID, origin = ORIGIN, year = YEAR, quarter = QUARTER,
             dest_mkt_id = DEST_CITY_MARKET_ID , dest = DEST,
             op_carrier = UNIQUE_CARRIER, distance = DISTANCE, passengers = PASSENGERS) %>%
      mutate(itin_fare = NA, itin_yield = NA, roundtrip = NA)

    T100 <- data.frame(T100)

    #extracts name from file
    filename <- gsub(" ", "",
                     tools::file_path_sans_ext(
                       basename(x)))
    assign(paste(filename), T100, envir = envir)

  }
}


# For CRAN submission
pos = 1
envir = as.environment(pos)
