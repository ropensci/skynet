#' Find Airport function
#'
#' Searches for airport information based on its IATA code or city name
#' It will display multiple airports as it works with partial names.
#'
#' @param x airport IATA code or city name
#'
#' @examples
#' \dontrun{
#' find_airport("Atlanta")
#'
#' find_airport("ATL")
#' }
#' @export
#'


find_airport <- function(x){
  filter(airportCode, grepl(x, origin) | grepl (x, city))
}

findAirport <- function(...){
  warning(paste("findAirport is deprecated, use find_airport(), instead."))
  do.call(find_airport, list(...))
}

globalVariables(c("airportCode", "origin", "city"))
