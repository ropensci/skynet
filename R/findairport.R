#' Find Airport function
#'
#' Searches for airport information based on its IATA code or city name
#'
#' @param x airport IATA code or city name
#'
#' @examples
#' \dontrun{
#' findAirport("Atlanta")
#'
#' findAirport("ATL")
#' }
#' @export
#'


findAirport <- function(x){
  filter(airportCode, grepl(x, origin) | grepl (x, city))
}
