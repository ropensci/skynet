#' Find Airport function
#'
#' @export
#'


findAirport <- function(x){
  filter(airportCode, grepl(x, origin) | grepl (x, city))
}
