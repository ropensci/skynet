#' Find Carrier function
#'
#' Searches for airport information based on its IATA code or city name
#'
#' @param x Carrier
#'
#' @examples
#' \dontrun{
#' findCarrier("United")
#'
#' findCarrier("UA")
#' }
#' @export
#'


findCarrier <- function(x){
  carriers %>%
    filter(grepl(x, op_carrier) | grepl (x, carrier_name)) %>%
    select(op_carrier, carrier_name, from, to)
}

globalVariables(c("carriers", "carrier_name"))
