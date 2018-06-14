#' Find Carrier function
#'
#' Searches for airport information based on its IATA code or city name
#'
#' @param x Carrier
#'
#' @examples
#' \dontrun{
#' find_carrier("United")
#'
#' find_carrier("UA")
#' }
#' @export
#'


find_carrier <- function(x){
  carriers %>%
    filter(grepl(x, op_carrier) | grepl (x, carrier_name)) %>%
    select(op_carrier, carrier_name, from, to)
}

findCarrier <- function(...){
  warning(paste("findCarrier is deprecated, use find_carrier(), instead."))
  do.call(find_carrier, list(...))
}

globalVariables(c("carriers", "carrier_name"))
