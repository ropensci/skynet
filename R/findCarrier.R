#' Find Carrier function
#'
#' Searches for airport information based on its IATA code or city name
#'
#' @param x Carrier
#'
#' @return A data frame with the carriers whose code or name matches \code{x}:
#' \code{op_carrier} (carrier code), \code{carrier_name}, and \code{from} and
#' \code{to}, the years in which the code was used by that carrier.
#'
#' @examples
#' find_carrier("United")
#'
#' find_carrier("UA")
#'
#' @export
#'


find_carrier <- function(x){
  skynet::carriers %>%
    filter(str_detect(op_carrier, regex(x, ignore_case = TRUE)) |
             str_detect(carrier_name, regex(x, ignore_case = TRUE))) %>%
    select(op_carrier, carrier_name, from, to)
}

findCarrier <- function(...){
  warning(paste("findCarrier is deprecated, use find_carrier(), instead."))
  do.call(find_carrier, list(...))
}

globalVariables(c("carriers", "carrier_name"))
