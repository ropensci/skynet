#' From To function
#'
#' Calculate edges weight from IATA Code
#'
#' @param x igraph object to query
#' @param y origin airport IATA code
#' @param orig "from" or "to" options
#'
#' @return A numeric value with the sum of the weights (passengers) of the edges
#' leaving (\code{orig = "from"}) or arriving at (\code{orig = "to"}) airport \code{y}.
#'
#' @examples
#' netDir <- make_net_dir(OD_Sample)
#' from_to_stats(netDir$gDir, "JFK", orig = "from")
#'
#' from_to_stats(netDir$gDir, "JFK", orig = "to")
#'
#'
#' @export
#'


from_to_stats <- function(x, y, orig){

  orig <- as.character(orig)

  if(orig == "from")
    sum(E(x)[.from(V(x)[y])]$weight)

  else
    sum(E(x)[.to(V(x)[y])]$weight)
}

fromto.stat <- function(...){
  warning(paste("fromto.stat is deprecated, use from_to_stats(), instead."))
  do.call(from_to_stats, list(...))
}

globalVariables(c("from", "to"))
