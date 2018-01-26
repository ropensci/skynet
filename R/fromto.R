#' From To function
#'
#' Calculate edges weight from IATA Code
#'
#' @param x igraph object to query
#' @param y origin airport IATA code
#' @param orig "from" or "to" options
#'
#' @examples
#' \dontrun{
#' netDir <- make.netDir(OD_Sample)
#' fromto.stat(netDir$gDir, "JFK", orig = "from")
#'
#' fromto.stat(netDir$gDir, "JFK", orig = "to")
#' }
#'
#' @export
#'


fromto.stat <- function(x, y, orig){

  orig <- as.character(orig)

  if(orig == "from")
    sum(E(x)[from(V(x)[y])]$weight)

  else
    sum(E(x)[to(V(x)[y])]$weight)
}

globalVariables(c("from", "to"))
