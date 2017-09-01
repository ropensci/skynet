#' "From To" function
#'
#' Calculate edges weight form Airport IATA code
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
