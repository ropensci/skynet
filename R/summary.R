#' Displays a summary of a skynet object
#'
#' @param object skynet object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @method summary skynet
#'
#'
#' @export
#' @examples
#' net <- make_net_dir(OD_Sample)
#' summary(net)

summary.skynet <- function(object, ...){

  output <- paste("Skynet Object:", "\n",
                  "Year:", paste0(t(distinct(object[[2]]["year"])),
                                  collapse = ", "), "\n",
                  "Quarter:", paste0(t(distinct(object[[2]]["quarter"])),
                                  collapse = ", "), "\n",
                  "Number of vertices/airports:", vcount(object[[1]]), "\n",
                  "Number of edges/routes:", ecount(object[[1]]), "\n")

  cat(output)
}
