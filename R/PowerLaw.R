#' Power Law
#'
#' Plots power law fit
#'
#' @param graph iGraph object
#'
#' @examples
#' \dontrun{
#' netDir <- make.netDir(OD_Sample)
#' fitPlaw(netDir$gDir)
#' }
#' @export
#'
#'
#' @export
#'

fitPlaw <- function(graph) {
  # calculate degree
  d <- degree(graph, mode = "all")
  dd <- degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree <- 1:max(d)
  probability <- dd[-1]
  # delete blank values
  nonzero.position <- which(probability != 0)
  probability <- probability[nonzero.position]
  degree <- degree[nonzero.position]
  reg <- lm(log(probability) ~ log(degree))
  cf <- coef(reg)
  plotnet <- data.frame(probability = probability, degree = degree)
  message(paste("Alpha =",
                round(-cf[[2]], 3)))
  message(paste("R square =",
                round(summary(reg)$r.squared, 3)))
  ggplot(plotnet, aes(y=log(probability), x=log(degree))) +
    geom_point(col = "#56B4E9")+
    geom_smooth(method="lm", se=FALSE, col = "#E69F00")

}


globalVariables(c("cf", "geom_smooth"))
