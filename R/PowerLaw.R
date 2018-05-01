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
  cozf <- coef(reg)
  power.law.fit <- function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  alpha <- -cozf[[2]]
  R.square <- summary(reg)$r.squared
  print(paste("Alpha =", round(alpha, 3)))
  print(paste("R square =", round(R.square, 3)))
  # plot
  plot(probability ~ degree, log = "xy",
       xlab = "Degree (log)", ylab = "Probability (log)",
       col = 1, main = "Degree Distribution")
  curve(power.law.fit, col = "red", add = TRUE, n = length(d))
}
