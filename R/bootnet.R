#' Network bootstrapping
#'
#' Bootstraps a network and returns output containing three network statistics:
#' Average Path Length, Transitivity, Mean Betweenness.
#'
#' @param g iGraph graph.
#' @param n Number of bootstraps to run. (500 default)
#' @param left_ci Confidence interval left limit. (0.005 default)
#' @param right_ci Confidence interval left limit (0.995 default)
#'
#' @examples
#' \dontrun{
#' bootnet(g, n = 500)
#' 
#' }
#' @export
#'


bootnet <- function(g, n = 500, left_ci = 0.005, right_ci = 0.995){
      avg <- c() # Creates empty vector for output
      trv <- c()
      btw <- c()
  for(i in 1:n){
    gBoot <- g %>%
      #rewires graph g 10 times the amount of edges in g, keeping the degree distribution.
      rewire(keeping_degseq(loops = FALSE, niter = ecount(g)*10))
    
    # Runs and saves statistics of interest
    avg[i] <- average.path.length(gBoot)
    trv[i] <- transitivity(gBoot)
    btw[i] <- mean(betweenness(gBoot)) 
  }
      # Calculates mean values and CIs
      avg <- c(quantile(avg, left_ci), quantile(avg, right_ci),
                        mean_random = mean(avg),
                        mean_empirical = average.path.length(g))
      
      trv <- c(quantile(trv, left_ci), quantile(trv, right_ci),
                 mean_random = mean(trv),
                 mean_empirical = transitivity(g))
      
      btw <- c(quantile(btw, left_ci), quantile(btw, right_ci),
               mean_random = mean(btw),
               mean_empirical = mean(betweenness(g)))
      
      bootstats <- t(data.frame(average.path.length = avg,
                                transitivity = trv,
                                betweenness = btw))
      bootstats <- as.data.frame(bootstats, col.names = colnames(bootstats))
      
      return(bootstats)
}
