#' skynet: Network analysis for BTS Data
#'
#' Creates networks from the BTS/Transtats data
#'
#' Given the DB1BCoupon and DB1BTicket, or the T-100 csv's exported
#' this package allows creating sociomatrixes and subsequent igraph graphs.
#' @author Filipe Teixeira
#' @references
#' NA
#' @examples
#' NA
#'
#' @keywords internal
#'
#'
#' @importFrom data.table data.table fread
#' @importFrom dplyr rename summarize mutate group_by select summarise left_join
#' @importFrom ggplot2 borders geom_curve ggplot
#' @importFrom ggplot2 scale_size_continuous geom_point coord_cartesian theme aes
#'
"_PACKAGE"
