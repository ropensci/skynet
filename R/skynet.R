#' skynet: Network analysis for BTS Data
#'
#' Creates networks from the BTS/Transtats data
#'
#' Given the DB1BCoupon and DB1BTicket, or the T-100 csv's exported
#' this package allows creating data frames and subsequent igraph graphs.
#' @author Filipe Teixeira
#' @references
#' NA
#' @examples
#' NA
#'
#' @keywords internal
#'
#'
#' @import igraph
#' @importFrom data.table data.table fread as.data.table .N :=
#' @importFrom dplyr rename summarize mutate group_by select summarise
#' @importFrom dplyr left_join %>% filter arrange mutate_all funs
#' @importFrom dplyr everything distinct ungroup
#' @importFrom ggplot2 borders geom_curve ggplot element_blank element_rect geom_smooth
#' @importFrom ggplot2 scale_size_continuous geom_point coord_cartesian theme aes
#' @importFrom graphics curve plot
#' @importFrom stats coef lm
#' @importFrom utils write.csv globalVariables download.file unzip
#'
"_PACKAGE"
