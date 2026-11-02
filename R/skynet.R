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
#' @importFrom dplyr left_join %>% filter arrange across
#' @importFrom dplyr everything distinct ungroup as_tibble
#' @importFrom ggplot2 annotation_borders ggplot element_blank element_rect geom_smooth
#' @importFrom ggplot2 scale_linewidth_continuous geom_point theme aes geom_text
#' @importFrom ggplot2 geom_path scale_colour_manual guide_legend scale_alpha_continuous
#' @importFrom ggplot2 scale_size_area coord_sf coord_quickmap
#' @importFrom graphics curve plot
#' @importFrom stats coef lm quantile
#' @importFrom utils write.csv globalVariables download.file unzip head
#' @importFrom httr POST progress GET
#' @importFrom geosphere gcIntermediate
#' @importFrom leaflet leaflet addProviderTiles addPolylines addCircleMarkers
#' @importFrom leaflet addLegend highlightOptions
#' @import stringr
"_PACKAGE"
