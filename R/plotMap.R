#' netMap
#'
#' Creates OD ggplot2 generated maps from make.net functions
#' Shows sample of 60% of flights
#'
#' @param x Data frame
#' @param pct percentage of edges to include
#'
#' @examples
#' \dontrun{
#' network <- make.netDir(OD_Sample)
#' netMap(network$netDir, pct = 10)
#' }
#' @export
#'

netMap <- function(x, pct = 60){

  if(nchar(x[[1]][1]) == 5){
    airports <- select(MetroLookup, origin, latitude, longitude)
    nodes <- x %>%
      nodeStatsMetro() %>%
      rename(origin = airport)

    x <- x %>%
      filter(origin != dest)

    }else{
      airports <- select(airportCode, origin, latitude, longitude)
      nodes <- x %>%
        nodeStats() %>%
        select(airport, latitude, longitude, freq) %>%
        rename(origin = airport) %>%
        mutate(description = origin)
      }
  #-----------------------------------------------------


  # Merges flights and airport data for Latitude/Longitude info

  flights <- merge(x, airports, by = "origin")
  if(!is.null(x[["op_carrier"]])){
    flights <- select(flights, origin, dest,
                      passengers, latitude, longitude, op_carrier)
  }else{
    flights <- select(flights, origin, dest,
                      passengers, latitude, longitude)
  }
  flights <- merge(flights, airports, by.x = "dest", by.y = "origin")

  #-----------------------------------------------------


  # Create World and Plot Map

  worldmap <- borders("world", colour="lightgrey",
                      fill="lightgrey") # create a layer of borders

  # Chooses color for carriers when present

  if(is.null(x[["op_carrier"]])){
   gcurve <- geom_curve(data=flights[flights$passengers > quantile(flights$passengers,
                                                                   prob = 1-(pct/100)),],
               aes(x = longitude.x, y = latitude.x, xend = longitude.y,
                   yend = latitude.y , size = passengers),
               alpha = .6, curvature = .2, color = "royalblue")
  }else{
    gcurve <- geom_curve(data=flights[flights$passengers > quantile(flights$passengers,
                                                                    prob = 1-(pct/100)),],
               aes(x = longitude.x, y = latitude.x, xend = longitude.y,
                   yend = latitude.y , size = passengers, col = op_carrier),
               alpha = .6, curvature = .2)}

    #-----------------------------------------------------

  ggplot() + worldmap + gcurve +
    scale_size_continuous(range = c(.0001,0.6)) +
    geom_point(data = nodes[nodes$freq > quantile(flights$passengers, prob = 1-0.6),],
               aes(x = longitude, y = latitude), col = "royalblue", alpha = .8,
               size = .1) +
    ggrepel::geom_text_repel(data = nodes[nodes$freq > quantile(nodes$freq,
                                                                prob = 1-4/100),],
                             aes(x = longitude, y = latitude,
                label = description), col = "black", size = 2,
                segment.color = NA, fontface = "bold") +
    coord_cartesian(xlim = c(-160, -65), ylim = c(16, 65)) +
    theme(panel.background = element_rect(fill="white"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")


}

globalVariables(c("latitude", "longitude",
                  "latitude.y", "gcurve", "airport"))
