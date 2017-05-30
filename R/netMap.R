#' netMap
#'
#'
#' library(maps)
#' library(ggrepel)
#' library(geosphere)

makemap <- function(net, nodes){

  # Create map

  map("world", c("USA", "Hawaii", "Puerto Rico"), xlim = c(-170, -65), ylim = c(16, 72),
      col="lightgrey", fill=TRUE, bg="white", lwd=0.1, mar = c(0,2,0,2))


  # Network A
  # Edge Colors
  col.1 <- adjustcolor("steelblue3", alpha=0.4)
  col.2 <- adjustcolor("steelblue3", alpha=0.4)
  edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
  edge.col <- edge.pal(100)


  # Create edges

  for(i in 1:nrow(net))  {
    node1 <- nodes[nodes$ORIGIN == net[i,]$ORIGIN,]
    node2 <- nodes[nodes$ORIGIN == net[i,]$DEST,]

    arc <- geosphere::gcIntermediate( c(node1[1,]$Longitude, node1[1,]$Latitude),
                                      c(node2[1,]$Longitude, node2[1,]$Latitude),
                                      n=1000, addStartEnd=TRUE)
    edge.ind <- round(100*net[i,]$weight / max(net$weight))

    lines(arc, col=edge.col[edge.ind], lwd=edge.ind/40)


  }


  text(nodes$Longitude[nodes$freq > quantile(nodes$freq, prob = .96)], nodes$Latitude[nodes$freq > quantile(nodes$freq, prob = .96)],
       nodes$ORIGIN[nodes$freq > quantile(nodes$freq, prob = .96)],cex=.5, adj=0, pos=2, offset=0.15 ,col="black")

  points(x=nodes$Longitude, y=nodes$Latitude, pch=21,
         cex=nodes$freq/(17^5), col="black", bg = adjustcolor("royalblue", alpha=0.7), lwd = .1)

}
