#' netUnd
#' @export

netUnd <- function(x = netMerged, disp = FALSE, cap = FALSE, merge = TRUE, alpha = 0.003, pct = 10){

  if(grepl("Int", deparse(substitute(x)), ignore.case = TRUE) == TRUE)
    nodes.y = nodesInt
  else
    nodes.y = nodes

  netUnd_all <- x %>%
    select(ORIGIN, DEST, PASSENGERS) %>%
    group_by(ORIGIN, DEST) %>%
    summarise(weight = sum(PASSENGERS))

  # Airport lookup
  air1 <- airportCode %>%
    mutate(ORIGIN_char = as.character(ORIGIN)) %>%
    select(ORIGIN, ORIGIN_char)

  air2 <- airportCode %>%
    mutate(DEST_char = as.character(ORIGIN), DEST = ORIGIN) %>%
    select(DEST, DEST_char)

  gUnd <<- graph_from_data_frame(netUnd_all, directed = TRUE, vertices = nodes.y)
  gUnd <<- as.undirected(gUnd, mode = "collapse", edge.attr.comb=list(weight = "sum"))

    if(disp == TRUE){

    # Run disparity filter

    #Create lookup table
    value_edges <- netUnd_all %>%
      select(ORIGIN, DEST) %>%
      mutate(ORIGIN_char = as.character(ORIGIN), DEST_char = as.character(DEST))

    # Creates igraph object
    gUnd_disp <<- semnet::getBackboneNetwork(gUnd, delete.isolates = T, alpha = alpha)
    netUnd_disp <<- get.data.frame(gUnd_disp)

    # Recode with Factor info
    netUnd_disp <- netUnd_disp %>%
      rename(ORIGIN = from, DEST = to)

    netUnd_disp <- left_join(netUnd_disp, value_edges,
                             by = c("ORIGIN" = "ORIGIN_char", "DEST" = "DEST_char"))

    netUnd_disp <- netUnd_disp %>%
      select(ORIGIN.y, DEST.y, weight, alpha) %>%
      mutate(ORIGIN = ORIGIN.y, DEST = DEST.y, ORIGIN.y = NULL, DEST.y = NULL) %>%
      select(ORIGIN, DEST, weight, alpha)

    gUnd_disp <<- gUnd_disp
    netUnd_disp <<- netUnd_disp

    # ----------------------------------------------------------------------------- #
                           # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else if(cap == TRUE){

    #Run 10% cap
    gUnd_cap <- subgraph.edges(gUnd, which(E(gDir_cap)$weight > quantile(E(gDir_cap)$weight, prob = 1-pct/100)), delete.vertices = TRUE)

    # Create datafram based on collapsed edges graph
    netUnd_cap <- igraph::as_data_frame(gUnd_cap)

    netUnd_cap <- netUnd_cap %>%
      rename(ORIGIN = from, DEST = to)

    netUnd_cap <- netUnd_cap %>%
      left_join(air1,
                by = c("ORIGIN" = "ORIGIN_char")) %>%
      select(ORIGIN.y, DEST, weight) %>%
      rename(ORIGIN = ORIGIN.y) %>%
      left_join(air2,
                by = c("DEST" = "DEST_char")) %>%
      select(ORIGIN, DEST.y, weight) %>%
      rename(DEST = DEST.y)

    netUnd_cap <<- netUnd_cap
    gUnd_cap <<- gUnd_cap

    # ----------------------------------------------------------------------------- #
    # End of 10% filter command #
    # ----------------------------------------------------------------------------- #


  }else if(merge == FALSE){

    # Run undirected with merge
    netUnd_all <- x %>%
      select(ORIGIN, DEST, PASSENGERS) %>%
      group_by(ORIGIN, DEST) %>%
      summarise(weight = sum(PASSENGERS))

    gUnd <<- graph_from_data_frame(netUnd_all, directed = FALSE, vertices = nodes.y)
    netUnd_all <<- netUnd_all

  }else{

    # Create datafram based on collapsed edges graph
    netUnd_all <- igraph::as_data_frame(gUnd)

    netUnd_all <- netUnd_all %>%
      rename(ORIGIN = from, DEST = to)


    netUnd_all <- netUnd_all %>%
      left_join(air1,
                by = c("ORIGIN" = "ORIGIN_char")) %>%
      select(ORIGIN.y, DEST, weight) %>%
      rename(ORIGIN = ORIGIN.y) %>%
      left_join(air2,
                by = c("DEST" = "DEST_char")) %>%
      select(ORIGIN, DEST.y, weight) %>%
      rename(DEST = DEST.y)



      ########
#      mutate(Col1 = if_else(is.na(ORIGIN.y),ORIGIN.y.y, ORIGIN.y),
#             Col2 = if_else(is.na(DEST.y),DEST.y.y, DEST.y)) %>%
#      select(Col1, Col2, weight) %>%
#      rename(ORIGIN = Col1, DEST = Col2)
###########


    netUnd_all <<- netUnd_all

  }
}


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
                            # End of netUnd command #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

